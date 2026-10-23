"""Deterministic pathway grouping and lipid-class mapping views (no LLM)."""
import argparse
import csv
import json
import sqlite3
from collections import Counter, defaultdict
from contextlib import closing
from pathlib import Path
from rdflib import Graph, Literal, URIRef, RDF
import pathway_knowledge as kb
from build_lipid_reaction_catalog import csv_table


def sides(row):
    return {s: {c['canonical_class'] for c in row.get('classes', []) if c['side_code'] == s} for s in ('L','R')}


def make_groups(folder):
    folder = Path(folder)
    rows = kb.read_json(folder/'candidates.json')
    report = kb.read_json(folder/'report.json')
    pathways = kb.read_json(folder/'pathways.json') if (folder/'pathways.json').exists() else []
    original = kb.read_json(folder/'pathway-membership.json') if (folder/'pathway-membership.json').exists() else []
    direct = defaultdict(set)
    for parent, child in original: direct[child].add(parent)
    graph = Graph().parse(folder/'candidates.ttl')
    category = {}
    for reaction, _, evidence in graph.triples((None,kb.M.evidenceJson,None)):
        try:
            value = json.loads(str(evidence))
            if isinstance(value,dict) and 'category' in value: category[str(reaction)] = value['category']
        except (ValueError,TypeError): pass
    memberships = []
    for row in rows:
        if row['source']['database'] != 'Reactome': continue
        identifier = row['source']['identifier']
        for pathway in row.get('pathway_ids', []):
            memberships.append({'reaction':row['reaction'],'pathway':pathway,
                'evidence':'direct' if pathway in direct[identifier] else 'ancestor', 'via_reaction':row['reaction'],'match_uri':''})
    by_reaction = defaultdict(list)
    for item in memberships: by_reaction[item['reaction']].append(item)
    matches = kb.read_json(folder/'cross-source-matches.json') if (folder/'cross-source-matches.json').exists() else []
    levels = {'verified-equivalence':'verified-derived','exact-chemical-equation':'equation-candidate',
              'source-cross-reference':'reference-candidate','class-signature':'class-candidate'}
    for match in matches:
        for member in by_reaction[match['reactome']]:
            memberships.append({'reaction':match['rhea'],'pathway':member['pathway'],'evidence':levels[match['kind']],
                'via_reaction':match['reactome'],'match_uri':match['uri']})
    memberships.sort(key=lambda x:(x['reaction'],x['pathway'],x['evidence'],x['via_reaction']))
    summaries = []
    for row in rows:
        lr = sides(row)
        cat = category.get(row['reaction'], 'rhea-reaction' if row['source']['database']=='Rhea' else 'unknown')
        transport = row['kind']=='transport'
        conversion = bool(lr['L'] and lr['R'] and lr['L'] != lr['R'])
        complete = row['class_mapping_status']=='complete'
        biochemical = cat in ('transition','rhea-reaction') and row['kind']!='BlackBoxEvent'
        eligible = conversion and complete and biochemical and not transport
        summaries.append({'reaction':row['reaction'],'source':row['source']['database'],
            'left_classes':';'.join(sorted(lr['L'])),'right_classes':';'.join(sorted(lr['R'])),
            'category':cat,'transport':transport,'class_conversion':conversion,'complete_mapping':complete,
            'mapping_eligible':eligible,'inferred':'unknown' if row.get('is_inferred') is None else str(row['is_inferred']).lower(),
            'exclusion_reasons':';'.join(reason for failed,reason in [(not conversion,'no-distinct-two-sided-class-conversion'),
                (not complete,'incomplete-class-mapping'),(not biochemical,'non-biochemical-or-unknown-event'),(transport,'transport')] if failed)})
    # Sidecar RDF keeps candidate membership distinct from source pathway facts.
    grouped = Graph(); grouped.bind('mslm',kb.M); grouped.bind('res',kb.RES)
    for feature in summaries:
        uri=URIRef(feature['reaction'])
        grouped.add((uri,kb.M.mappingEligible,Literal(feature['mapping_eligible'])))
        grouped.add((uri,kb.M.sourceEventCategory,Literal(feature['category'])))
        if feature['exclusion_reasons']:grouped.add((uri,kb.M.mappingExclusionReasons,Literal(feature['exclusion_reasons'])))
    ledger = kb.KNOWLEDGE/'source/id-registry.json'
    with kb.registry_lock(ledger):
        registry = kb.read_json(ledger)
        for item in memberships:
            node = kb.resource_uri('pathway-group-membership',report['taxon_id'],item['reaction'],item['pathway'],item['evidence'],item['via_reaction'],registry=registry,prefix='A')
            grouped.add((node,RDF.type,kb.M.PathwayGroupMembership))
            grouped.add((node,kb.M.memberReaction,URIRef(item['reaction'])))
            grouped.add((node,kb.M.memberPathway,URIRef('https://reactome.org/content/detail/'+item['pathway'])))
            grouped.add((node,kb.M.membershipEvidence,Literal(item['evidence'])))
            grouped.add((node,kb.M.organism,kb.TAXON[str(report['taxon_id'])]))
            grouped.add((node,kb.PROV.wasDerivedFrom,URIRef(item['match_uri'] or item['via_reaction'])))
            grouped.add((node,kb.DCT.identifier,Literal(str(node).rsplit('/',1)[-1])))
        kb.validate_id_registry(registry); kb.write_json(ledger,registry)
    csv_table(folder/'mapping-reactions.csv',summaries,list(summaries[0]) if summaries else ['reaction','source','mapping_eligible'])
    csv_table(folder/'pathway-groups.csv',memberships,['reaction','pathway','evidence','via_reaction','match_uri'])
    grouped.serialize(folder/'pathway-groups.ttl',format='turtle')
    with closing(sqlite3.connect(folder/'search.sqlite')) as db:
        with db:
            db.executescript('CREATE TABLE IF NOT EXISTS mapping_features (reaction TEXT PRIMARY KEY, mapping_eligible INTEGER, category TEXT, exclusion_reasons TEXT); CREATE TABLE IF NOT EXISTS pathway_groups (reaction TEXT, pathway TEXT, evidence TEXT, via_reaction TEXT, match_uri TEXT, PRIMARY KEY(reaction,pathway,evidence,via_reaction)); CREATE INDEX IF NOT EXISTS pathway_group_lookup ON pathway_groups(pathway,evidence);')
            db.execute('DELETE FROM mapping_features');db.execute('DELETE FROM pathway_groups')
            db.executemany('INSERT INTO mapping_features VALUES (?,?,?,?)',[(r['reaction'],r['mapping_eligible'],r['category'],r['exclusion_reasons']) for r in summaries])
            db.executemany('INSERT INTO pathway_groups VALUES (?,?,?,?,?)',[(m['reaction'],m['pathway'],m['evidence'],m['via_reaction'],m['match_uri']) for m in memberships])
    summary = {'taxon_id':report['taxon_id'],'scientific_name':report['scientific_name'],
        'source_reaction_count':len(rows),'mapping_eligible_count':sum(r['mapping_eligible'] for r in summaries),
        'pathway_count':len(pathways),'membership_counts':dict(Counter(x['evidence'] for x in memberships)),
        'source_files':{p:kb.sha(folder/p) for p in ['candidates.json','candidates.ttl','report.json','pathways.json','pathway-membership.json','cross-source-matches.json'] if (folder/p).exists()},
        'policy':'mapping/1: distinct two-sided classes, complete mapping, biochemical event, no transport; inferred allowed'}
    kb.write_json(folder/'mapping-groups-report.json',summary)
    return summary


def select_rows(folder, pathways=(), candidates='none', profile='mapping', measured=(), measured_mode='adjacent', bridge_depth=2, exclude_inferred=False):
    folder = Path(folder)
    metadata=kb.read_json(folder/'mapping-groups-report.json')
    for name,digest in metadata['source_files'].items():
        if kb.sha(folder/name)!=digest: raise ValueError('Stale grouping data; run preparation again: '+str(folder))
    if not 1<=bridge_depth<=4: raise ValueError('bridge_depth must be between 1 and 4')
    rows = kb.read_json(folder/'candidates.json')
    with (folder/'mapping-reactions.csv').open(encoding='utf-8',newline='') as f: features={r['reaction']:r for r in csv.DictReader(f)}
    with (folder/'pathway-groups.csv').open(encoding='utf-8',newline='') as f: memberships=list(csv.DictReader(f))
    allowed = {'direct','ancestor','verified-derived'}
    if candidates in ('equation','all'): allowed.add('equation-candidate')
    if candidates=='all': allowed.update(['class-candidate','reference-candidate'])
    known = {p['stId'] for p in kb.read_json(folder/'pathways.json')} if (folder/'pathways.json').exists() else set()
    if set(pathways)-known: raise ValueError('Unknown pathway IDs: '+', '.join(sorted(set(pathways)-known)))
    selected = {m['reaction'] for m in memberships if m['pathway'] in pathways and m['evidence'] in allowed}
    rows = [r for r in rows if (not pathways or r['reaction'] in selected) and
            (profile=='all' or features[r['reaction']]['mapping_eligible']=='True') and
            (not exclude_inferred or r.get('is_inferred') is not True)]
    if measured:
        classes={c['canonical_class'] for r in rows for c in r['classes'] if c['canonical_class'] in measured or c['lipidmaps'] in measured}
        if measured_mode=='bridge':
            adjacency=defaultdict(set)
            edges={}
            for r in rows:
                lr=sides(r); edges[r['reaction']]={(a,b) for a in lr['L'] for b in lr['R'] if a!=b}
                for a,b in edges[r['reaction']]: adjacency[a].add(b); adjacency[b].add(a)
            bridge_edges=set()
            def walk(node, path):
                if len(path)>1 and node in classes:
                    bridge_edges.update(frozenset((a,b)) for a,b in zip(path,path[1:]))
                if len(path)-1 >= bridge_depth: return
                for other in adjacency[node]-set(path): walk(other,path+[other])
            for seed in classes: walk(seed,[seed])
            keep={uri for uri,pairs in edges.items() if any(frozenset((a,b)) in bridge_edges for a,b in pairs)}
            rows=[r for r in rows if r['reaction'] in keep]
        else:
            rows=[r for r in rows if ((bool(sides(r)['L']&classes) and bool(sides(r)['R']&classes)) if measured_mode=='both' else bool((sides(r)['L']|sides(r)['R'])&classes))]
    return rows


def export_view(folder, output, **options):
    folder,output=Path(folder),Path(output)
    if folder.resolve()==output.resolve(): raise ValueError('Export directory must differ from source catalog')
    rows=select_rows(folder,**options)
    output.mkdir(parents=True,exist_ok=True)
    kb.write_json(output/'selected-reactions.json',rows)
    csv_table(output/'selected-reactions.csv',[{'reaction':r['reaction'],'source':r['source']['database'],'equation':r['equation']} for r in rows],['reaction','source','equation'])
    source=Graph().parse(folder/'class-pathways.ttl'); graph=Graph()
    for r in rows:
        projection=URIRef(r.get('projection_uri') or r['projection'])
        for triple in source.triples((projection,None,None)): graph.add(triple)
        for c in r['classes']:
            for triple in source.triples((URIRef(c['canonical_class']),None,None)):graph.add(triple)
    # Retain the selected membership evidence, including candidate labels.
    members=Graph().parse(folder/'pathway-groups.ttl')
    selected_uris={URIRef(r['reaction']) for r in rows}
    selected_paths={URIRef('https://reactome.org/content/detail/'+p) for p in options.get('pathways',[])}
    levels={'direct','ancestor','verified-derived'}
    if options.get('candidates') in ('equation','all'):levels.add('equation-candidate')
    if options.get('candidates')=='all':levels.update(['reference-candidate','class-candidate'])
    for member in members.subjects(RDF.type,kb.M.PathwayGroupMembership):
        if members.value(member,kb.M.memberReaction) not in selected_uris:continue
        if selected_paths and members.value(member,kb.M.memberPathway) not in selected_paths:continue
        if str(members.value(member,kb.M.membershipEvidence)) not in levels:continue
        for triple in members.triples((member,None,None)):graph.add(triple)
    graph.serialize(output/'selected-class-pathways.ttl',format='turtle')
    graph.serialize(output/'selected-class-pathways.jsonld',format='json-ld')
    kb.write_json(output/'selection.json',{'reaction_count':len(rows),'taxon_id':kb.read_json(folder/'report.json')['taxon_id'],
        'options':options,'source_catalog':str(folder.resolve()),'source_candidates_sha256':kb.sha(folder/'candidates.json'),
        'note':'Mapping view only; direction and candidate membership do not establish physiological flux.'})
    return len(rows)


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--catalog',type=Path);p.add_argument('--prepare-all',action='store_true',help='Prepare grouping sidecars for every bundled Reactome/integrated catalog')
    p.add_argument('--output',type=Path);p.add_argument('--pathway',nargs='*',default=[])
    p.add_argument('--candidates',choices=['none','equation','all'],default='none')
    p.add_argument('--profile',choices=['mapping','all'],default='mapping')
    p.add_argument('--measured-class',nargs='*',default=[]);p.add_argument('--measured-mode',choices=['adjacent','both','bridge'],default='adjacent')
    p.add_argument('--bridge-depth',type=int,choices=range(1,5),default=2);p.add_argument('--exclude-inferred',action='store_true')
    args=p.parse_args()
    if args.prepare_all:
        for folder in sorted((kb.KNOWLEDGE/'drafts').iterdir()):
            if (folder/'pathways.json').exists() and (folder/'candidates.json').exists():print(folder.name,make_groups(folder),flush=True)
        return
    if not args.catalog:p.error('--catalog or --prepare-all is required')
    print(make_groups(args.catalog),flush=True)
    if args.output: print('Selected reactions:',export_view(args.catalog,args.output,pathways=args.pathway,candidates=args.candidates,profile=args.profile,
        measured=args.measured_class,measured_mode=args.measured_mode,bridge_depth=args.bridge_depth,exclude_inferred=args.exclude_inferred))


if __name__=='__main__':main()
