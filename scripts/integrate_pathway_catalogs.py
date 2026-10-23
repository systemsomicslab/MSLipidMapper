"""Build a species-specific source-preserving Rhea/Reactome RDF union.

Class signatures are search relations, never reaction identity. A verified
equivalence requires a direct same-species source cross-reference AND agreement
of complete ChEBI/stoichiometry equations. No orthology-ID substitution is used.
"""
import argparse
import csv
import json
import re
import sqlite3
from collections import defaultdict
from contextlib import closing
from decimal import Decimal, InvalidOperation
from pathlib import Path
from rdflib import Graph, URIRef, Literal, RDF
import pathway_knowledge as kb
from build_lipid_reaction_catalog import csv_table, write_search_index


def equation_signature(row):
    """Orientation-neutral full equation; reject containers and transport."""
    sides = {'L': defaultdict(Decimal), 'R': defaultdict(Decimal)}
    seen = set()
    try:
        for p in row['participants']:
            if row['source']['database'] == 'Reactome':
                if p['entity_type'] != 'SimpleEntity' or len(p['chebi']) != 1:
                    return None
                side, term, coefficient = p['side'], p['chebi'][0], p['stoichiometry']
            else:
                if p.get('transport') in ('true', '1'):
                    return None
                # Multiple chemical references for a single Rhea participant
                # can describe a reactive part/polymer. Do not guess identity.
                if p['participant'] in seen:
                    return None
                seen.add(p['participant'])
                side = p['side'].rsplit('_', 1)[-1]
                term, coefficient = p['chebi'], p['coefficient']
            number = Decimal(coefficient)
            if not number.is_finite() or number <= 0:
                return None
            sides[side][term] += number
    except (KeyError, TypeError, InvalidOperation):
        return None
    left, right = (tuple(sorted(sides[s].items())) for s in ('L', 'R'))
    if not left or not right or left == right:
        return None
    return tuple(sorted((left, right)))


def class_signature(row):
    if row['class_mapping_status'] != 'complete':
        return None
    sides = {s: tuple(sorted({c['canonical_class'] for c in row['classes'] if c['side_code'] == s})) for s in ('L', 'R')}
    if not sides['L'] or not sides['R'] or sides['L'] == sides['R']:
        return None
    return tuple(sorted((sides['L'], sides['R'])))


def reactome_references(row):
    refs = set()
    for item in row.get('related_records', []):
        if item.get('relation') != 'http://www.w3.org/2000/01/rdf-schema#seeAlso':
            continue
        value = item['identifier']
        if not re.search(r'(?:identifiers\.org/reactome/|reactome\.org/content/detail/)', value):
            continue
        match = re.search(r'R-[A-Z]+-\d+', value)
        if match:
            refs.add('https://reactome.org/content/detail/' + match[0])
    return refs


def match_records(rhea, reactome):
    chemical, classes = defaultdict(list), defaultdict(list)
    target = {r['reaction']: r for r in reactome}
    for row in reactome:
        for index, signature in ((chemical, equation_signature(row)), (classes, class_signature(row))):
            if signature:
                index[signature].append(row['reaction'])
    matches = []
    for row in rhea:
        exact = set(chemical.get(equation_signature(row), []))
        related = set(classes.get(class_signature(row), []))
        referenced = reactome_references(row) & target.keys()
        for other in sorted(exact | related | referenced):
            kind = ('verified-equivalence' if other in exact & referenced else
                    'exact-chemical-equation' if other in exact else
                    'source-cross-reference' if other in referenced else 'class-signature')
            matches.append({'rhea': row['reaction'], 'reactome': other, 'kind': kind,
                'equation_agrees': other in exact, 'direct_source_reference': other in referenced,
                'class_signature_agrees': other in related,
                'orientation': 'orientation-neutral; physiological flux not inferred'})
    return matches


def build(rhea_folder, reactome_folder, output):
    folders = [Path(rhea_folder), Path(reactome_folder)]
    output = Path(output)
    if output.resolve() in {f.resolve() for f in folders}:
        raise ValueError('Integrated output must be separate from source catalogs')
    reports = [kb.read_json(f / 'report.json') for f in folders]
    taxa = {str(r['taxon_id']) for r in reports}
    if len(taxa) != 1:
        raise ValueError('Cannot integrate catalogs from different taxa')
    taxon = next(iter(taxa))
    if (output / 'report.json').exists() and str(kb.read_json(output / 'report.json')['taxon_id']) != taxon:
        raise ValueError('Output belongs to another organism')
    sources = [kb.read_json(f / 'candidates.json') for f in folders]
    for rows, expected in zip(sources, ['Rhea', 'Reactome']):
        if not rows or any(r['source']['database'] != expected for r in rows):
            raise ValueError('Expected a nonempty ' + expected + ' source catalog')
    records = [dict(r) for rows in sources for r in rows]
    if len({r['reaction'] for r in records}) != len(records):
        raise ValueError('Duplicate source reaction IDs')
    graph, cg = Graph(), Graph()
    classes, mappings = {}, {}
    input_hashes = {}
    for folder, report in zip(folders, reports):
        sg = Graph().parse(folder / 'candidates.ttl')
        ds = URIRef(report['dataset_uri'])
        if set(sg.objects(ds, kb.M.organism)) != {kb.TAXON[taxon]}:
            raise ValueError('Source RDF taxon does not match report')
        for triple in sg: graph.add(triple)
        for triple in Graph().parse(folder / 'class-pathways.ttl'): cg.add(triple)
        with (folder / 'classes.csv').open(encoding='utf-8', newline='') as handle:
            for row in csv.DictReader(handle):
                if row['uri'] in classes and any(classes[row['uri']][k] != row[k] for k in ('label','lipidmaps_class_id')):
                    raise ValueError('Conflicting canonical lipid class')
                classes[row['uri']] = row
        for key, value in kb.read_json(folder / 'class-mappings.json').items():
            if key in mappings and mappings[key] != value:
                raise ValueError('Conflicting ChEBI class mapping; align classifiers first')
            mappings[key] = value
        input_hashes[folder.name] = {name: kb.sha(folder / name) for name in ('report.json','candidates.json','candidates.ttl','class-pathways.ttl','classes.csv','class-mappings.json')}
    matches = match_records(*sources)
    by_reaction = defaultdict(list)
    ledger = kb.KNOWLEDGE / 'source/id-registry.json'
    with kb.registry_lock(ledger):
        registry = kb.read_json(ledger)
        def allocate(kind, *keys, prefix):
            return kb.resource_uri(kind, *keys, registry=registry, prefix=prefix)
        dataset = allocate('integrated-source-dataset', taxon, output.name, prefix='B')
        class_dataset = allocate('integrated-class-dataset', taxon, output.name, prefix='B')
        for g, ds in ((graph, dataset), (cg, class_dataset)):
            g.add((ds, RDF.type, kb.M.OrganismPathwayDataset))
            g.add((ds, kb.M.organism, kb.TAXON[taxon]))
            for report in reports:
                g.add((ds, kb.PROV.wasDerivedFrom, URIRef(report['dataset_uri' if g is graph else 'class_dataset_uri'])))
        for row in records:
            graph.add((dataset, kb.M.hasSourceReaction, URIRef(row['reaction'])))
            cg.add((class_dataset, kb.M.hasProjectionReaction, URIRef(row.get('projection_uri') or row['projection'])))
        for match in matches:
            node = allocate('cross-source-reaction-match', taxon, match['rhea'], match['reactome'], match['kind'], prefix='A')
            match['uri'] = str(node)
            graph.add((node, RDF.type, kb.M.CrossSourceReactionMatch))
            graph.add((node, kb.M.matchLeft, URIRef(match['rhea'])))
            graph.add((node, kb.M.matchRight, URIRef(match['reactome'])))
            graph.add((node, kb.M.matchKind, Literal(match['kind'])))
            graph.add((node, kb.M.matchEvidenceJson, Literal(json.dumps(match, sort_keys=True))))
            if match['kind'] == 'verified-equivalence':
                reaction = allocate('integrated-verified-reaction', taxon, match['rhea'], match['reactome'], prefix='R')
                graph.add((reaction, RDF.type, kb.M.VerifiedReactionGroup))
                graph.add((reaction, kb.M.organism, kb.TAXON[taxon]))
                for source in (match['rhea'], match['reactome']): graph.add((reaction, kb.DCT.source, URIRef(source)))
            for source, other in ((match['rhea'], match['reactome']), (match['reactome'], match['rhea'])):
                by_reaction[source].append({'reaction': other, 'kind': match['kind'], 'match_uri': str(node)})
        for g in (graph, cg):
            for subject in set(g.subjects()):
                if str(subject).startswith(str(kb.RES)):
                    g.add((subject, kb.DCT.identifier, Literal(str(subject).rsplit('/',1)[-1])))
        kb.validate_id_registry(registry); kb.write_json(ledger, registry)
    for row in records:
        row['cross_source_matches'] = by_reaction[row['reaction']]
        row['match_kinds'] = sorted({v['kind'] for v in row['cross_source_matches']})
        row['related_source_ids'] = sorted({v['reaction'] for v in row['cross_source_matches']})
    output.mkdir(parents=True, exist_ok=True)
    graph.serialize(output / 'candidates.ttl', format='turtle')
    graph.serialize(output / 'pathways.jsonld', format='json-ld')
    cg.serialize(output / 'class-pathways.ttl', format='turtle')
    for uri, row in classes.items():
        row['reaction_count'] = sum(any(c['canonical_class'] == uri for c in r['classes']) for r in records)
    csv_table(output / 'classes.csv', sorted(classes.values(), key=lambda r:r['uri']), ['uri','id','label','lipidmaps_class_id','classification_url','reaction_count'])
    kb.write_json(output / 'candidates.json', records)
    kb.write_json(output / 'class-mappings.json', mappings)
    kb.write_json(output / 'cross-source-matches.json', matches)
    for name in ('pathways.json','pathway-membership.json'):
        kb.write_json(output / name, kb.read_json(folders[1] / name))
    csv_table(output / 'cross-source-matches.csv', matches, ['uri','rhea','reactome','kind','equation_agrees','direct_source_reference','class_signature_agrees','orientation'])
    write_search_index(output, records, classes)
    with closing(sqlite3.connect(output / 'search.sqlite')) as db:
        db.executescript('CREATE TABLE source_records(reaction TEXT PRIMARY KEY, database_name TEXT, inferred INTEGER); CREATE TABLE cross_source_matches(uri TEXT PRIMARY KEY, rhea TEXT, reactome TEXT, kind TEXT); CREATE INDEX match_rhea ON cross_source_matches(rhea); CREATE INDEX match_reactome ON cross_source_matches(reactome);')
        db.executemany('INSERT INTO source_records VALUES (?,?,?)', [(r['reaction'],r['source']['database'],r.get('is_inferred')) for r in records])
        db.executemany('INSERT INTO cross_source_matches VALUES (?,?,?,?)', [(m['uri'],m['rhea'],m['reactome'],m['kind']) for m in matches]); db.commit()
    report = {'status':'integrated-source-catalog', 'source_database':'Rhea + Reactome', 'taxon_id':taxon,
        'scientific_name':reports[0]['scientific_name'], 'dataset_uri':str(dataset), 'class_dataset_uri':str(class_dataset),
        'candidate_count':len(records), 'source_counts':{r['source']['database']:len(rows) for rows in sources for r in rows[:1]},
        'mapped_lipidmaps_classification_count':len({r['lipidmaps_class_id'] for r in classes.values()}),
        'biochemical_class_node_count':len(classes), 'pathway_count':len(kb.read_json(output/'pathways.json')),
        'match_counts':{kind:sum(m['kind']==kind for m in matches) for kind in ['verified-equivalence','exact-chemical-equation','source-cross-reference','class-signature']},
        'source_catalogs':[{'folder':f.name,'dataset_uri':r['dataset_uri'],'source_release':r.get('source_release') or r.get('rhea_release')} for f,r in zip(folders,reports)],
        'input_hashes':input_hashes, 'limitations':['Source reaction counts are not deduplicated biochemical transformations.',
            'Class signatures and chemical equation agreement alone do not establish reaction identity.',
            'Human Reactome cross-references are never rewritten to mouse IDs.', 'Source organism evidence, inferred flags, licenses and participants remain unchanged.']}
    kb.write_json(output / 'report.json', report)
    from pathway_mapping import make_groups
    make_groups(output)
    print(json.dumps(report, indent=2))
    return report


if __name__ == '__main__':
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--rhea', required=True); p.add_argument('--reactome', required=True); p.add_argument('--output', required=True)
    args = p.parse_args(); build(args.rhea, args.reactome, args.output)
