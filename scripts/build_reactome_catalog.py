"""Build an offline species-specific Reactome catalog, preserving sets and complexes.

Only direct SimpleEntity ChEBI identities are projected to lipid classes.
Container components are archived as references, never free substrates.
"""
import argparse
import json
import re
import sqlite3
import xml.etree.ElementTree as ET
from collections import Counter
from contextlib import closing
from decimal import Decimal
from pathlib import Path
from rdflib import Graph, URIRef, Literal, RDF, RDFS
from rdflib.namespace import XSD
import pathway_knowledge as kb
from build_lipid_reaction_catalog import classification, csv_table, write_search_index
from lipid_ontology import lipid_terms
from collect_reactome import normalize_taxon, validate_event_taxon

BIO = 'http://biomodels.net/biology-qualifiers/'
RESOURCE = '{http://www.w3.org/1999/02/22-rdf-syntax-ns#}resource'


def references(element, qualifier):
    return sorted({node.attrib[RESOURCE] for group in element.iter('{' + BIO + '}' + qualifier)
                   for node in group.iter() if RESOURCE in node.attrib})


def stable_id(element):
    ids = {v.split('reactome:')[-1] for v in references(element, 'is') if 'reactome:' in v}
    if len(ids) != 1:
        raise ValueError('Missing/ambiguous Reactome identity: ' + element.attrib['id'])
    return next(iter(ids))


def species_record(element, ns):
    notes = element.find('s:notes', ns)
    text = ' '.join(notes.itertext()) if notes is not None else ''
    match = re.search(r'Derived from a Reactome (\w+)', text)
    kind = match[1] if match else 'Unknown'
    direct = references(element, 'is')
    chebi = sorted({'http://purl.obolibrary.org/obo/CHEBI_' + re.search(r'CHEBI[:_](\d+)', v)[1]
                    for v in direct if re.search(r'CHEBI[:_](\d+)', v)})
    return {'id': stable_id(element), 'name': element.attrib['name'], 'entity_type': kind,
            'compartment': element.attrib['compartment'], 'chebi': chebi,
            'source_references': direct, 'component_references': references(element, 'hasPart')}


def build(snapshot, classifier, output):
    folder, classifier, output = map(Path, (snapshot, classifier, output))
    output.mkdir(parents=True, exist_ok=True)
    manifest = kb.read_json(folder / 'manifest.json')
    taxon = normalize_taxon(manifest['taxon_id'])
    scientific_name = manifest['scientific_name']
    if (output / 'report.json').exists() and str(kb.read_json(output / 'report.json')['taxon_id']) != taxon:
        raise ValueError('Output belongs to another taxon; choose a new directory')
    for name, digest in manifest['files'].items():
        if kb.sha(folder / name) != digest:
            raise ValueError('Changed Reactome snapshot: ' + name)
    cm = kb.read_json(classifier / 'manifest.json')
    for name in ('chebi-all-is-a.json', 'chebi-all-chemical-forms.json', 'chebi-lipidmaps-xrefs-v3.json', 'lipidmaps-vocabulary.json'):
        if kb.sha(classifier / name) != cm['files'][name]:
            raise ValueError('Changed classification snapshot: ' + name)
    resolve, vocabulary = classification(classifier)
    lipids = lipid_terms(classifier)
    events = {v['stId']: v for v in kb.read_json(folder / 'events.json')}
    for event in events.values():
        validate_event_taxon(event, taxon)
        if event.get('isInferred') is not None and not isinstance(event['isInferred'], bool):
            raise ValueError('Invalid source inference flag: ' + event['stId'])
    pathways = kb.read_json(folder / 'pathways.json')
    membership = kb.read_json(folder / 'pathway-membership.json')
    parents = {}
    for parent, child in membership:
        parents.setdefault(child, set()).add(parent)
    def ancestors(identifier):
        seen, pending = set(), list(parents.get(identifier, []))
        while pending:
            item = pending.pop()
            if item not in seen:
                seen.add(item)
                pending.extend(parents.get(item, []))
        return sorted(seen)
    xml = ET.parse(folder / 'lipid-pathway.sbml').getroot()
    ns = {'s': xml.tag.split('}')[0][1:]}
    species = {v.attrib['id']: species_record(v, ns) for v in xml.findall('.//s:species', ns)}
    reactions = {stable_id(v): v for v in xml.findall('.//s:reaction', ns)}
    if set(events) != set(reactions):
        raise ValueError('SBML/event scope mismatch')
    graph, cg = Graph(), Graph()
    for g in (graph, cg):
        g.bind('mslm', kb.M); g.bind('res', kb.RES); g.bind('dct', kb.DCT)
    records, classes, mappings, queue = [], {}, {}, []
    ledger = kb.KNOWLEDGE / 'source/id-registry.json'
    with kb.registry_lock(ledger):
        registry = kb.read_json(ledger)
        def allocate(kind, *keys, prefix):
            return kb.resource_uri(kind, *keys, registry=registry, prefix=prefix)
        source_dataset = allocate('source-organism-catalog', taxon, output.name, prefix='B')
        class_dataset = allocate('class-organism-catalog', taxon, output.name, prefix='B')
        def source(g, subject, identifier):
            g.add((subject, kb.M.sourceDatabase, Literal('Reactome')))
            g.add((subject, kb.M.sourceIdentifier, Literal(identifier)))
            g.add((subject, kb.M.sourceRelease, Literal(manifest['source_release'])))
            g.add((subject, kb.M.sourceUrl, Literal('https://reactome.org/content/detail/' + identifier, datatype=XSD.anyURI)))
        for g, ds in ((graph, source_dataset), (cg, class_dataset)):
            g.add((ds, RDF.type, kb.M.OrganismPathwayDataset))
            g.add((ds, kb.M.organism, kb.TAXON[taxon]))
            g.add((ds, kb.DCT.license, URIRef(manifest['license_url'])))
            source(g, ds, manifest['root_pathway'])
        cg.add((class_dataset, kb.PROV.wasDerivedFrom, source_dataset))
        for p in pathways:
            uri = URIRef('https://reactome.org/content/detail/' + p['stId'])
            for g, ds in ((graph, source_dataset), (cg, class_dataset)):
                g.add((uri, RDF.type, kb.M.Pathway)); g.add((uri, RDFS.label, Literal(p['name'])))
                g.add((ds, kb.M.hasPathway, uri)); source(g, uri, p['stId'])
        for parent, child in membership:
            graph.add((URIRef('https://reactome.org/content/detail/' + parent), kb.M.hasEvent,
                       URIRef('https://reactome.org/content/detail/' + child)))
        for identifier, element in sorted(reactions.items()):
            event = events[identifier]
            uri = URIRef('https://reactome.org/content/detail/' + identifier)
            projection = allocate('catalog-class-projection', taxon, str(uri), prefix='R')
            graph.add((source_dataset, kb.M.hasSourceReaction, uri))
            cg.add((class_dataset, kb.M.hasProjectionReaction, projection))
            cg.add((projection, RDF.type, kb.M.ReactionTraversalProjection))
            cg.add((projection, kb.DCT.source, uri))
            cg.add((projection, kb.M.resolutionLevel, kb.M.ClassLevel))
            inferred = event.get('isInferred')
            reversible = element.attrib['reversible'] == 'true'
            direction = kb.M.Reversible if reversible else kb.M.SourceForward
            for g, subject in ((graph, uri), (cg, projection)):
                source(g, subject, identifier)
                g.add((subject, RDFS.label, Literal(element.attrib['name'])))
                g.add((subject, kb.M.organism, kb.TAXON[taxon]))
                if inferred is not None:
                    g.add((subject, kb.M.isInferred, Literal(inferred)))
                g.add((subject, kb.M.sourceStableIdVersion, Literal(event['stIdVersion'])))
                g.add((subject, kb.M.directionality, direction))
                g.add((subject, kb.M.curationStatus, kb.M.Draft))
            graph.add((uri, RDF.type, kb.M.LipidReaction))
            graph.add((uri, kb.M.resolutionLevel, kb.M.SourceChemicalResolution))
            graph.add((uri, kb.M.sourceEventType, Literal(event['schemaClass'])))
            assertion = allocate('catalog-reactome-source', identifier, manifest['source_release'], prefix='A')
            graph.add((uri, kb.M.hasSourceAssertion, assertion))
            graph.add((assertion, RDF.type, kb.M.SourceAssertion)); source(graph, assertion, identifier)
            if inferred is not None:
                graph.add((assertion, kb.M.isInferred, Literal(inferred)))
            graph.add((assertion, kb.M.assertionMethod, kb.M.OrthologyInference if inferred else kb.M.DatabaseAnnotation))
            for parent in parents.get(identifier, []):
                cg.add((URIRef('https://reactome.org/content/detail/' + parent), kb.M.hasEvent, projection))
            participants, matches, sides, names = [], [], {'L': set(), 'R': set()}, {'L': [], 'R': []}
            incomplete = False
            for side, section in (('L', 'listOfReactants'), ('R', 'listOfProducts')):
                for index, ref in enumerate(element.findall('s:' + section + '/s:speciesReference', ns)):
                    entity = species[ref.attrib['species']]
                    entity_uri = URIRef('https://reactome.org/content/detail/' + entity['id'])
                    part = allocate('reactome-participant', identifier, side, ref.attrib['species'], str(index), prefix='T')
                    value = format(Decimal(ref.attrib.get('stoichiometry', '1')), 'f')
                    if '.' not in value: value += '.0'
                    graph.add((uri, kb.M.hasParticipant, part)); graph.add((part, RDF.type, kb.M.ReactionParticipant))
                    graph.add((part, kb.M.participantEntity, entity_uri))
                    graph.add((part, kb.M.participantRole, kb.M.EquationLeft if side == 'L' else kb.M.EquationRight))
                    graph.add((part, kb.M.stoichiometry, Literal(value, datatype=XSD.decimal, normalize=False)))
                    graph.add((entity_uri, RDFS.label, Literal(entity['name'])))
                    graph.add((entity_uri, kb.M.sourceEntityType, Literal(entity['entity_type'])))
                    graph.add((entity_uri, kb.M.sourceCompartment, Literal(entity['compartment'])))
                    for component in entity['component_references']:
                        graph.add((entity_uri, kb.M.sourceComponentReference, URIRef(component)))
                    participant = {**entity, 'side': side, 'stoichiometry': value, 'mapping': []}
                    participants.append(participant); names[side].append(entity['name'])
                    # Set alternatives/complex components are not simultaneous free reactants.
                    if entity['entity_type'] != 'SimpleEntity':
                        if entity['entity_type'] != 'EntityWithAccessionedSequence':
                            incomplete = True
                            queue.append({'reaction': str(uri), 'entity': entity['id'], 'reason': 'container-or-unspecified-entity', 'entity_type': entity['entity_type']})
                        continue
                    for term in entity['chebi']:
                        graph.add((part, kb.M.sourceChemicalReference, URIRef(term)))
                        decision = resolve(term)
                        participant['mapping'].append({'chebi': term, **decision})
                        graph.add((part, kb.M.mappingEvidenceJson, Literal(json.dumps({'chebi': term, **decision}, sort_keys=True))))
                        if decision['status'] != 'mapped':
                            if term in lipids:
                                incomplete = True
                                queue.append({'reaction': str(uri), 'chebi': term, **decision})
                            continue
                        code = decision['codes'][0]
                        qualifier = decision.get('biochemical_label') or ''
                        label = qualifier or vocabulary[code]['label'] or code
                        canonical = allocate('catalog-lipid-class', code, qualifier, prefix='C')
                        classes[str(canonical)] = {'uri': str(canonical), 'id': str(canonical).rsplit('/', 1)[-1], 'label': label,
                            'lipidmaps_class_id': code, 'classification_url': vocabulary[code]['url']}
                        cg.add((canonical, RDF.type, kb.M.LipidEntity)); cg.add((canonical, RDFS.label, Literal(label)))
                        cg.add((canonical, kb.M.lipidmapsClassId, Literal(code)))
                        cg.add((projection, kb.M.leftClass if side == 'L' else kb.M.rightClass, canonical))
                        graph.add((part, kb.M.projectedClass, canonical))
                        matches.append({'chebi': term, 'canonical_class': str(canonical), 'label': label, 'lipidmaps': code, 'side_code': side, 'mapping': decision})
                        mappings[term.rsplit('_', 1)[-1]] = [label, code]; sides[side].add(label)
            status = 'partial' if incomplete else ('complete' if matches else 'no-direct-lipid-class')
            cg.add((projection, kb.M.classMappingComplete, Literal(status == 'complete')))
            equation = ' + '.join(names['L']) + (' <=> ' if reversible else ' => ') + ' + '.join(names['R'])
            graph.add((uri, kb.M.equation, Literal(equation)))
            graph.add((uri, kb.M.evidenceJson, Literal(json.dumps(event, sort_keys=True, ensure_ascii=False))))
            records.append({'reaction': str(uri), 'projection': str(projection), 'name': element.attrib['name'], 'equation': equation,
                'left': sorted(sides['L']), 'right': sorted(sides['R']), 'classes': matches, 'participants': participants,
                'class_mapping_status': status, 'kind': event['schemaClass'], 'is_inferred': inferred,
                'direction': 'reversible' if reversible else 'left-to-right (source SBML)',
                'pathway_ids': ancestors(identifier), 'pathway_names': [p['name'] for p in pathways if p['stId'] in ancestors(identifier)],
                'source': {'database': 'Reactome', 'identifier': identifier, 'stable_id_version': event['stIdVersion'], 'release': manifest['source_release']}})
            # Catalysts and modifiers remain source entities, not substrates.
            for ref in element.findall('s:listOfModifiers/s:modifierSpeciesReference', ns):
                graph.add((uri, kb.M.sourceModifier, URIRef('https://reactome.org/content/detail/' + species[ref.attrib['species']]['id'])))
        for g in (graph, cg):
            for subject in set(g.subjects()):
                if str(subject).startswith(str(kb.RES)):
                    g.add((subject, kb.DCT.identifier, Literal(str(subject).rsplit('/', 1)[-1])))
        kb.validate_id_registry(registry); kb.write_json(ledger, registry)
    graph.serialize(output / 'candidates.ttl', format='turtle')
    graph.serialize(output / 'pathways.jsonld', format='json-ld')
    cg.serialize(output / 'class-pathways.ttl', format='turtle')
    for key, value in classes.items():
        value['reaction_count'] = sum(any(c['canonical_class'] == key for c in r['classes']) for r in records)
    csv_table(output / 'classes.csv', sorted(classes.values(), key=lambda c: c['uri']), ['uri','id','label','lipidmaps_class_id','classification_url','reaction_count'])
    for filename, data in [('candidates.json', records), ('class-mappings.json', mappings), ('mapping-review-queue.json', queue), ('pathways.json', pathways), ('pathway-membership.json', membership)]:
        kb.write_json(output / filename, data)
    csv_table(output / 'reactions.csv', [{**r, 'pathways': ';'.join(r['pathway_ids'])} for r in records], ['reaction','name','kind','class_mapping_status','is_inferred','direction','pathways'])
    write_search_index(output, records, classes)
    with closing(sqlite3.connect(output / 'search.sqlite')) as db:
        db.executescript('CREATE TABLE pathways (id TEXT PRIMARY KEY, name TEXT); CREATE TABLE pathway_membership (parent TEXT, child TEXT, PRIMARY KEY(parent,child)); CREATE TABLE reactome_events (reaction TEXT PRIMARY KEY, stable_id TEXT, name TEXT, is_inferred INTEGER, direction TEXT);')
        db.executemany('INSERT INTO pathways VALUES (?,?)', [(p['stId'], p['name']) for p in pathways])
        db.executemany('INSERT INTO pathway_membership VALUES (?,?)', membership)
        db.executemany('INSERT INTO reactome_events VALUES (?,?,?,?,?)', [(r['reaction'], r['source']['identifier'], r['name'], r['is_inferred'], r['direction']) for r in records])
        db.commit()
    report = {'status': 'source-catalog-with-explicit-class-mapping-status', 'taxon_id': taxon, 'scientific_name': scientific_name,
        'source_database': 'Reactome', 'source_release': manifest['source_release'], 'license': manifest['license'], 'license_source': manifest['license_source'],
        'dataset_uri': str(source_dataset), 'class_dataset_uri': str(class_dataset), 'candidate_count': len(records), 'pathway_count': len(pathways),
        'mapped_lipidmaps_classification_count': len({c['lipidmaps_class_id'] for c in classes.values()}), 'biochemical_class_node_count': len(classes),
        'inferred_event_count': sum(r['is_inferred'] is True for r in records),
        'not_inferred_event_count': sum(r['is_inferred'] is False for r in records),
        'unknown_inference_event_count': sum(r['is_inferred'] is None for r in records),
        'mapping_status_counts': dict(Counter(r['class_mapping_status'] for r in records)),
        'events_with_class_mapping': sum(bool(r['classes']) for r in records), 'review_queue_count': len(queue),
        'classification_snapshot': classifier.name, 'classification_manifest_sha256': kb.sha(classifier / 'manifest.json'),
        'snapshot_manifest_sha256': kb.sha(folder / 'manifest.json'), 'classification_rules_sha256': kb.sha(kb.KNOWLEDGE / 'source/lipid-class-seeds.json'),
        'limitations': ['Scope is the Reactome ' + scientific_name + ' Metabolism of lipids hierarchy, including binding and regulatory events; not all lipid biology.',
            'Inferred status is source evidence, not experimental confirmation in the selected organism. Missing flags remain unknown.',
            'Only direct SimpleEntity ChEBI identities are projected. Sets, complexes and unspecified entities remain source participants and in the review queue.',
            'SBML direction is the source model direction, not a claim about physiological flux.',
            'Reactome and Rhea datasets are separate; counts must not be added as unique biochemical reactions.']}
    kb.write_json(output / 'report.json', report)
    from pathway_mapping import make_groups
    make_groups(output)
    print(report)


if __name__ == '__main__':
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--snapshot', required=True); p.add_argument('--classifier', required=True); p.add_argument('--output', required=True)
    args = p.parse_args(); build(args.snapshot, args.classifier, args.output)
