"""Exercise non-mouse collection/build without making network requests."""
import json
import shutil
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch
from rdflib import Graph, RDF, Literal

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'scripts'))
import pathway_knowledge as kb
import collect_reactome as collector
import reactome_pathways as command

CLASSIFIER = kb.KNOWLEDGE / 'snapshots/lipid-census-2026-10-15'
SOURCE = kb.KNOWLEDGE / 'source'


def fixture(taxon, name, prefix):
    root_id = f'R-{prefix}-556833'
    ids = [f'R-{prefix}-100', f'R-{prefix}-101']
    root = {'stId': root_id, 'species': name, 'name': 'Metabolism of lipids', 'type': 'Pathway',
            'children': [{'stId': i, 'species': name, 'name': 'PC conversion', 'type': 'Reaction'} for i in ids]}
    # One explicit non-inferred flag and one missing flag test human evidence handling.
    events = [{'dbId': n + 10, 'stId': i, 'stIdVersion': i + '.1', 'schemaClass': 'Reaction',
               'species': [{'dbId': 1, 'taxId': int(taxon), 'displayName': name}]} for n, i in enumerate(ids)]
    events[0]['isInferred'] = prefix != 'HSA'
    species = '''<species id="pc" name="PC" compartment="c"><notes><p xmlns="http://www.w3.org/1999/xhtml">Derived from a Reactome SimpleEntity.</p></notes><annotation><rdf:RDF><rdf:Description><bq:is><rdf:Bag><rdf:li rdf:resource="https://identifiers.org/reactome:R-ALL-1"/><rdf:li rdf:resource="https://identifiers.org/CHEBI:57643"/></rdf:Bag></bq:is></rdf:Description></rdf:RDF></annotation></species>'''
    reactions = ''.join(f'''<reaction id="rx{n}" name="PC reaction {n}" reversible="false"><annotation><rdf:RDF><rdf:Description><bq:is><rdf:Bag><rdf:li rdf:resource="https://identifiers.org/reactome:{i}"/></rdf:Bag></bq:is></rdf:Description></rdf:RDF></annotation><listOfReactants><speciesReference species="pc" stoichiometry="1"/></listOfReactants><listOfProducts><speciesReference species="pc" stoichiometry="1"/></listOfProducts></reaction>''' for n, i in enumerate(ids))
    sbml = f'''<sbml xmlns="http://www.sbml.org/sbml/level3/version1/core" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:bq="http://biomodels.net/biology-qualifiers/"><model><listOfSpecies>{species}</listOfSpecies><listOfReactions>{reactions}</listOfReactions></model></sbml>'''.encode()
    return root, events, sbml


class CommandsTests(unittest.TestCase):
    def test_collect_and_offline_build_human_and_rat(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            (root / 'source').mkdir()
            for name in ('id-registry.json', 'lipid-class-seeds.json'):
                shutil.copyfile(SOURCE / name, root / 'source' / name)
            with patch.object(kb, 'KNOWLEDGE', root):
                datasets = []
                for taxon, name, prefix in [('9606', 'Homo sapiens', 'HSA'), ('10116', 'Rattus norvegicus', 'RNO')]:
                    hierarchy, events, sbml = fixture(taxon, name, prefix)
                    snapshot, output = root / ('snapshot-' + taxon), root / ('output-' + taxon)
                    def archive(folder, filename, url, accept='application/json'):
                        value = 97 if filename.startswith('version-') else [hierarchy] if filename == 'hierarchy.json' else events
                        kb.write_json(folder / filename, value)
                        return value, {}
                    def request(folder, filename, url, accept, body=None):
                        if filename == 'lipid-pathway.sbml': return sbml
                        if filename == 'license.html': return b'CC0'
                        if filename == 'species.json':
                            return json.dumps([{'dbId': 1, 'taxId': int(taxon), 'displayName': name}]).encode()
                        return json.dumps(events).encode()
                    # Archive the SBML as the real request helper does.
                    def recording_request(folder, filename, *args, **kwargs):
                        data = request(folder, filename, *args, **kwargs)
                        (Path(folder) / filename).write_bytes(data)
                        return data
                    with patch.object(collector, 'archive', side_effect=archive), patch.object(collector, 'request', side_effect=recording_request):
                        collector.collect(snapshot, taxon)
                    args = command.parser().parse_args(['--taxon', taxon, '--snapshot', str(snapshot), '--output', str(output), '--classifier', str(CLASSIFIER), '--offline'])
                    with patch.object(command, 'collect', side_effect=AssertionError('offline must not fetch')):
                        result = command.run(args)
                    self.assertEqual(result[0]['scientific_name'], name)
                    report = kb.read_json(output / 'report.json')
                    self.assertEqual(report['taxon_id'], taxon)
                    self.assertEqual(report['mapped_lipidmaps_classification_count'], 1)
                    self.assertEqual(report['events_with_class_mapping'], 2)
                    self.assertEqual(report['unknown_inference_event_count'], 1)
                    self.assertEqual(report['inferred_event_count'], 0 if taxon == '9606' else 1)
                    graph = Graph().parse(output / 'candidates.ttl')
                    self.assertEqual(set(graph.objects(None, kb.M.organism)), {kb.TAXON[taxon]})
                    self.assertNotIn(Literal('10090'), set(graph.objects()))
                    unknown = 'https://reactome.org/content/detail/R-' + prefix + '-101'
                    from rdflib import URIRef
                    self.assertEqual(list(graph.objects(URIRef(unknown), kb.M.isInferred)), [])
                    datasets.append(report['dataset_uri'])
                self.assertNotEqual(*datasets)

    def test_cross_species_and_invalid_taxon_rejected(self):
        for taxon in ('../9606', '0', '-1', 'human', '９６０６'):
            with self.assertRaises(ValueError): collector.normalize_taxon(taxon)
        root, events, _ = fixture('9606', 'Homo sapiens', 'HSA')
        root['children'][0]['species'] = 'Mus musculus'
        with self.assertRaisesRegex(ValueError, 'Cross-species'):
            collector.index_hierarchy(root)
        with self.assertRaisesRegex(ValueError, 'taxon'):
            collector.validate_event_taxon(events[0], '10090')

    def test_invalid_cli_combinations_fail_before_network_or_build(self):
        for flags in [ ['--taxon','9606','10116','--snapshot','x','--output','y'],
                       ['--taxon','9606','--snapshot','x'],
                       ['--taxon','9606','--run-id','../escape'],
                       ['--list-species','--offline'] ]:
            args = command.parser().parse_args(flags)
            with patch.object(command, 'collect', side_effect=AssertionError('must not fetch')):
                with self.assertRaises(ValueError): command.run(args)

    def test_wrong_snapshot_taxon_does_not_build(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            kb.write_json(root / 'manifest.json', {'taxon_id': '10090'})
            args = command.parser().parse_args(['--taxon','9606','--snapshot',str(root),'--output',str(root/'out'),'--classifier',str(CLASSIFIER),'--offline'])
            with patch.object(command, 'build', side_effect=AssertionError('must not build')):
                with self.assertRaisesRegex(ValueError, 'another taxon'): command.run(args)

    def test_multiple_taxa_are_separate_and_duplicates_run_once(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            args = command.parser().parse_args(['--taxon','9606','10116','9606','--run-id','test-release',
                '--snapshot-root',str(root/'snapshots'),'--output-root',str(root/'drafts'),'--classifier',str(CLASSIFIER)])
            def collect(snapshot, taxon):
                snapshot.mkdir(parents=True)
                kb.write_json(snapshot / 'manifest.json', {'taxon_id': taxon})
            def build(snapshot, classifier, output):
                output.mkdir(parents=True)
                kb.write_json(output / 'report.json', {'taxon_id': kb.read_json(snapshot/'manifest.json')['taxon_id'],
                    'scientific_name':'fixture species','candidate_count':2,'pathway_count':1})
            with patch.object(command, 'collect', side_effect=collect) as fetch, patch.object(command, 'build', side_effect=build):
                result = command.run(args)
            self.assertEqual(fetch.call_count, 2)
            self.assertEqual([r['taxon_id'] for r in result], ['9606','10116'])
            self.assertNotEqual(result[0]['output'], result[1]['output'])

    def test_batch_skips_absent_pathway_and_continues_after_real_failure(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            args = command.parser().parse_args(['--all-species','--run-id','batch-test',
                '--snapshot-root',str(root/'snapshots'),'--output-root',str(root/'drafts'),'--classifier',str(CLASSIFIER)])
            def collect(snapshot, taxon):
                if taxon == '1773': raise command.MissingLipidPathway('No lipid root')
                if taxon == '4896': raise OSError('Simulated network failure')
                snapshot.mkdir(parents=True)
                kb.write_json(snapshot/'manifest.json', {'taxon_id': taxon})
            def build(snapshot, classifier, output):
                output.mkdir(parents=True)
                kb.write_json(output/'report.json', {'scientific_name':'Homo sapiens','candidate_count':2,'pathway_count':1})
            with patch.object(command,'list_species',return_value=[{'taxon_id':t} for t in ['1773','4896','9606']]), patch.object(command,'collect',side_effect=collect), patch.object(command,'build',side_effect=build):
                with self.assertRaisesRegex(ValueError, '1 species failed'): command.run(args)
            report = kb.read_json(root/'drafts/reactome-run-batch-test.json')
            self.assertEqual([r['taxon_id'] for r in report['completed']], ['9606'])
            self.assertEqual([r['taxon_id'] for r in report['skipped']], ['1773'])
            self.assertEqual([r['taxon_id'] for r in report['failed']], ['4896'])

    def test_absent_pathway_is_skipped_for_single_species(self):
        with tempfile.TemporaryDirectory() as temp:
            args = command.parser().parse_args(['--taxon','1773','--output-root',temp,'--classifier',str(CLASSIFIER)])
            with patch.object(command,'collect',side_effect=command.MissingLipidPathway('No lipid root')), patch.object(command,'build',side_effect=AssertionError('must not build')):
                self.assertEqual(command.run(args), [])


if __name__ == '__main__': unittest.main()
