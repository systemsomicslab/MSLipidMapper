import sys
import sqlite3
import unittest
import xml.etree.ElementTree as ET
from pathlib import Path
from rdflib import Graph, RDF, URIRef, Literal
sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'scripts'))
import pathway_knowledge as kb
from build_reactome_catalog import references, species_record, stable_id
from collect_reactome_mouse import index_hierarchy

SNAPSHOT = kb.KNOWLEDGE / 'snapshots/reactome-mouse-2026-10-15'
OUTPUT = kb.KNOWLEDGE / 'drafts/mouse-reactome-2026-10-15'


class ReactomeTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.rows = kb.read_json(OUTPUT / 'candidates.json')
        cls.graph = Graph().parse(OUTPUT / 'candidates.ttl')
        cls.cg = Graph().parse(OUTPUT / 'class-pathways.ttl')

    def test_scope_matches_independent_sbml_and_hierarchy(self):
        hierarchy = kb.read_json(SNAPSHOT / 'hierarchy.json')
        pending = list(hierarchy)
        while pending:
            node = pending.pop()
            if node['stId'] == 'R-MMU-556833':
                root = node; break
            pending.extend(node.get('children', []))
        pathways, events, memberships = index_hierarchy(root)
        xml = ET.parse(SNAPSHOT / 'lipid-pathway.sbml').getroot()
        sbml_ids = {stable_id(e) for e in xml.iter() if e.tag.endswith('}reaction')}
        self.assertEqual(set(events), sbml_ids)
        self.assertEqual(set(events), {r['source']['identifier'] for r in self.rows})
        self.assertEqual(len(events), 711)
        self.assertEqual(len(pathways), 117)
        self.assertEqual(len(self.rows), len(events))
        for parent, child in memberships:
            self.assertIn((URIRef('https://reactome.org/content/detail/' + parent), kb.M.hasEvent,
                           URIRef('https://reactome.org/content/detail/' + child)), self.graph)

    def test_taxon_source_and_inference_are_preserved(self):
        originals = {r['stId']: r for r in kb.read_json(SNAPSHOT / 'events.json')}
        for r in self.rows:
            event = originals[r['source']['identifier']]
            self.assertEqual({str(s['taxId']) for s in event['species']}, {'10090'})
            self.assertEqual(r['is_inferred'], event['isInferred'])
            uri = URIRef(r['reaction'])
            self.assertIn((uri, kb.M.isInferred, Literal(event['isInferred'])), self.graph)
            self.assertIn((uri, kb.M.sourceDatabase, Literal('Reactome')), self.graph)
            self.assertIn((uri, kb.M.sourceRelease, Literal('97')), self.graph)
            self.assertEqual(r['source']['stable_id_version'], event['stIdVersion'])
            self.assertTrue(r['pathway_ids'])

    def test_no_container_components_projected_as_free_substrates(self):
        containers = 0
        for row in self.rows:
            for part in row['participants']:
                if part['entity_type'] != 'SimpleEntity':
                    containers += 1
                    self.assertEqual(part['mapping'], [])
            terms = {c for p in row['participants'] if p['entity_type'] == 'SimpleEntity' for c in p['chebi']}
            self.assertTrue(all(c['chebi'] in terms for c in row['classes']))
        self.assertGreater(containers, 0)
        self.assertTrue(kb.read_json(OUTPUT / 'mapping-review-queue.json'))

    def test_rdf_jsonld_roundtrip_and_persistent_identifiers(self):
        other = Graph().parse(OUTPUT / 'pathways.jsonld', format='json-ld')
        self.assertEqual(set(self.graph), set(other))
        kb.validate_id_registry(kb.read_json(kb.KNOWLEDGE / 'source/id-registry.json'))
        for g in (self.graph, self.cg):
            for subject in set(g.subjects()):
                if str(subject).startswith(str(kb.RES)):
                    self.assertRegex(str(subject).rsplit('/', 1)[-1], r'^MSLM[A-Z]\d{6,}$')
        rhea = Graph().parse(kb.KNOWLEDGE / 'drafts/mouse-lipid-census-2026-10-15/class-pathways.ttl')
        shared = set(self.cg.subjects(kb.M.lipidmapsClassId)) & set(rhea.subjects(kb.M.lipidmapsClassId))
        self.assertGreater(len(shared), 40)
        for uri in shared:
            self.assertEqual(set(self.cg.objects(uri, kb.M.lipidmapsClassId)), set(rhea.objects(uri, kb.M.lipidmapsClassId)))

    def test_sqlite_memberships_and_no_fabricated_protein_evidence(self):
        with sqlite3.connect(OUTPUT / 'search.sqlite') as db:
            self.assertEqual(db.execute('SELECT count(*) FROM reactions').fetchone()[0], 711)
            self.assertEqual(db.execute('SELECT count(*) FROM pathways').fetchone()[0], 117)
            self.assertEqual(db.execute('SELECT count(*) FROM reactome_events WHERE is_inferred=1').fetchone()[0], 711)
            self.assertEqual(db.execute('SELECT count(*) FROM reactions WHERE reviewed_protein_count IS NOT NULL').fetchone()[0], 0)
            self.assertEqual(db.execute('SELECT count(*) FROM pathway_membership').fetchone()[0], len(kb.read_json(SNAPSHOT / 'pathway-membership.json')))


if __name__ == '__main__':
    unittest.main()
