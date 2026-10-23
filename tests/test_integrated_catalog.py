import copy
import sqlite3
import sys
import unittest
from pathlib import Path
from rdflib import Graph, RDF, URIRef
sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'scripts'))
import pathway_knowledge as kb
from integrate_pathway_catalogs import equation_signature, class_signature, match_records

OUTPUT = kb.KNOWLEDGE / 'drafts/mouse-integrated-2026-10-15'
RHEA = kb.KNOWLEDGE / 'drafts/mouse-lipid-census-2026-10-15'
REACTOME = kb.KNOWLEDGE / 'drafts/reactome-10090-all-species-first'


class IntegrationTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.rows = kb.read_json(OUTPUT/'candidates.json')
        cls.matches = kb.read_json(OUTPUT/'cross-source-matches.json')

    def test_union_preserves_every_source_record_and_evidence(self):
        indexed = {r['reaction']:r for r in self.rows}
        self.assertEqual(len(indexed), 3661)
        for source in (RHEA, REACTOME):
            for original in kb.read_json(source/'candidates.json'):
                row = indexed[original['reaction']]
                self.assertEqual({k:row[k] for k in original}, original)
        self.assertEqual(sum(r['source']['database']=='Reactome' and r['is_inferred'] for r in self.rows),711)

    def test_input_hashes_and_no_unverified_identity(self):
        report = kb.read_json(OUTPUT/'report.json')
        for folder, hashes in report['input_hashes'].items():
            for name, expected in hashes.items():
                self.assertEqual(kb.sha(OUTPUT.parent/folder/name), expected)
        self.assertEqual(report['match_counts']['verified-equivalence'],0)
        indexed = {r['reaction']:r for r in self.rows}
        for m in self.matches:
            self.assertEqual(m['direct_source_reference'],False)
            if m['kind']=='exact-chemical-equation':
                self.assertEqual(equation_signature(indexed[m['rhea']]),equation_signature(indexed[m['reactome']]))
                self.assertIsNotNone(equation_signature(indexed[m['rhea']]))
            else:
                self.assertEqual(m['kind'],'class-signature')
                self.assertEqual(class_signature(indexed[m['rhea']]),class_signature(indexed[m['reactome']]))

    def test_matching_does_not_rewrite_human_ids_and_needs_both_checks(self):
        row = next(r for r in self.rows if r['source']['database']=='Rhea' and len(r['participants'])>=2)
        rhea = copy.deepcopy(row)
        # A minimal complete chemical equation, independent of lipid mapping.
        rhea['participants'] = [{'participant':'a','side':'x_L','chebi':'CHEBI_A','coefficient':'1'},
                                {'participant':'b','side':'x_R','chebi':'CHEBI_B','coefficient':'1'}]
        reactome = {'reaction':'https://reactome.org/content/detail/R-MMU-1','source':{'database':'Reactome'},
            'participants':[{'entity_type':'SimpleEntity','side':'L','chebi':['CHEBI_A'],'stoichiometry':'1.0'},
                            {'entity_type':'SimpleEntity','side':'R','chebi':['CHEBI_B'],'stoichiometry':'1.0'}],
            'class_mapping_status':'partial','classes':[]}
        rhea['related_records']=[{'relation':'http://www.w3.org/2000/01/rdf-schema#seeAlso','identifier':'http://identifiers.org/reactome/R-HSA-1.2'}]
        self.assertEqual(match_records([rhea],[reactome])[0]['kind'],'exact-chemical-equation')
        rhea['related_records'][0]['identifier']='http://identifiers.org/reactome/R-MMU-1.2'
        self.assertEqual(match_records([rhea],[reactome])[0]['kind'],'verified-equivalence')
        reactome['participants'][0]['stoichiometry']='2'
        self.assertEqual(match_records([rhea],[reactome])[0]['kind'],'source-cross-reference')
        reactome['participants'][0]['entity_type']='DefinedSet'
        self.assertIsNone(equation_signature(reactome))
        rhea['participants'][0]['coefficient']='n'
        self.assertIsNone(equation_signature(rhea))

    def test_sqlite_counts_and_source_evidence(self):
        with sqlite3.connect(OUTPUT/'search.sqlite') as db:
            self.assertEqual(db.execute('SELECT count(*) FROM reactions').fetchone()[0],3661)
            self.assertEqual(db.execute('SELECT count(*) FROM cross_source_matches').fetchone()[0],1049)
            self.assertEqual(db.execute("SELECT count(*) FROM source_records WHERE database_name='Reactome' AND inferred=1").fetchone()[0],711)
            self.assertEqual(db.execute('SELECT count(*) FROM classes').fetchone()[0],119)

    def test_rdf_roundtrip_retains_sources_and_accessions(self):
        graph = Graph().parse(OUTPUT/'candidates.ttl')
        other = Graph().parse(OUTPUT/'pathways.jsonld', format='json-ld')
        self.assertEqual(set(graph),set(other))
        for folder in (RHEA,REACTOME):
            for triple in Graph().parse(folder/'candidates.ttl'):
                self.assertIn(triple,graph)
        self.assertEqual(len(list(graph.subjects(RDF.type,kb.M.CrossSourceReactionMatch))),1049)
        self.assertEqual(len(list(graph.subjects(RDF.type,kb.M.VerifiedReactionGroup))),0)
        self.assertEqual(len(list(graph.triples((None,URIRef('http://www.w3.org/2002/07/owl#sameAs'),None)))),0)
        for subject in set(graph.subjects()):
            if str(subject).startswith(str(kb.RES)):
                self.assertRegex(str(subject).rsplit('/',1)[-1],r'^MSLM[A-Z]\d{6,}$')


if __name__=='__main__': unittest.main()
