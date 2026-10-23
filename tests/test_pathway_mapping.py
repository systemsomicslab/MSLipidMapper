import csv
import io
import sqlite3
import sys
import unittest
from pathlib import Path
from unittest.mock import patch
from rdflib import Graph, RDF, URIRef
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'scripts'))
import pathway_knowledge as kb
from pathway_mapping import select_rows, sides

FOLDER=kb.KNOWLEDGE/'drafts/mouse-integrated-2026-10-15'


class MappingTests(unittest.TestCase):
    def test_profile_keeps_only_complete_biochemical_class_conversions(self):
        rows=select_rows(FOLDER)
        self.assertEqual(len(rows),1231)
        with (FOLDER/'mapping-reactions.csv').open(encoding='utf-8') as handle:
            features={r['reaction']:r for r in csv.DictReader(handle)}
        for r in rows:
            self.assertEqual(r['class_mapping_status'],'complete')
            self.assertTrue(sides(r)['L'] and sides(r)['R'])
            self.assertNotEqual(sides(r)['L'],sides(r)['R'])
            self.assertNotEqual(r['kind'],'transport')
            self.assertIn(features[r['reaction']]['category'],['transition','rhea-reaction'])

    def test_pathway_and_candidates_remain_separate(self):
        direct=select_rows(FOLDER,pathways=['R-MMU-428157'])
        equations=select_rows(FOLDER,pathways=['R-MMU-428157'],candidates='equation')
        classes=select_rows(FOLDER,pathways=['R-MMU-428157'],candidates='all')
        self.assertEqual(len(direct),21)
        self.assertTrue(all(r['source']['database']=='Reactome' for r in direct))
        self.assertEqual(len(equations),27)
        self.assertEqual(len(classes),38)
        self.assertTrue({r['reaction'] for r in direct}<={r['reaction'] for r in equations}<={r['reaction'] for r in classes})
        self.assertEqual(select_rows(FOLDER,pathways=['R-MMU-428157'],exclude_inferred=True),[])
        with self.assertRaisesRegex(ValueError,'Unknown pathway'):select_rows(FOLDER,pathways=['R-HSA-428157'])

    def test_measured_modes_and_intermediates_exclude_spurs(self):
        def row(name,a,b):return {'reaction':name,'classes':[{'canonical_class':a,'lipidmaps':a,'side_code':'L'},
            {'canonical_class':b,'lipidmaps':b,'side_code':'R'}]}
        rows=[row('AX','A','X'),row('XB','X','B'),row('XC','X','C')]
        features='reaction,mapping_eligible,inferred\nAX,True,false\nXB,True,false\nXC,True,false\n'
        def read_json(path):
            if path.name=='candidates.json':return rows
            if path.name=='mapping-groups-report.json':return {'source_files':{}}
            raise AssertionError(path)
        def open_file(path,*args,**kwargs):return io.StringIO(features if path.name=='mapping-reactions.csv' else 'reaction,pathway,evidence\n')
        with patch.object(kb,'read_json',side_effect=read_json),patch.object(Path,'open',open_file):
            self.assertEqual(select_rows(Path('fixture'),measured=['A','B'],measured_mode='both'),[])
            self.assertEqual({r['reaction'] for r in select_rows(Path('fixture'),measured=['A','B'],measured_mode='bridge',bridge_depth=2)},{'AX','XB'})
            self.assertEqual({r['reaction'] for r in select_rows(Path('fixture'),measured=['A','B'],measured_mode='bridge',bridge_depth=4)},{'AX','XB'})
            self.assertEqual(select_rows(Path('fixture'),measured=['A'],measured_mode='bridge'),[])

    def test_export_contains_only_selected_projections_with_membership(self):
        folder=kb.KNOWLEDGE/'views/mouse-glycerophospholipid-mapping'
        rows=kb.read_json(folder/'selected-reactions.json')
        self.assertEqual(len(rows),25)
        graph=Graph().parse(folder/'selected-class-pathways.ttl')
        other=Graph().parse(folder/'selected-class-pathways.jsonld',format='json-ld')
        self.assertEqual(set(graph),set(other))
        expected={URIRef(r.get('projection_uri') or r['projection']) for r in rows}
        self.assertEqual(set(graph.subjects(RDF.type,kb.M.ReactionTraversalProjection)),expected)
        self.assertTrue(set(graph.objects(None,kb.M.membershipEvidence)) )
        self.assertTrue(all(str(v) in ['direct','ancestor','verified-derived'] for v in graph.objects(None,kb.M.membershipEvidence)))
        gui=Graph().parse(folder/'selected-class-pathways-gui.ttl')
        self.assertEqual(set(gui.subjects(RDF.type,kb.M.ReactionTraversalProjection)),expected)
        self.assertEqual(set(gui.objects(None,kb.M.organism)),{kb.TAXON['10090']})

    def test_grouping_sqlite_and_rdf_agree(self):
        graph=Graph().parse(FOLDER/'pathway-groups.ttl')
        with sqlite3.connect(FOLDER/'search.sqlite') as db:
            self.assertEqual(db.execute('SELECT count(*) FROM mapping_features WHERE mapping_eligible=1').fetchone()[0],1231)
            count=db.execute('SELECT count(*) FROM pathway_groups').fetchone()[0]
            self.assertEqual(count,len(set(graph.subjects(RDF.type,kb.M.PathwayGroupMembership))))

    def test_all_species_have_grouping_without_source_edits(self):
        run=kb.read_json(kb.KNOWLEDGE/'drafts/reactome-run-all-species-first.json')
        self.assertEqual(len(run['completed']),14)
        for record in run['completed']:
            folder=Path(record['output']);report=kb.read_json(folder/'mapping-groups-report.json')
            self.assertEqual(report['taxon_id'],record['taxon_id'])
            self.assertEqual(report['source_reaction_count'],record['event_count'])
            for name,digest in report['source_files'].items():self.assertEqual(kb.sha(folder/name),digest)


if __name__=='__main__':unittest.main()
