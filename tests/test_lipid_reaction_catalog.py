import json
import sqlite3
import sys
import unittest
from collections import Counter
from pathlib import Path
from rdflib import Graph, RDF, BNode

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "scripts"))
import pathway_knowledge as kb
from build_lipid_reaction_catalog import bindings, classification
from lipid_ontology import lipid_terms, closure

SNAPSHOT = kb.KNOWLEDGE / "snapshots/lipid-census-2026-10-15"
OUTPUT = kb.KNOWLEDGE / "drafts/mouse-lipid-census-2026-10-15"


class CatalogTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.report = kb.read_json(OUTPUT / "report.json")
        cls.candidates = kb.read_json(OUTPUT / "candidates.json")
        cls.graph = Graph().parse(OUTPUT / "candidates.ttl")

    def test_global_census_is_complete_but_not_all_assigned_to_mouse(self):
        membership = {r["reaction"] for r in bindings(SNAPSHOT / "lipid-membership-v4.json")}
        universe = bindings(SNAPSHOT / "all-rhea-chemical-pairs.json")
        count = int(bindings(SNAPSHOT / "all-rhea-chemical-pair-count.json")[0]["count"])
        self.assertEqual(len(universe), count)
        terms = lipid_terms(SNAPSHOT)
        self.assertEqual(membership, {r["reaction"] for r in universe if r["chebi"] in terms})
        self.assertEqual(len(self.candidates), self.report["candidate_count"])
        self.assertLess(len(self.candidates), len(membership))
        self.assertTrue(all(c["protein_evidence"] for c in self.candidates))
        self.assertEqual(len({c["reaction"] for c in self.candidates}), len(self.candidates))
        source_ids = {r["reaction"] for r in bindings(SNAPSHOT / "rhea-approved-masters-v3.json")}
        self.assertTrue(all(c["reaction"] in source_ids for c in self.candidates))

    def test_exact_taxon_and_annotation_join(self):
        for path in SNAPSHOT.glob("mouse-proteins-*.json"):
            if path.name.endswith("meta.json"):
                continue
            self.assertTrue(all(p["organism"]["taxonId"] == 10090 for p in kb.read_json(path)["results"]))
        aliases = {r["annotated"]: r["reaction"] for r in bindings(SNAPSHOT / "rhea-directional-aliases-v3.json")}
        for candidate in self.candidates:
            for item in candidate["protein_evidence"]:
                annotated = "http://rdf.rhea-db.org/" + item["rhea"].split(":")[-1]
                self.assertEqual(aliases.get(annotated, annotated), candidate["reaction"])

    def test_charged_lipids_and_classification_are_not_lost(self):
        resolve, vocabulary = classification(SNAPSHOT)
        terms = {r["chebi"] for r in bindings(SNAPSHOT / "lipid-membership-v4.json")}
        for identifier, code in [("58608", "GP1001"), ("57880", "GP0601"), ("64716", "GP0401"), ("52639", "SP0201"),
                                 ("64612", "GP0201"), ("64573", "GP0201"), ("64572", "GP0201")]:
            term = "http://purl.obolibrary.org/obo/CHEBI_" + identifier
            self.assertIn(term, terms)
            self.assertEqual(resolve(term)["codes"], [code])
        self.assertEqual(resolve("urn:unknown-lipid")["status"], "unmapped")
        mappings = kb.read_json(OUTPUT / "class-mappings.json")
        self.assertTrue(all(value[1] in vocabulary for value in mappings.values()))
        self.assertEqual(len({v[1] for v in mappings.values()}), self.report["mapped_lipidmaps_classification_count"])
        self.assertIn("http://rdf.rhea-db.org/11164", {r["reaction"] for r in self.candidates})

    def test_transport_same_class_and_incomplete_mappings_are_retained(self):
        kinds = Counter(c["kind"] for c in self.candidates)
        self.assertGreater(kinds["transport"], 0)
        self.assertGreater(kinds["same-class-or-unresolved-conversion"], 0)
        self.assertTrue(any(c["class_mapping_status"] == "partial-or-unmapped" for c in self.candidates))
        self.assertTrue(any(not c["left"] or not c["right"] for c in self.candidates))

    def test_rdf_provenance_participants_and_round_trip(self):
        from rdflib import URIRef
        graph = self.graph
        for candidate in self.candidates:
            subject = URIRef(candidate["reaction"])
            self.assertEqual(set(graph.objects(subject, kb.M.taxon)), {kb.TAXON["10090"]})
            self.assertEqual(set(graph.objects(subject, kb.M.directionality)), {kb.M.UnknownDirection})
            self.assertTrue(list(graph.objects(subject, kb.M.sourceRelease)))
            self.assertTrue(list(graph.objects(subject, kb.M.hasSourceAssertion)))
            self.assertTrue(list(graph.objects(subject, kb.M.supportedByProtein)))
            for participant in graph.objects(subject, kb.M.hasParticipant):
                self.assertEqual(len(list(graph.objects(participant, kb.M.participantEntity))), 1)
                self.assertEqual(len(list(graph.objects(participant, kb.M.participantRole))), 1)
                self.assertEqual(len(list(graph.objects(participant, kb.M.stoichiometry))) +
                    len(list(graph.objects(participant, kb.M.coefficientExpression))), 1)
        self.assertFalse(any(isinstance(term, BNode) for triple in graph for term in triple))
        other = Graph().parse(OUTPUT / "pathways.jsonld", format="json-ld")
        missing, extra = set(graph) - set(other), set(other) - set(graph)
        self.assertTrue(not missing and not extra,
            f"RDF round-trip mismatch: {len(missing)} missing / {len(extra)} extra; examples: {list(missing)[:2]}")

    def test_search_index_matches_source_collection(self):
        with sqlite3.connect(OUTPUT / "search.sqlite") as connection:
            self.assertEqual(connection.execute("SELECT COUNT(*) FROM reactions").fetchone()[0], len(self.candidates))
            self.assertGreater(connection.execute("SELECT COUNT(*) FROM reactions WHERE mapping_status='partial-or-unmapped'").fetchone()[0], 0)
            self.assertGreater(connection.execute("SELECT COUNT(DISTINCT reaction) FROM reaction_classes JOIN classes ON class_uri=classes.uri WHERE lipidmaps_class_id='GP0101'").fetchone()[0], 0)

    def test_class_projection_uses_class_ids_and_preserves_partial_status(self):
        graph = Graph().parse(OUTPUT / "class-pathways.ttl")
        projections = set(graph.subjects(RDF.type, kb.M.ReactionTraversalProjection))
        self.assertEqual(len(projections), len(self.candidates))
        for projection in projections:
            self.assertTrue(str(projection).startswith(str(kb.RES) + "MSLMR"))
            for predicate in (kb.M.leftClass, kb.M.rightClass):
                for term in graph.objects(projection, predicate):
                    self.assertTrue(str(term).startswith(str(kb.RES) + "MSLMC"))
                    self.assertTrue(list(graph.objects(term, kb.M.lipidmapsClassId)))
        self.assertEqual(len(set(graph.subjects(RDF.type, kb.M.LipidEntity))), self.report["biochemical_class_node_count"])

    def test_normalization_keeps_form_changes_and_handles_cycles(self):
        root = "http://purl.obolibrary.org/obo/CHEBI_18059"
        result = closure([{"chebi": "neutral", "parent": root}, {"chebi": "specific", "parent": "zwitterion"}],
            [{"neutral": "neutral", "charged": "anion"}, {"neutral": "anion", "charged": "zwitterion"},
             {"neutral": "zwitterion", "charged": "neutral"}])
        self.assertEqual(result, {root, "neutral", "anion", "zwitterion", "specific"})


if __name__ == "__main__":
    unittest.main()
