"""Validate the checked-in measurement examples as real RDF without network access."""
import re
import unittest
from pathlib import Path

from rdflib import Graph, Literal, Namespace, RDF, URIRef
from rdflib.compare import isomorphic

ROOT = Path(__file__).resolve().parents[1]
M = Namespace("https://systemomicslab.github.io/MSLipidMapper/ontology/")
LOCAL = "https://systemomicslab.github.io/MSLipidMapper/resource/"


class IdentifierRDFTests(unittest.TestCase):
    def test_examples_use_ids_and_preserve_rdf_links(self):
        for filename in ("pathway-measurements-example.json", "organism-pathway-measurements-example.jsonld"):
            with self.subTest(filename=filename):
                graph = Graph().parse(ROOT / "inst/extdata/examples" / filename, format="json-ld")
                self.assertGreater(len(graph), 0)
                subjects = set(graph.subjects())
                for subject, predicate, obj in graph:
                    for term in (subject, obj):
                        if isinstance(term, URIRef) and str(term).startswith(LOCAL):
                            self.assertRegex(str(term), re.escape(LOCAL) + r"(?:MSLM[A-Z][0-9]{6,}|lipidmaps-classification/GP[0-9]{4})$")
                    if isinstance(obj, URIRef) and str(obj).startswith(LOCAL) and predicate != M.canonicalEntity:
                        self.assertIn(obj, subjects)
                dataset = next(graph.subjects(RDF.type, M.Dataset))
                self.assertEqual(str(graph.value(dataset, M.schemaVersion)), "4.0.0")
                self.assertEqual(str(graph.value(dataset, M.identifierScheme)), "mslm-accession-v1")
                pc = next(graph.subjects(M.msdialOntologyLabel, Literal("PC")))
                self.assertEqual(str(graph.value(pc, M.lipidMapsSubClassCode)), "GP0101")
                normalized = Graph()
                for s, p, o in graph:
                    if isinstance(o, Literal) and p in (M.value, M.validFeatureCount):
                        o = Literal(float(o) if p == M.value else int(o), datatype=o.datatype)
                    normalized.add((s, p, o))
                round_trip = Graph().parse(data=normalized.serialize(format="turtle"), format="turtle")
                self.assertTrue(isomorphic(normalized, round_trip))

    def test_organism_measurement_export_has_distinct_pe_classes(self):
        graph = Graph().parse(ROOT / "inst/extdata/examples/organism-pathway-measurements-example.jsonld", format="json-ld")
        self.assertEqual(len(set(graph.subjects(RDF.type, M.ClassReaction))), 6)
        self.assertEqual(len(set(graph.subjects(RDF.type, M.MolecularFeature))), 3)
        classes = [s for s in graph.subjects(RDF.type, M.LipidEntity)
                   if (s, M.lipidMapsSubClassCode, Literal("GP0201")) in graph]
        self.assertEqual(len(classes), 3)
        self.assertEqual({str(graph.value(s, M.classLabel)) for s in classes}, {"PE", "PMME", "PDME"})


if __name__ == "__main__":
    unittest.main()
