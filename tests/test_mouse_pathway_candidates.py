import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "scripts"))
from map_mouse_pathway_candidates import project


def row(side, chebi, annotated="1", transport="false"):
    return {"reaction": "http://rdf.rhea-db.org/10000", "annotated": annotated,
        "side": "http://rdf.rhea-db.org/10000_" + side,
        "participant": "http://rdf.rhea-db.org/participant/" + side,
        "compound": "http://rdf.rhea-db.org/Compound_" + chebi,
        "chebi": "http://purl.obolibrary.org/obo/CHEBI_" + chebi,
        "name": "test", "coefficient": "N", "transport": transport}


class ProjectionTests(unittest.TestCase):
    def test_annotation_variants_do_not_duplicate_participants(self):
        result, _ = project([row("L", "57643"), row("L", "57643", "2"), row("R", "58168")])
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0]["participants"]), 2)
        self.assertEqual(result[0]["participants"][0]["coefficient"], "N")

    def test_transport_and_same_class_not_candidates(self):
        self.assertEqual(project([row("L", "57643"), row("R", "57643")])[0], [])
        self.assertEqual(project([row("L", "57643", transport="true"), row("R", "58168")])[0], [])

    def test_unknown_compounds_are_queued_without_guessing(self):
        candidates, queue = project([row("L", "57643"), row("R", "999999")])
        self.assertEqual(candidates, [])
        self.assertIn("http://purl.obolibrary.org/obo/CHEBI_999999", queue)


if __name__ == "__main__":
    unittest.main()
