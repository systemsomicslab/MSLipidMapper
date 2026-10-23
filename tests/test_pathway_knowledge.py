"""Offline integration tests for organism datasets and update safety."""
import hashlib
import importlib.util
import json
import re
import shutil
import sqlite3
import tempfile
import unittest
from pathlib import Path

from rdflib import Graph, RDF, URIRef
from rdflib.namespace import RDFS

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("knowledge", ROOT / "scripts/pathway_knowledge.py")
kb = importlib.util.module_from_spec(spec)
spec.loader.exec_module(kb)
SNAPSHOT = kb.KNOWLEDGE / "snapshots/uniprot-2026-10-15"


class OrganismDatasetTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        scratch = ROOT / ".api-validation"
        scratch.mkdir(exist_ok=True)
        cls.tmp = tempfile.TemporaryDirectory(prefix="organism-tests-", dir=scratch)
        cls.base = Path(cls.tmp.name)
        cls.root = cls.base / "knowledge"
        for directory in ("curated", "source", "ontology"):
            shutil.copytree(kb.KNOWLEDGE / directory, cls.root / directory)
        cls.release = cls.base / "one"
        kb.build(SNAPSHOT, cls.release, "test", root=cls.root)

    @classmethod
    def tearDownClass(cls):
        cls.tmp.cleanup()

    def test_coverage_species_isolation_and_stage_preservation(self):
        self.assertTrue(kb.validate(self.release)["valid"])
        self.assertTrue(kb.validate(self.release.relative_to(ROOT))["valid"])
        self.assertTrue(kb.validate(kb.KNOWLEDGE / "datasets/0.2.0")["valid"])
        expected = {"9606": 6, "10090": 6, "10116": 6, "559292": 4, "83333": 1}
        for taxon, count in expected.items():
            folder = self.release / "organisms" / taxon
            g = Graph().parse(folder / "pathways.nt", format="nt")
            self.assertEqual(len(set(g.subjects(RDF.type, kb.M.ClassReaction))), count)
            self.assertNotIn((kb.RES.reaction_pe_to_pc, RDF.type, kb.M.ClassReaction), g)
            for subject in g.subjects(RDF.type, kb.M.OrganismReactionAssertion):
                self.assertEqual(set(g.objects(subject, kb.M.organism)), {kb.TAXON[taxon]})
                protein = g.value(subject, kb.M.enzymeProtein)
                self.assertEqual(set(g.objects(protein, kb.M.organism)), {kb.TAXON[taxon]})
            self.assertFalse(list(g.triples((None, kb.M.catalyzedBy, None))))
        human = kb.search(self.release, "9606", "PC")
        self.assertEqual(len(human["reactions"]), 2)
        self.assertEqual(kb.search(self.release, "83333", "PC")["classes"], [])
        self.assertEqual(kb.search(self.release, "9606", "' OR 1=1 --")["classes"], [])
        with self.assertRaises(ValueError):
            kb.search(self.release, "../9606", "PC")

    def test_reproducible_build_and_immutable_release(self):
        root = self.base / "stable-knowledge"
        shutil.copytree(self.root, root)
        first = self.base / "stable-first"
        kb.build(SNAPSHOT, first, "test", root=root)
        other = self.base / "two"
        kb.build(SNAPSHOT, other, "test", root=root)
        for path in first.rglob("*"):
            if path.is_file():
                self.assertEqual(path.read_bytes(), (other / path.relative_to(first)).read_bytes())
        with self.assertRaises(ValueError):
            kb.build(SNAPSHOT, other, "test", root=root)
        newer = self.base / "version-change"
        kb.build(SNAPSHOT, newer, "next", root=root)
        self.assertTrue(all(not x["added_triples"] and not x["removed_triples"] for x in kb.diff(first, newer).values()))

    def test_snapshot_tampering_is_rejected(self):
        snapshot = self.base / "tampered-snapshot"
        shutil.copytree(SNAPSHOT, snapshot)
        page = snapshot / "uniprot-001.json"
        page.write_text("{}", encoding="utf-8")
        with self.assertRaisesRegex(ValueError, "hash mismatch"):
            kb.build(snapshot, self.base / "bad-release", "bad", root=self.root)
        self.assertFalse((self.base / "bad-release").exists())

    def test_absent_annotation_does_not_copy_human_pathway(self):
        snapshot = self.base / "filtered-snapshot"
        shutil.copytree(SNAPSHOT, snapshot)
        page = snapshot / "uniprot-001.json"
        data = kb.read_json(page)
        data["results"] = [p for p in data["results"] if str(p["organism"]["taxonId"]) != "10090"]
        kb.write_json(page, data)
        manifest = kb.read_json(snapshot / "manifest.json")
        manifest["pages"][0]["sha256"] = kb.sha(page)
        manifest["pages"][0]["count"] = len(data["results"])
        kb.write_json(snapshot / "manifest.json", manifest)
        release = self.base / "filtered"
        kb.build(snapshot, release, "filtered", root=self.root)
        mouse = kb.read_json(release / "organisms/10090/manifest.json")
        self.assertEqual(mouse["status"], "no_supported_reactions")
        self.assertEqual(mouse["counts"]["reactions"], 0)
        changes = kb.diff(self.release, release)
        self.assertTrue(changes["10090"]["removed_triples"])

    def test_artifact_tampering_is_rejected(self):
        release = self.base / "tampered-release"
        shutil.copytree(self.release, release)
        with (release / "organisms/9606/classes.csv").open("a") as handle:
            handle.write("bad,data,here\n")
        with self.assertRaisesRegex(ValueError, "hash mismatch"):
            kb.validate(release)

    def test_opaque_ids_and_nonunique_lipidmaps_classification(self):
        graph = Graph().parse(self.release / "organisms/10090/pathways.nt", format="nt")
        classes = kb.derive(graph)[0]
        pe_family = [row for row in classes if row["lipidmaps_class_id"] == "GP0201"]
        self.assertEqual({row["label"] for row in pe_family}, {"PE", "PMME", "PDME"})
        self.assertEqual(len({row["uri"] for row in pe_family}), 3)
        self.assertTrue(all(re.fullmatch(r"MSLMC[0-9]{6,}", row["uri"].rsplit("/", 1)[-1]) for row in classes))
        self.assertEqual(len(kb.search(self.release, "10090", "GP0201")["classes"]), 3)
        vectors = kb.read_json(kb.KNOWLEDGE / "source/identifier-test-vectors.json")
        registry = {"scheme": kb.ID_SCHEME, "entries": {}, "records": {}, "counters": {}}
        for vector in vectors:
            self.assertEqual(str(kb.resource_uri(*vector["keys"], registry=registry)), vector["uri"])
        for reaction in graph.subjects(RDF.type, kb.M.ClassReaction):
            self.assertTrue(list(graph.objects(reaction, kb.M.sourceDatabase)))
            self.assertTrue(list(graph.objects(reaction, kb.M.sourceUrl)))
            for record in graph.objects(reaction, kb.M.hasSourceAssertion):
                self.assertIsNotNone(graph.value(record, kb.M.sourceIdentifier))
                self.assertIsNotNone(graph.value(record, kb.M.sourceRelease))

    def test_label_changes_do_not_change_resource_identity(self):
        root = self.base / "renamed-source"
        for directory in ("curated", "source", "ontology"):
            shutil.copytree(kb.KNOWLEDGE / directory, root / directory)
        path = root / "curated/phospholipid-pilot.ttl"
        path.write_text(path.read_text(encoding="utf-8").replace(
            '"diacylglycerophosphocholine"@en', '"Changed display label"@en'), encoding="utf-8")
        release = self.base / "renamed-label"
        kb.build(SNAPSHOT, release, "test", root=root)
        a = kb.search(self.release, "10090", "PC")["classes"][0]["uri"]
        b = kb.search(release, "10090", "PC")["classes"][0]["uri"]
        self.assertEqual(a, b)

    def test_accessions_are_not_reused_and_counter_rollback_is_rejected(self):
        registry = {"scheme": kb.ID_SCHEME, "entries": {}, "records": {}, "counters": {}}
        first = kb.resource_uri("test", "retired", registry=registry)
        registry["records"].clear()  # Simulate retirement while retaining high-water marks.
        second = kb.resource_uri("test", "new", registry=registry)
        self.assertNotEqual(first, second)
        self.assertEqual(second, kb.resource_uri("test", "new", registry=registry))
        registry["counters"]["Z"] = 0
        with self.assertRaisesRegex(ValueError, "backwards"):
            kb.validate_id_registry(registry)


if __name__ == "__main__":
    unittest.main()
