"""Build an explicitly provisional class projection from an archived mouse census."""
import argparse
import csv
from collections import defaultdict
from pathlib import Path
from rdflib import Graph, URIRef, Literal, RDF, RDFS
import pathway_knowledge as kb

# Exact generic ChEBI terms only; individual molecular species are not inferred.
MAPPINGS = {
    "57643": ("PC", "GP0101"), "64612": ("PE", "GP0201"),
    "57262": ("PS", "GP0301"), "64573": ("PMME", "GP0201"),
    "64572": ("PDME", "GP0201"), "58168": ("LPC", "GP0105"),
    "57875": ("LPC", "GP0105"), "64381": ("LPE", "GP0205"),
    "17815": ("DG", "GL0201"), "49172": ("DG", "GL0201"),
    "17855": ("TG", "GL0301"),
}


def project(rows):
    reactions = defaultdict(dict)
    for row in rows:
        key = tuple(sorted((k, v) for k, v in row.items() if k != "annotated"))
        reactions[row["reaction"]][key] = row
    candidates, unresolved = [], {}
    for reaction, participants in sorted(reactions.items()):
        sides = {"L": set(), "R": set()}
        classes = []
        for row in participants.values():
            chebi = row.get("chebi", "").rsplit("_", 1)[-1]
            side = row["side"].rsplit("_", 1)[-1]
            if chebi in MAPPINGS and side in sides:
                label, code = MAPPINGS[chebi]
                sides[side].add(label)
                classes.append({**row, "label": label, "lipidmaps": code, "side_code": side})
            elif row.get("chebi"):
                unresolved[row["chebi"]] = row["name"]
        if sides["L"] and sides["R"] and sides["L"] != sides["R"] and not any(
                row.get("transport") in ("true", "1") for row in participants.values()):
            candidates.append({"reaction": reaction, "left": sorted(sides["L"]),
                "right": sorted(sides["R"]), "classes": classes,
                "participants": list(participants.values())})
    return candidates, unresolved


def build(snapshot, output):
    snapshot, output = Path(snapshot), Path(output)
    manifest = kb.read_json(snapshot / "manifest.json")
    if manifest["taxon_id"] != "10090":
        raise ValueError("Mouse snapshot required")
    for name, expected in manifest["files"].items():
        if kb.sha(snapshot / name) != expected:
            raise ValueError("Snapshot hash mismatch: " + name)
    releases = {page["headers"].get("X-Release") for page in manifest["rhea_pages"]}
    if len(releases) != 1 or None in releases:
        raise ValueError("Missing or inconsistent Rhea release")
    rows = kb.read_json(snapshot / "participants.json")
    annotations = kb.read_json(snapshot / "annotations.json")
    proteins = defaultdict(set)
    for item in annotations:
        proteins[item["rhea"].split(":")[-1]].add(item["protein"])
    candidates, unresolved = project(rows)
    graph = Graph()
    graph.bind("mslm", kb.M)
    graph.bind("rhea", kb.RHEA)
    for item in candidates:
        reaction = URIRef(item["reaction"])
        graph.add((reaction, RDF.type, kb.M.ClassReactionCandidate))
        graph.add((reaction, kb.M.taxon, kb.TAXON["10090"]))
        graph.add((reaction, kb.M.reviewStatus, Literal("unreviewed-class-projection")))
        graph.add((reaction, kb.M.direction, Literal("unknown")))
        graph.add((reaction, kb.M.sourceDatabase, Literal("Rhea")))
        graph.add((reaction, kb.M.sourceIdentifier, Literal("RHEA:" + str(reaction).rsplit("/", 1)[-1])))
        graph.add((reaction, kb.M.sourceRelease, Literal(next(iter(releases)))))
        graph.add((reaction, kb.M.sourceUrl, reaction))
        graph.add((reaction, kb.DCT.source, reaction))
        for row in rows:
            if row["reaction"] == str(reaction):
                for protein in proteins[row["annotated"].rsplit("/", 1)[-1]]:
                    graph.add((reaction, kb.M.supportedByProtein, kb.UP[protein]))
        for row in item["classes"]:
            term = URIRef(row["chebi"])
            graph.add((reaction, kb.M.leftClass if row["side_code"] == "L" else kb.M.rightClass, term))
            graph.add((term, RDFS.label, Literal(row["label"])))
            graph.add((term, kb.M.lipidmapsClassId, Literal(row["lipidmaps"])))
    output.mkdir(parents=True, exist_ok=True)
    graph.serialize(output / "candidates.ttl", format="turtle")
    kb.write_json(output / "candidates.json", candidates)
    kb.write_json(output / "class-mappings.json", MAPPINGS)
    with (output / "candidates.csv").open("w", encoding="utf-8", newline="") as handle:
        writer = csv.writer(handle)
        writer.writerow(["rhea_uri", "left_classes", "right_classes", "review_status"])
        for item in candidates:
            writer.writerow([item["reaction"], ";".join(item["left"]), ";".join(item["right"]), "unreviewed"])
    kb.write_json(output / "unmapped-compounds.json", unresolved)
    report = {"taxon_id": "10090", "status": "draft-not-a-published-pathway-dataset",
        "protein_count": manifest["protein_count"], "master_reaction_count": manifest["master_reaction_count"],
        "candidate_count": len(candidates), "unmapped_compound_count": len(unresolved),
        "rhea_release": next(iter(releases)), "snapshot_manifest_sha256": kb.sha(snapshot / "manifest.json"),
        "limitations": ["GO selection is not an exhaustive mouse lipid pathway census.",
            "Exact generic ChEBI mappings only; unmapped compounds include cofactors and unsupported lipids.",
            "Left/right is canonical Rhea equation orientation, not physiological direction.",
            "Candidates require review of all participants before publication or measurement mapping."]}
    kb.write_json(output / "report.json", report)
    print(report)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--snapshot", required=True)
    parser.add_argument("--output", required=True)
    args = parser.parse_args()
    build(args.snapshot, args.output)
