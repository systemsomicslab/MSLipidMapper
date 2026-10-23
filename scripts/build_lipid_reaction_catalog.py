"""Build the complete source-defined lipid reaction catalog and mouse collection.

Unmapped lipid compounds, transport, one-sided synthesis/degradation and same-class
reactions are retained. Classification ambiguity never removes a source reaction.
"""
import argparse
import csv
import json
import sqlite3
import os
from decimal import Decimal, InvalidOperation
from contextlib import closing
from collections import defaultdict
from pathlib import Path
from rdflib import Graph, Literal, URIRef, RDF, RDFS
from rdflib.namespace import XSD
import pathway_knowledge as kb
from map_mouse_pathway_candidates import MAPPINGS


def bindings(path):
    return [{key: value["value"] for key, value in row.items()}
        for row in kb.read_json(path)["results"]["bindings"]]


def classification(folder):
    vocabulary = kb.read_json(folder / "lipidmaps-vocabulary.json")
    parents, direct, evidence = defaultdict(set), defaultdict(set), defaultdict(set)
    for row in bindings(folder / "chebi-all-is-a.json"):
        parents[row["chebi"]].add(row["parent"])
    for row in bindings(folder / "chebi-lipidmaps-xrefs-v3.json"):
        identifier = row["xref"].split(":", 1)[-1]
        if not identifier.startswith("LM"):
            continue
        # LM structure accessions embed category/main/subclass in their prefix.
        codes = [identifier[2:2+size] for size in (6, 4, 2) if identifier[2:2+size] in vocabulary]
        if codes:
            direct[row["chebi"]].add(codes[0])
            evidence[row["chebi"]].add(row["xref"])
    for identifier, (_, code) in MAPPINGS.items():
        direct["http://purl.obolibrary.org/obo/CHEBI_" + identifier] = {code}
        evidence["http://purl.obolibrary.org/obo/CHEBI_" + identifier].add("curated-exact-generic-class")
    for identifier, code in kb.read_json(kb.KNOWLEDGE / "source/lipid-class-seeds.json")["mappings"].items():
        if code not in vocabulary:
            raise ValueError("Unrecognized LIPID MAPS seed code: " + code)
        term = "http://purl.obolibrary.org/obo/CHEBI_" + identifier
        if direct[term] and direct[term] != {code}:
            raise ValueError("Conflicting exact source and curated classification: " + term)
        direct[term] = {code}
        evidence[term].add("curated-exact-generic-class")
    charge = defaultdict(set)
    for row in bindings(folder / "chebi-all-chemical-forms.json"):
        charge[row["charged"]].add(row["neutral"])
        charge[row["neutral"]].add(row["charged"])

    def biochemical_label(term):
        pending, seen, labels = {term}, set(), set()
        targets = {"http://purl.obolibrary.org/obo/CHEBI_64573": "PMME", "http://purl.obolibrary.org/obo/CHEBI_64572": "PDME"}
        while pending:
            node = pending.pop()
            if node in seen:
                continue
            seen.add(node)
            if node in targets:
                labels.add(targets[node])
            pending.update((parents[node] | charge[node]) - seen)
        return next(iter(labels)) if len(labels) == 1 else None

    def resolve(term):
        seen, frontier = set(), {term}
        for distance in range(30):
            frontier -= seen
            if not frontier:
                break
            seen.update(frontier)
            matched = {node for node in frontier if direct[node]}
            codes = set().union(*(direct[node] for node in matched)) if matched else set()
            if codes:
                return {"codes": sorted(codes), "status": "mapped" if len(codes) == 1 else "ambiguous",
                    "biochemical_label": biochemical_label(term) if codes == {"GP0201"} else None,
                    "method": "exact-xref-or-curated" if distance == 0 else "ontology-ancestor",
                    "distance": distance, "supporting_terms": sorted(matched),
                    "source_xrefs": sorted(set().union(*(evidence[node] for node in matched)))}
            frontier = set().union(*(parents[node] | charge[node] for node in frontier))
        return {"codes": [], "status": "unmapped", "method": "none"}
    return resolve, vocabulary


def csv_table(path, records, fields):
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(records)


def write_search_index(output, candidates, class_records):
    target = Path(output) / "search.sqlite"
    temporary = Path(output) / "search.sqlite.tmp"
    with closing(sqlite3.connect(temporary)) as connection:
        connection.executescript('''DROP TABLE IF EXISTS reactions; DROP TABLE IF EXISTS classes; DROP TABLE IF EXISTS reaction_classes;
CREATE TABLE reactions (uri TEXT PRIMARY KEY, equation TEXT, kind TEXT, left_classes TEXT, right_classes TEXT, mapping_status TEXT, reviewed_protein_count INTEGER, source_release TEXT);
CREATE TABLE classes (uri TEXT PRIMARY KEY, label TEXT, lipidmaps_class_id TEXT);
CREATE TABLE reaction_classes (reaction TEXT, class_uri TEXT, side TEXT, PRIMARY KEY(reaction,class_uri,side));
CREATE INDEX classification_code ON classes(lipidmaps_class_id);
CREATE INDEX class_reactions ON reaction_classes(class_uri);''')
        connection.executemany("INSERT INTO reactions VALUES (?,?,?,?,?,?,?,?)", [(
            r["reaction"], r["equation"], r["kind"], ";".join(r["left"]), ";".join(r["right"]), r["class_mapping_status"],
            len({a["protein"] for a in r["protein_evidence"] if a["reviewed"]}) if "protein_evidence" in r else None, r["source"]["release"]) for r in candidates])
        connection.executemany("INSERT INTO classes VALUES (?,?,?)", [(r["uri"], r["label"], r["lipidmaps_class_id"]) for r in class_records.values()])
        memberships = {(r["reaction"], c["canonical_class"], c["side_code"]) for r in candidates for c in r["classes"]}
        connection.executemany("INSERT INTO reaction_classes VALUES (?,?,?)", sorted(memberships))
        connection.commit()
    os.replace(temporary, target)


def build(snapshot, output):
    folder, output = Path(snapshot), Path(output)
    manifest = kb.read_json(folder / "manifest.json")
    for name, expected in manifest["files"].items():
        if kb.sha(folder / name) != expected:
            raise ValueError("Changed snapshot: " + name)
    for name in ("chebi-all-is-a", "chebi-all-chemical-forms", "chebi-lipidmaps-xrefs-v3",
                 "all-rhea-chemical-pairs", "rhea-approved-masters-v3", "rhea-directional-aliases-v3", "rhea-related-records-v3"):
        metadata = kb.read_json(folder / (name + ".json.meta.json"))
        headers = {key.lower(): value for key, value in metadata["headers"].items()}
        if headers.get("x-release") != manifest["rhea_release"]:
            raise ValueError("Mixed or missing source release: " + name)
    rows = kb.read_json(folder / "participants.json")
    membership = bindings(folder / "lipid-membership-v4.json")
    lipid_terms = {r["chebi"] for r in membership}
    lipid_reactions = {r["reaction"] for r in membership}
    aliases = {r["annotated"]: r["reaction"] for r in bindings(folder / "rhea-directional-aliases-v3.json")}
    support, unresolved_annotations = defaultdict(list), []
    for annotation in kb.read_json(folder / "annotations.json"):
        annotated = "http://rdf.rhea-db.org/" + annotation["rhea"].split(":")[-1]
        reaction = aliases.get(annotated, annotated)
        if reaction in lipid_reactions:
            support[reaction].append(annotation)
        elif annotated not in aliases and annotated not in lipid_reactions:
            unresolved_annotations.append(annotation)
    # Non-lipid source annotations are not called unresolved: check against the
    # complete approved master list included in the aliases query below.
    approved = {r["reaction"] for r in bindings(folder / "rhea-approved-masters-v3.json")}
    unresolved_annotations = [a for a in unresolved_annotations if
        "http://rdf.rhea-db.org/" + a["rhea"].split(":")[-1] not in approved]
    grouped = defaultdict(list)
    for row in rows:
        grouped[row["reaction"]].append(row)
    if set(grouped) != lipid_reactions:
        raise ValueError("Incomplete global reaction set")
    references = defaultdict(list)
    for row in bindings(folder / "rhea-related-records-v3.json"):
        references[row["reaction"]].append({"relation": row["predicate"], "identifier": row["identifier"]})
    resolve, vocabulary = classification(folder)
    decisions = {term: resolve(term) for term in sorted(lipid_terms)}
    output.mkdir(parents=True, exist_ok=True)
    # Preserve the entire global reference, without assigning it to mouse.
    global_rows = [{"rhea": uri, "equation": participants[0]["equation"],
        "mouse_evidence": len(support.get(uri, [])), "transport": participants[0].get("transport", "unknown")}
        for uri, participants in sorted(grouped.items())]
    csv_table(output / "global-reference.csv", global_rows, ["rhea", "equation", "mouse_evidence", "transport"])
    kb.write_json(output / "classification-decisions.json", decisions)
    kb.write_json(output / "unresolved-mouse-annotations.json", unresolved_annotations)
    candidates, mappings, queue = [], {}, []
    graph = Graph()
    class_graph = Graph()
    class_graph.bind("mslm", kb.M)
    class_records = {}
    graph.bind("mslm", kb.M)
    graph.bind("rhea", kb.RHEA)
    registry_path = kb.KNOWLEDGE / "source/id-registry.json"
    with kb.registry_lock(registry_path):
        registry = kb.read_json(registry_path)
        source_dataset = kb.resource_uri("source-organism-catalog", "10090", output.name, registry=registry, prefix="B")
        class_dataset = kb.resource_uri("class-organism-catalog", "10090", output.name, registry=registry, prefix="B")
        for target, dataset in [(graph, source_dataset), (class_graph, class_dataset)]:
            target.add((dataset, RDF.type, kb.M.OrganismPathwayDataset))
            target.add((dataset, kb.M.organism, kb.TAXON["10090"]))
            target.add((dataset, RDFS.label, Literal("Mus musculus lipid reaction catalog")))
            target.add((dataset, kb.M.knowledgeRelease, Literal("Rhea-" + manifest["rhea_release"] + "/UniProt-" + manifest["uniprot_release"])))
        class_graph.add((class_dataset, kb.PROV.wasDerivedFrom, source_dataset))
        for uri in sorted(support):
            participants = grouped[uri]
            reaction = URIRef(uri)
            graph.add((source_dataset, kb.M.hasSourceReaction, reaction))
            graph.add((reaction, RDF.type, kb.M.LipidReaction))
            graph.add((reaction, kb.M.taxon, kb.TAXON["10090"]))
            graph.add((reaction, kb.M.sourceDatabase, Literal("Rhea")))
            graph.add((reaction, kb.M.sourceIdentifier, Literal("RHEA:" + uri.rsplit("/", 1)[-1])))
            graph.add((reaction, kb.M.sourceRelease, Literal(manifest["rhea_release"])))
            source_url = "https://www.rhea-db.org/rhea/" + uri.rsplit("/", 1)[-1]
            graph.add((reaction, kb.M.sourceUrl, Literal(source_url, datatype=XSD.anyURI)))
            graph.add((reaction, kb.DCT.source, reaction))
            graph.add((reaction, kb.M.directionality, kb.M.UnknownDirection))
            graph.add((reaction, kb.M.resolutionLevel, kb.M.SourceChemicalResolution))
            graph.add((reaction, kb.M.curationStatus, kb.M.Draft))
            source_assertion = kb.resource_uri("catalog-rhea-source", uri, registry=registry, prefix="A")
            graph.add((reaction, kb.M.hasSourceAssertion, source_assertion))
            graph.add((source_assertion, RDF.type, kb.M.SourceAssertion))
            graph.add((source_assertion, kb.M.sourceDatabase, Literal("Rhea")))
            graph.add((source_assertion, kb.M.sourceIdentifier, Literal("RHEA:" + uri.rsplit("/", 1)[-1])))
            graph.add((source_assertion, kb.M.sourceRelease, Literal(manifest["rhea_release"])))
            graph.add((source_assertion, kb.M.sourceUrl, Literal(source_url, datatype=XSD.anyURI)))
            graph.add((source_assertion, kb.M.assertionMethod, kb.M.DatabaseCurated))
            graph.add((reaction, kb.M.equation, Literal(participants[0]["equation"])))
            for ref in references[uri]:
                graph.add((reaction, URIRef(ref["relation"]), URIRef(ref["identifier"])))
            classes, side_labels, all_lipids = [], {"L": set(), "R": set()}, set()
            projection = kb.resource_uri("catalog-class-projection", "10090", uri, registry=registry, prefix="R")
            class_graph.add((class_dataset, kb.M.hasProjectionReaction, projection))
            class_graph.add((projection, RDF.type, kb.M.ReactionTraversalProjection))
            class_graph.add((projection, kb.DCT.source, reaction))
            class_graph.add((projection, kb.M.taxon, kb.TAXON["10090"]))
            class_graph.add((projection, kb.M.resolutionLevel, kb.M.ClassLevel))
            class_graph.add((projection, kb.M.directionality, kb.M.UnknownDirection))
            class_graph.add((projection, kb.M.sourceDatabase, Literal("Rhea")))
            class_graph.add((projection, kb.M.sourceIdentifier, Literal("RHEA:" + uri.rsplit("/", 1)[-1])))
            class_graph.add((projection, kb.M.sourceRelease, Literal(manifest["rhea_release"])))
            class_graph.add((projection, kb.M.sourceUrl, Literal(source_url, datatype=XSD.anyURI)))
            for row in participants:
                side = row["side"].rsplit("_", 1)[-1]
                if side not in side_labels:
                    raise ValueError("Unknown Rhea side")
                term = row.get("chebi")
                compound = URIRef(row["compound"])
                participant = kb.resource_uri("source-reaction-participant", uri, side, row["participant"], registry=registry, prefix="T")
                graph.add((participant, RDF.type, kb.M.ReactionParticipant))
                graph.add((participant, kb.M.participantEntity, compound))
                graph.add((participant, kb.M.participantRole, kb.M.EquationLeft if side == "L" else kb.M.EquationRight))
                try:
                    coefficient = Decimal(row["coefficient"])
                    if not coefficient.is_finite() or coefficient <= 0:
                        raise ValueError("Invalid source coefficient")
                    lexical = format(coefficient, "f")
                    if "." not in lexical:
                        lexical += ".0"
                    graph.add((participant, kb.M.stoichiometry, Literal(lexical, datatype=XSD.decimal, normalize=False)))
                except InvalidOperation:
                    graph.add((participant, kb.M.coefficientExpression, Literal(row["coefficient"])))
                graph.add((participant, kb.DCT.source, URIRef(row["participant"])))
                graph.add((reaction, kb.M.leftParticipant if side == "L" else kb.M.rightParticipant, participant))
                graph.add((reaction, kb.M.hasParticipant, participant))
                graph.add((compound, RDFS.label, Literal(row["name"])))
                if term:
                    graph.add((compound, kb.M.sourceChemicalReference, URIRef(term)))
                if term not in lipid_terms:
                    continue
                all_lipids.add(term)
                decision = decisions[term]
                if decision["status"] != "mapped":
                    queue.append({"reaction": uri, "chebi": term, "name": row["name"], **decision})
                    continue
                code = decision["codes"][0]
                label = vocabulary[code]["label"] or code
                if decision.get("biochemical_label"):
                    label = decision["biochemical_label"]
                qualifier = label if label in ("PMME", "PDME") else ""
                canonical = kb.resource_uri("catalog-lipid-class", code, qualifier, registry=registry, prefix="C")
                class_graph.add((canonical, RDF.type, kb.M.LipidEntity))
                class_graph.add((canonical, RDFS.label, Literal(label)))
                class_graph.add((canonical, kb.M.lipidmapsClassId, Literal(code)))
                class_graph.add((projection, kb.M.leftClass if side == "L" else kb.M.rightClass, canonical))
                class_records[str(canonical)] = {"uri": str(canonical), "id": str(canonical).rsplit("/", 1)[-1],
                    "label": label, "lipidmaps_class_id": code, "classification_url": vocabulary[code]["url"]}
                mappings[term.rsplit("_", 1)[-1]] = [label, code]
                classes.append({**row, "label": label, "lipidmaps": code, "side_code": side, "mapping": decision, "canonical_class": str(canonical)})
                side_labels[side].add(label)
                chemical_reference = URIRef(term)
                graph.add((chemical_reference, kb.M.lipidmapsClassId, Literal(code)))
                graph.add((chemical_reference, kb.M.mappingMethod, Literal(decision["method"])))
                graph.add((reaction, kb.M.leftClass if side == "L" else kb.M.rightClass, chemical_reference))
            protein_evidence = support[uri]
            for item in protein_evidence:
                protein = kb.UP[item["protein"]]
                assertion = kb.resource_uri("mouse-rhea-source", uri, item["protein"], item["rhea"], registry=registry, prefix="A")
                graph.add((reaction, kb.M.hasSourceAssertion, assertion))
                graph.add((reaction, kb.M.supportedByProtein, protein))
                graph.add((assertion, RDF.type, kb.M.SourceAssertion))
                graph.add((assertion, kb.M.sourceDatabase, Literal("UniProtKB")))
                graph.add((assertion, kb.M.sourceIdentifier, Literal(item["protein"])))
                graph.add((assertion, kb.M.sourceRelease, Literal(manifest["uniprot_release"])))
                graph.add((assertion, kb.M.sourceUrl, Literal("https://www.uniprot.org/uniprotkb/" + item["protein"], datatype=XSD.anyURI)))
                graph.add((assertion, kb.M.assertionMethod, kb.M.DatabaseCurated if item["reviewed"] else kb.M.DatabaseAnnotation))
                graph.add((assertion, kb.M.reviewed, Literal(item["reviewed"])))
                graph.add((assertion, kb.M.evidenceJson, Literal(json.dumps(item["evidence"], sort_keys=True))))
            transport = any(row.get("transport") in ("true", "1") for row in participants)
            kind = "transport" if transport else "class-conversion" if side_labels["L"] and side_labels["R"] and side_labels["L"] != side_labels["R"] else "same-class-or-unresolved-conversion"
            graph.add((reaction, kb.M.reactionKind, Literal(kind)))
            class_complete = all(decisions[t]["status"] == "mapped" for t in all_lipids)
            class_graph.add((projection, kb.M.classMappingComplete, Literal(class_complete)))
            candidates.append({"reaction": uri, "projection_uri": str(projection), "equation": participants[0]["equation"], "kind": kind,
                "left": sorted(side_labels["L"]), "right": sorted(side_labels["R"]), "classes": classes,
                "participants": participants, "lipid_compounds": sorted(all_lipids),
                "protein_evidence": protein_evidence, "source": {"database": "Rhea", "release": manifest["rhea_release"]},
                "related_records": references[uri],
                "class_mapping_status": "complete" if class_complete else "partial-or-unmapped"})
        for subject in set(class_graph.subjects()):
            if str(subject).startswith(str(kb.RES)):
                class_graph.add((subject, kb.DCT.identifier, Literal(str(subject).rsplit("/", 1)[-1])))
        for subject in set(graph.subjects()):
            if str(subject).startswith(str(kb.RES)):
                graph.add((subject, kb.DCT.identifier, Literal(str(subject).rsplit("/", 1)[-1])))
        kb.validate_id_registry(registry)
        kb.write_json(registry_path, registry)
    graph.serialize(output / "candidates.ttl", format="turtle")
    graph.serialize(output / "pathways.jsonld", format="json-ld")
    class_graph.serialize(output / "class-pathways.ttl", format="turtle")
    for identifier, record in class_records.items():
        record["reaction_count"] = sum(any(c["canonical_class"] == identifier for c in r["classes"]) for r in candidates)
    csv_table(output / "classes.csv", sorted(class_records.values(), key=lambda c: c["uri"]),
        ["uri", "id", "label", "lipidmaps_class_id", "classification_url", "reaction_count"])
    kb.write_json(output / "candidates.json", candidates)
    kb.write_json(output / "class-mappings.json", mappings)
    kb.write_json(output / "mapping-review-queue.json", queue)
    summary = [{"uri": item["reaction"], "equation": item["equation"], "kind": item["kind"],
        "left": ";".join(item["left"]), "right": ";".join(item["right"]), "mapping_status": item["class_mapping_status"],
        "protein_count": len({a["protein"] for a in item["protein_evidence"]}),
        "reviewed_protein_count": len({a["protein"] for a in item["protein_evidence"] if a["reviewed"]})} for item in candidates]
    csv_table(output / "reactions.csv", summary, ["uri", "equation", "kind", "left", "right", "mapping_status", "protein_count", "reviewed_protein_count"])
    write_search_index(output, candidates, class_records)
    report = {"taxon_id": "10090", "scientific_name": "Mus musculus", "status": "source-catalog-with-explicit-class-mapping-status",
        "dataset_uri": str(source_dataset), "class_dataset_uri": str(class_dataset),
        "scope": manifest["scope"], "global_reference_reactions": len(lipid_reactions), "candidate_count": len(candidates),
        "mapped_lipidmaps_classification_count": len({value[1] for value in mappings.values()}),
        "biochemical_class_node_count": len(class_records),
        "global_mapped_classification_count": len({c for d in decisions.values() if d["status"] == "mapped" for c in d["codes"]}),
        "mapped_lipid_compound_count": len(mappings), "rhea_release": manifest["rhea_release"], "uniprot_release": manifest["uniprot_release"],
        "reviewed_supported_reactions": sum(any(a["reviewed"] for a in r["protein_evidence"]) for r in candidates),
        "class_mapping_complete_reactions": sum(r["class_mapping_status"] == "complete" for r in candidates),
        "unresolved_source_annotation_count": len(unresolved_annotations),
        "unresolved_source_rhea_id_count": len({a["rhea"] for a in unresolved_annotations}),
        "reaction_kinds": {kind: sum(r["kind"] == kind for r in candidates) for kind in sorted({r["kind"] for r in candidates})},
        "snapshot_manifest_sha256": kb.sha(folder / "manifest.json"),
        "classification_rules_sha256": kb.sha(kb.KNOWLEDGE / "source/lipid-class-seeds.json"),
        "mapping_rules_version": "lipidmaps-xref-chebi-nearest/1",
        "classification_levels": {str(size): len({v[1] for v in mappings.values() if len(v[1]) == size}) for size in (2, 4, 6, 8)},
        "limitations": ["Complete only for the documented source/query/release, not all known biology or all pathway databases.",
            "LIPID MAPS structure cross-reference classification prefixes and ChEBI ancestry are deterministic projections; ambiguous/unmapped compounds remain searchable.",
            "Mouse membership uses exact taxon UniProt Rhea annotations; unreviewed evidence is retained separately.",
            "Rhea canonical equation sides do not establish physiological reaction direction."]}
    kb.write_json(output / "report.json", report)
    print(json.dumps(report, indent=2), flush=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--snapshot", required=True)
    parser.add_argument("--output", required=True)
    args = parser.parse_args()
    build(args.snapshot, args.output)
