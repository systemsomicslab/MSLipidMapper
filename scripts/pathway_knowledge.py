"""Build immutable, organism-specific class pathway releases without an LLM.

Install requirements-knowledge.txt, then use fetch, build, search, validate, diff.
RDF is authoritative; SQLite and CSV are reproducible views of that RDF.
"""
from __future__ import annotations

import argparse
import csv
import hashlib
import json
import re
import sqlite3
import tempfile
import os
import time
from contextlib import closing
from contextlib import contextmanager
from datetime import datetime, timezone
from pathlib import Path
from urllib.parse import urlencode
from urllib.request import Request, urlopen

from rdflib import Dataset, Graph, Literal, Namespace, RDF, RDFS, URIRef
from rdflib.namespace import XSD

ROOT = Path(__file__).resolve().parents[1]
KNOWLEDGE = ROOT / "inst/extdata/pathway_knowledge"
M = Namespace("https://systemomicslab.github.io/MSLipidMapper/ontology/")
RES = Namespace("https://systemomicslab.github.io/MSLipidMapper/resource/")
PROV = Namespace("http://www.w3.org/ns/prov#")
DCT = Namespace("http://purl.org/dc/terms/")
RHEA = Namespace("http://rdf.rhea-db.org/")
TAXON = Namespace("http://purl.uniprot.org/taxonomy/")
UP = Namespace("http://purl.uniprot.org/uniprot/")
FORMAT = "mslipidmapper-organism-pathway/3"
ID_SCHEME = "mslm-accession-v1"
_ACTIVE_REGISTRY = None
PREFIXES = {"organism-release": "V", "organism-dataset": "B", "organism-pathway": "P",
            "reaction-participant": "T", "organism-assertion": "A", "reaction-source": "A"}


def validate_id_registry(registry):
    if registry.get("scheme") != ID_SCHEME:
        raise ValueError("Unsupported accession registry")
    used = set()
    for record in registry["records"].values():
        accession = record["accession"]
        match = re.fullmatch(r"MSLM([A-Z])([0-9]{6,})", accession)
        if not match or accession in used:
            raise ValueError("Duplicate or invalid accession")
        used.add(accession)
        if registry["counters"].get(match[1], 0) < int(match[2]):
            raise ValueError("Accession counter cannot move backwards")


def resource_uri(kind, *keys, registry=None, prefix=None):
    """Allocate an accession once and reuse it from the persistent ledger."""
    registry = registry if registry is not None else _ACTIVE_REGISTRY
    if registry is None:
        raise ValueError("An allocation registry is required")
    values = [ID_SCHEME, kind, *map(str, keys)]
    key = "".join(f"{len(value.encode('utf-8'))}:{value}" for value in values)
    if key not in registry["records"]:
        prefix = prefix or PREFIXES.get(kind, "Z")
        number = registry["counters"].get(prefix, 0) + 1
        registry["counters"][prefix] = number
        registry["records"][key] = {"accession": f"MSLM{prefix}{number:06d}", "kind": kind, "keys": list(map(str, keys))}
    return RES[registry["records"][key]["accession"]]


@contextmanager
def registry_lock(path):
    lock = Path(str(path) + ".lockdir")
    deadline = time.monotonic() + 30
    while True:
        try:
            lock.mkdir()
            break
        except FileExistsError:
            if time.monotonic() >= deadline:
                raise ValueError(f"Registry locked; check for a running writer: {lock}")
            time.sleep(0.05)
    try:
        yield
    finally:
        lock.rmdir()


def register_ids(root=KNOWLEDGE):
    with registry_lock(root / "source/id-registry.json"):
        return _register_ids(root)


def _register_ids(root):
    """Allocate once; a committed registry preserves identities across label edits."""
    path = root / "source/id-registry.json"
    old = read_json(path) if path.exists() else None
    registry = old if old and old["scheme"] == ID_SCHEME else {
        "scheme": ID_SCHEME, "entries": {}, "records": {}, "counters": {}, "aliases": {}}
    validate_id_registry(registry)
    if old and old["scheme"] != ID_SCHEME:
        archive = root / "source/id-registry-v1-sha256.json"
        if not archive.exists():
            write_json(archive, old)
    graph = Dataset(default_union=True)
    for directory in ("curated", "organisms", "examples"):
        for source in sorted((root / directory).glob("*")):
            if source.suffix in (".ttl", ".trig"):
                graph.parse(source, format="trig" if source.suffix == ".trig" else "turtle")
    terms = {term for triple in graph for term in (triple[0], triple[2])
             if isinstance(term, URIRef) and str(term).startswith(str(RES))}
    for term in sorted(terms, key=str):
        types = set(graph.objects(term, RDF.type))
        prefix = next((p for t, p in [(M.LipidEntity, "C"), (M.LipidReaction, "R"),
            (M.ReactionTraversalProjection, "R"), (M.ReactionParticipant, "T"),
            (M.SourceAssertion, "A"), (M.OrganismReactionAssertion, "A"),
            (M.CurationRecord, "K"), (M.Enzyme, "N"), (M.Gene, "G"),
            (M.LipidSubclassObservation, "J"), (M.Dataset, "B")] if t in types), "E")
        registry["entries"].setdefault(str(term), str(resource_uri("registered-resource", term, registry=registry, prefix=prefix)))
        if old and old["scheme"] != ID_SCHEME and str(term) in old["entries"]:
            registry["aliases"][old["entries"][str(term)]] = registry["entries"][str(term)]
    write_json(path, registry)
    return {"registry": str(path), "entries": len(registry["entries"])}


def apply_ids(graph, registry):
    converted = Graph()
    for triple in graph:
        terms = []
        for term in triple:
            if isinstance(term, URIRef) and str(term).startswith(str(RES)) and not (
                re.fullmatch(re.escape(str(RES)) + r"MSLM[A-Z][0-9]{6,}", str(term)) or str(term).startswith(str(RES) + "lipidmaps-classification/")):
                if str(term) not in registry["entries"]:
                    raise ValueError(f"Unregistered resource; run register and review: {term}")
                term = URIRef(registry["entries"][str(term)])
            terms.append(term)
        converted.add(tuple(terms))
    for subject in set(converted.subjects()):
        if re.fullmatch(re.escape(str(RES)) + r"MSLM[A-Z][0-9]{6,}", str(subject)):
            converted.add((subject, DCT.identifier, Literal(str(subject).rsplit("/", 1)[-1])))
    return converted


def read_json(path):
    return json.loads(Path(path).read_text(encoding="utf-8"))


def write_json(path, value):
    path = Path(path)
    content = json.dumps(value, ensure_ascii=False, sort_keys=True, indent=2, allow_nan=False) + "\n"
    with tempfile.NamedTemporaryFile(dir=path.parent, prefix=".json-", delete=False) as handle:
        temporary = Path(handle.name)
        handle.write(content.encode("utf-8"))
    try:
        os.replace(temporary, path)
    finally:
        if temporary.exists():
            temporary.unlink()


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def organisms(root=KNOWLEDGE):
    with (root / "source/organisms.csv").open(encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def inputs(root=KNOWLEDGE):
    config = read_json(root / "source/class-pathways.json")
    graph = Graph()
    paths = [root / "curated/phospholipid-pilot.ttl", root / "curated/pemt-stages.ttl"]
    for path in paths:
        graph.parse(path, format="turtle")
    ids = {rid for p in config["pathways"] for rid in p["reactions"]}
    defs = []
    for rid in sorted(ids):
        reaction = RES[rid]
        if (reaction, None, None) not in graph:
            raise ValueError(f"Undefined curated reaction: {rid}")
        rhea = set(graph.objects(reaction, M.rheaReaction))
        for source in graph.objects(reaction, M.hasSourceAssertion):
            for identifier in graph.objects(source, M.sourceIdentifier):
                if str(identifier).startswith("RHEA:"):
                    rhea.add(RHEA[str(identifier).split(":")[1]])
        if len(rhea) != 1:
            raise ValueError(f"Expected exactly one canonical Rhea stage for {rid}")
        participants = []
        for part in graph.objects(reaction, M.hasParticipant):
            entity = graph.value(part, M.participantEntity)
            role = graph.value(part, M.participantRole)
            if entity is None or role not in (M.Substrate, M.Product, M.Cofactor):
                raise ValueError(f"Invalid participant: {part}")
            participants.append((entity, role, graph.value(part, M.stoichiometry)))
        if not participants:
            for pred, role in [(M.hasSubstrateEntity, M.Substrate), (M.hasProductEntity, M.Product)]:
                participants.extend((entity, role, None) for entity in graph.objects(reaction, pred))
        lipid_participants = [(e, role) for e, role, _ in participants if (e, RDF.type, M.LipidEntity) in graph]
        if not any(role == M.Substrate for _, role in lipid_participants) or not any(role == M.Product for _, role in lipid_participants):
            raise ValueError(f"Missing class substrate/product for {rid}")
        for entity, _ in lipid_participants:
            if str(entity).removeprefix(str(RES)) not in config["class_labels"]:
                raise ValueError(f"Missing explicit class label: {entity}")
        defs.append((reaction, next(iter(rhea)), participants))
    return config, graph, paths, defs


def fetch(output, root=KNOWLEDGE):
    """Archive complete UniProt JSON response pages, including pagination."""
    _, _, _, defs = inputs(root)
    taxa = [o["taxon_id"] for o in organisms(root)]
    reactions = sorted({"RHEA:" + str(rhea).rsplit("/", 1)[-1] for _, rhea, _ in defs})
    query = "reviewed:true AND (" + " OR ".join("organism_id:" + t for t in taxa) + ") AND (" + " OR ".join('cc_catalytic_activity:"' + r + '"' for r in reactions) + ")"
    url = "https://rest.uniprot.org/uniprotkb/search?" + urlencode({"query": query, "format": "json", "size": 500})
    output = Path(output)
    if output.exists():
        raise ValueError("Snapshot directory already exists; use a new snapshot name")
    output.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="knowledge-fetch-", dir=output.parent) as tmp:
        staging = Path(tmp) / "snapshot"
        staging.mkdir()
        pages = []
        seen = set()
        while url:
            if url in seen:
                raise ValueError("Pagination loop")
            seen.add(url)
            with urlopen(Request(url, headers={"User-Agent": "MSLipidMapper-knowledge-builder/1"}), timeout=90) as response:
                payload = response.read()
                headers = response.headers
            parsed = json.loads(payload)
            if not isinstance(parsed.get("results"), list):
                raise ValueError("Unexpected UniProt response")
            name = f"uniprot-{len(pages) + 1:03d}.json"
            (staging / name).write_bytes(payload)
            pages.append({"file": name, "sha256": sha(staging / name), "url": url,
                          "release": headers.get("X-UniProt-Release"), "count": len(parsed["results"])})
            link = re.search(r'<([^>]+)>;\s*rel="next"', headers.get("Link", ""))
            url = link.group(1) if link else None
            if url and not url.startswith("https://rest.uniprot.org/"):
                raise ValueError("Unexpected pagination host")
        write_json(staging / "manifest.json", {"source": "UniProtKB", "query": query,
                   "retrieved_at": datetime.now(timezone.utc).isoformat(), "taxa": taxa,
                   "rhea_ids": reactions, "license": "CC BY 4.0", "pages": pages})
        staging.rename(output)
    return {"snapshot": str(output), "proteins": sum(p["count"] for p in pages)}


def annotations(snapshot):
    manifest = read_json(snapshot / "manifest.json")
    found = []
    for page in manifest["pages"]:
        path = snapshot / page["file"]
        if path.parent.resolve() != snapshot.resolve() or sha(path) != page["sha256"]:
            raise ValueError("Snapshot path/hash mismatch")
        for protein in read_json(path)["results"]:
            if protein.get("entryType") != "UniProtKB reviewed (Swiss-Prot)":
                continue
            taxon = str(protein["organism"]["taxonId"])
            for comment in protein.get("comments", []):
                if comment.get("commentType") != "CATALYTIC ACTIVITY":
                    continue
                reaction = comment.get("reaction", {})
                for ref in reaction.get("reactionCrossReferences", []):
                    if ref.get("database") == "Rhea":
                        found.append({"taxon": taxon, "protein": protein["primaryAccession"],
                            "rhea": ref["id"].upper(), "reaction": reaction,
                            "protein_name": protein.get("proteinDescription", {}).get("recommendedName", {}).get("fullName", {}).get("value", protein["primaryAccession"]),
                            "genes": sorted({g["geneName"]["value"] for g in protein.get("genes", []) if "geneName" in g})})
    return manifest, found


def copy_description(source, target, subjects):
    """Copy local descriptions, following internal URI objects without cycles."""
    pending, visited = list(subjects), set()
    while pending:
        subject = pending.pop()
        if subject in visited:
            continue
        visited.add(subject)
        for triple in source.triples((subject, None, None)):
            target.add(triple)
            obj = triple[2]
            if isinstance(obj, URIRef) and str(obj).startswith(str(RES)):
                pending.append(obj)


def graph_json(graph):
    expanded = json.loads(graph.serialize(format="json-ld", auto_compact=False))
    def canonical(value):
        if isinstance(value, dict):
            return {key: canonical(item) for key, item in sorted(value.items())}
        if isinstance(value, list):
            return sorted((canonical(item) for item in value), key=lambda item: json.dumps(item, sort_keys=True))
        return value
    return canonical(expanded)


def derive(graph):
    """Generate the public map-making tables exclusively from the release RDF."""
    classes, reactions, participants = [], [], []
    for entity in sorted(set(graph.subjects(RDF.type, M.LipidEntity)), key=str):
        classes.append({"uri": str(entity), "id": str(graph.value(entity, DCT.identifier) or ""),
                        "lipidmaps_class_id": str(graph.value(entity, M.lipidMapsSubClassCode) or ""),
                        "lipidmaps_mapping_relation": str(graph.value(entity, M.lipidMapsMappingRelation) or ""),
                        "label": str(graph.value(entity, M.classLabel) or ""),
                        "ontology": str(graph.value(entity, M.msdialOntologyLabel) or "")})
    for reaction in sorted(set(graph.subjects(RDF.type, M.ClassReaction)), key=str):
        sources = []
        for record in graph.objects(reaction, M.hasSourceAssertion):
            sources.append({"record_uri": str(record), "database": str(graph.value(record, M.sourceDatabase) or ""),
                "identifier": str(graph.value(record, M.sourceIdentifier) or ""),
                "url": str(graph.value(record, M.sourceUrl) or ""),
                "release": str(graph.value(record, M.sourceRelease) or ""),
                "version_known": graph.value(record, M.sourceVersionKnown).toPython() if graph.value(record, M.sourceVersionKnown) is not None else None,
                "retrieved_on": str(graph.value(record, M.retrievedOn) or "")})
        reactions.append({"uri": str(reaction), "rhea": str(graph.value(reaction, M.rheaReaction)),
            "direction": str(graph.value(reaction, M.directionality)),
            "label": str(graph.value(reaction, RDFS.label) or reaction.rsplit("/", 1)[-1]),
            "sources_json": json.dumps(sorted(sources, key=lambda x: (x["database"], x["identifier"])), sort_keys=True)})
        for part in sorted(graph.objects(reaction, M.hasParticipant), key=str):
            entity = graph.value(part, M.participantEntity)
            participants.append({"reaction": str(reaction), "entity": str(entity),
                "role": str(graph.value(part, M.participantRole)),
                "stoichiometry": str(graph.value(part, M.stoichiometry) or ""),
                "is_lipid_class": int((entity, RDF.type, M.LipidEntity) in graph)})
    return classes, reactions, participants


def write_tables(folder, graph):
    classes, reactions, participants = derive(graph)
    tables = [("classes", classes, ["uri", "id", "lipidmaps_class_id", "lipidmaps_mapping_relation", "label", "ontology"]),
              ("reactions", reactions, ["uri", "rhea", "direction", "label", "sources_json"]),
              ("participants", participants, ["reaction", "entity", "role", "stoichiometry", "is_lipid_class"])]
    with closing(sqlite3.connect(folder / "search.sqlite")) as db, db:
        for name, rows, fields in tables:
            db.execute(f"CREATE TABLE {name} ({', '.join(f + ' TEXT NOT NULL' for f in fields)})")
            db.executemany(f"INSERT INTO {name} VALUES ({','.join('?' for _ in fields)})", [[r[f] for f in fields] for r in rows])
            with (folder / f"{name}.csv").open("w", encoding="utf-8", newline="") as handle:
                writer = csv.DictWriter(handle, fieldnames=fields, lineterminator="\n")
                writer.writeheader()
                writer.writerows(rows)
        db.execute("CREATE INDEX class_uri ON classes(uri)")
        db.execute("CREATE INDEX participant_entity ON participants(entity)")


def build(snapshot, output, release, root=KNOWLEDGE):
    global _ACTIVE_REGISTRY
    with registry_lock(root / "source/id-registry.json"):
        registry = read_json(root / "source/id-registry.json")
        if registry["scheme"] != ID_SCHEME:
            raise ValueError("Run register to migrate the accession ledger first")
        validate_id_registry(registry)
        previous = _ACTIVE_REGISTRY
        _ACTIVE_REGISTRY = registry
        try:
            return _build(snapshot, output, release, root)
        finally:
            _ACTIVE_REGISTRY = previous


def _build(snapshot, output, release, root):
    if not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9._-]*", release):
        raise ValueError("Invalid release identifier")
    config, source, source_paths, defs = inputs(root)
    registry = _ACTIVE_REGISTRY
    snapshot = Path(snapshot)
    origin, evidence = annotations(snapshot)
    expected_taxa = {o["taxon_id"] for o in organisms(root)}
    expected_rhea = {"RHEA:" + str(r).rsplit("/", 1)[-1] for _, r, _ in defs}
    if not expected_taxa.issubset(set(origin["taxa"])) or not expected_rhea.issubset(set(origin["rhea_ids"])):
        raise ValueError("Snapshot does not cover configured taxa/reactions; fetch again")
    output = Path(output)
    if output.exists():
        raise ValueError("Release directory exists; releases are immutable")
    output.parent.mkdir(parents=True, exist_ok=True)
    files = source_paths + [root / "source/reaction-source-releases.json", root / "source/id-registry.json", root / "source/lipidmaps-class-mappings.json", root / "ontology/mslm-organism-datasets.ttl", root / "source/class-pathways.json", root / "source/organisms.csv",
                            root / "source/source-manifest.yml", Path(__file__)]
    hashes = {p.name: sha(p) for p in files}
    hashes["snapshot-manifest.json"] = sha(snapshot / "manifest.json")
    catalog = {"format": FORMAT, "release": release, "scope": config["scope"], "organisms": []}
    with tempfile.TemporaryDirectory(prefix="knowledge-build-", dir=output.parent) as tmp:
        staging = Path(tmp) / "release"
        staging.mkdir()
        for organism in organisms(root):
            taxon = organism["taxon_id"]
            graph = Graph()
            dataset = resource_uri("organism-release", taxon, release)
            series = resource_uri("organism-dataset", taxon)
            graph.add((dataset, RDF.type, M.OrganismPathwayDataset))
            graph.add((series, RDF.type, M.OrganismPathwayDatasetSeries))
            graph.add((series, M.organism, TAXON[taxon]))
            graph.add((dataset, DCT.isVersionOf, series))
            graph.add((dataset, M.organism, TAXON[taxon]))
            graph.add((dataset, DCT.hasVersion, Literal(release)))
            graph.add((dataset, DCT.description, Literal(config["scope"])))
            included = set()
            for reaction, rhea, participants in defs:
                rid = str(reaction).removeprefix(str(RES))
                matches = [e for e in evidence if e["taxon"] == taxon and e["rhea"] == "RHEA:" + str(rhea).rsplit("/", 1)[-1]]
                if not matches:
                    continue
                included.add(rid)
                # Copy evidence descriptions but never inherit human catalysts into other taxa.
                for pred in (M.hasSourceAssertion, M.hasCurationRecord):
                    for obj in source.objects(reaction, pred):
                        graph.add((reaction, pred, obj))
                        copy_description(source, graph, [obj])
                graph.add((reaction, RDF.type, M.ClassReaction))
                graph.add((reaction, M.rheaReaction, rhea))
                graph.add((reaction, PROV.wasDerivedFrom, rhea))
                graph.add((reaction, M.directionality, source.value(reaction, M.directionality) or M.UnknownDirection))
                graph.add((reaction, M.resolutionLevel, M.ClassLevel))
                graph.add((reaction, RDFS.label, Literal(rid.replace("_", " "))))
                # Every reaction carries direct provenance plus paired source records.
                rhea_id = "RHEA:" + str(rhea).rsplit("/", 1)[-1]
                source_records = list(graph.objects(reaction, M.hasSourceAssertion))
                if not any(str(graph.value(record, M.sourceIdentifier)) == rhea_id for record in source_records):
                    record = resource_uri("reaction-source", "Rhea", rhea_id)
                    graph.add((reaction, M.hasSourceAssertion, record))
                    graph.add((record, RDF.type, M.SourceAssertion))
                    graph.add((record, M.sourceDatabase, Literal("Rhea")))
                    graph.add((record, M.sourceIdentifier, Literal(rhea_id)))
                    graph.add((record, M.sourceUrl, Literal("https://www.rhea-db.org/rhea/" + str(rhea).rsplit("/", 1)[-1], datatype=XSD.anyURI)))
                    graph.add((record, M.assertionMethod, M.DatabaseCurated))
                provenance = read_json(root / "source/reaction-source-releases.json")
                for record in graph.objects(reaction, M.hasSourceAssertion):
                    database = str(graph.value(record, M.sourceDatabase))
                    metadata = provenance[database]
                    graph.add((record, M.sourceRelease, Literal(metadata["release"])))
                    graph.add((record, M.sourceVersionKnown, Literal(metadata["version_known"])))
                    graph.add((record, M.retrievedOn, Literal(metadata["retrieved_on"], datatype=XSD.date)))
                    for predicate in (M.sourceDatabase, M.sourceIdentifier, M.sourceUrl, M.sourceRelease):
                        for value in list(graph.objects(record, predicate)):
                            graph.add((reaction, predicate, value))
                    graph.add((reaction, DCT.source, URIRef(str(graph.value(record, M.sourceUrl)))))
                for number, (entity, role, coefficient) in enumerate(sorted(participants, key=lambda p: (str(p[1]), str(p[0])))):
                    part = resource_uri("reaction-participant", registry["entries"][str(reaction)], role,
                                        registry["entries"].get(str(entity), str(entity)))
                    graph.add((reaction, M.hasParticipant, part))
                    graph.add((part, RDF.type, M.ReactionParticipant))
                    graph.add((part, M.participantEntity, entity))
                    graph.add((part, M.participantRole, role))
                    if coefficient is not None:
                        graph.add((part, M.stoichiometry, coefficient))
                    copy_description(source, graph, [entity])
                    key = str(entity).removeprefix(str(RES))
                    if key in config["class_labels"]:
                        graph.add((entity, M.classLabel, Literal(config["class_labels"][key])))
                for match in matches:
                    assertion = resource_uri("organism-assertion", taxon, match["protein"],
                                             registry["entries"][str(reaction)], rhea)
                    graph.add((dataset, M.hasOrganismAssertion, assertion))
                    graph.add((assertion, RDF.type, M.OrganismReactionAssertion))
                    graph.add((assertion, M.organism, TAXON[taxon]))
                    graph.add((assertion, M.classReaction, reaction))
                    graph.add((assertion, M.rheaReaction, rhea))
                    graph.add((assertion, M.enzymeProtein, UP[match["protein"]]))
                    graph.add((UP[match["protein"]], RDF.type, M.Enzyme))
                    graph.add((UP[match["protein"]], RDFS.label, Literal(match["protein_name"])))
                    graph.add((UP[match["protein"]], M.organism, TAXON[taxon]))
                    for gene in match["genes"]:
                        graph.add((UP[match["protein"]], M.geneSymbol, Literal(gene)))
                    graph.add((assertion, M.uniprotReviewed, Literal(True)))
                    graph.add((assertion, M.assertionMethod, M.DatabaseCurated))
                    graph.add((assertion, PROV.wasDerivedFrom, UP[match["protein"]]))
                    graph.add((assertion, M.sourceRelease, Literal(",".join(sorted({p["release"] or "unknown" for p in origin["pages"]})))))
                    graph.add((assertion, M.annotationEvidence, Literal(json.dumps(match["reaction"], sort_keys=True))))
            for pathway in config["pathways"]:
                members = sorted(set(pathway["reactions"]) & included)
                if not members:
                    continue
                uri = resource_uri("organism-pathway", taxon, pathway["id"])
                graph.add((dataset, M.hasPathway, uri))
                graph.add((uri, RDF.type, M.Pathway))
                graph.add((uri, RDFS.label, Literal(pathway["label"])))
                graph.add((uri, M.membershipMethod, Literal(pathway["membership_method"])))
                for rid in members:
                    graph.add((uri, M.hasReaction, RES[rid]))
            # Classification IDs are external mappings, not individual structure LM_IDs.
            # GP0201 includes PE-NMe and PE-NMe2: never merge these distinct entities.
            mappings = read_json(root / "source/lipidmaps-class-mappings.json")
            for item in mappings["classes"]:
                entity = RES[item["source_key"]]
                if (entity, RDF.type, M.LipidEntity) not in graph:
                    continue
                code = item["lipidmaps_class_id"]
                if not re.fullmatch(r"[A-Z]{2}[0-9]{4}", code):
                    raise ValueError("Invalid LIPID MAPS subclass code")
                classification = RES["lipidmaps-classification/" + code]
                graph.add((entity, M.lipidMapsSubClassCode, Literal(code)))
                graph.add((entity, M.lipidMapsClassification, classification))
                graph.add((entity, M.lipidMapsMappingRelation, M[item["relation"]]))
                graph.add((classification, RDF.type, M.LipidMapsClassification))
                graph.add((classification, DCT.identifier, Literal(code)))
                graph.add((classification, RDFS.label, Literal(item["classification_label"])))
                graph.add((classification, RDFS.seeAlso, URIRef(item["classification_url"])))
            graph = apply_ids(graph, registry)
            folder = staging / "organisms" / taxon
            folder.mkdir(parents=True)
            # Sorted N-Triples is valid RDF and avoids serializer-specific blank-node/order changes.
            (folder / "pathways.nt").write_text("\n".join(sorted(graph.serialize(format="nt").splitlines())) + "\n", encoding="utf-8")
            write_json(folder / "pathways.jsonld", graph_json(graph))
            write_tables(folder, graph)
            classes, reactions, _ = derive(graph)
            metadata = {"format": FORMAT, "id_scheme": ID_SCHEME, "dataset_uri": str(dataset),
                "dataset_series_uri": str(series), "release": release, **organism,
                "status": "available" if included else "no_supported_reactions",
                "scope": config["scope"], "rules": config["rules"], "input_sha256": hashes,
                "snapshot": origin, "counts": {"classes": len(classes), "reactions": len(reactions)},
                "included_reactions": sorted(included), "unsupported_reactions": sorted({str(r).removeprefix(str(RES)) for r, _, _ in defs} - included),
                "files": {p.name: sha(p) for p in sorted(folder.iterdir())}}
            write_json(folder / "manifest.json", metadata)
            catalog["organisms"].append({k: metadata[k] for k in ("taxon_id", "scientific_name", "status", "counts")})
        write_json(root / "source/id-registry.json", registry)
        hashes["id-registry.json"] = sha(root / "source/id-registry.json")
        for entry in catalog["organisms"]:
            manifest_path = staging / "organisms" / entry["taxon_id"] / "manifest.json"
            manifest = read_json(manifest_path)
            manifest["input_sha256"] = hashes
            write_json(manifest_path, manifest)
        write_json(staging / "catalog.json", catalog)
        result = validate(staging)
        write_json(staging / "validation.json", result)
        staging.rename(output)
    return catalog


def validate(folder):
    folder = Path(folder).resolve()
    catalog = read_json(folder / "catalog.json")
    checked = []
    for entry in catalog["organisms"]:
        taxon = entry["taxon_id"]
        if not taxon.isdigit():
            raise ValueError("Invalid taxon identifier")
        location = folder / "organisms" / taxon
        manifest = read_json(location / "manifest.json")
        for name, expected in manifest["files"].items():
            if Path(name).name != name or sha(location / name) != expected:
                raise ValueError(f"Artifact hash mismatch: {taxon}/{name}")
        graph = Graph().parse(location / "pathways.nt", format="nt")
        from rdflib.compare import isomorphic
        if not isomorphic(graph, Graph().parse(location / "pathways.jsonld", format="json-ld")):
            raise ValueError("RDF/JSON-LD mismatch")
        if manifest.get("id_scheme") == ID_SCHEME:
            for triple in graph:
                for term in (triple[0], triple[2]):
                    if isinstance(term, URIRef) and str(term).startswith(str(RES)) and not re.fullmatch(
                        re.escape(str(RES)) + r"(?:MSLM[A-Z][0-9]{6,}|lipidmaps-classification/[A-Z]{2}[0-9]{4})", str(term)):
                        raise ValueError(f"Non-ID local URI: {term}")
            for entity in graph.subjects(RDF.type, M.LipidEntity):
                if len(list(graph.objects(entity, M.lipidMapsSubClassCode))) != 1:
                    raise ValueError("Lipid class requires one LIPID MAPS classification ID")
                if len(list(graph.objects(entity, M.classLabel))) != 1:
                    raise ValueError("Distinct biochemical class identities must not be merged")
            for reaction in graph.subjects(RDF.type, M.ClassReaction):
                if len(list(graph.objects(reaction, M.rheaReaction))) != 1:
                    raise ValueError("Class reaction must retain one canonical Rhea stage")
                for predicate in (M.hasSourceAssertion, M.sourceDatabase, M.sourceIdentifier, M.sourceUrl, M.sourceRelease):
                    if not list(graph.objects(reaction, predicate)):
                        raise ValueError("Reaction missing direct source metadata")
        for assertion in graph.subjects(RDF.type, M.OrganismReactionAssertion):
            if set(graph.objects(assertion, M.organism)) != {TAXON[taxon]}:
                raise ValueError("Cross-species assertion")
        for reaction in graph.subjects(RDF.type, M.ClassReaction):
            if not list(graph.subjects(M.classReaction, reaction)):
                raise ValueError("Reaction without organism evidence")
            for role in (M.Substrate, M.Product):
                if not any((p, M.participantRole, role) in graph and (graph.value(p, M.participantEntity), RDF.type, M.LipidEntity) in graph for p in graph.objects(reaction, M.hasParticipant)):
                    raise ValueError("Reaction missing class substrate/product")
        rows = derive(graph)
        if manifest.get("id_scheme") != ID_SCHEME:
            classes = rows[0] if manifest.get("id_scheme") == "mslm-sha256-v1" else [
                {key: row[key] for key in ("uri", "label", "ontology")} for row in rows[0]]
            rows = (classes, [{k: v for k, v in row.items() if k != "sources_json"} for row in rows[1]], rows[2])
        if manifest["counts"] != {"classes": len(rows[0]), "reactions": len(rows[1])}:
            raise ValueError("Incorrect counts")
        with closing(sqlite3.connect(f"{(location / 'search.sqlite').as_uri()}?mode=ro", uri=True)) as db:
            db.row_factory = sqlite3.Row
            for table, expected in zip(("classes", "reactions", "participants"), rows):
                actual = [dict(r) for r in db.execute(f"SELECT * FROM {table}")]
                normalized = [{k: str(v) for k, v in row.items()} for row in expected]
                if actual != normalized:
                    raise ValueError("SQLite/RDF mismatch")
        checked.append({"taxon_id": taxon, "status": manifest["status"], "counts": manifest["counts"]})
    return {"valid": True, "validation_scope": "RDF syntax, JSON-LD equivalence, provenance, species isolation, hashes and search index; not full SHACL or biochemical validation", "organisms": checked}


def search(folder, taxon, query):
    if not isinstance(query, str) or not query.strip():
        raise ValueError("query must be non-empty")
    query = query.strip()
    location = Path(folder) / "organisms" / taxon
    if not taxon.isdigit() or not (location / "manifest.json").exists():
        raise ValueError("Unknown organism dataset")
    meta = read_json(location / "manifest.json")
    with closing(sqlite3.connect(f"{(location.resolve() / 'search.sqlite').as_uri()}?mode=ro", uri=True)) as db:
        db.row_factory = sqlite3.Row
        if meta.get("id_scheme") == ID_SCHEME:
            classes = [dict(r) for r in db.execute("SELECT * FROM classes WHERE instr(lower(label), lower(?)) > 0 OR instr(lower(uri), lower(?)) > 0 OR lipidmaps_class_id = ? COLLATE NOCASE ORDER BY CASE WHEN label = ? COLLATE NOCASE THEN 0 ELSE 1 END, label", (query, query, query, query))]
            reactions = [dict(r) for r in db.execute("SELECT DISTINCT r.* FROM reactions r LEFT JOIN participants p ON p.reaction=r.uri LEFT JOIN classes c ON c.uri=p.entity WHERE instr(lower(r.label),lower(?)) > 0 OR instr(lower(r.rhea),lower(?)) > 0 OR c.label = ? COLLATE NOCASE OR c.lipidmaps_class_id = ? COLLATE NOCASE OR r.uri = ? ORDER BY r.uri", (query, query, query, query, query))]
        else:
            classes = [dict(r) for r in db.execute("SELECT * FROM classes WHERE instr(lower(label), lower(?)) > 0 OR instr(lower(uri), lower(?)) > 0 ORDER BY label", (query, query))]
            reactions = [dict(r) for r in db.execute("SELECT DISTINCT r.* FROM reactions r LEFT JOIN participants p ON p.reaction=r.uri LEFT JOIN classes c ON c.uri=p.entity WHERE instr(lower(r.label),lower(?)) > 0 OR instr(lower(r.rhea),lower(?)) > 0 OR c.label = ? COLLATE NOCASE ORDER BY r.uri", (query, query, query))]
    return {"taxon_id": taxon, "release": meta["release"], "status": meta["status"], "classes": classes, "reactions": reactions}


def diff(old, new):
    a = read_json(Path(old) / "catalog.json")
    b = read_json(Path(new) / "catalog.json")
    result = {}
    for taxon in sorted({o["taxon_id"] for o in a["organisms"] + b["organisms"]}):
        def triples(root):
            path = Path(root) / "organisms" / taxon / "pathways.nt"
            graph = Graph().parse(path, format="nt") if path.exists() else Graph()
            versions = dict(graph.subject_objects(DCT.isVersionOf))
            normalized = Graph()
            for subject, predicate, obj in graph:
                if predicate != DCT.hasVersion:
                    if predicate == DCT.identifier and subject in versions:
                        obj = Literal(str(versions[subject]).rsplit("/", 1)[-1])
                    normalized.add((versions.get(subject, subject), predicate, versions.get(obj, obj)))
            graph = normalized
            # Legacy releases use readable version paths; normalize those as well.
            return {re.sub(r"/release/[^/ >]+", "/release/{release}", line) for line in graph.serialize(format="nt").splitlines() if str(DCT.hasVersion) not in line}
        before, after = triples(old), triples(new)
        result[taxon] = {"added_triples": sorted(after - before), "removed_triples": sorted(before - after)}
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    sub.add_parser("register", help="Allocate/review stable IDs for curated source records")
    p = sub.add_parser("fetch"); p.add_argument("--output", required=True)
    p = sub.add_parser("build"); p.add_argument("--snapshot", required=True); p.add_argument("--output", required=True); p.add_argument("--release", required=True)
    p = sub.add_parser("validate"); p.add_argument("release_dir")
    p = sub.add_parser("search"); p.add_argument("release_dir"); p.add_argument("--taxon", required=True); p.add_argument("--query", required=True)
    p = sub.add_parser("diff"); p.add_argument("old"); p.add_argument("new")
    args = parser.parse_args()
    if args.command == "register": result = register_ids()
    elif args.command == "fetch": result = fetch(args.output)
    elif args.command == "build": result = build(args.snapshot, args.output, args.release)
    elif args.command == "validate": result = validate(args.release_dir)
    elif args.command == "search": result = search(args.release_dir, args.taxon, args.query)
    else: result = diff(args.old, args.new)
    print(json.dumps(result, indent=2, ensure_ascii=False))


if __name__ == "__main__":
    main()
