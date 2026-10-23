"""Archive every approved lipid-involving Rhea reaction and exact mouse evidence.

Lipid membership normalizes ChEBI:18059 ancestry, conjugate forms and tautomers.
No GO filter, class whitelist, or two-lipid-side filter is used.
"""
import argparse
import json
import re
from pathlib import Path
from urllib.parse import urlencode
from collect_mouse_pathways import archive, ENDPOINT
import pathway_knowledge as kb
from lipid_ontology import lipid_terms, result_json

PREFIX = '''PREFIX rh: <http://rdf.rhea-db.org/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX ch: <http://purl.obolibrary.org/obo/CHEBI_>
PREFIX chebihash: <http://purl.obolibrary.org/obo/chebi#>
PREFIX chemrof: <https://w3id.org/chemrof/>
'''


def sparql(folder, name, query):
    (folder / (name + ".rq")).write_text(query, encoding="utf-8")
    data, meta = archive(folder, name + ".json", ENDPOINT + "?" + urlencode({"query": query, "format": "json"}), "application/sparql-results+json")
    rows = [{key: value["value"] for key, value in row.items()} for row in data["results"]["bindings"]]
    print(name, len(rows), "rows", flush=True)
    return rows, meta


def collect(output):
    folder = Path(output)
    folder.mkdir(parents=True, exist_ok=True)
    query = PREFIX + '''SELECT DISTINCT ?reaction ?chebi WHERE {
 GRAPH <https://sparql.rhea-db.org/rhea> {
 ?reaction rdfs:subClassOf rh:Reaction ; rh:status rh:Approved ; rh:side/rh:contains/rh:compound ?compound .
 ?compound (rh:chebi|rh:reactivePart/rh:chebi|rh:underlyingChebi) ?chebi . }
} ORDER BY ?reaction ?chebi'''
    universe, metadata = sparql(folder, "all-rhea-chemical-pairs", query)
    count_query = PREFIX + "SELECT (COUNT(*) AS ?count) WHERE { { " + query.replace(
        "ORDER BY ?reaction ?chebi", "").replace(PREFIX, "") + " } }"
    counts, _ = sparql(folder, "all-rhea-chemical-pair-count", count_query)
    if len(counts) != 1 or int(counts[0]["count"]) != len(universe):
        raise ValueError("Truncated Rhea chemical pair universe")
    allowed = lipid_terms(folder)
    lipids = [row for row in universe if row["chebi"] in allowed]
    kb.write_json(folder / "lipid-membership-v4.json", result_json(lipids))
    if not lipids:
        raise ValueError("Empty lipid reaction census")
    ids = sorted({r["reaction"] for r in lipids})
    id_set = set(ids)
    rows, batches = [], []
    previous = folder / "manifest.json"
    if previous.exists():
        old = kb.read_json(previous)
        cached = folder / "participants.json"
        if old["rhea_release"] == metadata["headers"].get("X-Release") and cached.exists():
            if old["files"]["participants.json"] != kb.sha(cached):
                raise ValueError("Changed cached participants")
            rows = [r for r in kb.read_json(cached) if r["reaction"] in id_set]
    remaining = sorted(set(ids) - {row["reaction"] for row in rows})
    print("Lipid reactions:", len(ids), "additional participant fetch:", len(remaining), flush=True)
    for start in range(0, len(remaining), 100):
        values = " ".join("rh:" + uri.rsplit("/", 1)[-1] for uri in remaining[start:start+100])
        query = PREFIX + '''SELECT DISTINCT ?reaction ?equation ?side ?participant ?compound ?name ?chebi ?coefficient ?transport ?annotated WHERE {
 VALUES ?reaction { ''' + values + ''' }
 GRAPH <https://sparql.rhea-db.org/rhea> {
 ?reaction rh:equation ?equation ; rh:side ?side .
 OPTIONAL { ?reaction rh:isTransport ?transport }
 BIND(?reaction AS ?annotated)
 ?side ?contains ?participant . ?contains rdfs:subPropertyOf rh:contains ; rh:coefficient ?coefficient .
 ?participant rh:compound ?compound . ?compound rh:name ?name .
 OPTIONAL { ?compound (rh:chebi|rh:reactivePart/rh:chebi|rh:underlyingChebi) ?chebi }
 }} ORDER BY ?reaction ?side ?participant ?annotated'''
        batch, meta = sparql(folder, f"additional-participants-v4-{start//100+1:03d}", query)
        rows.extend(batch)
        batches.append(meta)
    sparql(folder, "rhea-approved-masters-v3", PREFIX + '''SELECT DISTINCT ?reaction WHERE {
 GRAPH <https://sparql.rhea-db.org/rhea> { ?reaction rdfs:subClassOf rh:Reaction ; rh:status rh:Approved }
 } ORDER BY ?reaction''')
    aliases, alias_meta = sparql(folder, "rhea-directional-aliases-v3", PREFIX + '''SELECT DISTINCT ?reaction ?annotated WHERE {
 GRAPH <https://sparql.rhea-db.org/rhea> {
 ?reaction rdfs:subClassOf rh:Reaction ; rh:status rh:Approved ; (rh:directionalReaction|rh:bidirectionalReaction) ?annotated
 }} ORDER BY ?reaction ?annotated''')
    batches.append(alias_meta)
    _, cross_meta = sparql(folder, "rhea-related-records-v3", PREFIX + '''SELECT DISTINCT ?reaction ?predicate ?identifier WHERE {
 GRAPH <https://sparql.rhea-db.org/rhea> {
 ?reaction rdfs:subClassOf rh:Reaction ; rh:status rh:Approved .
 ?reaction (rh:directionalReaction|rh:bidirectionalReaction)? ?variant .
 VALUES ?predicate { rh:ec rh:citation rdfs:seeAlso }
 ?variant ?predicate ?identifier
 }} ORDER BY ?reaction ?predicate ?identifier''')
    batches.append(cross_meta)
    query = 'organism_id:10090 AND cc_catalytic_activity:"rhea:*"'
    url = "https://rest.uniprot.org/uniprotkb/search?" + urlencode({"query": query, "format": "json", "size": 500,
        "fields": "accession,reviewed,protein_name,gene_names,organism_id,cc_catalytic_activity"})
    proteins, pages, seen = [], [], set()
    while url:
        if url in seen or not url.startswith("https://rest.uniprot.org/"):
            raise ValueError("Invalid pagination")
        seen.add(url)
        data, meta = archive(folder, f"mouse-proteins-{len(pages)+1:03d}.json", url)
        proteins.extend(data["results"])
        pages.append(meta)
        headers = {k.lower(): v for k, v in meta["headers"].items()}
        link = re.search(r'<([^>]+)>;\s*rel="next"', headers.get("link", ""))
        url = link[1] if link else None
    if any(str(p["organism"]["taxonId"]) != "10090" for p in proteins):
        raise ValueError("Cross species protein")
    if len({p["primaryAccession"] for p in proteins}) != len(proteins):
        raise ValueError("Duplicate protein pagination")
    totals = {int({k.lower(): v for k, v in p["headers"].items()}["x-total-results"]) for p in pages}
    if totals != {len(proteins)}:
        raise ValueError("Incomplete protein pagination")
    annotations = []
    for protein in proteins:
        for comment in protein.get("comments", []):
            if comment.get("commentType") != "CATALYTIC ACTIVITY":
                continue
            reaction = comment.get("reaction", {})
            for ref in reaction.get("reactionCrossReferences", []):
                if ref["database"] == "Rhea":
                    annotations.append({"protein": protein["primaryAccession"], "rhea": ref["id"],
                        "reviewed": protein.get("entryType") == "UniProtKB reviewed (Swiss-Prot)",
                        "evidence": reaction.get("evidences", [])})
    covered = {row["reaction"] for row in rows}
    if covered != set(ids):
        raise ValueError("Incomplete reaction participant census")
    releases = {m["headers"].get("X-Release") for m in [metadata, *batches]}
    if len(releases) != 1 or None in releases:
        raise ValueError("Mixed/missing Rhea release")
    uniprot_releases = {{k.lower(): v for k, v in p["headers"].items()}["x-uniprot-release"] for p in pages}
    if len(uniprot_releases) != 1:
        raise ValueError("Mixed UniProt releases")
    rows.sort(key=lambda r: (r["reaction"], r["side"], r["participant"], r.get("chebi", "")))
    kb.write_json(folder / "participants.json", rows)
    kb.write_json(folder / "annotations.json", annotations)
    manifest = {"taxon_id": "10090", "rhea_release": next(iter(releases)), "uniprot_release": next(iter(uniprot_releases)),
        "uniprot_query": query, "protein_count": len(proteins), "global_lipid_reaction_count": len(ids),
        "chemical_pair_universe_count": len(universe),
        "scope": "All approved Rhea reactions involving ChEBI:18059 descendants including conjugate acid/base and tautomeric forms; mouse membership requires exact UniProt evidence with reviewed status retained.",
        "files": {p.name: kb.sha(p) for p in folder.iterdir() if p.is_file() and p.name != "manifest.json"}}
    kb.write_json(folder / "manifest.json", manifest)
    print(json.dumps({k: v for k, v in manifest.items() if k != "files"}), flush=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", required=True)
    collect(parser.parse_args().output)
