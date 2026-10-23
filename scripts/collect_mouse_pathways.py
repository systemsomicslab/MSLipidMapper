"""Archive a mouse-only lipid-enzyme/Rhea census before class mapping.

No LLM, fuzzy mapping or inferred orthologs. Raw responses and query hashes are
retained; no class reaction is promoted merely because a protein is lipid-related.
"""
from __future__ import annotations
import argparse
import json
import re
import time
import gzip
from pathlib import Path
from urllib.parse import urlencode
from urllib.request import Request, urlopen
from datetime import datetime, timezone
import pathway_knowledge as kb

QUERY = 'organism_id:10090 AND reviewed:true AND fragment:false AND cc_catalytic_activity:"rhea:*" AND go:0006629'
ENDPOINT = "https://sparql.rhea-db.org/sparql"


def archive(folder, name, url, accept="application/json"):
    path = folder / name
    metadata_path = folder / (name + ".meta.json")
    if path.exists():
        metadata = kb.read_json(metadata_path)
        if metadata["url"] != url or metadata["sha256"] != kb.sha(path):
            raise ValueError("Existing snapshot request/hash mismatch")
        return kb.read_json(path), metadata
    for attempt in range(3):
        try:
            with urlopen(Request(url, headers={"Accept": accept, "Accept-Encoding": "gzip", "User-Agent": "MSLipidMapper-mouse-census/1"}), timeout=90) as response:
                body = response.read()
                headers = dict(response.headers)
                encoding = response.headers.get("Content-Encoding", "identity")
                if encoding.lower() == "gzip":
                    body = gzip.decompress(body)
            data = json.loads(body)
            path.write_bytes(body)
            metadata = {"url": url, "sha256": kb.sha(path), "headers": headers,
                        "stored_content_encoding": "identity", "transport_content_encoding": encoding,
                        "retrieved_at": datetime.now(timezone.utc).isoformat()}
            kb.write_json(metadata_path, metadata)
            return data, metadata
        except Exception:
            if attempt == 2:
                raise
            time.sleep(2 * (attempt + 1))


def fetch(folder):
    folder = Path(folder)
    folder.mkdir(parents=True, exist_ok=True)
    url = "https://rest.uniprot.org/uniprotkb/search?" + urlencode({
        "query": QUERY, "format": "json", "size": 500,
        "fields": "accession,protein_name,gene_names,organism_id,cc_catalytic_activity,go_id"})
    proteins, pages = [], []
    seen = set()
    while url:
        if url in seen:
            raise ValueError("Pagination loop")
        seen.add(url)
        data, metadata = archive(folder, f"uniprot-{len(pages)+1:03d}.json", url)
        proteins.extend(data["results"])
        pages.append(metadata)
        headers = {k.lower(): v for k, v in metadata["headers"].items()}
        link = re.search(r'<([^>]+)>;\s*rel="next"', headers.get("link", ""))
        url = link[1] if link else None
        if url and not url.startswith("https://rest.uniprot.org/"):
            raise ValueError("Unexpected pagination host")
    annotations = []
    for protein in proteins:
        if str(protein["organism"]["taxonId"]) != "10090":
            raise ValueError("Cross-species entry")
        for comment in protein.get("comments", []):
            if comment.get("commentType") == "CATALYTIC ACTIVITY":
                reaction = comment.get("reaction", {})
                for ref in reaction.get("reactionCrossReferences", []):
                    if ref["database"] == "Rhea":
                        annotations.append({"protein": protein["primaryAccession"],
                            "rhea": ref["id"], "evidence": reaction.get("evidences", [])})
    ids = sorted({a["rhea"].split(":")[1] for a in annotations}, key=int)
    print(f"UniProt: {len(proteins)} mouse proteins, {len(ids)} annotated Rhea IDs", flush=True)
    rows, rhea_pages = [], []
    for start in range(0, len(ids), 40):
        values = " ".join("rh:" + identifier for identifier in ids[start:start+40])
        query = '''PREFIX rh: <http://rdf.rhea-db.org/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT DISTINCT ?annotated ?reaction ?equation ?side ?participant ?compound ?name ?chebi ?coefficient ?transport
WHERE {
 VALUES ?annotated { %s }
 GRAPH <https://sparql.rhea-db.org/rhea> {
  ?reaction (rh:directionalReaction|rh:bidirectionalReaction)? ?annotated ;
    rdfs:subClassOf rh:Reaction ; rh:status rh:Approved ; rh:equation ?equation ; rh:side ?side .
  OPTIONAL { ?reaction rh:isTransport ?transport }
  ?side ?contains ?participant .
  ?contains rdfs:subPropertyOf rh:contains ; rh:coefficient ?coefficient .
  ?participant rh:compound ?compound .
  ?compound rh:name ?name .
  OPTIONAL { ?compound (rh:chebi|rh:underlyingChebi|rh:reactivePart/rh:chebi) ?chebi }
 }
} ORDER BY ?reaction ?side ?participant''' % values
        query_path = folder / f"rhea-{start//40+1:03d}.rq"
        query_path.write_text(query, encoding="utf-8")
        url = ENDPOINT + "?" + urlencode({"query": query, "format": "json"})
        data, metadata = archive(folder, f"rhea-{start//40+1:03d}.json", url, "application/sparql-results+json")
        rows.extend([{key: item["value"] for key, item in row.items()} for row in data["results"]["bindings"]])
        rhea_pages.append(metadata)
        print(f"Rhea batch {start//40+1}: {len(data['results']['bindings'])} participant rows", flush=True)
    covered = {row["annotated"].rsplit("/", 1)[-1] for row in rows}
    manifest = {"taxon_id": "10090", "scientific_name": "Mus musculus", "uniprot_query": QUERY,
        "scope": "Reviewed, non-fragment mouse proteins annotated to GO:0006629 (including descendants) and Rhea catalytic activity; not all mouse lipid metabolism",
        "uniprot_pages": pages, "rhea_pages": rhea_pages,
        "protein_count": len(proteins), "annotated_rhea_count": len(ids),
        "master_reaction_count": len({r["reaction"] for r in rows}),
        "unresolved_annotated_rhea_ids": sorted(set(ids)-covered),
        "files": {p.name: kb.sha(p) for p in sorted(folder.iterdir()) if p.is_file() and p.name != "manifest.json"}}
    kb.write_json(folder / "annotations.json", annotations)
    kb.write_json(folder / "participants.json", rows)
    manifest["files"]["annotations.json"] = kb.sha(folder / "annotations.json")
    manifest["files"]["participants.json"] = kb.sha(folder / "participants.json")
    kb.write_json(folder / "manifest.json", manifest)
    print(json.dumps({k: manifest[k] for k in ("taxon_id", "protein_count", "annotated_rhea_count", "master_reaction_count", "unresolved_annotated_rhea_ids")}, indent=2), flush=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", required=True)
    fetch(parser.parse_args().output)
