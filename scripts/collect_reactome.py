"""Archive species-specific lipid pathway events and SBML from Reactome.

All leaves below the selected species' Metabolism of lipids pathway are retained,
including inferred, binding and regulatory events. No class whitelist is used.
"""
import argparse
import gzip
import hashlib
import json
import re
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime, timezone
from pathlib import Path
from urllib.request import Request, urlopen
import pathway_knowledge as kb
from collect_mouse_pathways import archive
from reactome_species import normalize_taxon, MissingLipidPathway

BASE = "https://reactome.org/ContentService"


def object_index(tree):
    """Resolve Reactome's repeated-object integer references within one response."""
    index, pending = {}, [tree]
    while pending:
        value = pending.pop()
        if isinstance(value, dict):
            if "dbId" in value:
                previous = index.get(value["dbId"], {})
                index[value["dbId"]] = {**previous, **value} if len(value) >= len(previous) else {**value, **previous}
            pending.extend(value.values())
        elif isinstance(value, list):
            pending.extend(value)
    return index


def root_records(tree):
    index = object_index(tree)
    return [index[value if isinstance(value, int) else value["dbId"]] for value in tree], index


def request(folder, name, url, accept, body=None):
    path = folder / name
    meta_path = folder / (name + ".meta.json")
    body_bytes = body.encode("utf-8") if body is not None else None
    body_hash = hashlib.sha256(body_bytes).hexdigest() if body_bytes else None
    if path.exists():
        meta = kb.read_json(meta_path)
        if meta["url"] != url or meta.get("request_body_sha256") != body_hash or meta["sha256"] != kb.sha(path):
            raise ValueError("Changed cached Reactome request: " + name)
        return path.read_bytes()
    for attempt in range(3):
        try:
            headers = {"Accept": accept, "Accept-Encoding": "gzip", "User-Agent": "MSLipidMapper-Reactome/1"}
            if body is not None:
                headers["Content-Type"] = "text/plain"
            with urlopen(Request(url, data=body_bytes, headers=headers), timeout=120) as response:
                data = response.read()
                response_headers = dict(response.headers)
                if response.headers.get("Content-Encoding", "").lower() == "gzip":
                    data = gzip.decompress(data)
            path.write_bytes(data)
            kb.write_json(meta_path, {"url": url, "method": "POST" if body is not None else "GET",
                "request_body": body, "request_body_sha256": body_hash, "sha256": kb.sha(path),
                "stored_content_encoding": "identity", "headers": response_headers,
                "retrieved_at": datetime.now(timezone.utc).isoformat()})
            return data
        except Exception:
            if attempt == 2:
                raise
            time.sleep(2 * (attempt + 1))


def index_hierarchy(root, scientific_name=None):
    scientific_name = scientific_name or root['species']
    match = re.fullmatch(r'R-([A-Z]+)-[0-9]+', root['stId'])
    if not match:
        raise ValueError('Invalid source pathway stable ID')
    prefix = 'R-' + match[1] + '-'
    pathways, events, membership = {}, {}, set()
    pending = [(root, None)]
    while pending:
        node, parent = pending.pop()
        if node.get("species") != scientific_name or not node["stId"].startswith(prefix):
            raise ValueError("Cross-species hierarchy event: " + node['stId'])
        identifier = node["stId"]
        (pathways if node["type"] == "Pathway" else events)[identifier] = {k: v for k, v in node.items() if k != "children"}
        if parent:
            membership.add((parent, identifier))
        pending.extend((child, identifier) for child in node.get("children", []))
    return pathways, events, sorted(membership)


def species_list(folder):
    tree = json.loads(request(Path(folder), 'species.json', BASE + '/data/species/main', 'application/json'))
    records, _ = root_records(tree)
    return sorted([{'taxon_id': str(s['taxId']), 'scientific_name': s.get('displayName') or s['name'][0]}
                   for s in records], key=lambda s: int(s['taxon_id']))


def validate_event_taxon(record, taxon):
    taxa = {str(s['taxId']) for s in record.get('species', [])}
    if taxa != {str(taxon)}:
        raise ValueError('Wrong or missing event taxon: ' + record['stId'])


def collect(output, taxon):
    taxon = normalize_taxon(taxon)
    folder = Path(output)
    folder.mkdir(parents=True, exist_ok=True)
    if (folder / 'manifest.json').exists() and str(kb.read_json(folder / 'manifest.json')['taxon_id']) != taxon:
        raise ValueError('Snapshot belongs to another taxon; choose a new directory')
    version, _ = archive(folder, "version-start.json", BASE + "/data/database/version", "text/plain")
    supported = {s['taxon_id']: s['scientific_name'] for s in species_list(folder)}
    if taxon not in supported:
        raise ValueError('Taxon is not a Reactome main species; use --list-species')
    scientific_name = supported[taxon]
    hierarchy, _ = archive(folder, "hierarchy.json", BASE + "/data/eventsHierarchy/" + taxon + "?pathwaysOnly=false")
    pending, roots = list(hierarchy), []
    while pending:
        node = pending.pop()
        pending.extend(node.get("children", []))
        if node.get("name") == "Metabolism of lipids" and node.get("species") == scientific_name:
            roots.append(node)
    if not roots:
        raise MissingLipidPathway('No Metabolism of lipids root for ' + scientific_name + '; no dataset was built')
    if len(roots) != 1:
        raise ValueError('No unique Metabolism of lipids root for ' + scientific_name + '; no dataset was built')
    root = roots[0]
    pathways, leaves, membership = index_hierarchy(root, scientific_name)
    print(f"Reactome {version}, {scientific_name} ({taxon}): {len(pathways)} pathways, {len(leaves)} reaction-like events", flush=True)
    contained, _ = archive(folder, "contained-events.json", BASE + "/data/pathway/" + root["stId"] + "/containedEvents")
    contained_records, _ = root_records(contained)
    contained_ids = {node["stId"] for node in contained_records if node.get("schemaClass") != "Pathway"}
    if contained_ids != set(leaves):
        raise ValueError(f"Hierarchy/contained-event mismatch: missing {set(leaves)-contained_ids}, extra {contained_ids-set(leaves)}")
    request(folder, "lipid-pathway.sbml", BASE + "/exporter/event/" + root["stId"] + ".sbml", "application/sbml+xml")
    ids = sorted(leaves)

    def batch(start):
        name = f"events-{start//20+1:03d}.json"
        tree = json.loads(request(folder, name, BASE + "/data/query/ids?view=flatten", "application/json", ",".join(ids[start:start+20])))
        data, objects = root_records(tree)
        if {r["stId"] for r in data} != set(ids[start:start+20]):
            raise ValueError("Incomplete event batch")
        for record in data:
            species = [objects[s if isinstance(s, int) else s["dbId"]] for s in record.get("species", [])]
            record["species"] = species
            validate_event_taxon(record, taxon)
        print(name, len(data), flush=True)
        return data

    records = []
    with ThreadPoolExecutor(max_workers=2) as executor:
        for future in as_completed([executor.submit(batch, start) for start in range(0, len(ids), 20)]):
            records.extend(future.result())
    end, _ = archive(folder, "version-end.json", BASE + "/data/database/version", "text/plain")
    if version != end:
        raise ValueError("Reactome release changed during acquisition; use a new snapshot")
    request(folder, "license.html", "https://reactome.org/license", "text/html")
    kb.write_json(folder / "events.json", sorted(records, key=lambda r: r["stId"]))
    kb.write_json(folder / "pathways.json", sorted(pathways.values(), key=lambda r: r["stId"]))
    kb.write_json(folder / "pathway-membership.json", membership)
    manifest = {"source_database": "Reactome", "source_release": str(version), "taxon_id": taxon, "scientific_name": scientific_name,
        "root_pathway": root["stId"], "pathway_count": len(pathways), "event_count": len(records),
        "scope": 'Every reaction-like event recursively contained in the official ' + scientific_name + ' Metabolism of lipids pathway; includes regulatory and binding events.',
        "license": "CC0-1.0", "license_url": "https://creativecommons.org/publicdomain/zero/1.0/", "license_source": "https://reactome.org/license",
        "attribution": "Reactome Pathway Knowledgebase, release " + str(version),
        "files": {p.name: kb.sha(p) for p in sorted(folder.iterdir()) if p.is_file() and p.name != "manifest.json"}}
    kb.write_json(folder / "manifest.json", manifest)
    print(json.dumps({k: v for k, v in manifest.items() if k != "files"}, indent=2), flush=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", required=True)
    parser.add_argument('--taxon', required=True)
    args = parser.parse_args()
    collect(args.output, args.taxon)
