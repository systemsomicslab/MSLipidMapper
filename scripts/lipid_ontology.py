"""Normalize ChEBI is-a and chemical forms for offline traversal."""
from collections import defaultdict, deque
import pathway_knowledge as kb


def bindings(path):
    return [{key: value["value"] for key, value in row.items()}
        for row in kb.read_json(path)["results"]["bindings"]]


def lipid_terms(folder):
    return closure(bindings(folder / "chebi-all-is-a.json"), bindings(folder / "chebi-all-chemical-forms.json"))


def closure(is_a, chemical_forms):
    children, forms = defaultdict(set), defaultdict(set)
    for row in is_a:
        children[row["parent"]].add(row["chebi"])
    for row in chemical_forms:
        forms[row["neutral"]].add(row["charged"])
        forms[row["charged"]].add(row["neutral"])
    seen, queue = set(), deque(["http://purl.obolibrary.org/obo/CHEBI_18059"])
    while queue:
        node = queue.popleft()
        if node not in seen:
            seen.add(node)
            queue.extend((children[node] | forms[node]) - seen)
    return seen


def result_json(rows):
    return {"head": {"vars": sorted({key for row in rows for key in row})},
        "results": {"bindings": [{key: {"type": "uri", "value": value} for key, value in row.items()} for row in rows]}}
