"""Species discovery using only the Python standard library."""
import json
from urllib.request import Request, urlopen


class MissingLipidPathway(ValueError):
    """The species exists in Reactome but has no lipid metabolism root."""


def normalize_taxon(taxon):
    taxon = str(taxon)
    if not taxon.isascii() or not taxon.isdigit() or int(taxon) <= 0:
        raise ValueError('Taxonomy ID must be a positive integer')
    return str(int(taxon))


def list_species():
    request = Request('https://reactome.org/ContentService/data/species/main',
                      headers={'Accept': 'application/json', 'User-Agent': 'MSLipidMapper-Reactome/1'})
    with urlopen(request, timeout=90) as response:
        tree = json.load(response)
    objects, pending = {}, [tree]
    while pending:
        value = pending.pop()
        if isinstance(value, dict):
            if 'dbId' in value:
                previous = objects.get(value['dbId'], {})
                objects[value['dbId']] = {**value, **previous} if len(previous) > len(value) else {**previous, **value}
            pending.extend(value.values())
        elif isinstance(value, list):
            pending.extend(value)
    records = [objects[v if isinstance(v, int) else v['dbId']] for v in tree]
    return sorted([{'taxon_id': normalize_taxon(s['taxId']),
                    'scientific_name': s.get('displayName') or s['name'][0]} for s in records],
                  key=lambda s: int(s['taxon_id']))
