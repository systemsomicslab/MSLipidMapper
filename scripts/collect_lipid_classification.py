"""Archive ChEBI lipid hierarchy, LIPID MAPS cross-references and class vocabulary."""
from pathlib import Path
from urllib.request import Request, urlopen
from html.parser import HTMLParser
import argparse
import re
import pathway_knowledge as kb
from collect_lipid_reaction_catalog import PREFIX, sparql


class ClassParser(HTMLParser):
    def __init__(self):
        super().__init__()
        self.href = None
        self.text = []
        self.classes = {}

    def handle_starttag(self, tag, attrs):
        if tag == "a":
            self.href = dict(attrs).get("href", "")
            self.text = []

    def handle_data(self, data):
        if self.href is not None:
            self.text.append(data)

    def handle_endtag(self, tag):
        if tag == "a" and self.href is not None:
            text = " ".join("".join(self.text).split())
            match = re.search(r"\[([A-Z]{2}[0-9]{0,6})\]", text)
            if match:
                self.classes[match[1]] = {"code": match[1], "label": text[:match.start()].strip(), "url": self.href}
            route = re.search(r"/lmsd_search/([1-8])([0-9]*)$", self.href)
            if route and len(route[2]) in (0, 2, 4, 6):
                code = dict(zip("12345678", ("FA", "GL", "GP", "SP", "ST", "PR", "SL", "PK")))[route[1]] + route[2]
                self.classes[code] = {"code": code, "label": text.split("[")[0].strip(), "url": self.href}
            self.href = None


def collect(folder):
    folder = Path(folder)
    folder.mkdir(parents=True, exist_ok=True)
    isa_query = PREFIX + '''SELECT ?chebi ?parent WHERE {
 GRAPH <http://sparql.rhea-db.org/chebi> { ?chebi rdfs:subClassOf ?parent . FILTER(isIRI(?chebi) && isIRI(?parent)) }
 }'''
    isa_rows, _ = sparql(folder, "chebi-all-is-a", isa_query)
    form_query = PREFIX + '''SELECT DISTINCT ?neutral ?charged WHERE {
 GRAPH <http://sparql.rhea-db.org/chebi> {
 { ?charged chemrof:conjugate_base_of ?neutral }
 UNION { ?neutral chemrof:conjugate_acid_of ?charged }
 UNION { ?charged chemrof:tautomer_of ?neutral }
 UNION { ?charged rdfs:subClassOf ?restriction . ?restriction owl:onProperty <http://purl.obolibrary.org/obo/RO_0018036> ; owl:someValuesFrom ?neutral }
 UNION { ?neutral rdfs:subClassOf ?restriction . ?restriction owl:onProperty chebihash:has_major_microspecies_at_pH_7_3 ; owl:someValuesFrom ?charged }
 FILTER(isIRI(?neutral) && isIRI(?charged))
 }} ORDER BY ?neutral ?charged'''
    form_rows, _ = sparql(folder, "chebi-all-chemical-forms", form_query)
    for name, query_text, rows, order in [("is-a", isa_query, isa_rows, "ORDER BY ?chebi ?parent"),
                                         ("chemical-forms", form_query, form_rows, "ORDER BY ?neutral ?charged")]:
        count_query = PREFIX + "SELECT (COUNT(*) AS ?count) WHERE { { " + query_text.replace(PREFIX, "").replace(order, "") + " } }"
        counts, _ = sparql(folder, "chebi-all-" + name + "-count", count_query)
        if len(counts) != 1 or int(counts[0]["count"]) != len(rows):
            raise ValueError("Truncated ontology " + name)
    query = PREFIX + '''PREFIX o: <http://www.geneontology.org/formats/oboInOwl#>
SELECT DISTINCT ?chebi ?xref WHERE { GRAPH <http://sparql.rhea-db.org/chebi> {
 ?chebi o:hasDbXref ?xref . FILTER(STRSTARTS(LCASE(STR(?xref)), "lipidmaps:"))
 }} ORDER BY ?chebi ?xref'''
    sparql(folder, "chebi-lipidmaps-xrefs-v3", query)
    path = folder / "lipidmaps-browse.html"
    if not path.exists():
        url = "https://www.lipidmaps.org/databases/lmsd/browse"
        with urlopen(Request(url, headers={"User-Agent": "MSLipidMapper-classification/1"}), timeout=90) as response:
            path.write_bytes(response.read())
        kb.write_json(folder / "lipidmaps-browse.meta.json", {"url": url, "sha256": kb.sha(path)})
    parser = ClassParser()
    parser.feed(path.read_text(encoding="utf-8"))
    if len(parser.classes) < 100:
        raise ValueError(f"Incomplete LIPID MAPS vocabulary: {len(parser.classes)} codes")
    kb.write_json(folder / "lipidmaps-vocabulary.json", parser.classes)
    print("LIPID MAPS classification vocabulary:", len(parser.classes), flush=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", required=True)
    collect(parser.parse_args().output)
