"""Validate synthetic JSON-LD as actual RDF, not just JSON-shaped data.

Requires rdflib and jsonschema. Run from the repository root after generating
the example with tests/create-pathway-measurements-example.R.
"""
import json
from pathlib import Path

import jsonschema
from rdflib import Graph, Literal, Namespace, RDF, URIRef
from rdflib.compare import isomorphic

ROOT = Path(__file__).resolve().parents[1]
OUTPUT = ROOT / "inst/extdata/examples/pathway-measurements-example.json"
MSLM = Namespace("https://systemomicslab.github.io/MSLipidMapper/ontology/")

document = json.loads(OUTPUT.read_text(encoding="utf-8"))
schema = json.loads((ROOT / "inst/extdata/pathway_measurements.schema.json").read_text())
jsonschema.Draft202012Validator.check_schema(schema)
jsonschema.Draft202012Validator(schema, format_checker=jsonschema.FormatChecker()).validate(document)
graph = Graph().parse(str(OUTPUT), format="json-ld")
assert len(graph) > 0
assert len(set(graph.subjects(RDF.type, MSLM.Sample))) == 4
assert len(set(graph.subjects(RDF.type, MSLM.MolecularFeature))) == 3

subclass = next(graph.subjects(MSLM.msdialOntologyLabel, Literal("PC")))
scopes = list(graph.subjects(MSLM.observedLipidSubclass, subclass))
whole = next(s for s in scopes if (s, MSLM.scopeType, Literal("subclass")) in graph)
subset = next(s for s in scopes if (s, MSLM.acylChainSignature, Literal("18:0")) in graph)

def by_sample(scope):
    observations = {}
    for observation in graph.subjects(MSLM.measuredEntity, scope):
        sample = graph.value(observation, MSLM.inSample)
        observations[str(graph.value(sample, MSLM.identifier))] = observation
    return observations

whole_samples = by_sample(whole)
assert [float(graph.value(whole_samples[s], MSLM.value)) for s in ("S1", "S2", "S3", "S4")] == [15, 12, 20, 30]
subset_samples = by_sample(subset)
assert graph.value(subset_samples["S2"], MSLM.value) is None
assert graph.value(subset_samples["S2"], MSLM.valueStatus) == MSLM.MissingValue
assert float(graph.value(subset_samples["S3"], MSLM.value)) == 0
assert graph.value(subset_samples["S3"], MSLM.valueStatus) == MSLM.AvailableValue
assert len(set(graph.objects(subset, MSLM.memberFeature))) == 1
assert (URIRef(str(subclass).replace("/subclass/PC", "/subclass/PE")), RDF.type, MSLM.MSDialMappableLipidEntity) in graph

# Topology is retained, but display attributes never become RDF predicates.
original = json.loads((ROOT / "inst/extdata/examples/remodeling_lipidonly.cyjs").read_text())
assert len(set(graph.subjects(RDF.type, MSLM.PathwayConnection))) == len(original["elements"]["edges"])
assert all(isinstance(o, URIRef) for o in graph.objects(None, MSLM.sourceEntity))
for predicate in ("path", "pathpng", "position", "style", "SUID", "effectSize", "pValue"):
    assert not list(graph.triples((None, MSLM[predicate], None)))

# All locally minted referenced resources are described in the graph.
subjects = set(graph.subjects())
for _, _, obj in graph:
    if isinstance(obj, URIRef) and "/dataset/synthetic_example/" in str(obj):
        assert obj in subjects, f"Undescribed local reference: {obj}"

# Sample metadata survives as an RDF JSON literal; no remote context is needed.
sample = next(graph.subjects(MSLM.identifier, Literal("S1")))
metadata = graph.value(sample, MSLM.sampleMetadata)
assert metadata.datatype == RDF.JSON
assert json.loads(str(metadata))["class"] == "Control"
round_trip = Graph().parse(data=graph.serialize(format="turtle"), format="turtle")
# RDFLib's JSON-LD parser can retain lexical "10"^^xsd:double, while Turtle
# canonicalizes it to "10.0"^^xsd:double. Compare normalized literal values.
canonical = Graph()
for subject, predicate, obj in graph:
    if isinstance(obj, Literal) and predicate in (MSLM.value, MSLM.validFeatureCount):
        obj = Literal(float(obj) if predicate == MSLM.value else int(obj), datatype=obj.datatype)
    canonical.add((subject, predicate, obj))
assert isomorphic(canonical, round_trip)
ontology = Graph().parse(str(ROOT / "inst/extdata/pathway_knowledge/ontology/mslm-core.ttl"), format="turtle")
ontology.parse(str(ROOT / "inst/extdata/pathway_knowledge/ontology/mslm-measurements.ttl"), format="turtle")
print(f"PASS: JSON schema, RDF links/values/missingness, metadata, topology and Turtle round trip ({len(graph)} triples)")
