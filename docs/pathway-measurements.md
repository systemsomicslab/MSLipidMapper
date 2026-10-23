# Pathway measurements as RDF / JSON-LD (schema v4.0.0)

`mztab_pathway_measurements()` uses cyjs only to read pathway connections and
biological labels. Its output is JSON-LD with `@context` and `@graph`, not a
Cytoscape document. Resources have URI identifiers, classes, properties and
explicit URI references. The inline context needs no external download.
Specification: https://www.w3.org/TR/json-ld11/

```r
result <- MSLipidMapper::mztab_pathway_measurements(
  "study.mztab", "remodeling_lipidonly.cyjs",
  output_file = "study-pathway-measurements.json",
  acyl_chains = c("18:0", "24:1"),
  aggregate = "sum", normalization = "none"
)
result$document
result$node_mapping
```

For RStudio, edit and source `scripts/run_mztab_pathway_measurements.R`.
The synthetic four-sample output is
`inst/extdata/examples/pathway-measurements-example.json`.

## RDF structure

| Resource class | Meaning and links |
| --- | --- |
| `Dataset` | Dataset ID, normalization, unit, assay, group column and pathway |
| `Pathway` | Imported metabolic pathway |
| `PathwayLipidNode` | Represents a lipid subclass entity in the pathway |
| `MSDialMappableLipidEntity` | Subclass label and optional canonical entity URI |
| `PathwayEntity`, `PathwayJunction` | Other biological entities and topology junctions |
| `PathwayConnection` | Source and target references, supplied biological annotations |
| `Sample` | Sample ID, group, dataset, original metadata as an RDF JSON literal |
| `MolecularFeature` | Feature ID, lipid name, subclass, chains and dataset |
| `LipidMeasurementScope` | Whole subclass or acyl subset, pathway node, subclass, member features, aggregation and detection status |
| `SampleMeasurement` | Measured feature/scope, sample, dataset, numerical value, value availability and valid feature count |

Relationships use `{"@id": "..."}`; sample IDs are not dynamic predicate names.
PC/PE remain the pathway endpoints. Subsets are observations on those nodes.
Edges become `PathwayConnection`, not automatically curated reactions. Supplied
interaction, reaction, enzyme, gene and source annotations are retained.
No chain-preserving reaction is inferred.

No Cytoscape `elements`, `data`, `path`, `pathpng`, positions, styles, SUID, zoom,
selection state or plot URLs are exported. Original graph IDs are only used to
look up short fixed accessions in a persistent ledger (`mslm-accession-v1`).
Names and original sample/feature identifiers remain literal metadata,
not readable URI components. Canonical class links use the committed ID registry;
LIPID MAPS classification codes are recorded separately from structure LM_IDs.
See [accession and source policy](pathway-accessions.md). Historical exports retain
their original identifiers; regenerated v4 exports use the new scheme.

## Measurements

No FC, p-values or comparisons are computed. Samples are retained unless
explicitly excluded. Later tests retrieve individual values through
`SampleMeasurement -> inSample -> Sample -> sampleGroup`.

Subsets retain chain definitions, member feature references and aggregation
methods. Memberships may overlap. Original feature values and chain annotations
allow rebuilding subsets and changing aggregation.

Missing numerical values are JSON `null`; measured zeros remain `0`.
JSON null creates no RDF value triple. Explicit `MissingValue` status and an
identified measurement node retain the missing observation as RDF. Finite values
have `AvailableValue`. Aggregates use finite values and remain missing if all
members are missing. Valid feature counts are recorded. Empty scopes are
`NotMeasured`; `Observed` means feature rows exist, not detection in every sample.

No normalization is performed. Declare actual upstream methods and units.
Existing annotations or rule-based parsing determine chains, with a guard
against multi-chain lipid sum-composition names such as `PC 34:1`.

## Mapping and validation

Matching is exact after trimming whitespace. For custom node names, supply
`node_subclasses = c("node-id" = "PC")`. Unmatched input subclasses still retain
their molecular values and subclass resources. Diagnostics are returned separately.

JSON schema: `inst/extdata/pathway_measurements.schema.json`.
RDF vocabulary: `inst/extdata/pathway_knowledge/ontology/mslm-measurements.ttl`.
Tests cover JSON structure, RDF triples and Turtle round trips.
Shiny comparison selection and the earlier pairwise RDF function are separate.

## REST API

Upload a CSV or mzTab-M with YAML containing `output.format: jsonld` (or `both`).
`measurements.include_subsets: false` exports whole-subclass aggregates only;
`true` adds acyl subsets. Optional `measurements.acyl_chains` limits the subsets;
if omitted, all parsed chains are used. See [REST API usage](rest-api.md).
# Mapping from LIPID MAPS identifiers

Generic CSV upload accepts a feature annotation table containing a feature key,
a required display label, and a LIPID MAPS ID column; no Ontology column is required.
Keys match assay column names. Labels are stored in `label` and `Metabolite name`
for display and lipid-chain parsing; pathway classes still come from Ontology
or LIPID MAPS IDs. The key and label may use the same input column. The upload form
auto-selects recognized ID columns and also allows a custom column name to be
selected manually. Imported IDs are retained as `rowData$LIPIDMAPS_ID`.

Feature mapping prefers MS-DIAL `Ontology` / `opt_global_Ontology` labels.
For rows without an ontology (including `Unknown`), `subclass` is used when
available, then LIPID MAPS identifiers provide a fallback. Recognized columns
include `LIPIDMAPS`, `lipidmaps_id`, `LM_ID`, `lipidmaps_class_id`, and mzTab-M
`database_identifier`. Bare LM structure IDs, LMSD structure URLs and exact
classification codes are supported. Structure IDs are mapped to their six-character
classification code, not treated as pathway structure identities.

Organism views resolve codes against the selected organism dataset. A code
shared by multiple distinct pathway classes remains unmapped; no abundance is
duplicated across those classes. Supplied ontology labels are retained even if
they do not match the selected pathway. Measurements and pairwise subclass
statistics share the same fallback resolver (statistics use the bundled mapping).

