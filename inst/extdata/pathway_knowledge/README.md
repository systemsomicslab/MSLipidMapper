# MSLipidMapper Pathway Knowledge

This directory contains the machine-readable reaction knowledge used by
MSLipidMapper. RDF/Turtle is the canonical representation. Cytoscape networks
are views and must refer to canonical resources rather than redefine reactions.

The initial `0.1.0` pilot contains one manually reviewed human ceramide
glucosylation reaction and four human glycerophospholipid headgroup reactions.
It establishes the data contract before bulk imports from Rhea and Reactome are
added. Acyl-chain remodelling reactions are deliberately kept out of this first
headgroup pilot because positional resolution and rule-instantiated molecular
species require a separate curation policy.

## Layout

- `ontology/mslm-core.ttl`: core classes and properties.
- `ontology/mslm-shapes.ttl`: SHACL validation rules.
- `source/source-manifest.yml`: source releases and retrieval provenance.
- `curated/ceramide-pilot.ttl`: reviewed canonical entities and reactions.
- `curated/phospholipid-pilot.ttl`: reviewed PC/PE/PS headgroup reactions.
- `curated/pemt-stages.ttl`: Rhea-traced PE -> PMME -> PDME -> PC traversal stages.
- `organisms/`: taxon-specific UniProt enzyme-to-Rhea assertion graphs.
- `examples/subclass-statistics-example.ttl`: subclass-centred statistical
  observations, including explicit unmeasured intermediate subclasses.
- `queries/observed-subclasses-through-gaps.rq`: traverses reaction stages
  between observed subclasses without requiring intermediate observations.
- `releases/mslm-pathway-knowledge-v0.1.ttl`: release graph importing the
  ontology and curated pilot graph.

Every reaction must keep source assertions separate from the canonical
reaction. Database-curated, manually curated, rule-instantiated, and heuristic
claims must never be represented as if they had the same evidence status.

## Statistics and pathway traversal

Experimental statistics belong to `LipidSubclassObservation` nodes identified
by dataset and comparison. They do not belong to reaction edges. A reaction is
stable reference knowledge, whereas an effect size, p-value, detection status,
and sample size are properties of one experimental observation.

Missing intermediate lipids must be represented explicitly as `NotMeasured` or
`NotDetected`; absence of an observation is never interpreted as zero. Direct
`hasSubstrateEntity` and `hasProductEntity` edges are traversal projections of
canonical Rhea stages. They allow graph queries to connect two observed lipid
subclasses across any number of unobserved intermediate subclasses.

## mzTab-M to metabolic-state observations

For comparison-independent exports, use `mztab_pathway_measurements()`.
It reads pathway connections from cyjs and exports RDF as JSON-LD, connecting
lipid nodes to sample-level whole-subclass and acyl-subset measurements, original
molecular values and sample metadata. Display attributes are excluded, and it
computes no FC or p-values. See `docs/pathway-measurements.md` for the data contract.
The pairwise RDF function below remains a separate entry point.

`mztab_pairwise_subclass_rdf()` accepts an mzTab-M path or a
`SummarizedExperiment`. It computes every pair of sample classes and returns a
statistics table plus Turtle RDF. Subclass abundance is aggregated per sample
before the two-group test. Optional `acyl_chains` create additional scopes that
include only molecular species carrying each requested chain.

```r
state <- mztab_pairwise_subclass_rdf(
  "study.mztab",
  group_col = "class",
  acyl_chains = c("18:0", "24:1"),
  aggregate = "sum",
  test = "welch_t",
  output_file = "study-metabolic-state.ttl"
)
state$statistics
```

The function never computes substrate/product ratios. Each whole-subclass or
chain-filtered result becomes its own `LipidSubclassObservation`, allowing the
same observations to be projected onto any Rhea pathway traversal later.
