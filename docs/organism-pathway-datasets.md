# Organism-specific lipid class pathway datasets

The implemented pilot provides ready-to-use organism datasets, independent of
cyjs. RDF is authoritative; CSV and SQLite are derived views. Construction,
search and measurement mapping do not use an LLM.

## Release 0.4.0 coverage

| NCBI taxon | Organism | Classes | Reactions |
| --- | --- | ---: | ---: |
| 9606 | Human | 5 | 6 |
| 10090 | Mouse | 5 | 6 |
| 10116 | Rat | 5 | 6 |
| 559292 | S. cerevisiae S288C | 5 | 4 |
| 83333 | E. coli K-12 | 2 | 1 |

This is a limited glycerophospholipid pilot, **not complete lipid metabolism**.
The grouping is curated by MSLipidMapper, not a complete imported Reactome
pathway. It covers PC/PE/PS conversions and PE -> PMME -> PDME -> PC stages.
The lumped PE -> PC reaction is excluded. Ceramide is not included yet.
Class labels and grouping are explicit in `source/class-pathways.json`.

Common class reactions come from the existing curated RDF. Organism inclusion
requires an exact taxon and exact Rhea catalytic-activity annotation on a
reviewed UniProt entry. The archived official API response contains 17 entries,
UniProt release 2026_03, retrieved 2026-10-15. Original catalytic-activity
evidence is retained. Reviewed status is not proof of experimental confirmation,
tissue expression, activity or flux. Human enzymes are never copied into other
species. Missing support means absent annotation in this snapshot, not absent
biology. Unknown directionality and coefficients remain unknown.

## Files

The directory `inst/extdata/pathway_knowledge/datasets/0.4.0/` contains a catalog,
validation report and `organisms/<taxon>/` directories. Each organism contains:

- `pathways.nt`: canonical RDF, sorted N-Triples.
- `pathways.jsonld`: equivalent knowledge JSON-LD.
- `manifest.json`: scope, source releases, input/artifact hashes, omitted reactions.
- `classes.csv`, `reactions.csv`, `participants.csv`: map-making tables.
- `search.sqlite`: rebuildable local search index.

Use the tables to make cyjs manually or with an LLM. Use stable URIs, retain
reaction nodes and use lipid-class participants for graph connectivity.
Auxiliary compounds are retained but should not create traversal shortcuts.
Layout edits do not update RDF. A species without support is marked
`no_supported_reactions`, not presented as a complete empty pathway.

All local instances now use short fixed accessions; classes additionally carry LIPID MAPS
classification IDs. PE, PMME and PDME remain distinct classes under GP0201.
See [accession and reaction source policy](pathway-accessions.md).
The historical 0.2.0 and 0.3.0 releases are retained unchanged.

The original response and request manifest are stored under
`snapshots/uniprot-2026-10-15/`. UniProt is recorded as CC BY 4.0; the existing
Rhea/Reactome class definitions retain source and curation descriptions and the
source manifest. Human curation notes describe the common projection's origin,
not experimental evidence for another species.

## Use from R

```r
MSLipidMapper::list_organism_pathway_datasets()
mouse <- MSLipidMapper::read_organism_pathway("10090")
mouse$classes
mouse$reactions
MSLipidMapper::search_organism_pathway("PC", "10090")

result <- MSLipidMapper::organism_pathway_measurements(
  "study.mztab", taxon_id = "10090",
  output_file = "mouse-measurements.jsonld",
  dataset_id = "study-1-mouse", aggregate = "sum",
  normalization = "none", unit = "arbitrary"
)
```

No cyjs, browser or external service is needed. The adapter reuses existing
sample aggregation without creating reaction edges; actual class reactions
are attached from RDF. It retains molecular values and unmapped classes.
All-missing aggregates stay missing. Class matching is exact against configured
labels, including explicitly defined intermediate labels PMME and PDME.
No normalization, FC, tests or molecular-species reaction inference is performed.
Optional acyl subsets remain measurements of the same class. The export records
taxon, knowledge release, mapping-rule version and canonical class links.
Sample species metadata are not inferred/checked: choose the correct taxon.
Use distinct dataset IDs when combining species/studies.

A small synthetic output is bundled at
`inst/extdata/examples/organism-pathway-measurements-example.jsonld` and can be
regenerated with `Rscript tests/create-organism-pathway-example.R`. It demonstrates
PC aggregation, an all-missing PC sample, an unmapped TG feature and unmeasured
intermediate classes; its numbers are demonstration data, not biological results.

## Build and update

Python 3.10+ and the pinned construction dependencies are needed only for
building and validating. R readers use existing package dependencies.

```text
python -m pip install -r scripts/requirements-knowledge.txt
python scripts/pathway_knowledge.py fetch --output snapshots/uniprot-NEW
python scripts/pathway_knowledge.py register
python scripts/pathway_knowledge.py build --snapshot snapshots/uniprot-NEW --release 0.5.0 --output releases/0.5.0
python scripts/pathway_knowledge.py validate releases/0.5.0
python scripts/pathway_knowledge.py diff inst/extdata/pathway_knowledge/datasets/0.4.0 releases/0.5.0
python scripts/pathway_knowledge.py search releases/0.5.0 --taxon 10090 --query GP0101
```

The fetcher follows pagination and archives original responses. Offline build
checks hashes and publishes only after validation. Existing snapshots/releases
cannot be overwritten. Add taxa in `source/organisms.csv`; add reviewed class
reactions in curated RDF and the explicit grouping configuration. Register and
review IDs for new curated records before building. Fetch again
when changing the configured taxa or reactions. Rebuild, inspect unsupported
reactions and review the RDF diff before distributing a new release. The diff
normalizes release-specific URIs while retaining substantive evidence changes.

The pilot automatically fetches UniProt annotations. Bulk Rhea/Reactome
reaction ingestion and a Shiny selection UI are not implemented here.

## Verification

Validation checks artifact/input hashes, RDF syntax, JSON-LD equivalence,
species isolation, annotation support, participants, counts and SQLite/RDF
consistency. It does not run full SHACL or establish biochemical correctness.
Existing SHACL shapes describe a different pilot contract; unknown stage
coefficients must not be replaced with invented values to satisfy them.

```text
python -m unittest discover -s tests -p test_pathway_knowledge.py -v
```

R tests in `tests/testthat/test-organism-pathways.R` cover local reads/search
and cyjs-free measurement output with missing aggregates and canonical links.
Existing cyjs-based export remains supported.
