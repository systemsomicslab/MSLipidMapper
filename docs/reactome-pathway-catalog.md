# Reactome mouse lipid pathway catalog

Reactome release **97**, acquired 2026-10-15, contributes a separate mouse
(`Mus musculus`, NCBI Taxonomy 10090) dataset. The scope is the complete hierarchy
under **R-MMU-556833, Metabolism of lipids**: **117 pathways and 711 unique
reaction-like events**. These include binding and regulatory events, not just
chemical conversions. Hierarchy, containedEvents and SBML event identities agree.

All **711 events have Reactome `isInferred=true`**. They must not be described as
experimentally demonstrated mouse reactions. This flag, stable ID version,
release, event type, original event evidence, source URL, participants, source
model direction and direct pathway membership are retained in RDF. Ancestor
pathway IDs and names are also searchable in the GUI. Source SBML direction does
not establish physiological flux.

## Class coverage

The deterministic ChEBI / LIPID MAPS classifier archived in
`lipid-census-2026-10-15` is reused independently of the Reactome release. No LLM
is involved in retrieval, classification, RDF generation or search.

* 68 LIPID MAPS classification codes / 68 biochemical class nodes.
* 508 events have at least one direct class assignment.
* 341 events have complete direct mappings under the documented rules.
* 324 events have unresolved lipid mappings or container/unspecified participants.
* 46 events have no directly mapped lipid class and no flagged unresolved lipid/container.

Only **SimpleEntity direct ChEBI identities** are class-projected. DefinedSet,
CandidateSet, Complex and Polymer participants retain source identity and
component references. Their members are **not expanded into simultaneous free
substrates**. Consequently, source event coverage is complete for this hierarchy,
while class projection coverage is intentionally incomplete. The review queue has
518 entries (entries, not unique reactions).

The existing persistent class accessions are reused across Rhea and Reactome.
Internal dataset, projection, participant and assertion IDs use the accession
ledger; public Reactome and ChEBI IDs remain their official source identifiers.
The source dataset is `MSLMB000008`, and the class projection dataset is
`MSLMB000009`. Distinct source reactions are never merged solely because they
share a class signature; Rhea and Reactome counts must not be added as counts of
unique biochemical transformations.

## Reproduce

For other organisms, use the [Taxonomy ID command](reactome-multispecies-command.md):
`python scripts/reactome_pathways.py --taxon 9606 10116`.

Install `scripts/requirements-knowledge.txt` in the build environment. Run from
the repository root:

```powershell
python scripts/collect_reactome_mouse.py --output inst/extdata/pathway_knowledge/snapshots/reactome-mouse-2026-10-15
python scripts/build_reactome_catalog.py --snapshot inst/extdata/pathway_knowledge/snapshots/reactome-mouse-2026-10-15 --classifier inst/extdata/pathway_knowledge/snapshots/lipid-census-2026-10-15 --output inst/extdata/pathway_knowledge/drafts/mouse-reactome-2026-10-15
```

The collector archives checksummed responses, request metadata, start/end release
checks, hierarchy, all event details and SBML. Cached responses are verified
before reuse. The builder verifies the snapshot and classifier hashes. For a new
release, use new snapshot/output directories and retain the accession ledger.

Outputs in `inst/extdata/pathway_knowledge/drafts/mouse-reactome-2026-10-15`:

* `candidates.ttl` and equivalent `pathways.jsonld`: source reaction RDF.
* `class-pathways.ttl`: organism-specific lipid class projections and pathways.
* `candidates.json`, CSV files and `search.sqlite`: local search views.
* `pathways.json`, `pathway-membership.json`: hierarchy and direct memberships.
* `mapping-review-queue.json`, `report.json`: unresolved data and coverage limits.

SQLite contains reactions/classes, reaction-class links, pathways, direct pathway
memberships, and Reactome event inference/direction. UniProt reviewed-protein
counts are NULL for Reactome, rather than fabricated as zero evidence.

In R, launch `MSLipidMapper::run_pathway_database_app()`. Select
`mouse-reactome-2026-10-15 [source catalog]`; search a class code such as GP0101,
a Reactome ID, or a pathway name. The Pathways tab lists source pathways. Select
a reaction to inspect participants, mapping decisions and source evidence.

The RDF additions are defined in
`inst/extdata/pathway_knowledge/ontology/mslm-reactome.ttl`.

## License and sources

Reactome data and derived data files are **CC0-1.0** under the
[official Reactome license](https://reactome.org/license). This import uses data,
not Reactome artwork or branding. Source attribution and license links are
retained. No KEGG database retrieval is part of this workflow.

Official API documentation: [Reactome Content Service](https://reactome.org/dev/content-service/).
This run archives the actual API schema in the snapshot and uses the public
ContentService endpoints for the database version, eventsHierarchy,
containedEvents, query/ids and SBML export.
