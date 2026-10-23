# Source-complete lipid reaction catalog

Run `MSLipidMapper::run_pathway_database_app()` and select
`mouse-lipid-census-2026-10-15 [source catalog]`. Coverage is reported in the
Dataset / sources tab. The earlier draft and release 0.4.0 remain separate.

## Inclusion and evidence

The reference includes every approved Rhea master reaction involving a ChEBI
lipid (CHEBI:18059 descendant), including conjugate acid/base and tautomeric forms. Current
ChEBI uses `https://w3id.org/chemrof/conjugate_base_of` links. Following only
uncharged lipid ancestors omits many phospholipids. Queries, responses, release
headers and hashes are archived. Independent COUNT queries check the complete
reaction/ChEBI reference universe, is-a edges and chemical-form edges before
the lipid census is derived offline. OWL tautomer restrictions are normalized
alongside direct chemical-form links, so zwitterionic PE is retained.

Mouse membership requires an exact Rhea annotation on a UniProt entry with taxon
10090. The UniProt query has no GO, class, reviewed-status or fragment filter.
Reviewed status and evidence remain attached to each protein; annotations are
not experimental proof. Directional IDs are resolved through official Rhea
mappings. Unresolved source IDs remain in a queue. The global reference is not
automatically assigned to mouse.

Transport, same-class transformations, synthesis/degradation with one mapped
lipid side, and incomplete class mappings are retained. Rhea equation left/right
sides do not establish physiological direction.

## Classification and files

LIPID MAPS codes come from exact ChEBI structure cross-references and the official
classification vocabulary, supplemented by explicit generic-class seeds in
`source/lipid-class-seeds.json`. Charge forms and descendants inherit the nearest
supported classification only when assignments agree. Ambiguous/unmapped
compounds remain searchable without fabricated codes. Names are not fuzzily
matched, and no workflow step calls an LLM.

Codes can be category, main-class or subclass level; the report separates these
levels. PMME and PDME remain distinct nodes despite sharing GP0201.

Files in `inst/extdata/pathway_knowledge/drafts/mouse-lipid-census-2026-10-15/`:

- `class-pathways.ttl`: class-level RDF with persistent MSLM identifiers,
  classification codes, source records and explicit mapping completeness.
- `classes.csv`: canonical classes and supporting reaction counts.
- `candidates.ttl`, `pathways.jsonld`: equivalent source-level RDF with all
  participants, coefficients, mouse evidence, source versions and external
  references. Symbolic coefficients remain expressions.
- `candidates.json`, `reactions.csv`: browseable source records.
- `search.sqlite`: offline reaction search database.
- `global-reference.csv`: global reference with mouse evidence counts.
- `classification-decisions.json`, `mapping-review-queue.json`,
  `unresolved-mouse-annotations.json`: decisions and unresolved data.
- `report.json`: coverage, source releases, source/rule hashes and limitations.

## Rebuild

From the repository root with `scripts/requirements-knowledge.txt` installed:

```powershell
python scripts/collect_lipid_classification.py --output inst/extdata/pathway_knowledge/snapshots/lipid-census-2026-10-15
python scripts/collect_lipid_reaction_catalog.py --output inst/extdata/pathway_knowledge/snapshots/lipid-census-2026-10-15
python scripts/build_lipid_reaction_catalog.py --snapshot inst/extdata/pathway_knowledge/snapshots/lipid-census-2026-10-15 --output inst/extdata/pathway_knowledge/drafts/mouse-lipid-census-2026-10-15
```

Use a new dated snapshot directory for a new source release. Cached requests and
hashes are verified rather than silently replaced. Building runs offline and
uses the persistent accession registry under a lock.

Completeness refers to these source releases and inclusion rules, not all
biological knowledge, experimentally established mouse pathways, or every
pathway database. External Rhea cross-references preserve links to resources
such as Reactome and KEGG; they are not independent imports or organism evidence.
