# Mouse pathway acquisition: 2026-10-15

Target: Mus musculus, NCBI Taxonomy 10090. This is a source acquisition and
class-projection draft, not a released, reviewed pathway map.

The archived UniProt query selects reviewed, non-fragment mouse proteins with
GO:0006629 and Rhea catalytic annotations. It returned 788 proteins. Their
annotations resolved to 3,110 approved master reactions in Rhea 2026_03.
These include non-lipid chemistry and must not be described as 3,110 mouse
lipid-class reactions. Unresolved source IDs remain in the snapshot manifest.

The first exact mapping scope is PC, PE, PS, PMME, PDME, LPC, LPE, DG and TG.
Each supported generic ChEBI identifier has an explicit LIPID MAPS classification
code. PMME and PDME remain distinct despite sharing GP0201. Molecular species
are not assigned to classes by their names. Classification references:
https://www.lipidmaps.org/databases/lmsd/browse and
https://www.lipidmaps.org/lmsd_search/30105 (LPC),
https://www.lipidmaps.org/lmsd_search/30205 (LPE),
https://www.lipidmaps.org/lmsd_search/20201 (DG),
https://www.lipidmaps.org/lmsd_search/20301 (TG).

The mapper produced 41 reaction candidates with different mapped classes on
the two canonical Rhea sides. These are partial class projections requiring
participant review, not physiological directional edges. Transport reactions
are excluded. The 3,214 unmapped ChEBI terms include ordinary cofactors as well
as unsupported lipid classes; this number is not a missing-lipid count.

Outputs under `inst/extdata/pathway_knowledge/`:

- `snapshots/mouse-lipid-2026-10-15/`: raw responses, exact queries, retrieval
  metadata, release headers, checksums, protein evidence, reaction participants.
- `drafts/mouse-lipid-2026-10-15/candidates.ttl`: provisional RDF with official
  Rhea reaction URIs, mouse taxon, UniProt protein support and reaction source.
- `drafts/mouse-lipid-2026-10-15/candidates.json`: complete retrieved participant
  rows for candidate review, including coefficients and unmapped compounds.
- `drafts/mouse-lipid-2026-10-15/candidates.csv`: readable candidate index.
- `drafts/mouse-lipid-2026-10-15/unmapped-compounds.json`: mapping review queue.
- `drafts/mouse-lipid-2026-10-15/report.json`: machine-readable run summary.

Use Python with `scripts/requirements-knowledge.txt` installed:

```powershell
python scripts/collect_mouse_pathways.py --output inst/extdata/pathway_knowledge/snapshots/mouse-lipid-2026-10-15
python scripts/map_mouse_pathway_candidates.py --snapshot inst/extdata/pathway_knowledge/snapshots/mouse-lipid-2026-10-15 --output inst/extdata/pathway_knowledge/drafts/mouse-lipid-2026-10-15
```

Acquisition resumes from validated cached responses. Mapping is offline and
checks every archived file hash and a consistent Rhea release before use.
Neither step calls an LLM. Candidates have no new local instance IDs: reactions
and compounds retain their official Rhea and ChEBI URIs. Promotion to a release
must use the persistent MSLM accession registry, review ambiguous class mappings,
retain the underlying molecular reaction and evidence, then build the mouse
dataset and its search index. The existing release 0.4.0 is unchanged.
