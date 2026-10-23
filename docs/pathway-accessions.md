# Fixed accessions and reaction provenance (0.4.0)

All current local instance URIs use short fixed accessions, for example
`https://systemomicslab.github.io/MSLipidMapper/resource/MSLMR000005`.
Names are labels. LIPID MAPS classification codes remain distinct properties;
Rhea/UniProt/Taxonomy resources keep their official IDs. Schema vocabulary terms
remain readable names.

| Prefix | Meaning |
| --- | --- |
| MSLMC | Lipid class |
| MSLMR | Reaction / reaction stage |
| MSLMP | Organism pathway |
| MSLMA | Source assertion / organism reaction evidence |
| MSLMT | Reaction participant |
| MSLMB / MSLMV | Dataset series / release |
| MSLMK | Curation record |
| MSLME / MSLMN / MSLMG | Other entity / enzyme / gene |
| MSLMD / MSLMS / MSLMF | Experimental dataset / sample / feature |
| MSLMO / MSLMM / MSLMI / MSLMU / MSLMX | Scope or observation / measurement / comparison / imported class / graph-view resource |

`source/id-registry.json` is the committed knowledge registration ledger. It
contains immutable record keys, assigned accessions, high-water counters and
legacy aliases. Allocation is serialized with a lock directory and saved with
atomic replacement. New reactions/species append allocations; reordering,
label corrections and rebuilding reuse existing entries. IDs are never
renumbered or recycled. The registry must be retained alongside the source.
`register` adds curated source records; `build` registers generated resources
and records the final registry hash in every organism manifest. Reserved
allocations may remain after a failed build; they are not reclaimed.

Experimental RDF uses a persistent SQLite ledger in the R application **data**
directory, not a disposable cache. Set
`options(MSLipidMapper.id_registry = "/shared/identifiers.sqlite")` to select the
ledger. Keep and back up this file. Exports that will be combined must share
the same experimental registry; independently initialized registries are
separate allocation authorities and may assign the same short number to
different records. Knowledge and experimental prefixes are reserved separately
for new allocations. Historical reserved counters seed experimental counters.
The bundled synthetic examples use `source/example-id-registry.sqlite`.

SQLite serializes writers with `BEGIN IMMEDIATE`. An export allocates in one
transaction and commits before writing the RDF output; failures roll back the
allocation transaction. The immutable identity keys remain dataset/record IDs,
reaction/role/entity IDs or taxon/protein/reaction IDs. Byte-length-prefixed UTF-8
framing avoids key ambiguity, but **the public accession is not a hash**.

## Source metadata on every reaction

Every class reaction directly carries:

- `sourceDatabase`: source database names.
- `sourceIdentifier`: source reaction identifiers.
- `sourceUrl`: original record URLs.
- `sourceRelease`: recorded source version.
- `dcterms:source`: URI links to original records.
- `hasSourceAssertion`: detailed, ID-addressed records pairing these fields.

The detailed records preserve database/identifier/URL/version associations when
a reaction has multiple sources. `reactions.csv` and SQLite include
`sources_json` with the same paired records. Individual PEMT stages receive an
explicit Rhea source assertion as well.

The inherited Rhea/Reactome pilot did not record an exact historical database
release. These sources retain `current-at-curation`, `sourceVersionKnown false`
and the recorded date 2026-08-30; the version is not invented. UniProt
`2026_03` belongs to organism-enzyme annotations and is kept separately from the
reaction-definition source. New imports should supply exact source versions.

## Compatibility

0.2.0 and 0.3.0 are immutable historical releases. The default is now 0.4.0;
new measurement RDF is schema 4.0.0. The prior hash registry is archived as
`source/id-registry-v1-sha256.json`. Known legacy resource aliases are retained
in the current ledger. Classification mappings are unchanged: PE, PMME and
PDME keep distinct MSLMC IDs while sharing the broader GP0201 classification.
