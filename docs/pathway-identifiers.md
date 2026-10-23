# RDF resource identifiers and LIPID MAPS classification

This document describes the historical 0.3.0 hash-based release.
Current 0.4.0 uses [short fixed accessions and reaction sources](pathway-accessions.md).

Knowledge release 0.3.0 and measurement schema 3.0.0 use IDs for all local
instance resources. Names remain labels; schema class/property names remain
readable vocabulary terms. Authoritative Rhea, UniProt and NCBI Taxonomy URIs
retain their source identifiers.

## Identity

Local instance URIs have this form:

```text
https://systemomicslab.github.io/MSLipidMapper/resource/id/<64 lowercase hex characters>
```

The shared Python/R algorithm is `mslm-sha256-v1`. For each component in
`[scheme, kind, key1, key2, ...]`, concatenate its UTF-8 byte length, a colon and
its UTF-8 bytes, then take SHA-256. Byte-length framing avoids delimiter/slug
collisions. This is an identifier, not a content hash: changing labels,
positions, abundance values or evidence text does not change identity.

Curated source instances are allocated once in the committed
`source/id-registry.json`. Legacy descriptive URIs are lookup keys for migration,
not emitted instance identifiers. A name change must preserve the allocated
ID. New source records are registered with `pathway_knowledge.py register`, then
the registry additions are reviewed. Never regenerate or recycle existing IDs.

Generated resources use immutable keys:

| Resource | Identity keys |
| --- | --- |
| Organism dataset series | Taxonomy ID |
| Organism release | Taxonomy ID, release |
| Pathway | Taxonomy ID, fixed pathway configuration key |
| Reaction | Registered curated reaction/stage ID |
| Participant | Reaction ID, role ID, participant entity ID |
| Organism assertion | Taxonomy ID, UniProt accession, reaction ID, Rhea ID |
| Sample/feature | Dataset identifier, resource kind, original record identifier |
| Measurement scope | Pathway node ID, scope type, sorted chain identifiers |
| Sample measurement | Measured feature/scope ID, sample identifier |
| Statistical comparison/observation | Dataset, exact comparison and scope keys |

Version-specific dataset IDs link to the persistent series through
`dcterms:isVersionOf`. Other reference-knowledge IDs are stable across releases.
Changing a fixed configuration key is an identity migration, not a label edit.
Changing a dataset identifier creates a different experimental dataset.
Cross-language UTF-8 test vectors are committed in
`source/identifier-test-vectors.json`.

## LIPID MAPS IDs

Classification codes (`GP0101`) identify classifications; individual structure
LM_IDs (`LMGP02010325`) identify particular molecules. They are not interchangeable.

| Biochemical class | Classification ID | Relationship |
| --- | --- | --- |
| PC (diacyl) | GP0101 | Equivalent classification |
| PE (unmodified diacyl) | GP0201 | Encompassing classification |
| PS (diacyl) | GP0301 | Equivalent classification |
| PMME / PE-NMe | GP0201 | Encompassing classification |
| PDME / PE-NMe2 | GP0201 | Encompassing classification |

LIPID MAPS includes N-methylated derivatives within GP0201. Therefore PE, PMME
and PDME must have three distinct internal class IDs even though all carry
GP0201. No synthetic LIPID MAPS code or individual molecule ID is assigned to
these generic classes. The explicit mapping is stored in
`source/lipidmaps-class-mappings.json`, with evidence links and verification date.

Every knowledge class carries `lipidMapsSubClassCode`, `lipidMapsClassification`
and `lipidMapsMappingRelation`. Classification concepts use a code-based URI
`.../resource/lipidmaps-classification/GP0201`, `dcterms:identifier "GP0201"`
and a link to the official classification page. These local representations
are not presented as official LIPID MAPS RDF URIs. Biological class labels and
MS-DIAL matching keys remain separate. Search accepts classification IDs and
returns all corresponding classes without merging them.

Official references: [LIPID MAPS classification](https://www.lipidmaps.org/lmsd_search/30201),
[PE-NMe2 record and classification](https://www.lipidmaps.org/databases/lmsd/LMGP02010325),
[PE-NMe and PE-NMe2 standards](https://www.lipidmaps.org/index.php/databases/standardspectraDB/search?class=302&page=3).

## Compatibility

Release 0.2.0 and legacy curated source graphs are preserved as historical input
assets; their descriptive identifiers are not rewritten in place. Current
organism APIs default to 0.3.0, current class mapping resolves through the
registry, and newly generated measurement/statistical RDF uses opaque IDs.
Updating old map references can use the registry. Tests traverse graph
relationships rather than decoding labels from URI paths.
