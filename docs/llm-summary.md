# LLM summary (`output.format: llm`)

`llm-summary.json` is a compact export intended to be pasted into, or attached
to, a large language model prompt. It carries the same measurements as the
JSON-LD export but uses plain names instead of URIs, stores values as arrays
aligned to one sample order, and represents conversions as explicit reaction
nodes. For the bundled 884-feature × 48-sample example it is about 70 KB,
compared with about 160 MB for the three JSON-LD files.

Request it through the REST API with any combination of formats:

```yaml
output:
  format: [pdf, jsonld, llm]   # or: llm, both (= pdf + jsonld)
  llm_features: false          # true adds every molecular feature's values
```

## Structure

```json
{
  "format": "mslipidmapper-llm-summary/1",
  "about": "...one-paragraph description for the model...",
  "dataset": {"id": "...", "normalization": "none", "aggregation": "sum", "unit": "unspecified",
              "group_column": "class", "ignored_exclude_groups": []},
  "samples": {"ids": ["S1", "S2", "S3", "S4"], "groups": ["Control", "Control", "Treatment", "Treatment"]},
  "lipids": {
    "PC": {"pathways": ["remodeling", "global"], "values": [10, 12, 20, 24], "n_features": 1,
           "groups": {"Control": {"mean": 11, "sd": 1.41, "n": 2}, "Treatment": {"mean": 22, "sd": 2.83, "n": 2}}},
    "PS": {"pathways": ["remodeling", "global"], "status": "not_measured"}
  },
  "reactions": [
    {"id": "R24", "substrates": ["PC"], "products": ["PS"], "pathways": ["remodeling", "global"],
     "enzymes": [{"name": "phosphatidylserine synthase 1", "genes": ["PTDSS1"],
                  "compartment": "endoplasmic reticulum membrane"}],
     "cosubstrates": ["L-serine"], "coproducts": ["choline"]}
  ],
  "unmapped_classes": ["..."],
  "features": [{"name": "PC 16:0_18:1", "class": "PC", "chains": ["16:0", "18:1"], "values": [10, 12, 20, 24]}]
}
```

- `values` follow `samples.ids`; `null` is a missing value, `0` a measured zero.
  Values are after the configured normalization and class aggregation.
- `groups` gives mean, SD and the number of finite values per sample group.
  No fold changes or tests are computed.
- `lipids` lists every measured class plus every pathway entity; entities absent
  from the data have `status: not_measured`. `unmapped_classes` are measured
  classes that appear in no pathway.
- With `measurements.include_subsets: true`, each lipid also has
  `acyl_subsets` keyed by chain (all parsed chains unless
  `measurements.acyl_chains` selects some).
- Numbers are rounded to six significant digits.

## Reactions

Reactions are derived from the bundled pathway maps. Junction points in a map
(unlabelled, `Node 1`-style or numeric labels) join their inputs into one
reaction, and chained junctions are merged; a direct edge between two entities
is a single-step reaction. Identical conversions found in several pathways are
listed once with all their pathways.

Enzymes, co-substrates, co-products and compartments come from
`inst/extdata/pathway_knowledge/reaction-annotations.csv`. A row annotates
every reaction whose substrates include `substrate` and whose products include
`product`, using the MS-DIAL class names shown on the maps. Multiple values in a
cell are separated by `;`. Add rows to extend the annotations; no code change is
needed. The initial rows cover the curated pilot reactions (PTDSS1, PTDSS2,
PISD, PEMT and UGCG).
