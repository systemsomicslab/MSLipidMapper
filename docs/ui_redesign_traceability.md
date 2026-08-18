# UI redesign sample traceability

This checklist traces the static prototype in `docs/ui_redesign_sample.html` against the modules mounted by `R/run_mslipidmapper_app.R`.

## Mounted dashboard modules

| App area | Source UI | Prototype coverage |
| --- | --- | --- |
| Upload | `mod_upload_ui()` in `R/upload_module.R` | Covered as five Upload subpages: lipidomics input, sample selection, metadata/class, feature preview, transcriptome. The first page keeps MS-DIAL/Generic as a left-side mode switch and places sample metadata input on the right. Includes MS-DIAL, Generic assay/feature files, ontology shortcut, SE summary KPIs, master sample table, analysis sample preview, sample metadata mapping, class-level use table, rowData preview, transcriptome upload, organism, and tx row/col preview area. |
| Normalize | `mod_normalize_ui()` in `R/norm_module.R` | Covered. Includes method, method explanation area, x label angle, QC plot, plot format/download, data format/download. |
| Analysis hub | `run_mslipidmapper_app.R` hub cards | Covered. Includes PCA, OPLS-DA, Feature, Heatmap, Correlation, Volcano, Enrichment, Decoupled chains, Pathway analysis. |
| PCA | `mod_pca_generic_ui()` in `R/mod_pca_generic.R` | Covered. Includes lipid/transcriptome dataset, PC axes, point size, loading PC, top-N, heatmap top, z-limit, run, score plot, loading plot, loading heatmap, PDF downloads. Score/loading plots are square and kept visible together. |
| OPLS-DA | `mod_oplsda_generic_ui()` in `R/mod_oplsda_generic.R` | Covered. Includes lipid/transcriptome dataset, class group selection, score point size, VIP threshold, top VIP, heatmap top, z-limit, run, score plot, VIP plot, VIP CSV/PDF, VIP heatmap PDF. Score/VIP plots are square and kept visible together. |
| Feature shared advanced | `mod_feature_generic_ui()` and advanced modal in `R/mod_feature_generic.R` | Covered. Includes Lipid/Gene switch and shared Advanced drawer sections for colors/order, palette/order table, panel split, statistics, comparison mode, plot styling, y-axis labels, dot/box/violin controls, and heatmap row z-score/top-N. |
| Lipid Feature | `mod_plot_lipid_ui()` in `R/mod_plot_lipid.R` | Covered. Includes class-level plot, molecule-level plot, plot type, lipid class, molecule, aggregation, top-N molecules, top-N chains, chain filters, heatmap/bar/acyl-chain composition tabs, SVG downloads. The two single-feature plots are square and visible together. |
| Gene Feature | `mod_plot_gene_ui()` in `R/mod_plot_tx.R` | Covered by Feature's Gene switch plus single gene square plot and high-variance heatmap representation in the shared feature layout. |
| Heatmap | `mod_heatmap_ui()` in `R/Heatmap.R` | Covered. Includes Class heatmap and Molecule TopVar heatmap modes, aggregation, top-N, variability metric, samples on rows/columns, scaling, tile size, max heatmap size, shared palette, clustering, molecule subclass annotation, PDF/PNG downloads, large heatmap output. |
| Correlation | `mod_plot_cor_ui()` in `R/corrlation_module.R` | Covered. Includes Single, Focus, All pair modes; lipid/gene and ratio pair types; Pearson/Spearman; log2/raw transform; static/interactive view; focus thresholds; all-pair top-N and max lipids/genes heatmap; scatter, heatmap/table area; CSV/PDF downloads. |
| Volcano | `mod_volcano_ui()` in `R/mod_volcano.R` | Covered. Includes lipid/gene dataset, group A/B, test, p adjustment, raw/adjusted p mode, p threshold, FC threshold, point size, ggplot/plotly view, plot/table tabs, PDF/CSV downloads. Volcano plot is square. |
| Enrichment | `mod_lipid_enrich_ui()` in `R/Enrich_module.R` | Covered. Includes background/significant files, dynamic lipid/ontology/significant columns, chain-term generation, dropping missing ontology, p-adjust method/cutoff, Barplot/UpSet, font and UpSet controls, metadata/plot/table tabs, plot/significant/background downloads. |
| Decoupled chains | `mod_decoupled_chains_ui()` in `R/mod_decoupled_chains.R` | Covered. Includes Heatmap/Panels modes, chain/subclass selectors, correlation method, compare-to total/rest, species and chain-fraction thresholds, font sizes, point size/alpha, outlier method/k, lm R2, log10 pseudo, heatmap PDF and panels PDF. Scatter panels are square and visible as a set. |
| Pathway analysis | `mod_cyto_ui()` in `R/Mod_cyto.R` | Covered. Includes network import/select/delete/fit, pathway mapping, dot/violin/box plot type, multi-omics overlay, enrichment CSV apply/clear, differential apply/clear, PDF export with/without popups, delete selected, class aggregation, heatmap top-N, acyl-chain filtering, network canvas, selected-node left/right plots and downloads. |
| Ontology builder | `mod_utility_ontology_builder_ui()` in `R/mod_utility_ontology_builder.R` | Covered. Includes upload/paste mode, CSV/TSV file, separator, lipid-name column, parse/build, status/preview, feature CSV and failed-list downloads. |
| Pathway editor | `mod_utility_pathway_editor_ui()` in `R/mod_utility_pathway_editor.R` | Covered. Includes network import, fit after import, load replace, edit mode move/add node/add edge/delete, reset layout, fit view, horizontal/vertical align, export filename, .cyjs export, editable canvas. |

## Not mounted in the current dashboard

These UI functions exist in `R/` but are not mounted by `run_mslipidmapper_app.R` in the current dashboard body:

| Source | Status |
| --- | --- |
| `mod_ccd_analysis_ui()` in `R/mod_ccd_analysis.R` | Not represented in the prototype because the current app does not add a menu item/tab for it. |
| `chain_metadata_app.R` standalone app | Not represented because it is a separate utility app, not part of `run_mslipidmapper_app.R`. |
| Older/alternate wrappers such as `plot_module.R` or `heatmap_module.R` | Not represented separately where superseded by mounted modules. |

## Layout constraints traced from code

- Plots with explicit square layout or `aspect.ratio = 1` are represented with `aspect-ratio: 1 / 1`.
- Multi-plot pages keep related plots visible together: PCA score/loading, OPLS score/VIP, lipid class/molecule feature plots, pathway selected-node left/right plots, and decoupled chain panels.
- Tall heatmaps and network canvases remain non-square where the source UI uses tall fixed-height outputs rather than square frames.
