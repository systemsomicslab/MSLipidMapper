# Analysis and figure settings

The former Advanced setting button has been replaced by controls with explicit scope.

| Location | Contents | Scope |
| --- | --- | --- |
| Analysis controls in each tab | Dataset, method, thresholds, feature selection, Top-N, and Run | Current analysis |
| Figure settings in Feature | Fonts, axis labels, dot/box/violin styling, split metadata, statistical annotations | Feature only |
| Figure settings in Correlation | Axis, legend, r/p annotation, and heatmap fonts | Correlation only |
| Figure settings in Pathway | Fonts, axis labels, feature plot styling, split metadata, statistical annotations | Pathway only |
| Shared: colors / order | Sample-group colors and display order | Analyses using sample groups |

Every analysis tab has a Figure settings button for general appearance controls: point size/alpha, fonts, axis label text, legend visibility, and plot geometry such as dot/box/violin. Analysis-specific choices remain in the tab: PCA axes and loading components, Top-N, heatmap orientation/clustering and scale limits, displayed result subsets, interactive/static view, enrichment plot type, and pathway popup content. Export buttons remain next to the plots. Data selection, statistical methods, and analytical thresholds also remain in the analysis controls.

Volcano and Enrichment use result categories rather than sample groups, so they do not show a sample-group settings button. Enrichment's Figure settings contains its font controls and local value-axis label.

Figure styling for Feature, Correlation, and Pathway is independent. Shared colors and order still propagate between analyses. Project files retain each panel's settings; older project files inherit their original shared styles on restore.

Local controls retain their original Shiny IDs, values, and update semantics. The dialog presents the existing bound controls rather than creating duplicates. Closing and reopening it preserves values. Controls that previously took effect with Run retain that behavior.
