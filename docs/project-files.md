# Project files (initial version)

Open **Project file** from the left sidebar to save or restore a portable
`.mslm.rds` snapshot. The controls appear only on that page, leaving analysis
pages free of the project panel.

1. Click **Prepare project file**. The browser sends the current pathway graph,
   including node positions, to the server.
2. When preparation completes, click **Download prepared project**.
3. To resume in another browser connection, upload that file with **Restore
   project (.mslm.rds)**. The saved data/settings replace those in that connection.

Preparation freezes the snapshot. To include subsequent changes, prepare again.
The file includes input data; the original CSV/mzTab files are not needed when
restoring. Keep the project file private if the underlying data is private. Load
only trusted project files, since this initial format uses R serialization.

## Saved

- Full lipid SummarizedExperiment, sample metadata/edits, and selected samples.
- Raw and sample-aligned transcriptomics SummarizedExperiments, when present.
- Applied normalized lipid data and the method/offset used to produce it.
- Shared Feature Advanced settings, including group order, palette, statistical
  comparisons, and plot formatting; selected Feature dataset.
- Pathway Analysis networks, names, selected network, node positions and applied
  comparison/filter settings. Current browser graph is captured during preparation;
  layout and drag completion events also synchronize inactive networks' positions.
- File format version, application version and UTC save time.

## Not saved in version 1

- Individual PCA, OPLS-DA, Heatmap, Correlation, Volcano, Enrichment and Acyl chain
  panel controls or cached results; Feature child-panel controls.
- Utility Pathway editor and Ontology builder work in progress.
- Draft/unapplied controls, temporary image URLs/files, generated projections,
  enrichment borders, popup panels, browser zoom/pan, or authentication state.
- Source-file bytes and external database/rules snapshots. Pin app/data versions
  separately if exact regeneration across software/database updates is required.

The UI names the saved scope. Reconfigure other panel controls and rerun analyses
after restoration. This is a workspace snapshot, not an R process checkpoint.

## Validation and compatibility

The application checks the format/schema, data object types, normalization
dimensions and graph structure before replacing data. Unsupported schema versions
and corrupt files are rejected. An application version mismatch is displayed in
the restore status; compatibility across different application versions is not
guaranteed. Editing input data invalidates restored normalization, as with normal
uploads. Restored images are regenerated within the receiving session.

## Tests

`tests/testthat/test-project-session.R` covers serialization, invalid inputs,
Shiny module restoration, normalized-data invalidation, snapshot preparation,
browser graph identity, positions, and removal of stale image references.
Existing pathway workspace tests also cover the normal settings commit flow.
`tests/check-project-capture.cjs` checks the generated Project UI script's browser
capture protocol and reconnect handling using a small JavaScript harness.

Full browser download/upload and multiple-PC checks are still needed before
production use. The feature does not add authentication or computation workers.
