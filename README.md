 MSLipidMapper

<p align="center">
  <img src="docs/MSLipidMapper.png" width="900" alt="MSLipidMapper">
</p>

MSLipidMapper is an interactive Shiny dashboard for lipidomics data exploration.  
It converts uploaded tables into a project (SummarizedExperiment-based) and provides normalization, exploratory plots, differential analysis, and Cytoscape.js network/pathway visualization (in-browser).

---

## Features

- Import lipidomics tables (MS-DIAL alignment table / Generic wide-format CSV)
- Project structure based on `SummarizedExperiment`
- Sample metadata editor (`sample_id`, `class`, `use`)
- Normalization workflow + download normalized data
- Analysis panels (e.g., PCA / Heatmap / Volcano / Correlation; depends on build)
- Cytoscape.js network/pathway viewer + export

---

## Run MSLipidMapper locally

### Step 1: Clone this repository

```bash
git clone "https://github.com/systemsomicslab/MSLipidMapper.git"
cd MSLipidMapper
```

### Step 2A: Start with batch file (Windows)

Windows users can start the app by double-clicking:

- `MSLipidMapper.bat`

> **Important:** Docker Desktop must be running before you execute the batch file.

### Step 2B: Manual Docker (any OS)

```bash
docker build -t mslipidmapper .
docker run --rm -p 3838:3838 -p 9000:9000 mslipidmapper
```

Open:

- Shiny app: `http://localhost:3838`
- (optional) Static server (node images etc.): `http://localhost:9000`

> If you see `address already in use`, the port is already occupied.  
---

## Prepare input files

MSLipidMapper does **not** accept raw MS files. Prepare a table exported from MS-DIAL or a generic wide-format CSV.

### Supported input types

#### A) MS-DIAL alignment table (recommended)

- Export the alignment result table from MS-DIAL (CSV/TSV).
- The file should include:
  - annotation columns (lipid name/class etc.)
  - sample intensity columns (one column per sample)

MSLipidMapper reads the sample intensity matrix and constructs an analysis-ready object.

#### B) Generic wide-format CSV (matrix)

A generic matrix is supported when your data are already arranged as:

- **Rows**: lipid molecules / features  
- **Columns**: samples  
- **Cells**: abundances / intensities  

Minimum requirements:

- 1st column: feature ID (lipid name or any unique identifier)
- remaining columns: numeric intensities (samples)

> Tip: keep sample IDs consistent across lipidome, metadata, and transcriptome files.

---

## Optional input files

### Sample metadata CSV (recommended)

You can upload metadata separately (CSV) for grouping, coloring, and statistical comparisons.

Requirements:

- one row per sample
- must contain a column that matches sample IDs in the lipidomics table
- additional columns can be anything (group, condition, timepoint, batch, etc.)

In the app, you will select/map:

- `sample_id` column
- `class` column (group label; recommended)

### Transcriptome CSV (optional)

You can optionally load transcriptome (gene expression) data.
The app attempts to align samples by `sample_id` when possible.

Used by multi-omics panels (e.g., lipid–gene correlation) if enabled in your build.

---

## App workflow

### 1) Upload & Edit (project setup)

1. Choose input type (MS-DIAL / Generic).
2. Upload lipidomics file.
3. (Optional) Upload sample metadata and/or transcriptome CSV.
4. Enter a **unique project name**.
5. Click **Submit** to build the project.

After upload, you can edit the sample table (colData) in the app.

Common fields:

- `sample_id` : unique sample identifier used across data
- `class`     : group label used for coloring and comparisons
- `use`       : TRUE/FALSE to include/exclude samples in downstream analysis

Notes:

- samples with `use = FALSE` are excluded from analysis outputs
- columns you assign as `sample_id` / `class` are treated as metadata (not numeric assay)

### 2) Normalize

Go to the **Normalize** tab and run normalization to generate the normalized dataset.

- normalization is applied to the analysis-ready matrix
- normalized data can be downloaded from the app

> If your build restricts analysis before normalization, you must complete this step to activate analysis tabs.

### 3) Analysis

Depending on enabled modules, analysis may include:

- PCA / exploratory plots
- Heatmap (class-level aggregation and/or molecule-level views)
- Volcano / differential analysis
- Correlation (lipid–lipid, lipid–gene if transcriptome is loaded)

#### Log visualization offset rule

When log visualization requires an offset (e.g., `log2(x + offset)`), MSLipidMapper uses:

- `offset = (minimum positive value) / 2`

This avoids `log(0)` while keeping the offset small relative to the data.

---

## Pathway analysis

Typical usage:

1. Load a network/pathway file (.cyjs .gml).
2. Nodes may represent lipid classes, molecules, or pathway entities.
3. Clicking a node opens a popup panel with plots and related information.

Export options may include:

- SVG/PNG export of the network
- export of node-linked plots/images

---

## Example data

Example inputs are provided under:

- `inst/examples/`

---
