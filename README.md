# MSLipidMapper

<p align="center">
  <img src="docs/MSLipidMapper.png" width="900" alt="MSLipidMapper application overview">
</p>

MSLipidMapper is an interactive Shiny workspace for lipidomics analysis.
Uploaded data are converted to
`SummarizedExperiment` objects and used throughout normalization, exploratory
analysis, differential analysis, acyl-chain analysis, enrichment, and
Cytoscape.js-based pathway visualization.

MSLipidMapper accepts processed abundance tables. It does not process raw mass
spectrometry files.

The bundled pathway database also has a standalone browser:

```r
MSLipidMapper::run_pathway_database_app()
```

Select an organism and release or draft, search lipid classes, LIPID MAPS
classification codes and reaction IDs, inspect participants and sources, and
download CSV results or dataset RDF. It runs locally without measurement uploads
or external services. Draft candidates are explicitly marked as unreviewed.

The mouse source catalog currently contains 2,950 reactions and 118 biochemical
class nodes linked to 116 LIPID MAPS classification codes (Rhea/UniProt 2026_03).
Reactions with incomplete class mapping remain searchable. See the
[coverage definition and rebuild workflow](docs/lipid-reaction-catalog.md).

## Main features

- Import an MS-DIAL Alignment Table CSV
- Import an MS-DIAL mzTab-M file directly
- Import generic sample-by-lipid data with a separate lipid-to-Ontology table
- Edit sample metadata and include or exclude samples from analysis
- Normalize abundances and export normalized data
- Visualize lipidomics data interactively
- Perform enrichment and acyl-chain-level analysis
- View, edit, and export Cytoscape.js pathway networks
- Render pathway PDFs from the command line without starting Shiny
- Submit lipidomics files through a standalone REST API and retrieve pathway PDFs

## Installation

### Install as an R package

R 4.3 or later is required. Install the Bioconductor dependencies first, then
install MSLipidMapper from GitHub.

```r
install.packages(c("BiocManager", "remotes"))

BiocManager::install(c(
  "SummarizedExperiment",
  "S4Vectors",
  "ComplexHeatmap",
  "clusterProfiler",
  "GO.db",
  "ropls",
  "rgoslin"
), ask = FALSE, update = FALSE)

remotes::install_github(
  "systemsomicslab/MSLipidMapper",
  dependencies = TRUE
)
```

Launch the application with:

```r
MSLipidMapper::run_mslipidmapper()
```

The Shiny application uses port `3838` by default. Bundled example files,
pathway networks, and `lipid_rules.yaml` are installed with the package.

### Run with Docker

Docker Desktop or another Docker runtime must already be running.

```bash
git clone https://github.com/systemsomicslab/MSLipidMapper.git
cd MSLipidMapper
docker build -t mslipidmapper .
docker run --rm -p 3838:3838 -p 7310:7310 mslipidmapper
```

The Docker build prefers precompiled packages from the container repository
matching the Bioconductor base image. These versions can be older than current
CRAN releases; packages already supplied by the base image are retained. Dependencies
unavailable as container binaries use the base image's regular repositories as a
fallback (which may also supply binaries). Downloads
and package installation use four workers by default; adjust this for the Docker
CPU and memory limits with `--build-arg MSLIPIDMAPPER_BUILD_CPUS=2` (or `=8`).
Installed runtime dependency versions are recorded in
`/srv/app/docker-dependency-versions.csv` inside the image. Dependency installation
also verifies that every required analysis and plotting package can be loaded.

Open:

- Shiny application: `http://localhost:3838`
- Plot/static asset API: `http://localhost:7310`

On Windows, `MSLipidMapper.bat` builds and starts the container. Docker Desktop
must be running before the launcher is used.

## Standalone REST API

The analysis REST service is separate from Shiny and the existing port-7310
plot/static API. It accepts multipart POST uploads of an MS-DIAL CSV or mzTab-M
file plus YAML settings, runs a background analysis job, and provides status and
PDF or RDF/JSON-LD downloads. Set `output.format: jsonld` and
`measurements.include_subsets: true|false` to control sample-level subset exports.
The default Compose configuration supports direct lab-network
access without a token.

```bash
unset MSLIPIDMAPPER_API_TOKEN
docker compose -f compose.api.yml up -d --build
```

Call `http://SERVER_LAN_IP:8000` from another lab computer. Compose publishes
port 8000 on all server interfaces by default; set `MSLIPIDMAPPER_BIND_ADDRESS`
to a specific LAN IP if desired. See [REST API documentation](docs/rest-api.md) for POST examples,
configuration, authentication, persistence and server operation.

## Lipidomics input

Choose one of the following formats on the Upload page.

### MS-DIAL Alignment Table CSV

Upload an Alignment Table exported by MS-DIAL as CSV. MSLipidMapper reads the
MS-DIAL annotation fields and sample abundance columns and builds an
analysis-ready `SummarizedExperiment`.

The current loader expects the standard MS-DIAL Alignment Table layout,
including its multi-row header and annotation columns. A manually simplified
CSV should be imported with the Generic option instead.

### MS-DIAL mzTab-M

Upload an MS-DIAL mzTab-M file (`.mztab` or `.mzTab`) directly. MSLipidMapper
reads the abundance data, sample information, and lipid annotations and builds
the same analysis-ready structure used by the other input formats.

### Generic CSV with Ontology table

Generic import uses two files.

#### Assay CSV

The assay table is arranged as samples by lipids:

| sample_id | class | PC 34:1 | PE 36:2 |
|---|---|---:|---:|
| Sample_1 | Control | 1200 | 820 |
| Sample_2 | Treatment | 950 | 1100 |

- one row per sample
- one `sample_id` column
- an optional `class` column
- all remaining selected columns are numeric lipid abundances

#### Feature/Ontology CSV

The feature table maps assay column names to lipid Ontology values:

| lipid | Ontology |
|---|---|
| PC 34:1 | PC |
| PE 36:2 | PE |

Lipid names must match the abundance-column names in the assay CSV.


## Command-line pathway mapping

Pathway PDFs can be generated from a YAML configuration without starting the
Shiny application. A complete example is installed at
`inst/extdata/examples/pathway-cli.yml`.

From an installed package:

```bash
Rscript -e "quit(status=MSLipidMapper::mslipidmapper_cli(commandArgs(TRUE)))" pathway --config parameters.yml
```

From a source checkout after installing the package:

```bash
Rscript inst/scripts/mslipidmapper.R pathway --config inst/extdata/examples/pathway-cli.yml
```

Input, network, and output paths can be overridden at execution time:

```bash
Rscript inst/scripts/mslipidmapper.R pathway \
  --config parameters-mztab.yml \
  --input data/results.mzTab \
  --network pathways/custom.cyjs \
  --output results/custom-pathway.pdf
```

Useful options:

- `--input`: overrides the lipidomics input path
- `--network`, `--cyjs`, or `-n`: uses one custom network
- `--output` or `-o`: overrides the PDF file or output directory
- `--acyl-chains` or `-a`: retains molecules containing exact acyl chains
- `--acyl-match`: selects `any` or `all` matching for multiple chains

The CLI accepts both MS-DIAL Alignment Table CSV and MS-DIAL mzTab-M input.

### RIKEN Lipidomics Excel to mzTab-M

The RIKEN workbooks have variable annotation widths (including ion-mobility
columns) and two abundance blocks separated by `Lipid IS` (or the legacy
`standard class` label). The converter finds
the header and section boundaries by label, preserves the workbook sample
metadata as assay custom fields, and writes one mzTab-M file per worksheet.

```powershell
python scripts/riken_xlsx_to_mztab.py `
  "../RIKEN Lipidomics/RIKEN_LIPIDOMICS_dataset/dataset" `
  "results/riken_mztab"
```

Normalized abundance and samples marked `Public` are selected by default. Use
`--abundance raw`, `--privacy all`, `--sheet NAME`, or
`--group-by "Category,Tissue/Species,Genotype/Background"` when needed. Invalid
workbooks are reported and skipped in batch mode; add `--strict` to stop on the
first error. The script requires Python 3.10+ and `openpyxl`.

When no custom network is specified, the bundled remodeling, ceramide, and
global pathway networks are rendered. The YAML file controls normalization,
group inclusion or exclusion, plot type (`dot`, `box`, or `violin`), colors,
fonts, and output dimensions. Chrome or Chromium is required for network PDF
rendering; use `pathway.browser` or the `MSLIPIDMAPPER_BROWSER` environment
variable if it is not detected automatically.

Molecule-level acyl-chain filtering can also be set in YAML with
`analysis.acyl_chain_filter.chains` and `match: any|all`. It is applied before
normalization and lipid-class aggregation. Only explicitly parsed acyl chains
are matched: total-composition names such as `PC 34:1` are not guessed, and
sphingoid bases are not treated as acyl chains.

## Pathway structure with sample-level measurements

Prebuilt organism-specific class pathway datasets are also available for human,
mouse, rat, yeast and E. coli. This initial glycerophospholipid pilot supports
local search and `organism_pathway_measurements()` JSON-LD export without cyjs.
See [organism dataset coverage, usage and update workflow](docs/organism-pathway-datasets.md).

`mztab_pathway_measurements()` reads an existing cyjs pathway and exports RDF as
JSON-LD with sample metadata, molecular abundances and whole-subclass/acyl-subset
measurements. Display attributes are excluded; no FC or p-values are computed.
See [the data contract and usage](docs/pathway-measurements.md).

## Example data

Examples are available in `inst/extdata/examples/` and can be located from an
installed package with:

```r
example_dir <- system.file("extdata", "examples", package = "MSLipidMapper")
list.files(example_dir, recursive = TRUE)
```

Bundled examples include MS-DIAL-style lipidomics data, sample metadata,
Cytoscape networks, a pathway CLI configuration, and example pathway PDFs.

## Reactome pathway catalog

The standalone pathway database browser also includes a mouse Reactome release 97
catalog (117 pathways, 711 source events), with inference flags and deterministic
LIPID MAPS class projections. See [Reactome coverage and rebuild instructions](docs/reactome-pathway-catalog.md).
To build datasets for other species, run `python scripts/reactome_pathways.py --taxon 9606 10116`.
See [species selection, offline builds and update commands](docs/reactome-multispecies-command.md).
Mouse Rhea and Reactome catalogs can also be searched together using the
[source-preserving integrated catalog](docs/integrated-pathway-catalog.md).
Select organism-specific pathways and measured lipid classes with
[mapping views and filtered RDF exports](docs/pathway-mapping-views.md).

## License

See [LICENSE](LICENSE).
