\name{mztab_pathway_measurements}
\alias{mztab_pathway_measurements}
\title{Export pathway and sample-level measurements as RDF / JSON-LD}
\usage{
mztab_pathway_measurements(
  x, network, output_file = NULL, group_col = "class",
  subclass_col = "subclass", name_col = "Metabolite name",
  assay_name = "abundance", acyl_chains = character(),
  aggregate = c("sum", "mean", "median"),
  exclude_sample_classes = character(), node_subclasses = NULL,
  dataset_id = NULL, normalization = "unknown", unit = "unspecified",
  rules_file = NULL
)
}
\arguments{
\item{x}{mzTab-M path or lipidomics SummarizedExperiment.}
\item{network}{cyjs path or parsed JSON list with elements.nodes and elements.edges.}
\item{output_file}{Optional output JSON-LD (.json) path.}
\item{group_col}{Sample group column in colData.}
\item{subclass_col}{Lipid subclass column in rowData.}
\item{name_col}{Lipid name column in rowData.}
\item{assay_name}{Assay containing the supplied abundance values.}
\item{acyl_chains}{Chains for independent, overlapping subsets. NULL selects all parsed chains; character() disables subsets.}
\item{aggregate}{Within-sample aggregation: sum, mean or median.}
\item{exclude_sample_classes}{Exact sample groups to exclude; default none.}
\item{node_subclasses}{Optional named character vector mapping node IDs to subclasses.}
\item{dataset_id}{Dataset identifier.}
\item{normalization}{Description of upstream normalization; no normalization is performed.}
\item{unit}{Abundance unit.}
\item{rules_file}{Optional chain-parser rules file.}
}
\value{A list containing document (JSON-LD), node_mapping, acyl_coverage and se.}
\description{
Reads pathway connections from cyjs and exports RDF with sample metadata,
molecular measurements, whole-subclass measurements and acyl-subset measurements.
No FC, p-values or group comparisons are computed.
}
\details{
RDF resources are serialized in JSON-LD using an inline context and graph.
Display attributes such as plot paths, positions and styles are excluded.
Non-finite values become JSON null, including all-missing sums.
Each subset records its definition, member IDs and valid feature counts.
Nodes are matched exactly against subclasses; custom mapping can be supplied.
}
