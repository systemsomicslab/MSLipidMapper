\name{organism_pathways}
\alias{list_organism_pathway_datasets}
\alias{read_organism_pathway}
\alias{search_organism_pathway}
\alias{organism_pathway_measurements}
\title{Use prebuilt organism-specific lipid class pathway datasets}
\description{Read and search versioned RDF-derived organism datasets locally,
or attach sample measurements and canonical reaction knowledge to JSON-LD
without supplying a Cytoscape network. The bundled release is a limited
glycerophospholipid pilot, not a complete organism metabolic reconstruction.}
\usage{
list_organism_pathway_datasets(release = "0.4.0")
read_organism_pathway(taxon_id, release = "0.4.0")
search_organism_pathway(query, taxon_id, release = "0.4.0")
organism_pathway_measurements(x, taxon_id, output_file = NULL,
  release = "0.4.0", ...)
}
\arguments{
\item{release}{Knowledge release identifier, independent of the package version.}
\item{taxon_id}{Exact NCBI taxonomy identifier. Species is never inferred.}
\item{query}{Non-empty literal search string; no regular expressions or LLM.}
\item{x}{mzTab-M file path or lipidomics SummarizedExperiment.}
\item{output_file}{Optional JSON-LD destination.}
\item{...}{Arguments to \code{mztab_pathway_measurements}, except network,
node_subclasses and output_file. Includes aggregation, sample filters,
dataset ID, normalization description, units and optional acyl subsets.}
}
\value{A catalog data frame, dataset list, search result list, or measurement
result list including the document, node mapping and knowledge manifest.}
\details{Organism inclusion requires an exact taxon and Rhea catalytic-activity
annotation on a reviewed UniProt entry. This is database annotation support,
not a claim of experimentally established activity in every tissue. Unknown
directionality remains unknown. Molecular values are retained; pathway mapping
uses exact class labels. An empty result does not imply biological absence.}
