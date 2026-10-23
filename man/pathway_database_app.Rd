\name{pathway_database_app}
\alias{pathway_database_app}
\alias{run_pathway_database_app}
\title{Standalone pathway database browser}
\usage{
pathway_database_app()
run_pathway_database_app(launch.browser = interactive(), host = "127.0.0.1", port = NULL)
}
\arguments{
\item{launch.browser}{Whether to open the system browser.}
\item{host}{Listening address, default localhost.}
\item{port}{Optional listening port.}
}
\description{Browse and search bundled organism pathway releases and explicitly marked draft candidates. Inspect participants, LIPID MAPS class codes, dataset provenance and reaction sources. Download search results as CSV or the selected dataset as RDF. Uses local files without external services.}
\value{A Shiny application object, or the result of running the application.}
\examples{
if (interactive()) run_pathway_database_app()
}
