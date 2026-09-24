\name{run_mslipidmapper_api}
\alias{run_mslipidmapper_api}
\title{Run the standalone file-upload REST API}
\usage{
run_mslipidmapper_api(
  host = "127.0.0.1", port = 8000L,
  data_dir = "mslipidmapper-jobs",
  token = Sys.getenv("MSLIPIDMAPPER_API_TOKEN"),
  max_upload_mb = 100, job_timeout = 900, max_pending = 32L
)
}
\arguments{
\item{host}{Interface to listen on; defaults to loopback.}
\item{port}{HTTP port.}
\item{data_dir}{Persistent directory for job inputs, state, logs and results.}
\item{token}{Empty string for no authentication (default), or a shared Bearer token of at least 16 characters.}
\item{max_upload_mb}{Maximum request body size in MiB. Configure an ingress limit at the reverse proxy too.}
\item{job_timeout}{Maximum elapsed seconds per analysis worker.}
\item{max_pending}{Maximum combined queued and running jobs.}
}
\value{Invisibly, the Plumber run result.}
\description{
Accepts multipart POST requests containing input and config files and renders
the three bundled pathways with a separate R worker. No Shiny session is needed.
An empty token disables authentication for lab-network operation. When a token
is configured, all endpoints except /healthz require it.
}
\details{
One worker executes at a time. Queued jobs survive restart; interrupted running
jobs are marked failed. Only one API process can own a data directory. Stored
jobs are retained until the administrator removes their directories. Without a
token, reachable clients can access all jobs. An optional shared token also
grants access to all jobs; it is not per-user authentication.
See docs/rest-api.md in the source repository for the HTTP contract and deployment.
}
