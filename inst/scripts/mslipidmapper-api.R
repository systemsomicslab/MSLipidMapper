#!/usr/bin/env Rscript
number <- function(name, default) as.numeric(Sys.getenv(name, unset = default))
MSLipidMapper::run_mslipidmapper_api(
  host = Sys.getenv("MSLIPIDMAPPER_API_HOST", "127.0.0.1"),
  port = number("MSLIPIDMAPPER_API_PORT", "8000"),
  data_dir = Sys.getenv("MSLIPIDMAPPER_JOB_DIR", "mslipidmapper-jobs"),
  max_upload_mb = number("MSLIPIDMAPPER_MAX_UPLOAD_MB", "100"),
  job_timeout = number("MSLIPIDMAPPER_JOB_TIMEOUT", "900"),
  max_pending = number("MSLIPIDMAPPER_MAX_PENDING", "32")
)
