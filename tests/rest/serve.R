# Integration-test helper for an existing MSLipidMapper development image.
# Mount the checkout read-only at /src; installs only inside the test container.
dir.create("/tmp/rest-library", showWarnings = FALSE)
.libPaths(c("/tmp/rest-library", .libPaths()))
if (!requireNamespace("filelock", quietly = TRUE)) {
  install.packages("filelock", repos = "https://cloud.r-project.org", lib = .libPaths()[1])
}
install.packages("/src", repos = NULL, type = "source", lib = .libPaths()[1])
library(MSLipidMapper)
Sys.unsetenv("LC_ALL")
testthat::test_file("/src/tests/testthat/test-rest-api.R", env = asNamespace("MSLipidMapper"),
                    reporter = "summary", stop_on_failure = TRUE)
MSLipidMapper::run_mslipidmapper_api(host = "0.0.0.0", port = 8000,
  data_dir = "/tmp/rest-jobs", max_upload_mb = 2, job_timeout = 300)
