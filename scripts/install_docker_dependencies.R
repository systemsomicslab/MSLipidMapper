#!/usr/bin/env Rscript
# Keep the binary repository paired with the R / OS in the Bioconductor image.
# Newer CRAN sources must not replace compatible, precompiled dependencies.
cores <- suppressWarnings(as.integer(Sys.getenv("MSLIPIDMAPPER_BUILD_CPUS", "4")))
if (is.na(cores) || cores < 1L) stop("MSLIPIDMAPPER_BUILD_CPUS must be a positive integer")
options(Ncpus = cores, timeout = max(600, getOption("timeout")))
started <- proc.time()[["elapsed"]]

description <- read.dcf("DESCRIPTION")
fields <- intersect(c("Depends", "Imports", "LinkingTo"), colnames(description))
required <- trimws(sub("\\s*\\(.*$", "", unlist(strsplit(description[1, fields], ","))))
# Preserve the optional analysis and plotting features shipped by the image.
required <- setdiff(unique(c(required, "ropls", "rgoslin", "UpSetR", "patchwork",
  "ggrepel", "ggpmisc", "ggprism", "colourpicker")), "R")

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager", repos = "https://cloud.r-project.org")
}
repos <- BiocManager::repositories()
binary_repo <- repos["BioCcontainers"]
if (is.na(binary_repo) || !nzchar(binary_repo)) {
  stop("Use the Bioconductor Docker base image: its binary repository is required")
}
binary_db <- available.packages(repos = binary_repo)
if (!nrow(binary_db)) stop("The Bioconductor container binary index is unavailable")

installed <- rownames(installed.packages())
closure <- unique(c(required, unlist(tools::package_dependencies(
  intersect(required, rownames(binary_db)), db = binary_db,
  which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE), use.names = FALSE)))
missing <- setdiff(closure, c("R", installed))
binaries <- intersect(missing, rownames(binary_db))
source_only <- setdiff(missing, binaries)
message("[deps] Parallel workers: ", cores,
  "; already installed: ", sum(closure %in% installed),
  "; container binaries: ", length(binaries),
  "; source fallback: ", paste(source_only, collapse = ", "))

if (length(binaries)) {
  # Base R downloads repository packages serially. Fetch independent archives
  # concurrently, then install from a temporary local repository so installation
  # still follows dependency order and uses package-level locking.
  local_repo <- tempfile("mslm-binary-repo-")
  contrib <- file.path(local_repo, "src", "contrib")
  dir.create(contrib, recursive = TRUE)
  downloads <- parallel::mclapply(binaries, function(pkg) {
    utils::download.packages(pkg, destdir = contrib, repos = binary_repo,
      available = binary_db, quiet = TRUE)
  }, mc.cores = min(cores, length(binaries)), mc.preschedule = FALSE)
  good <- vapply(downloads, function(x) is.matrix(x) && nrow(x) == 1L &&
    file.exists(x[1, 2]), logical(1))
  if (!all(good)) stop("Binary downloads failed: ", paste(binaries[!good], collapse = ", "))
  message("[deps] Binary downloads completed after ",
    round(proc.time()[["elapsed"]] - started, 1), " seconds")
  # These archives use binary-specific names. write_PACKAGES(type = "source")
  # omits the necessary File field, so retain it and point the available matrix
  # directly at the downloaded files.
  local_db <- binary_db[binaries, , drop = FALSE]
  local_db[, "Repository"] <- paste0("file://", contrib)
  local_db[, "File"] <- vapply(downloads, function(x) basename(x[1, 2]), character(1))
  install.packages(binaries, repos = paste0("file://", local_repo),
    available = local_db, dependencies = FALSE, Ncpus = cores)
  missing_binaries <- setdiff(binaries, rownames(installed.packages()))
  if (length(missing_binaries)) {
    stop("Binary installation failed: ", paste(missing_binaries, collapse = ", "))
  }
  unlink(local_repo, recursive = TRUE)
}

if (length(source_only)) {
  # Only packages without a matching container binary take the source path.
  # Already installed dependencies are retained rather than upgraded wholesale.
  install.packages(source_only, repos = repos, Ncpus = cores,
    dependencies = c("Depends", "Imports", "LinkingTo"))
}
missing <- setdiff(required, rownames(installed.packages()))
if (length(missing)) stop("Missing runtime dependencies: ", paste(missing, collapse = ", "))
for (pkg in required) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Cannot load runtime dependency: ", pkg)
}
write.csv(data.frame(package = required,
  version = vapply(required, function(pkg) as.character(packageVersion(pkg)), character(1))),
  "/srv/app/docker-dependency-versions.csv", row.names = FALSE)
message("[deps] All runtime dependencies loaded successfully in ",
  round(proc.time()[["elapsed"]] - started, 1), " seconds")
