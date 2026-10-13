test_that("empty tokens are accepted and invalid non-empty tokens are rejected", {
  expect_error(run_mslipidmapper_api(token = "", port = -1), "Port and resource limits")
  expect_error(run_mslipidmapper_api(token = "short"), "must be empty")
  expect_error(run_mslipidmapper_api(token = NA_character_), "must be empty")
  expect_error(run_mslipidmapper_api(token = "a-valid-long-test-token", port = -1), "Port and resource limits")
})

test_that("API configuration cannot select server paths or executables", {
  parse <- function(x, ext = "csv") .mslm_api_config(charToRaw(x), ext)
  expect_error(parse("pathway:\n  browser: /bin/sh"), "Unsupported API setting")
  expect_error(parse("pathway:\n  network: /etc/passwd"), "Unsupported API setting")
  expect_error(parse("analysis:\n  acyl_chain_filter:\n    rules: /etc/passwd"), "custom rules")
  expect_error(parse("input:\n  format: mztab-m"), "does not match")
  expect_error(parse("plot:\n  svg_width: 10000"), "between")
  expect_error(parse("- not\n- a mapping"), "mapping")
  cfg <- parse("input:\n  file: /etc/passwd\noutput:\n  directory: /tmp/other\n  pdf: /tmp/other.pdf")
  expect_null(cfg$input$file)
  expect_null(cfg$output$directory)
  expect_null(cfg$output$pdf)
  expect_identical(cfg$input$format, "msdial")
  expect_identical(parse("{}", "mztab")$input$format, "mztab-m")
})

test_that("JSON-LD output and subset options are validated", {
  parse <- function(x) .mslm_api_config(charToRaw(x), "mztab")
  expect_identical(parse("{}")$output$format, "pdf")
  expect_false(parse("output:\n  format: jsonld")$measurements$include_subsets)
  cfg <- parse('output:\n  format: both\nmeasurements:\n  include_subsets: true\n  acyl_chains: ["18:0", "24:1"]')
  expect_true(cfg$measurements$include_subsets)
  expect_equal(cfg$measurements$acyl_chains, c("18:0", "24:1"))
  expect_error(parse("output:\n  format: rds"), "output.format")
  expect_error(parse('measurements:\n  include_subsets: "true"'), "true or false")
  expect_error(parse('measurements:\n  include_subsets: true\n  acyl_chains: ["../file"]'), "chain strings")
  expect_error(parse('measurements:\n  acyl_chains: ["18:0"]'), "requires")
  expect_error(parse('measurements:\n  rules: /etc/passwd'), "Unsupported")
})

test_that("JSON-LD workers export whole subclasses and optional subsets without a browser", {
  root <- tempfile("jsonld-job-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  rows <- c("MTD\tmzTab-version\t2.0.0-M",
    "MTD\tassay[1]\tS1", "MTD\tassay[2]\tS2",
    "MTD\tstudy_variable[1]\tControl",
    "MTD\tstudy_variable[1]-assay_refs\tassay[1]",
    "MTD\tstudy_variable[1]-description\tControl",
    "MTD\tstudy_variable[2]\tTreatment",
    "MTD\tstudy_variable[2]-assay_refs\tassay[2]",
    "MTD\tstudy_variable[2]-description\tTreatment",
    "SMH\tSML_ID\tchemical_name\tchemical_formula\ttheoretical_neutral_mass\tabundance_assay[1]\tabundance_assay[2]\topt_global_Ontology",
    "SML\t1\tPC 16:0_18:1\tC42H82NO8P\t759.5778\t10\t20\tPC",
    "SML\t2\tPC 18:0_18:1\tC42H82NO8P\t759.5778\t5\t0\tPC")
  writeLines(rows, file.path(root, "input.mztab"))
  saveRDS(list(input = "input.mztab"), file.path(root, "request.rds"))
  cfg <- .mslm_api_config(charToRaw("input:\n  use_rgoslin_if_no_ontology: false\noutput:\n  format: jsonld"), "mztab")
  scopes <- function(doc) Filter(function(n) identical(n[["@type"]], "mslm:LipidMeasurementScope"), doc[["@graph"]])
  for (enabled in c(FALSE, TRUE)) {
    cfg$measurements$include_subsets <- enabled
    yaml::write_yaml(cfg, file.path(root, "config.yml"))
    .mslm_api_worker(root)
    result <- jsonlite::read_json(file.path(root, "result.json"))
    expect_equal(unlist(result$artifacts), c("remodeling-measurements.json", "ceramide-measurements.json", "global-measurements.json"))
    expect_identical(result$measurements$include_subsets, enabled)
    doc <- jsonlite::read_json(file.path(root, "artifacts", "remodeling-measurements.json"))
    scope_types <- vapply(scopes(doc), function(n) n$scopeType, character(1))
    expect_true("subclass" %in% scope_types)
    expect_identical("acyl_subset" %in% scope_types, enabled)
    if (enabled) expect_equal(unlist(result$measurements$pathways$remodeling$subset_chains), c("16:0", "18:0", "18:1"))
    expect_null(doc$elements)
  }
  cfg$measurements$acyl_chains <- "18:0"
  cfg$analysis$include_groups <- "Control"
  yaml::write_yaml(cfg, file.path(root, "config.yml"))
  .mslm_api_worker(root)
  result <- jsonlite::read_json(file.path(root, "result.json"))
  expect_equal(result$measurements$samples, 1L)
  expect_equal(unlist(result$measurements$pathways$remodeling$subset_chains), "18:0")
})

test_that("persistent queue is locked and restart preserves queued jobs", {
  root <- tempfile("api-manager-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  manager <- .mslm_api_manager(root, 30, 1, "unused-worker.R")
  on.exit(manager$close(), add = TRUE)
  # An incomplete upload must neither block the queue nor become runnable.
  dir.create(file.path(root, paste0("job_", paste(rep("0", 32), collapse = ""))))
  before <- list.dirs(root, recursive = FALSE)
  expect_error(manager$submit(stop("bad upload")), "bad upload")
  expect_identical(list.dirs(root, recursive = FALSE), before)
  probe <- file.path(root, "probe.R")
  writeLines(c('args <- commandArgs(TRUE)',
    'lock <- filelock::lock(args[1], timeout = 0)',
    'quit(status = if (is.null(lock)) 0L else 1L)'), probe)
  result <- processx::run(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", probe, file.path(root, "service.lock")),
    env = c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)),
    error_on_status = FALSE)
  expect_identical(result$status, 0L)
  upload <- list(input = charToRaw("data"), extension = "csv", config = list(input = list(format = "msdial")))
  id <- manager$submit(upload)
  expect_error(manager$submit(upload), "queue is full")
  expect_error(manager$lookup("../../etc/passwd"), "not found")
  dir <- manager$lookup(id)
  expect_identical(.mslm_api_state(dir)$status, "queued")
  manager$close()
  manager <- .mslm_api_manager(root, 30, 1, "unused-worker.R")
  expect_identical(.mslm_api_state(dir)$status, "queued")
  .mslm_api_record(dir, "running")
  manager$close()
  manager <- .mslm_api_manager(root, 30, 1, "unused-worker.R")
  expect_identical(.mslm_api_state(dir)$status, "failed")
})

test_that("scheduler progresses without HTTP polling and handles worker failures", {
  previous <- Sys.getenv("MSLM_API_WORKER_ENV_TEST", unset = NA_character_)
  Sys.setenv(MSLM_API_WORKER_ENV_TEST = "inherited")
  on.exit(if (is.na(previous)) Sys.unsetenv("MSLM_API_WORKER_ENV_TEST") else
    Sys.setenv(MSLM_API_WORKER_ENV_TEST = previous), add = TRUE)
  root <- tempfile("api-workers-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  script <- file.path(root, "worker.R")
  writeLines(c('dir <- commandArgs(TRUE)[1]',
    'writeLines(Sys.getenv("MSLM_API_WORKER_ENV_TEST"), file.path(dir, "result.json"))'), script)
  manager <- .mslm_api_manager(root, 10, 3, script)
  on.exit(manager$close(), add = TRUE)
  upload <- list(input = charToRaw("data"), extension = "csv", config = list())
  first <- manager$submit(upload)
  second <- manager$submit(upload)
  deadline <- Sys.time() + 15
  repeat {
    manager$tick()
    if (.mslm_api_state(manager$lookup(second))$status == "succeeded") break
    if (Sys.time() > deadline) stop("Test worker did not finish")
    Sys.sleep(0.1)
  }
  expect_identical(.mslm_api_state(manager$lookup(first))$status, "succeeded")
  expect_identical(readLines(file.path(manager$lookup(first), "result.json")), "inherited")
  writeLines('quit(status = 1)', script)
  failed <- manager$submit(upload)
  deadline <- Sys.time() + 10
  repeat {
    manager$tick()
    if (.mslm_api_state(manager$lookup(failed))$status == "failed") break
    if (Sys.time() > deadline) stop("Failure was not detected")
    Sys.sleep(0.1)
  }
  expect_identical(.mslm_api_state(manager$lookup(failed))$status, "failed")
})

test_that("worker timeout ends the job", {
  root <- tempfile("api-timeout-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  script <- file.path(root, "worker.R")
  writeLines('Sys.sleep(60)', script)
  manager <- .mslm_api_manager(root, 0.2, 1, script)
  on.exit(manager$close(), add = TRUE)
  id <- manager$submit(list(input = charToRaw("data"), extension = "csv", config = list()))
  manager$tick()
  Sys.sleep(0.4)
  manager$tick()
  expect_identical(.mslm_api_state(manager$lookup(id))$status, "failed")
  expect_match(.mslm_api_state(manager$lookup(id))$error, "timeout")
})
