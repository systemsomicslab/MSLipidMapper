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
  root <- tempfile("api-workers-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  script <- file.path(root, "worker.R")
  writeLines(c('dir <- commandArgs(TRUE)[1]',
    'writeLines("{}", file.path(dir, "result.json"))'), script)
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
