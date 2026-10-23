# Base-R regression test for the actual session initialization expressions.
app <- parse("R/run_mslipidmapper_app.R")[[1]][[3]]
body <- as.list(app[[3]])
is_server <- vapply(body, function(x) is.call(x) && identical(x[[1]], as.name("<-")) && identical(x[[2]], as.name("server")), logical(1))
server <- body[[which(is_server)]] [[3]]
init <- as.list(server[[3]])[2:5]
image_root <- tempfile("session-test-")
dir.create(image_root)
make_session <- function() {
  env <- new.env(parent = globalenv())
  env$image_root <- image_root
  env$api_port <- 7310L
  env$session <- new.env()
  env$session$userData <- new.env()
  env$session$onSessionEnded <- function(callback) env$cleanup <- callback
  for (expr in init) eval(expr, env)
  env
}
a <- make_session()
b <- make_session()
stopifnot(a$image_dir != b$image_dir,
          a$session$userData$mslm_images$prefix != b$session$userData$mslm_images$prefix)
writeLines("A", file.path(a$image_dir, "class_PC.svg"))
writeLines("B", file.path(b$image_dir, "class_PC.svg"))
stopifnot(readLines(file.path(a$image_dir, "class_PC.svg")) == "A")
a$cleanup()
stopifnot(!dir.exists(a$image_dir), dir.exists(b$image_dir),
          readLines(file.path(b$image_dir, "class_PC.svg")) == "B")
b$cleanup()
unlink(image_root, recursive = TRUE)
cat("PASS: independent images and cleanup for two sessions\n")