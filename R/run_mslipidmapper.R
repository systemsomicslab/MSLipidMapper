#' Launch the MSLipidMapper Shiny application
#'
#' @param launch.browser logical. If TRUE, open browser.
#' @param host character. Shiny host.
#' @param port integer. Shiny port.
#' @param api_enable logical. If TRUE, enable inline API server.
#' @param api_host character. API host.
#' @param api_port integer. API port.
#' @param api_public_host character. Public API host embedded into URLs.
#' @param ... Additional arguments passed to [run_mslipidmapper_app()].
#'
#' @return Invisibly, the result of [shiny::runApp()].
#' @export
run_mslipidmapper <- function(
  launch.browser = interactive(),
  host = "127.0.0.1",
  port = getOption("shiny.port", 3838L),
  api_enable = TRUE,
  api_host = host,
  api_port = 7310L,
  api_public_host = host,
  ...
) {
  run_mslipidmapper_app(
    launch.browser = launch.browser,
    host = host,
    port = port,
    api_enable = api_enable,
    api_host = api_host,
    api_port = api_port,
    api_public_host = api_public_host,
    rules_yaml_path = .mslm_rules_yaml_path(),
    ...
  )
}
