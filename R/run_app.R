#' Run the Shiny App
#'
#' This function launches the Shiny app.
#' @export
run_app <- function() {
  app_dir <- system.file("app", package = "ProgelApp")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `ProgelApp`.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
