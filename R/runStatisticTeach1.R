#' Launch the StatisticTeach1 Shiny Application
#'
#' Opens the interactive Shiny interface for exploring descriptive
#' statistics and probability distributions.
#'
#' @param host The IPv4 address that the application should listen on.
#'   Defaults to the \code{shiny.host} option, if set, or
#'   \code{"127.0.0.1"} otherwise.
#' @param port The TCP port that the application should listen on.
#'   If \code{NULL} (the default) and the \code{shiny.port} option is
#'   set, that port will be used; otherwise a random available port is
#'   chosen.
#' @param launch.browser Logical.  If \code{TRUE} (the default in
#'   interactive sessions), the system default web browser is opened
#'   automatically after the app starts.  Can also be a function that
#'   receives the application URL.
#'
#' @return No return value, called for its side effect of launching a
#'   Shiny application.
#'
#' @examples
#' if (interactive()) {
#'   runStatisticTeach1()
#' }
#'
#' @import shiny
#' @import shinydashboard
#' @import dplyr
#' @import ggplot2
#' @import mixdist
#' @import descriptr
#' @import DescTools
#' @import RColorBrewer
#' @import tibble
#' @import tidyr
#' @importFrom shinyBS bsModal
#' @importFrom shinyjs useShinyjs disable enable
#' @importFrom shinyWidgets pickerInput actionBttn prettyCheckbox dropdown switchInput animateOptions
#' @importFrom readxl excel_sheets read_excel
#' @importFrom rlang syms sym
#' @importFrom magrittr %<>%
#' @importFrom colourpicker colourInput colourWidget
#' @importFrom DT renderDT datatable formatRound formatStyle styleEqual
#' @export
runStatisticTeach1 <- function(host          = getOption("shiny.host", "127.0.0.1"),
                                port          = NULL,
                                launch.browser = interactive()) {
  appDir <- system.file("SatisticTeach1App", package = "StatisticTeach1")
  if (appDir == "") {
    stop(
      "Could not find the StatisticTeach1 app directory. ",
      "Try re-installing the package with: install.packages('StatisticTeach1')",
      call. = FALSE
    )
  }

  shiny::runApp(
    appDir,
    display.mode  = "normal",
    launch.browser = launch.browser,
    port          = port,
    host          = host
  )
}
