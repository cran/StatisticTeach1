#' Shiny GUI for StatisticTeach1 package
#'@import shiny
#'@import shinydashboard
#'@importFrom shinyBS bsModal
#'@importFrom shinyjs useShinyjs
#'@importFrom shinyjs disable
#'@importFrom shinyjs enable
#'@importFrom shinyWidgets pickerInput
#'@importFrom shinyWidgets actionBttn
#'@importFrom shinyWidgets prettyCheckbox
#'@importFrom shinyWidgets dropdown
#'@importFrom shinyWidgets switchInput
#'@importFrom shinyWidgets animateOptions
#'@importFrom readxl excel_sheets
#'@importFrom readxl read_excel
#'@import dplyr
#'@import RColorBrewer
#'@importFrom rlang syms
#'@importFrom rlang sym
#'@import ggplot2
#'@import mixdist
#'@import descriptr
#'@import DescTools
#'@import tibble
#'@import tidyr
#'@importFrom colourpicker colourInput updateColourInput
#'@importFrom magrittr %<>%
#'@description runStatisticTeach1() loads interactive user interface built using R 'shiny'.
#'@details The interactive user interface to provide an easy way for to perform descriptive statistical analysis
#' @param launch.browser If true,
#' the system's default web browser will be launched automatically
#' after the app is started. Defaults to true in interactive sessions
#' only. This value of this parameter can also be a function to call with
#' the application's URL.
#' @param port is the TCP port that the application should listen on.
#' If the port is not specified,
#' and the shiny.port option is set (with options(shiny.port = XX)),
#' then that port will be used.
#' Otherwise, use a random port.
#' @param host The IPv4 address that the application should listen on.
#' Defaults to the shiny.host option, if set, or "127.0.0.1" if not.
#' @return No return value
#' @examples
#' if(interactive()){
#' runStatisticTeach1()
#' }
#' @export


runStatisticTeach1 <- function(host = "127.0.0.1",
                                port = NULL,launch.browser = TRUE) {
  appDir <- system.file("StatisticTeach1App", package = "StatisticTeach1")
  if (appDir == "") {
    stop("Could not find StatisticTeach1.
         Try re-installing `StatisticTeach1`.", call. = FALSE)
  }

  shiny::runApp(appDir,
                display.mode = "normal",
                launch.browser = launch.browser,
                port = port,
                host = getOption("shiny.host", host)
  )
}
