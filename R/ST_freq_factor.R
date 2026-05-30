#' Frequency Table for Categorical Data
#'
#' Computes a frequency table for a categorical (factor) variable,
#' including absolute frequency, cumulative frequency, relative frequency
#' percentage, and cumulative relative frequency percentage.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param variable Unquoted name of the column (factor) in \code{data}.
#'
#' @return A \code{data.frame} with five columns: \code{Levels},
#'   \code{Frequency}, \code{Cum.Frequency}, \code{Percent}, and
#'   \code{Cum.Percent}.
#'
#' @examples
#' ST_freq_factor(iris, Species)
#'
#' @importFrom rlang enquo
#' @importFrom dplyr pull select
#' @importFrom stats na.omit
#' @export
ST_freq_factor <- function(data, variable) {

  varyable <- rlang::enquo(variable)

  fdata <-
    data |>
    dplyr::pull(!!varyable) |>
    stats::na.omit()

  var_name <-
    data |>
    dplyr::select(!!varyable) |>
    names()

  percent <- function(x, y) round((x / y) * 100, 2)

  level_names <- levels(fdata)
  data_len    <- length(fdata)
  result      <- as.vector(table(fdata))
  cum         <- cumsum(result)
  per         <- percent(result, data_len)
  cum_per     <- percent(cum, data_len)

  ftable <- data.frame(
    Levels         = level_names,
    Frequency      = result,
    Cum.Frequency  = cum,
    Percent        = per,
    Cum.Percent    = cum_per
  )

  return(ftable)
}
