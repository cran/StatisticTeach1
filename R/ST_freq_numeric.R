#' Frequency Table for Continuous Data
#'
#' Computes a frequency table for a continuous (numeric) variable by
#' dividing the data range into equal-width intervals (bins).
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param variable Unquoted name of the numeric column in \code{data}.
#' @param bins A positive integer giving the number of intervals into
#'   which the data range is divided. Defaults to \code{5}.
#'
#' @return A \code{data.frame} with seven columns: \code{lower},
#'   \code{upper}, \code{midpoint}, \code{frequency}, \code{cumulative},
#'   \code{freq_percent}, and \code{cum_percent}.
#'
#' @examples
#' ST_freq_numeric(mtcars, mpg, bins = 5)
#'
#' @importFrom rlang enquo
#' @importFrom dplyr pull select
#' @importFrom stats na.omit
#' @export
ST_freq_numeric <- function(data, variable, bins = 5) {

  varyable <- rlang::enquo(variable)

  fdata <-
    data |>
    dplyr::pull(!!varyable) |>
    stats::na.omit()

  if (!is.numeric(bins) || length(bins) != 1L || bins < 1) {
    stop("'bins' must be a single positive integer.", call. = FALSE)
  }
  bins <- as.integer(bins)

  bin_size <- function(x, b) (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) / b

  intervals <- function(x, b) {
    binsize  <- bin_size(x, b)
    interval <- min(x)
    for (i in seq_len(b - 1L)) {
      interval <- c(interval, interval[i] + binsize)
    }
    c(interval, max(x))
  }

  freq_count <- function(x, b, inta) {
    result <- numeric(b)
    for (i in seq_len(b)) {
      k <- i + 1L
      if (i < b) {
        result[i] <- sum(x >= inta[i] & x < inta[k])
      } else {
        result[i] <- sum(x >= inta[i] & x <= inta[k])
      }
    }
    result
  }

  percent <- function(x, y) round((x / y) * 100, 2)

  inta     <- intervals(fdata, bins)
  result   <- freq_count(fdata, bins, inta)
  data_len <- length(fdata)
  cum      <- cumsum(result)
  per      <- percent(result, data_len)
  cum_per  <- percent(cum, data_len)

  lower <- inta[-length(inta)]
  upper <- inta[-1L]

  data.frame(
    lower        = lower,
    upper        = upper,
    midpoint     = (lower + upper) / 2,
    frequency    = result,
    cumulative   = cum,
    freq_percent = per,
    cum_percent  = cum_per
  )
}
