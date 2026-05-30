#' Frequency table for continuous data
#' Frequency table for continuous data.
#' @param data A \code{data.frame} or a \code{tibble}.
#' @param variable Column in \code{data}.
#' @param bins Number of intervals into which the data must be split.
#' @return A \code{data.frame} with frequency, cumulative frequency, frequency percent and cumulative frequency
#' percent.
#' @examples
#' ST_freq_numeric(mtcars, mpg,bins = 5)
#' @export


ST_freq_numeric <- function(data, variable, bins = 5) {

  var_name <- deparse(substitute(variable))
  varyable <- rlang::enquo(variable)

  fdata <-
    data %>%
    dplyr::pull(!! varyable) %>%
    stats::na.omit()
  if (!is.numeric(bins)) {
    stop("bins must be integer value")
  }
  if (is.numeric(bins)) {
    bins <- as.integer(bins)
  }
  var_name <-
    data %>%
    dplyr::select(!! varyable) %>%
    names()

  bin_size <- function(data, bins) {
    return((max(data, na.rm = TRUE) - min(data, na.rm = TRUE)) / bins)
  }

  intervals <- function(data, bins, na.rm = TRUE) {
    binsize <- bin_size(data, bins)
    bin <- bins - 1
    interval <- min(data)
    for (i in seq_len(bin)) {
      out <- interval[i] + binsize
      interval <- c(interval, out)
    }
    interval <- c(interval, max(data))
    return(interval)
  }
  freq <- function(data, bins, inta) {
    result <- numeric(bins)
    for (i in seq_len(bins)) {
      k <- i + 1
      if (i < bins) {
        f <- data >= inta[i] & data < inta[k]
      } else {
        f <- data >= inta[i] & data <= inta[k]
      }
      result[i] <- sum(f)
    }
    return(result)
  }
  percent <- function(x, y) {
    out <- round((x / y) * 100, 2)
    return(out)}

  n_bins   <- bins
  inta     <- intervals(fdata, bins)
  result   <- freq(fdata, bins, inta)
  data_len <- length(fdata)
  cum      <- cumsum(result)
  per      <- percent(result, data_len)
  cum_per  <- percent(cum, data_len)

  na_count <-
    data %>%
    dplyr::pull(!! varyable) %>%
    is.na() %>%
    sum()

  if (na_count > 0) {
    na_freq <- na_count
  } else {
    na_freq <- 0
  }

  n_obs <-
    data %>%
    dplyr::pull(!! varyable) %>%
    length()

  lower_n <- n_bins + 1
  lower <- inta[-lower_n]
  upper <- inta[-1]

  freq_data <-
    data.frame(lower  = lower,
                   upper  = upper,
                   midpoint     = (lower+upper)/2,
                   frequency    = result,
                   cumulative   = cum,
                   freq_percent = per,
                   cum_percent  = cum_per)

  return(freq_data)
}
