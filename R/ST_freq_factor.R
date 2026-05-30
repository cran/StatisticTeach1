#' Frequency table for categorical data
#' Frequency table for categorical data.
#' @param data A \code{data.frame} or a \code{tibble}.
#' @param variable Column (factor) in \code{data}.
#' @return A \code{data.frame} with frequency, cumulative frequency, frequency percent and cumulative frequency
#' percent.
#' @examples
#' ST_freq_factor(iris,Species)
#' @export

ST_freq_factor <- function(data, variable) {

  var_name <- deparse(substitute(variable))
  varyable <- rlang::enquo(variable)
  fdata <-
    data %>%
    dplyr::pull(!! varyable) %>%
    stats::na.omit()

  var_name <-
    data %>%
    dplyr::select(!! varyable) %>%
    names()

  cq <-
    fdata %>%
    sort() %>%
    unique()

  result <-
    fdata %>%
    table() %>%
    as.vector()

  percent <- function(x, y) {
    out <- round((x / y) * 100, 2)
    return(out)}

  level_names <- levels(fdata)
  data_len    <- length(fdata)
  len         <- length(result)
  cum         <- cumsum(result)
  per         <- percent(result, data_len)
  cum_per     <- percent(cum, data_len)

  ftable <- data.frame(
    Levels          = level_names,
    Frequency       = result,
    `Cum Frequency` = cum,
    Percent         = per,
    `Cum Percent`   = cum_per
  )

  na_count <-
    data %>%
    dplyr::pull(!! varyable) %>%
    is.na() %>%
    sum()

  n_obs <-
    data %>%
    dplyr::pull(!! varyable) %>%
    length()

  if (na_count > 0) {
    na_data <- dplyr::pull(data, !! varyable)

    var_count <-
      na_data %>%
      table() %>%
      as.vector() %>%
      sum()

    na_freq <- n_obs - var_count
  } else {
    na_freq <- 0
  }
  return(ftable )
}
