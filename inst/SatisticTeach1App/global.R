# global.R — loaded once when the Shiny app starts
# Packages are loaded here so all app files (ui.R, server.R, pages/) share them.
# library() calls in inst/ Shiny apps are accepted by CRAN policy.

library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(rlang)
library(magrittr)
library(descriptr)
library(DescTools)
library(RColorBrewer)
library(readxl)
library(mixdist)
library(tibble)
library(tidyr)
library(colourpicker)
library(DT)

# ── Helper functions ──────────────────────────────────────────────────────────

kurtosis <- function(x) {
  m4 <- mean((x - mean(x))^4)
  m4 / (sd(x)^4) - 3
}

kurtosis_standardized <- function(x) {
  n  <- length(x)
  m4 <- mean((x - mean(x))^4)
  (m4 / (sd(x)^4)) - 3 * ((n - 1) / ((n - 2) * (n - 3)))
}

skewness <- function(x) {
  m3 <- mean((x - mean(x))^3)
  m3 / (sd(x)^3)
}

skewness_standardized <- function(x) {
  n   <- length(x)
  m3  <- mean((x - mean(x))^3)
  s3  <- sd(x)^3
  (m3 / s3) * sqrt(n * (n - 1)) / (n - 2)
}

geometric.mean <- function(x, na.rm = TRUE) {
  if (is.null(nrow(x))) {
    exp(mean(log(x), na.rm = na.rm))
  } else {
    exp(apply(log(x), 2, mean, na.rm = na.rm))
  }
}

bsModalNoClose <- function(...) {
  b <- shinyBS::bsModal(...)
  b[[2]]$`data-backdrop` <- "static"
  b[[2]]$`data-keyboard` <- "false"
  b
}

# ── ggplot themes and aesthetic options ───────────────────────────────────────

themesb <- list(
  "grey"     = ggplot2::theme_grey(),
  "minimal"  = ggplot2::theme_minimal(),
  "bw"       = ggplot2::theme_bw(),
  "classic"  = ggplot2::theme_classic(),
  "linedraw" = ggplot2::theme_linedraw(),
  "light"    = ggplot2::theme_light(),
  "dark"     = ggplot2::theme_dark(),
  "test"     = ggplot2::theme_test(),
  "void"     = ggplot2::theme_void()
)

fuentes <- list(
  "mono"      = "mono",
  "sans"      = "sans",
  "serif"     = "serif",
  "Courier"   = "Courier",
  "Helvetica" = "Helvetica",
  "Times"     = "Times",
  "Palatino"  = "Palatino"
)

paletas <- c(
  "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2",
  "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys",
  "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples",
  "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd",
  "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu",
  "RdYlGn", "Spectral", "Set3"
)

# ── Multivariate global measures ─────────────────────────────────────────────

Multiva <- function(datos) {
  n <- nrow(datos)
  p <- ncol(datos)
  vector.uno    <- matrix(rep(1, n), ncol = 1)
  vector.media  <- (t(datos) %*% vector.uno) / n
  S             <- cov(datos) * (n - 1) / n
  DN            <- diag(1 / sqrt(diag(S)))
  VT <- sum(diag(S))
  VM <- VT / p
  VG <- det(S)
  VE <- VG^(1 / p)
  cat("----------------------------------------------------------------\n")
  cat("-Global measures of variability\n",
      "1.- Total variance =", VT, "\n",
      "2.- Mean variance =", VM, "\n",
      "3.- Generalized Variance =", VG, "\n",
      "4.- Effective Variance =", VE, "\n")
  cat("----------------------------------------------------------------\n")
}

# ── Quantile helper for probability distributions ────────────────────────────

valor_x_lower <- function(distr, prob, df = NULL, param1 = NULL, param2 = NULL) {
  switch(distr,
    "Normal"      = qnorm(prob),
    "t-Student"   = qt(prob, df = df, lower.tail = TRUE),
    "Chi^2"       = qchisq(prob, df = df, lower.tail = TRUE),
    "F-Fisher"    = qf(prob, df1 = param1, df2 = param2, lower.tail = TRUE),
    "Exponential" = qexp(prob, rate = param1, lower.tail = TRUE),
    "Logistics"   = qlogis(prob, location = param1, scale = param2, lower.tail = TRUE),
    "Log-Normal"  = qlnorm(prob, meanlog = param1, sdlog = param2, lower.tail = TRUE),
    "Gamma"       = qgamma(prob, shape = param1, rate = param2, lower.tail = TRUE),
    "Weibull"     = qweibull(prob, shape = param1, scale = param2, lower.tail = TRUE),
    "Beta"        = qbeta(prob, shape1 = param1, shape2 = param2, lower.tail = TRUE),
    stop("Distribution not supported.", call. = FALSE)
  )
}
