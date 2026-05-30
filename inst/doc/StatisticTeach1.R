## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# install.packages("StatisticTeach1")

## ----eval=FALSE---------------------------------------------------------------
# library(StatisticTeach1)
# runStatisticTeach1()

## -----------------------------------------------------------------------------
library(StatisticTeach1)
ST_freq_factor(iris, Species)

## -----------------------------------------------------------------------------
ST_freq_numeric(mtcars, mpg, bins = 5)

