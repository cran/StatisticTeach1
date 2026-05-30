# StatisticTeach1

**StatisticTeach1** is an R package that provides an interactive Shiny application for teaching and learning basic concepts in statistics and probability.

## Features

- Descriptive statistics for continuous variables (univariate and bivariate)
- Descriptive statistics for qualitative variables
- Multivariate analysis
- Discrete and continuous probability distributions
- Load and analyse your own data (CSV or Excel)

## Installation

```r
# Install from CRAN
install.packages("StatisticTeach1")

# Or install the development version from GitHub
# install.packages("remotes")
remotes::install_github("JavierDeLaHoz/StatisticTeach1")
```

## Usage

```r
library(StatisticTeach1)

# Launch the interactive app
runStatisticTeach1()

# Standalone frequency table functions
ST_freq_factor(iris, Species)
ST_freq_numeric(mtcars, mpg, bins = 5)
```

## License

GPL-3
