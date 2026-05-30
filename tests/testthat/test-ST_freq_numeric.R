test_that("ST_freq_numeric returns a data.frame with correct columns", {
  result <- ST_freq_numeric(mtcars, mpg, bins = 5)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("lower", "upper", "midpoint", "frequency",
                          "cumulative", "freq_percent", "cum_percent"))
  expect_equal(nrow(result), 5L)
})

test_that("ST_freq_numeric frequencies sum to total observations", {
  result <- ST_freq_numeric(mtcars, mpg, bins = 5)
  expect_equal(sum(result$frequency), nrow(mtcars))
})

test_that("ST_freq_numeric errors on invalid bins", {
  expect_error(ST_freq_numeric(mtcars, mpg, bins = -1))
  expect_error(ST_freq_numeric(mtcars, mpg, bins = "five"))
})
