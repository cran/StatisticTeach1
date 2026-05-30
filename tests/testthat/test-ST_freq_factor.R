test_that("ST_freq_factor returns a data.frame with correct columns", {
  result <- ST_freq_factor(iris, Species)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("Levels", "Frequency", "Cum.Frequency", "Percent", "Cum.Percent"))
  expect_equal(nrow(result), 3L)
  expect_equal(sum(result$Frequency), 150L)
})

test_that("ST_freq_factor percentages sum to 100", {
  result <- ST_freq_factor(iris, Species)
  expect_equal(result$Cum.Percent[nrow(result)], 100)
})
