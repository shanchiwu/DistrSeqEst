# Example dataset
df <- data.frame(
  y = rbinom(10, 1, 0.5),
  x1 = rnorm(10),
  x2 = runif(10),
  x3 = rnorm(10),
  x4 = rnorm(10)
)

test_that("Basic interest-only formula works", {
  result <- prepare_data(df, interest = y ~ x1)
  expect_equal(ncol(result$X), 2) # Intercept + x1
  expect_equal(length(result$y), 10)
  expect_equal(result$interest_term, c("(Intercept)", "x1"))
  expect_equal(result$interest_idx, 1:2)
})

test_that("Interest and nuisance variables are combined properly", {
  result <- prepare_data(df, interest = y ~ x1, nuisance = ~ -1 + x2 + x3)
  expect_equal(ncol(result$X), 4) # (Intercept, x1, x2, x3)
  expect_true(all(c("x2", "x3") %in% colnames(result$X)))
})

test_that("Duplicated variables in both interest and nuisance are handled", {
  expect_message(
    result <- prepare_data(df, interest = y ~ x1 + x2 -1, nuisance = ~ x2 + x3, verbose = 1),
    "appear in both interest and nuisance"
  )
  expect_true("x3" %in% colnames(result$X))  # exclude x2
  expect_false(any(duplicated(colnames(result$X))))
})

test_that("Intercept conflict between interest and nuisance triggers error", {
  expect_error(
    prepare_data(df,
                 interest = y ~ x1 + 1,
                 nuisance = ~ x2 + 1),
    "Both interest and nuisance contain an intercept term"
  )
})

test_that("Works with no intercept in interest", {
  result <- prepare_data(df, interest = y ~ -1 + x1 + x2)
  expect_false("(Intercept)" %in% colnames(result$X))
  expect_equal(length(result$interest_term), 2)
})

test_that("Verbose output appears at level >= 3", {
  expect_message(
    prepare_data(df, interest = y ~ x1, nuisance = ~ -1 + x2, verbose = 3),
    "Data Dimension:"
  )
})

test_that("Nuisance: use dot (.) selects all except response", {
  result <- prepare_data(df, interest = y ~ x1 + x2 + x3 -1, nuisance = y ~ . -1, verbose = 0)
  expect_true(all(setdiff(names(df), "y") %in% colnames(result$X)))
})

test_that("Nuisance: interaction and I() terms are handled", {
  result <- prepare_data(df, interest = y ~ x1 + x2 + x3 -1, nuisance = ~ I(x2^2) + x2:x3 -1)
  expect_true(any(grepl("I\\(x2\\^2\\)", colnames(result$X))))
  expect_true(any(grepl("x2:x3", colnames(result$X))))
})

test_that("Nuisance: log() and poly() terms are supported", {
  result <- prepare_data(df, interest = y ~ x1 + x2 + x3 -1, nuisance = ~ log(x3^2) + poly(x4, 2) -1)
  expect_true(any(grepl("log\\(x3\\^2\\)", colnames(result$X))))
  expect_true(any(grepl("poly\\(x4", colnames(result$X))))
})
