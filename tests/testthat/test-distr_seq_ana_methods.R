test_that("print and summary methods for distr.seq.fit work", {
  fit <- readRDS(test_path("fixtures/fit_distr_seq.rds"))

  expect_output(print(fit), "Object of class <distr.seq.fit>")
  expect_s3_class(summary(fit), "summary.distr.seq.fit")
  expect_output(print(summary(fit)), "Summary of Distributed Sequential Estimation")
})

test_that("plot_coef returns ggplot object", {
  fit <- readRDS(test_path("fixtures/fit_distr_seq.rds"))
  expect_invisible(plot_coef(fit, "X2"))
})
