test_that("Multiple data.frames without split_col return data_list", {
  df1 <- data.frame(x = 1:3)
  df2 <- data.frame(y = 4:6)
  result <- gen_data_list(df1, df2)
  expect_s3_class(result, "data_list")
  expect_equal(length(result), 2)
  expect_equal(result[[1]], df1)
  expect_equal(result[[2]], df2)
})

test_that("Single data.frame with split_col splits correctly", {
  df <- data.frame(group = c("A", "B", "A"), value = 1:3)
  result <- gen_data_list(df, split_col = "group")
  expect_s3_class(result, "data_list")
  expect_equal(length(result), 2)
  expect_equal(result$A, df[c(1,3), "value", drop = FALSE])
  expect_equal(result$B, df[2, "value", drop = FALSE])
})

#-------- Error Case --------
test_that("Error when split_col is used with multiple data.frames", {
  df1 <- data.frame(x = 1:3)
  df2 <- data.frame(y = 4:6)
  expect_error(gen_data_list(df1, df2, split_col = "x"),
               "only one data.frame should be passed")
})

test_that("Error when split_col is not a single character string", {
  df <- data.frame(g = 1:3)
  expect_error(gen_data_list(df, split_col = 123), "`split_col` must be a character")
  expect_error(gen_data_list(df, split_col = c("g", "h")), "`split_col` must be a character")
})

test_that("Error when split_col column does not exist", {
  df <- data.frame(a = 1:3)
  expect_error(gen_data_list(df, split_col = "b"), "Column b not found in data")
})

test_that("Error when non-data.frame is passed without split_col", {
  df <- data.frame(x = 1:3)
  expect_error(gen_data_list(df, 1:3), "All arguments must be data.frames")
})
