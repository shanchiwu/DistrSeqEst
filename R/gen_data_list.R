#' @title Data List Generator
#'
#' @description
#' Generate a standardized \code{data_list} object from multiple data frames or by splitting a single data frame according to a specified column.
#' This is typically used as input for distributed or sequential procedures where data are organized by group or source.
#'
#' @param ... One or more \code{data.frame} objects. If \code{split_col} is not specified, all objects must be data frames and will be directly combined into a list.
#' @param split_col Optional. A single character string specifying the column name used to split a single data frame. When provided, only one data frame should be passed through \code{...}.
#'
#' @returns A \code{data_list} object, which is a named or unnamed list of \code{data.frame} objects. When \code{split_col} is used, the list will be named according to the levels of the split factor.
#'
#' @details
#' - If \code{split_col} is provided, the function splits the input data frame by the specified column, removes that column, and returns a list of sub-data frames.
#' - If \code{split_col} is omitted, multiple data frames can be provided, which are directly collected into a list.
#' - The resulting object has class \code{"data_list"}, which may be used for downstream methods.
#'
#' @examples
#' # Example 1: Splitting a single data frame by a column
#' df <- data.frame(group = rep(c("A", "B"), each = 3), x = 1:6)
#' dl <- gen_data_list(df, split_col = "group")
#'
#' # Example 2: Providing multiple data frames directly
#' df1 <- data.frame(x = 1:3)
#' df2 <- data.frame(x = 4:6)
#' df3 <- data.frame(x = 7:9, y = 10:12)
#' dl2 <- gen_data_list(df1, df2, df3)
#'
#' @export


gen_data_list <- function(..., split_col = NULL) {
  args <- list(...)

  if (!is.null(split_col)) {
    if (length(args) != 1) {
      stop("When `split_col` is provided, only one data.frame should be passed.")
    }
    data <- args[[1]]
    if (!is.data.frame(data)) stop("Input must be a data.frame.")
    if (!is.character(split_col) || length(split_col) != 1) {
      stop("`split_col` must be a character.")
    }
    if (!(split_col %in% names(data))) {
      stop(paste("Column", split_col, "not found in data"))
    }

    split_vec <- as.factor(data[[split_col]])
    data_nosplit <- data
    data_nosplit[[split_col]] <- NULL

    split_list <- split(seq_len(nrow(data_nosplit)), split_vec)
    data_list <- lapply(split_list, function(idx) {
      data_nosplit[idx, , drop = FALSE]
    })

  } else {
    if (!all(sapply(args, is.data.frame))) {
      stop("All arguments must be data.frames when `split_col` is not used.")
    }
    data_list <- args
  }

  class(data_list) <- "data_list"
  return(data_list)
}
