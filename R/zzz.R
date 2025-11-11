#' @importFrom Rdpack reprompt
#' @keywords internal
#' @noRd
.invisible_use_Rdpack <- function() {
  # use a symbol so that Imports is justified, but never executed
  if (FALSE) Rdpack::reprompt()  # not run in checks; only to satisfy namespace usage
  invisible(NULL)
}
