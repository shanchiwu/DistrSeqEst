#' @importFrom stats model.frame model.matrix model.response

prepare_data <- function(data, interest, nuisance = NULL, verbose = 0){
  mf_interest <- model.frame(interest, data)
  X_interest <- model.matrix(interest, mf_interest)
  has_intercept_interest <- "(Intercept)" %in% colnames(X_interest)
  y <- model.response(mf_interest)

  nuisance_cols <- NULL
  if (!is.null(nuisance)) {
    mf_nuisance <- model.frame(nuisance, data)
    X_nuisance <- model.matrix(nuisance, mf_nuisance)
    has_intercept_nuisance <- "(Intercept)" %in% colnames(X_nuisance)

    if (has_intercept_interest && has_intercept_nuisance) {
      stop("Both interest and nuisance contain an intercept term. Please remove one using '-1' in the formula.")
    }

    overlap_terms <- intersect(setdiff(colnames(X_interest), "(Intercept)"),
                               setdiff(colnames(X_nuisance), "(Intercept)"))
    if (verbose >=1 && length(overlap_terms) > 0) {
      message("Warning: The following terms appear in both interest and nuisance: ",
              paste(overlap_terms, collapse = ", "),
              ". These terms will be removed from the nuisance to avoid duplication.")

    }

    nuisance_cols <- setdiff(colnames(X_nuisance), colnames(X_interest))
    X_nuisance <- X_nuisance[, nuisance_cols, drop = FALSE]

    X <- cbind(X_interest, X_nuisance)
  } else {
    X <- X_interest
  }

  interest_term <- colnames(X_interest)
  interest_idx <- match(interest_term, colnames(X))

  if(verbose >= 3){
    message(paste0("Data Dimension: ", paste(dim(X), collapse = ", "), "\n",
                   "Interest terms: ", paste(interest_term, collapse = ", "), "\n",
                   "Nuisance terms: ", paste(nuisance_cols, collapse = ", ")))
  }

  return(list(X = X, y = y, interest_term = interest_term, interest_idx = interest_idx))
}
