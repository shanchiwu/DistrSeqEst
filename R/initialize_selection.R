initialize_selection <- function(X, init_N, max_try = 1000, verbose = 1) {
  n <- nrow(X)
  p <- ncol(X)
  index <- seq_len(n)

  for (i in seq_len(max_try)){
    labeled_id <- sample(index, size = init_N)
    X_init <- X[labeled_id, , drop = FALSE]

    # Check if X is full rank
    if (qr(X_init)$rank == p) {
      unlabeled_id <- setdiff(index, labeled_id)
      if (verbose >= 1) message("Initialization complete.")
      return(list(labeled_id = labeled_id, unlabeled_id = unlabeled_id))
    }
  }

  stop("Failed to find full-rank initial design matrix after ", max_try, " tries.")
}
