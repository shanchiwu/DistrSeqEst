#' @importFrom stats gaussian glm.fit coef
#' @importFrom pROC roc auc var
#' @import progress
#'
#' @title Sequential estimation Method
#'
#' @description
#' Performs adaptive sequential estimation for generalized linear models under various
#' sampling strategies. The procedure begins with an initial subsample and sequentially
#' selects additional data points based on a specified criterion (e.g., D-optimality).
#' The algorithm stops once the desired estimation precision is achieved.
#'
#' This function is particularly useful when data are abundant but computational or
#' labeling cost constraints make full estimation impractical.
#'
#' @param data A \code{data.frame} object.
#' @param interest A \code{formula} specifying the variables of interest. These parameters are assumed to be common across datasets.
#' @param nuisance An optional \code{list} of \code{formula} objects, each corresponding to the nuisance parameters specific to one dataset. If \code{NULL}, only the interest terms are modeled.
#' @param init_N An \code{integer} specifying the initial sample size to be selected in each sequential procedure.
#' @param gamma A numeric \code{vector} indicating the weights for aggregating estimates across sequences. If \code{NULL}, default weights will be assigned based on the chosen \code{alternative}.
#' @param d1 A positive \code{numeric} value specifying the required precision for the estimate of the interest parameter.
#' @param d2 An optional \code{numeric} specifying the required precision for the AUC (Area Under Curve). Used only if applicable.
#' @param alpha Type I error rate. Default is \code{0.05}.
#' @param beta Required only if \code{alternative = "beta.protect"}. Specifies the minimal effect size to be protected under Type II error constraint.
#' @param alpha2 Type I error level for the AUC-based stopping rule. Default is 0.05.
#' @param family A \code{family} object specifying the error distribution and link function, e.g., \code{binomial()}, \code{gaussian()}, etc.
#' @param alternative A \code{character} string specifying the hypothesis framework. Options are:
#'   \describe{
#'     \item{"two.sided"}{Symmetric hypothesis testing.}
#'     \item{"beta.protect"}{Protective design against a specific alternative effect size (\code{beta}).}
#'   }
#' @param adaptive A \code{character} string specifying the sampling strategy. Must be one of:
#'   \describe{
#'     \item{"random"}{Uniform random sampling.}
#'     \item{"D.opt"}{D-optimal design based on Fisher information.}
#'     \item{"A.opt"}{A-optimal design minimizing trace of the inverse information matrix.}
#'   }
#' @param verbose Controls the level of console output.
#'   \describe{
#'     \item{0}{Silent mode (no output).}
#'     \item{1}{Minimal output.}
#'     \item{2}{Progress updates and ETA.}
#'     \item{3}{Detailed iteration logs and timing info.}
#'   }
#' @param max_try The maximum number of attempts allowed for selecting a valid initial subset (e.g., full-rank design matrix).
#'
#' @returns A \code{seq.fit} object (list) with the following components:
#' \describe{
#'   \item{fit}{Final fitted model (from \code{glm.fit}).}
#'   \item{coef_path}{A data frame recording the sequence of coefficient estimates at each iteration.}
#'   \item{Nj}{Final sample size used.}
#'   \item{mu}{A numeric value representing the maximum eigenvalue of the (weighted) covariance matrix of interest terms, used for the stopping criterion under \code{"two.sided"} hypothesis.}
#'   \item{Sigma}{Fisher information matrix at stopping.}
#'   \item{labeled_id}{Indices of selected samples.}
#'   \item{interest_term}{Names of variables of interest.}
#'   \item{auc_fit}{An object of class \code{"roc"} (from \pkg{pROC}), or \code{NULL} if not applicable.}
#'   \item{time}{Computation time as a \code{difftime} object.}
#' }
#'
#' @example inst/examples/example_seq_ana.R
#'
#' @seealso [distr_seq_ana()]
#'
#' @export

seq_ana = function(data, interest, nuisance = NULL,
                   init_N,
                   gamma = 1, # weight of seq.
                   d1, # precision of theta
                   d2 = NULL, # precision of AUC
                   alpha = 0.05,
                   beta = NULL,
                   alpha2 = 0.05,
                   family = gaussian,
                   alternative = c("two.sided", "beta.protect"),
                   adaptive = c('random', 'D.opt', 'A.opt'),
                   verbose = 1, max_try = 1000){
  t_start <- Sys.time()

  adaptive = match.arg(adaptive)
  alternative = match.arg(alternative)

  if(alternative == "beta.protect" && is.null(beta)){
    stop("`beta` should be provided if `alternative` is 'beta.protect'.")
  }

  # Create progress bar if applicable
  pb <- progress_controller(type = "init", verbose = verbose, total = nrow(data), d2 = d2)
  if (!is.null(pb)) {
    replicate(init_N-1, progress_controller(type = "tick", pb_env = pb)) # tick initial N sample
  }

  # Create data.matrix X, response y, interest params. index
  data_obj <- prepare_data(data, interest, nuisance, verbose = verbose)
  X <- data_obj$X
  y <- data_obj$y
  interest_term <- data_obj$interest_term

  # Get initial data (index) w/o collinearity
  ids <- initialize_selection(X, init_N, verbose = verbose, max_try = max_try)
  labeled_id <- ids$labeled_id
  unlabeled_id <- ids$unlabeled_id

  tps_start <- Sys.time()
  coef_path <- list()

  best <- NULL
  repeat{
    Nj <- length(labeled_id)
    X_fit <- X[labeled_id,]
    y_fit <- y[labeled_id]
    fit <- glm.fit(x = X_fit, y = y_fit, family = family)

    # Calculate weight and Sigma matrix
    beta_est <- coef(fit)
    coef_path[[length(coef_path) + 1]] <- beta_est
    w <- fit$weights
    s <- sigma(fit)
    Sigma <- t(X_fit * w) %*% X_fit # pxn * nxp

    # Calculate AUC for logistic regression
    auc_fit <- NULL
    auc_var <- NULL
    if(!is.null(d2)){
      auc_fit <- pROC::roc(y_fit, fit$fitted.values, quiet=T)
      auc_hat <- pROC::auc(auc_fit)
      auc_var <- pROC::var(auc_fit)
    }

    # Check stopping criteria
    stopped <- check_stopped(Nj, init_N, interest_term, d1, d2, beta_est, Sigma, s,
                             gamma, alpha, beta, auc_var, alpha2, alternative = alternative)

    # Calculate time per sample
    tps_end <- Sys.time()
    tps <- as.numeric(tps_end - tps_start, units = "secs") / (Nj - init_N + 1)

    # Update progress bar
    if(alternative == "beta.protect"){
      st1_l <- sprintf("%.4f", stopped$st1)
      st1_r <- Nj
    }
    if(alternative == "two.sided"){
      st1_l <- sprintf("%.4f", stopped$mu)
      st1_r <- sprintf("%.4f", stopped$st1)
    }

    tokens <- list(tps_end = format(tps_end, "%Y-%m-%d %H:%M:%S"),
                   tps = sprintf("%.3f", tps), idx = sprintf("%8d", best),
                   st1_l = st1_l, st1_r = st1_r,
                   auc.var = sprintf("%.6f", auc_var),
                   st2 = sprintf("%.6f", stopped$st2))
    progress_controller(type = "tick", verbose = 3, pb_env = pb, tokens = tokens)

    if (stopped$is.stop) break

    w_all <- get_weight(X, beta_est, family)
    best <- switch(adaptive,
                   'random' = sample(unlabeled_id, 1),
                   'D.opt' = D_optimal(X, w_all, labeled_id, unlabeled_id),
                   'A.opt' = A_optimal(X, w_all, labeled_id, unlabeled_id),
                   stop("Unknown adaptive method"))

    if(identical(best, integer(0))){
      stop("All data has been exhausted before stopping criterion was met.")
    }

    labeled_id <- c(labeled_id, best)
    unlabeled_id <- unlabeled_id[unlabeled_id != best]
  }

  progress_controller(type = "done", pb_env = pb)
  t_end <- Sys.time()

  out <- list(fit = fit, coef_path = as.data.frame(do.call(rbind, coef_path)),
              Nj = Nj, mu = stopped$mu, Sigma = Sigma,
              labeled_id = labeled_id, interest_term = interest_term, auc_fit = auc_fit,
              time = difftime(t_end, t_start, units="secs"))
  class(out) <- "seq.fit"

  return(out)
}

