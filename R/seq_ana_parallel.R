utils::globalVariables(c("group"))
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel stopImplicitCluster
#'
#' @title Parallelized Adaptive Sequential Estimation Method
#'
#' @description
#' Performs adaptive sequential estimation for generalized linear models under various sampling strategies.
#' This parallelized version evaluates candidate data points concurrently using multiple cores,
#' significantly accelerating computation for large datasets. The procedure begins with an initial subsample
#' and sequentially augments it based on a specified optimality criterion (e.g., D-optimality).
#' The process stops once the target estimation precision is achieved.
#'
#' This function is particularly useful when data are abundant but computational or labeling cost constraints
#' make full estimation impractical. Parallelism allows for substantial speed-ups in evaluating
#' candidate points at each step, especially in large-scale settings.
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
#' @param adaptive A \code{character} string specifying the sampling strategy. Only support for "random".
#' @param verbose Controls the level of console output.
#'   \describe{
#'     \item{0}{Silent mode (no output).}
#'     \item{1}{Minimal output.}
#'     \item{2}{Progress updates and ETA.}
#'     \item{3}{Detailed iteration logs and timing info.}
#'   }
#' @param max_try The maximum number of attempts allowed for selecting a valid initial subset (e.g., full-rank design matrix).
#' @param cores An \code{integer} specifying the number of CPU cores to use for parallel evaluation.
#'              If set to 1, computation proceeds sequentially.
#' @param backend A \code{character} string indicating which parallel backend to use.
#'                Options are \code{"none"}, \code{"doParallel"}, or \code{"doMC"}.
#'                When \code{cores > 1}, a valid backend must be specified and properly registered.
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
#' #' @details
#' Internally, the function uses the \pkg{foreach} framework to parallelize candidate evaluation steps.
#' When \code{cores > 1}, a valid parallel backend must be selected and initialized via the \code{backend} argument.
#' The function supports both \pkg{doParallel} and \pkg{doMC} (Unix-only) backends.
#' If \code{backend = "none"}, computation defaults to single-core execution using \code{foreach::registerDoSEQ()}.
#'
#' @seealso [seq_ana()]
#'
#' @export
seq_ana_parallel = function(data, interest, nuisance = NULL,
                            init_N,
                            gamma = 1, # weight of seq.
                            d1, # precision of theta
                            d2 = NULL, # precision of AUC
                            alpha = 0.05,
                            beta = NULL,
                            alpha2 = 0.05,
                            family = gaussian,
                            alternative = c("two.sided", "beta.protect"),
                            adaptive = c('random'),
                            verbose = 1, max_try = 1000,
                            cores = 1, backend = c("none", "doParallel", "doMC")){
  t_start <- Sys.time()

  adaptive = match.arg(adaptive)
  alternative = match.arg(alternative)
  backend <- match.arg(backend)

  if (cores > 1 && backend == "none") {
    stop("Requested multiple cores but did not register a parallel backend. Please set backend = 'doMC' or 'doParallel'.")
  }

  if(backend == "none"){
    if (verbose >= 2) message("Using backend: doSEQ with 1 cores.")
    foreach::registerDoSEQ()

  }else if (backend == "doMC") {
    if (.Platform$OS.type != "unix") {
      stop("doMC is only available on Unix-like systems.")
    }
    if (verbose >= 2) message("Using backend: doMC with ", cores, " cores.")
    doMC::registerDoMC(cores)

  } else if (backend == "doParallel") {
    if (verbose >= 2) message("Using backend: doParallel with ", cores, " cores.")
    doParallel::registerDoParallel(cores)

  } else {
    stop("Unsupported backend: ", backend)
  }

  if(alternative == "beta.protect" && is.null(beta)){
    stop("`beta` should be provided if `alternative` is 'beta.protect'.")
  }

  # Create data.matrix X, response y, interest params. index
  data_obj <- prepare_data(data, interest, nuisance, verbose = verbose)
  X <- data_obj$X
  y <- data_obj$y
  interest_term <- data_obj$interest_term

  # Get initial data (index) w/o collinearity
  ids <- initialize_selection(X, init_N, verbose = verbose, max_try = max_try)
  labeled_id <- ids$labeled_id
  unlabeled_id <- sample(ids$unlabeled_id)

  n <- length(unlabeled_id)
  index_groups <- split(1:n, (0:(n - 1)) %% cores)
  .combine_list <- function(x, y) append(x, list(y))

  results <- foreach(group = index_groups, .combine = .combine_list, .init = list()) %dopar% {
    coef_path <- list()
    for (j in group) {
      labeled_now <- c(labeled_id, unlabeled_id[1:j])
      Nj <- length(labeled_now)
      X_fit <- X[labeled_now,]
      y_fit <- y[labeled_now]
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

      if (stopped$is.stop){
        t_end <- Sys.time()

        out <- list(fit = fit, coef_path = as.data.frame(do.call(rbind, coef_path)),
                    Nj = Nj, mu = stopped$mu, Sigma = Sigma,
                    labeled_id = labeled_now, interest_term = interest_term, auc_fit = auc_fit,
                    time = difftime(t_end, t_start, units="secs"))
        class(out) <- "seq.fit"

        return(out)
      }
    }
    return(NA)
  }

  stopImplicitCluster()

  return(results)
}
