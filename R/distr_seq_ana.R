utils::globalVariables(c("j"))
#' @importFrom stats family
#' @importFrom pROC roc
#' @importFrom doRNG %dorng%
#' @importFrom parallel stopCluster
#'
#' @title Distributed Sequential Method
#'
#' @description
#' Performs distributed sequential estimation across multiple data sources.
#' Each data source is processed independently using a sequential design, and final estimates are aggregated
#' using federated weights. This approach is particularly suitable for large-scale or privacy-constrained settings
#' where data cannot be pooled centrally.
#'
#' @param data_list A \code{data_list} object. Each element must be a \code{data.frame} (dimension of data can be different). Used for distributed sequential design.
#' @param interest A \code{formula} specifying the variables of interest. These parameters are assumed to be common across datasets.
#' @param nuisance An optional \code{list} of \code{formula} objects, each corresponding to the nuisance parameters specific to one dataset. If \code{NULL}, only the interest terms are modeled.
#' @param init_N An \code{integer} specifying the initial sample size to be selected in each sequential procedure.
#' @param model Fitted model type. Current support \code{"lm"} and \code{"glm"}
#' @param fit_args Further arguments passed to \code{lm.fit} or \code{glm.fit}
#' @param gamma A numeric \code{vector} indicating the weights for aggregating estimates across sequences. If \code{NULL}, default weights will be assigned based on the chosen \code{alternative}.
#' @param d1 A positive \code{numeric} value specifying the required precision for the estimate of the interest parameter.
#' @param d2 An optional \code{numeric} specifying the required precision for the AUC (Area Under Curve). Used only if applicable.
#' @param alpha Type I error rate. Default is \code{0.05}.
#' @param beta Required only if \code{alternative = "beta.protect"}. Specifies the minimal effect size to be protected under Type II error constraint.
#' @param alpha2 Type I error level for the AUC-based stopping rule. Default is 0.05.
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
#' @param cores An \code{integer} specifying the number of CPU cores to use for parallel evaluation.
#'              If set to 1, computation proceeds sequentially.
#' @param backend A \code{character} string indicating which parallel backend to use.
#'                Options are \code{"none"}, \code{"doParallel"}, or \code{"doMC"}.
#'                When \code{cores > 1}, a valid backend must be specified and properly registered.
#'
#' @returns A \code{distr.seq.fit} object (list) with the following components:
#' \describe{
#'   \item{fits}{A list of \code{seq.fit} objects, one for each data source.}
#'   \item{N_star}{Total number of observations used across all sequences.}
#'   \item{Nj}{Vector of sample sizes used in each sequence.}
#'   \item{rho}{Relative weight (\code{Nj / N_star}) for each sequence.}
#'   \item{beta_est}{Aggregated estimate of the interest parameters based on federated weights.}
#'   \item{Sigma_est}{Aggregated information matrix of the interest parameters.}
#'   \item{auc_fit}{An object of class \code{"roc"} (from \pkg{pROC}), or \code{NULL} if AUC is not applicable.}
#'   \item{time_list}{Vector of computation times (per sequence).}
#'   \item{time_total}{Total elapsed time as a \code{difftime} object.}
#'   \item{alternative}{The hypothesis type used in sequential procedures.}
#'   \item{adaptive}{The sampling strategy applied to each sequence.}
#'   }
#'
#' @details
#' Internally, the function uses the \pkg{foreach} framework to parallelize candidate evaluation steps.
#' When \code{cores > 1}, a valid parallel backend must be selected and initialized via the \code{backend} argument.
#' The function supports both \pkg{doParallel} and \pkg{doMC} (Unix-only) backends.
#' If \code{backend = "none"}, computation defaults to single-core execution using \code{foreach::registerDoSEQ()}.
#'
#' @example inst/examples/example_distr_seq_est.R
#'
#' @seealso [seq_ana()]
#' @export
#'
distr_seq_ana = function(data_list,
                         interest, # formula of interest param.
                         nuisance = NULL, # list of formula of other param.
                         init_N,
                         model = c("lm","glm"),
                         fit_args = list(),
                         gamma = NULL, # weight of seq.
                         d1, # precision of theta
                         d2 = NULL, # precision of AUC
                         alpha = 0.05,
                         beta = NULL,
                         alpha2 = 0.05,
                         alternative = c("two.sided", "beta.protect"),
                         adaptive = c('random', 'D.opt', 'A.opt'),
                         verbose = 1,
                         max_try = 1000,
                         cores = 1,
                         backend = c("none", "doParallel", "doMC")){
  t_start <- Sys.time()

  # Check inputs
  adaptive <- match.arg(adaptive)
  alternative <- match.arg(alternative)
  backend <- match.arg(backend)
  M = length(data_list)

  if (!inherits(data_list, "data_list")) {
    stop("Input must be a `data_list` object generated by `gen_data_list()`.")
  }

  if(!is.null(nuisance)){
    if(length(nuisance) != M) stop("`nuisance` must be a list the same length as `data_list`.")
  }

  if(alternative == "beta.protect" && is.null(beta)){
    stop("`beta` should be provided if `alternative` is 'beta.protect'.")
  }

  # Adjust the weight of each seq.
  if(is.null(gamma)) gamma <- rep(1/M, M)
  if(alternative == "two.sided" && sum(gamma^2)!=1){
    gamma <- gamma / sum(gamma)
  }
  if(alternative == "beta.protect" && sum(1/(gamma^2))!=1){
    gamma <- gamma * sqrt(sum(1 / gamma^2))
  }

  if (cores > 1 && backend == "none") {
    stop("Requested multiple cores but did not register a parallel backend. Please set backend = 'doMC' or 'doParallel'.")
  }

  if(backend == "doMC" && .Platform$OS.type != "unix"){
    stop("DoMC is only available on Unix-like systems.")
  }

  if (backend == "doMC" && !requireNamespace("doMC", quietly = TRUE)) {
    stop("Package 'doMC' is required for backend = 'doMC'. Please install it.")
  }

  if (backend == "none"){
    message("Using backend: doSEQ with 1 cores.")
    foreach::registerDoSEQ()

  } else if (backend == "doMC") {
    message("Using doMC (Unix fork-based) with ", cores, " cores.")
    doMC::registerDoMC(cores)

  } else if (backend == "doParallel") {
    message("Using doParallel (socket-based) with ", cores, " cores.")
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    on.exit(stopCluster(cl), add = TRUE)

  } else{
    stop("Unsupported backend: ", backend)
  }

  # fit model for each sequence.
  .combine_list <- function(x, y) append(x, list(y))

  fits <- foreach(j = 1:M, .combine = .combine_list, .init = list(),
                  .export = c("seq_ana", "get_weight", "prepare_data",
                              "initialize_selection", "check_stopped",
                              "design_select_dispatch", "progress_controller"),
                  .packages = "DistrSeqEst",
                  .errorhandling = "pass") %dorng% {
    if (verbose >= 1) message(sprintf("Procedure %d start.", j))

    res <- seq_ana(data_list[[j]], interest = interest, nuisance = nuisance[[j]],
                   init_N = init_N, model = model, fit_args = fit_args, gamma = gamma[j],
                   d1 = d1, d2 = d2, alpha = alpha, beta = beta,
                   alternative = alternative, adaptive = adaptive,
                   verbose = verbose, max_try = max_try)

    if (verbose >= 1) message(sprintf("Procedure %d complete!\n", j))
    res
  }

  # federated estimation
  time_list <- sapply(fits, function(r) r$time)

  Nj_s <- sapply(fits, function(r) r$Nj)
  N_star <- sum(Nj_s)
  rho <- Nj_s / N_star

  # Calculate est. of beta (interest)
  interest_coefs <- lapply(fits, function(r) r$fit$coef[r$interest_term])
  beta_est <- mapply(function(rho, beta) rho * beta,
                     rho = rho, beta = interest_coefs, SIMPLIFY = F)
  beta_est <- Reduce('+', beta_est)

  # Calculate est. of Sigma
  interest_Sigma <- lapply(fits, function(r)  r$Sigma[r$interest_term,r$interest_term])
  Sigma_est <- mapply(function(rho, Sigma) rho * Sigma ,
                      rho = rho, Sigma = interest_Sigma, SIMPLIFY = F)
  Sigma_est <- Reduce('+', Sigma_est)

  # Compute total AUC
  auc_fit <- NULL
  if(!is.null(d2)){
    Y_true <- unlist(sapply(fits, function(r) r$auc_fit[["response"]]))
    Y_prob <- unlist(sapply(fits, function(r) r$auc_fit[["predictor"]]))
    auc_fit <- pROC::roc(Y_true, Y_prob, quiet=T)
  }

  t_end <- Sys.time()

  out <- list(fits = fits, N_star = N_star, Nj = Nj_s,
              rho = rho, beta_est = beta_est, Sigma_est = Sigma_est,
              auc_fit = auc_fit,
              time_list = time_list,
              time_total = difftime(t_end, t_start, units="secs"),
              alternative = alternative, adaptive = adaptive)
  class(out) <- "distr.seq.fit"

  return(out)
}
