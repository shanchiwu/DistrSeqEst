#' @importFrom stats qchisq qnorm sigma
#' @title Evaluate Stopping Criterion in Sequential Design
#'
#' @description
#' This function checks whether the current sample size \eqn{k} satisfies the stopping criteria under two types of optimality conditions:
#' \describe{
#'   \item{Two-sided inference}{Based on the largest eigenvalue of the information matrix, used to control confidence region width for interest parameters.}
#'   \item{Beta-protect design}{Protects power against a specific alternative effect size.}
#' }
#' If `d2` is specified, an additional stopping rule is checked for precision on AUC estimates.
#'
#' @param model Fitted model type. Current support \code{"lm"} and \code{"glm"}
#' @param k Current total number of samples.
#' @param N0 Initial number of labeled samples.
#' @param interest_term A vector of indices indicating which components of the parameter vector \eqn{\theta} are of primary interest.
#' @param d1 Desired precision for estimating the interest parameters (interpreted as standard error).
#' @param d2 Optional. Desired standard error for the AUC estimate. If specified, a second stopping rule is applied.
#' @param theta Estimated parameter vector.
#' @param Sigma Estimated covariance matrix of \eqn{\theta}.
#' @param s Optional. Residual standard error (used in legacy version, currently not active).
#' @param gamma Scalar or vector weight for normalizing the design criterion.
#' @param alpha Type I error level for constructing confidence intervals.
#' @param beta Type II error constraint (only required if `alternative = "beta.protect"`).
#' @param auc_var Estimated variance of the AUC. Required only when `d2` is not `NULL`.
#' @param alpha2 Type I error level for the AUC-based stopping rule. Default is 0.05.
#' @param alternative Character string, either `"two.sided"` or `"beta.protect"`, determining the stopping rule.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{is.stop}{Logical value indicating whether the stopping criterion is satisfied.}
#'   \item{mu}{For two-sided inference, the computed maximal eigenvalue of the normalized information matrix.}
#'   \item{st1}{Threshold value for the first stopping rule.}
#'   \item{st2}{Threshold value for the AUC stopping rule (only if `d2` is provided).}
#' }
#'
#' @details
#' This function implements two types of stopping criteria commonly used in distributed sequential estimation:
#'
#' \strong{(1) Fixed-size confidence set (two-sided inference):}
#' The goal is to ensure that the confidence ellipsoid for the parameters of interest has a maximum axis length no greater than a prespecified value.
#' Based on Chen et al. (2024), this criterion monitors the largest eigenvalue of the projected covariance matrix \eqn{\mu_k = \lambda_{\max}\left(k \cdot \Sigma^{-1}_{\text{interest}}\right)}.
#' The procedure stops when the sample size \eqn{k} is large enough such that the maximum axis length of the confidence region satisfies the desired precision:
#' \deqn{
#' \mu_k \leq \frac{d_1^2 \cdot k}{\gamma \cdot \chi^2_{1 - \alpha}(p)}
#' }
#' where \eqn{p} is the number of interest parameters and \eqn{\gamma} is a weight adjustment factor (equal to 1 when only one sequence).
#'
#' \strong{(2) Beta-protection confidence set (open-ended design):}
#' Following the concept from Wijsman (1981) and extended by Wang & Chang (2017), this criterion aims to control the Type II error under a specific alternative effect size \eqn{\|\theta\|}.
#' The procedure stops when the accumulated information is sufficient to guarantee 1-\eqn{\beta} power for a one-sided test:
#' \deqn{
#' k \geq N_0 + \left( \frac{z_{\alpha} + \tilde{z}_{\beta}}{\gamma \cdot d_1 \cdot \|\theta\|} \right)^2
#' }
#' where \eqn{z_q} and \eqn{\tilde{z}_q} denotes the \eqn{(1-q)^{1/p}} and \eqn{(1-q^{1/p})} quantiles of the standard normal, respectively. They adjusted for multiple components (Bonferroni-like correction), and \eqn{\gamma} again serves as a scale adjustment.
#'
#' When a secondary AUC-based stopping criterion is specified via \code{d2}, the function evaluates whether the estimated AUC variance \code{auc_var} is below the required threshold:
#' \deqn{
#' \text{AUC variance} \leq \left( \frac{d_2}{z_{1-\alpha'/2}} \right)^2
#' }
#' The overall stopping rule requires both the primary and AUC criteria to be satisfied (logical AND).
#'
#' These rules are derived to ensure asymptotic validity (consistency and efficiency) of the resulting estimators under distributed, adaptive sequential procedures. See Chen et al. (2024, *Canadian Journal of Statistics*) and Wang & Chang (2017, *Biometrika*) for theoretical guarantees.
#'
#'
#' @references
#' Chen, Z., Wang, Z., & Chang, Y.-C. I. (2024). Distributed sequential estimation procedures. *Canadian Journal of Statistics*, 52(1), 271–290. https://doi.org/10.1002/cjs.11762
#'
#' Wang, Z., & Chang, Y.-C. I. (2017). Integration of distributed sequential procedures for massive-data analysis. *Biometrika*, 103(1), 1–21. https://doi.org/10.1093/biomet/asx028
#'
#' @examples
#' # Evaluate stopping rule for two-sided inference
#' Sigma <- diag(1, 3)
#' theta <- c(0.3, 0.2, 0.1)
#' check_stopped(model = "glm", k = 100, N0 = 20, interest_term = 1:2, d1 = 0.1,
#'               theta = theta, Sigma = Sigma, alternative = "two.sided", alpha = 0.05)
#' @export

check_stopped <- function(model, k, N0, interest_term, d1, d2 = NULL, theta, Sigma,
                          s, gamma = 1, alpha, beta = NULL, auc_var = NULL,
                          alpha2 = 0.05,
                          alternative = c("two.sided", "beta.protect")){
  p0 <- length(interest_term)
  mu <- NULL

  if(alternative == "two.sided"){
    Sigma_inv <- inv(Sigma)
    mu <- lam_max(k * Sigma_inv[interest_term,interest_term])

    a_tilde2 <- gamma * qchisq(1 - alpha, df = p0)

    st1 <- switch (model,
                   "lm"  = d1^2 * k / a_tilde2 / (s^2 + 1/k),
                   "glm" = d1^2 * k / a_tilde2,
                   stop("Unknown model")
    )
    cond1 <- mu <= st1
  }

  if(alternative == "beta.protect"){
    theta_hat <- sqrt(c(crossprod(theta, Sigma %*% theta))) # compute ||theta_hat*||
    tau <- 1 / (gamma*d1*(1e-7 + theta_hat))
    pivot <- qnorm((1-alpha)^(1/p0)) + qnorm(1-beta^(1/p0))

    st1 <- switch (model,
                   "lm"  = N0 + (pivot*tau)^2,
                   "glm" = (pivot*tau)^2,
                   stop("Unknown model")
    )
    cond1 <- k >= st1
  }
  is.stop <- cond1

  st2 <- NULL
  if(!is.null(d2)){
    st2 <- ( d2/qnorm(1-alpha2/2) )^2
    cond2 <- auc_var <= st2

    is.stop <- cond1 && cond2
  }

  out <- list(is.stop = is.stop, mu = mu, st1 = st1, st2 = st2)
}
