% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distr_seq_ana.R
\name{distr_seq_ana}
\alias{distr_seq_ana}
\title{Distributed Sequential Method}
\usage{
distr_seq_ana(
  data_list,
  interest,
  nuisance = NULL,
  init_N,
  model = c("lm", "glm"),
  fit_args = list(),
  gamma = NULL,
  d1,
  d2 = NULL,
  alpha = 0.05,
  beta = NULL,
  alpha2 = 0.05,
  alternative = c("two.sided", "beta.protect"),
  adaptive = c("random", "D.opt", "A.opt"),
  verbose = 1,
  max_try = 1000,
  cores = 1,
  backend = c("none", "doParallel", "doMC")
)
}
\arguments{
\item{data_list}{A \code{data_list} object. Each element must be a \code{data.frame} (dimension of data can be different). Used for distributed sequential design.}

\item{interest}{A \code{formula} specifying the variables of interest. These parameters are assumed to be common across datasets.}

\item{nuisance}{An optional \code{list} of \code{formula} objects, each corresponding to the nuisance parameters specific to one dataset. If \code{NULL}, only the interest terms are modeled.}

\item{init_N}{An \code{integer} specifying the initial sample size to be selected in each sequential procedure.}

\item{model}{Fitted model type. Current support \code{"lm"} and \code{"glm"}}

\item{fit_args}{Further arguments passed to \code{lm.fit} or \code{glm.fit}}

\item{gamma}{A numeric \code{vector} indicating the weights for aggregating estimates across sequences. If \code{NULL}, default weights will be assigned based on the chosen \code{alternative}.}

\item{d1}{A positive \code{numeric} value specifying the required precision for the estimate of the interest parameter.}

\item{d2}{An optional \code{numeric} specifying the required precision for the AUC (Area Under Curve). Used only if applicable.}

\item{alpha}{Type I error rate. Default is \code{0.05}.}

\item{beta}{Required only if \code{alternative = "beta.protect"}. Specifies the minimal effect size to be protected under Type II error constraint.}

\item{alpha2}{Type I error level for the AUC-based stopping rule. Default is 0.05.}

\item{alternative}{A \code{character} string specifying the hypothesis framework. Options are:
\describe{
\item{"two.sided"}{Symmetric hypothesis testing.}
\item{"beta.protect"}{Protective design against a specific alternative effect size (\code{beta}).}
}}

\item{adaptive}{A \code{character} string specifying the sampling strategy. Must be one of:
\describe{
\item{"random"}{Uniform random sampling.}
\item{"D.opt"}{D-optimal design based on Fisher information.}
\item{"A.opt"}{A-optimal design minimizing trace of the inverse information matrix.}
}}

\item{verbose}{Controls the level of console output.
\describe{
\item{0}{Silent mode (no output).}
\item{1}{Minimal output.}
\item{2}{Progress updates and ETA.}
\item{3}{Detailed iteration logs and timing info.}
}}

\item{max_try}{The maximum number of attempts allowed for selecting a valid initial subset (e.g., full-rank design matrix).}

\item{cores}{An \code{integer} specifying the number of CPU cores to use for parallel evaluation.
If set to 1, computation proceeds sequentially.}

\item{backend}{A \code{character} string indicating which parallel backend to use.
Options are \code{"none"}, \code{"doParallel"}, or \code{"doMC"}.
When \code{cores > 1}, a valid backend must be specified and properly registered.}
}
\value{
A \code{distr.seq.fit} object (list) with the following components:
\describe{
\item{fits}{A list of \code{seq.fit} objects, one for each data source.}
\item{N_star}{Total number of observations used across all sequences.}
\item{Nj}{Vector of sample sizes used in each sequence.}
\item{rho}{Relative weight (\code{Nj / N_star}) for each sequence.}
\item{beta_est}{Aggregated estimate of the interest parameters based on federated weights.}
\item{Sigma_est}{Aggregated information matrix of the interest parameters.}
\item{auc_fit}{An object of class \code{"roc"} (from \pkg{pROC}), or \code{NULL} if AUC is not applicable.}
\item{time_list}{Vector of computation times (per sequence).}
\item{time_total}{Total elapsed time as a \code{difftime} object.}
\item{alternative}{The hypothesis type used in sequential procedures.}
\item{adaptive}{The sampling strategy applied to each sequence.}
}
}
\description{
Performs distributed sequential estimation across multiple data sources.
Each data source is processed independently using a sequential design, and final estimates are aggregated
using federated weights. This approach is particularly suitable for large-scale or privacy-constrained settings
where data cannot be pooled centrally.
}
\details{
Internally, the function uses the \pkg{foreach} framework to parallelize candidate evaluation steps.
When \code{cores > 1}, a valid parallel backend must be selected and initialized via the \code{backend} argument.
The function supports both \pkg{doParallel} and \pkg{doMC} (Unix-only) backends.
If \code{backend = "none"}, computation defaults to single-core execution using \code{foreach::registerDoSEQ()}.
}
\examples{
b1 = c(-2,2,1,1,0)
b2 = c(-2,2,1,1,0.5)
b3 = c(-2,2,1,1,0.5,0)
b4 = c(-1.5,2,1,1,0)
b5 = c(-2.5,2,1,1,1)

gen_data = function(N, b){
  p = length(b)
  X = cbind(rep(1,N), matrix(rnorm(n=N*(p-1)), ncol = p-1))
  Xb = X \%*\% b

  p1 = 1 / (1+exp(-Xb))
  Y = rbinom(n=N, size=1, prob=p1)

  data = data.frame(X,Y)
}

set.seed(1)

if(interactive()){
  df1 = gen_data(10000,b1)
  df2 = gen_data(10000,b2)
  df3 = gen_data(10000,b3)
  df4 = gen_data(10000,b4)
  df5 = gen_data(10000,b5)
  df_list = gen_data_list(df1,df2,df3,df4,df5)


  interest = Y ~ X1 + X2 + X3 -1
  # using . in RHS should give Y in LHS
  nuisance = list(Y ~ . -1,                     # All other param.
                  NULL,                         # No nuisance
                  ~ X4 + X5 + X6 -1,            # Different num. of param.
                  ~ I(X2^2) + X4:X5 -1,         # I(), interaction
                  ~ log(X3^2) + poly(X4,2) -1)  # log(), poly()
  gamma = rep(1/5,5) # weight of sequence

  fit_all = distr_seq_ana(df_list, interest = interest, nuisance = nuisance,
                          init_N = 100, gamma = gamma, d1 = 0.3, alpha = 0.05,
                          model = "glm", fit_args = list(family = binomial()),
                          alternative = "two.sided", adaptive = "A.opt",
                          verbose = 3, max_try = 1000)
}

}
\seealso{
\code{\link[=seq_ana]{seq_ana()}}
}
