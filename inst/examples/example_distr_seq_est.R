b1 = c(-2,2,1,1,0)
b2 = c(-2,2,1,1,0.5)
b3 = c(-2,2,1,1,0.5,0)
b4 = c(-1.5,2,1,1,0)
b5 = c(-2.5,2,1,1,1)

gen_data = function(N, b){
  p = length(b)
  X = cbind(rep(1,N), matrix(rnorm(n=N*(p-1)), ncol = p-1))
  Xb = X %*% b

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
                          family = binomial(),
                          alternative = "two.sided", adaptive = "A.opt",
                          verbose = 3, max_try = 1000)
}

