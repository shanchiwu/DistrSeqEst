b = c(-2,2,1,1,0)

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
  df = gen_data(10000,b)

  fit = seq_ana(df, interest = Y ~ X1 + X2 + X3 -1, nuisance = Y ~ . -1,
                init_N = 100, model = "glm", d1 = 0.3, alpha = 0.05,
                family = binomial(),
                alternative = "two.sided", adaptive = "D.opt",
                verbose = 3, max_try = 1000)
}

