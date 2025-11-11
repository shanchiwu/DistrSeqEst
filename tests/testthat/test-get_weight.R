#-------- Gaussian --------
test_that("get_weight agrees with glm.fit weights (gaussian)", {
  set.seed(1)
  X <- cbind(1, rnorm(100))
  beta <- c(0.5, -0.2)
  eta <- X %*% beta
  y <- eta + rnorm(100)  # gaussian error

  fam <- gaussian()
  fit <- glm.fit(x = X, y = y, family = fam)

  w1 <- fit$weights
  w2 <- as.vector(get_weight(X, fit$coefficients, fam))

  expect_equal(w2, w1, tolerance = 1e-5)
})

test_that("get_weight returns correct Gaussian working weights (identity link)", {
  set.seed(1)
  X <- cbind(1, rnorm(100))
  beta <- c(0.5, -0.2)
  eta <- X %*% beta

  # identity link: mu = eta, dmu/deta = 1, Var(mu) = 1 ⇒ weight = 1
  expected <- rep(1, length(eta))
  w <- as.vector(get_weight(X, beta, gaussian(link = "identity")))

  expect_equal(w, expected)
})



#-------- Binomial --------
test_that("get_weight agrees with glm.fit weights (binomial)", {
  set.seed(1)
  X <- cbind(1, rnorm(100))
  beta <- c(-1, 0.5)
  eta <- X %*% beta
  prob <- 1 / (1 + exp(-eta))
  y <- rbinom(100, size = 1, prob = prob)

  fam <- binomial()
  fit <- glm.fit(x = X, y = y, family = fam)

  w1 <- fit$weights
  w2 <- as.vector(get_weight(X, fit$coefficients, fam))

  expect_equal(w2, w1, tolerance = 1e-5)
})

test_that("get_weight returns correct Binomial working weights", {
  set.seed(123)
  X <- cbind(1, rnorm(100))
  beta <- c(-1, 0.5)
  eta <- X %*% beta
  mu <- 1 / (1 + exp(-eta))  # inverse logit

  expected <- as.vector(mu * (1 - mu))
  w <- as.vector(get_weight(X, beta, binomial()))

  expect_equal(w, expected)
})

#-------- Poisson --------
test_that("get_weight agrees with glm.fit weights (poisson)", {
  set.seed(1)
  X <- cbind(1, rnorm(100))
  beta <- c(0.3, 0.7)
  eta <- X %*% beta
  mu <- exp(eta)
  y <- rpois(100, lambda = mu)

  fam <- poisson()
  fit <- glm.fit(x = X, y = y, family = fam)

  w1 <- fit$weights
  w2 <- as.vector(get_weight(X, fit$coefficients, fam))

  expect_equal(w2, w1, tolerance = 1e-5)
})


test_that("get_weight returns correct Poisson working weights", {
  set.seed(123)
  X <- cbind(1, rnorm(100))
  beta <- c(0.5, 1)
  eta <- X %*% beta
  mu <- exp(eta)  # poisson default link is log

  expected <- as.vector(mu) # expect weight = mu
  w <- as.vector(get_weight(X, beta, poisson()))

  expect_equal(w, expected)
})

#-------- Gamma --------
test_that("get_weight agrees with glm.fit weights (Gamma, log link)", {
  set.seed(123)
  X <- cbind(1, runif(100))
  beta <- c(0.1, 0.3)
  eta <- X %*% beta
  mu <- exp(eta)
  y <- rgamma(100, shape = 2, scale = mu / 2)

  fam <- Gamma(link = "log")
  fit <- glm.fit(x = X, y = y, family = fam)

  w1 <- fit$weights
  w2 <- as.vector(get_weight(X, fit$coefficients, fam))

  expect_equal(w2, w1, tolerance = 1e-5)
})

test_that("get_weight returns correct Gamma working weights (log link)", {
  set.seed(1)
  X <- cbind(1, rnorm(100))
  beta <- c(0.2, -0.5)
  eta <- X %*% beta
  mu <- exp(eta)

  # Gamma with log link: weight = 1
  expected <- rep(1, length(mu))
  w <- as.vector(get_weight(X, beta, Gamma(link = "log")))

  expect_equal(w, expected)
})
