## code to prepare `DATASET` dataset goes here

library(INLA)

n <- 1000
# Covariate without error:
z <- rnorm(n, mean = 0, sd = 1)
# Berkson error:
u_b <- rnorm(n)
w_b <- rnorm(n, mean = 1 + 2*z, sd = 1)
x <- w_b + u_b
# Response:
y <- 1 + 2*x + 2*z + rnorm(n)
# Classical error:
u_c <- rnorm(n)
w_c <- x + u_c
# Missingness:
m_pred <- -1.5 - 0.5*z # This gives a mean probability of missing of ca 0.2.
m_prob <- exp(m_pred)/(1 + exp(m_pred))
m_index <- rbinom(n, 1, prob = m_prob) # MAR
# m_index <- sample(1:n, 0.2*n, replace = FALSE) # MCAR
w_c[m_index] <- NA

simulated_data <- data.frame(y = y, w = w_c, z = z)

attach(simulated_data)
n <- nrow(simulated_data)


# Priors for model of interest coefficients
prior.beta = c(0, 1/1000) # N(0, 10^3)
# Priors for exposure model coefficients
prior.alpha <- c(0, 1/10000) # N(0, 10^4)
# Priors for measurement error variance and true x-value
prior.prec.u_b <- c(0.5, 0.5) # Gamma(0.5, 0.5)
prior.prec.u_c <- c(0.5, 0.5) # Gamma(0.5, 0.5)
prior.prec.x <- c(0.5, 0.5) # Gamma(0.5, 0.5)
# Initial values
prec.u_b <- 1
prec.u_c <- 1
prec.x <- 1

# OBS: double check these, they are not the same as the latest version of the script file



Y <- matrix(NA, 3*n, 3)

Y[1:n, 1] <- y               # Regression model of interest response
Y[n+(1:n), 2] <- w           # Error model response
Y[2*n+(1:n), 3] <- rep(0, n) # Exposure model response

beta.0 <- c(rep(1, n), rep(NA, 2*n))
beta.x <- c(1:n, rep(NA, n), rep(NA, n))
u.b.tilde <- c(1:n, rep(NA, n), rep(NA, n))
beta.z <- c(z, rep(NA, 2*n))

id.r <- c(rep(NA, n), 1:n, 1:n)
weight.r <- c(rep(1, n), rep(1, n), rep(-1, n))

alpha.0 = c(rep(NA, n), rep(NA, n), rep(1, n))
alpha.z = c(rep(NA, n), rep(NA, n), z)

dd <- data.frame(Y = Y,
                 beta.0 = beta.0,
                 beta.x = beta.x,
                 u.b.tilde = u.b.tilde,
                 beta.z = beta.z,
                 id.r = id.r,
                 weight.r = weight.r,
                 alpha.0 = alpha.0,
                 alpha.z = alpha.z)


formula = Y ~ beta.0 - 1 +
  f(beta.x, copy="id.r",
    hyper = list(beta = list(param = prior.beta, fixed=FALSE))) +
  f(id.r, weight.r, model="iid", values = 1:n,
    hyper = list(prec = list(initial = -15, fixed=TRUE))) +
  f(u.b.tilde, model = "iid", values = 1:n,
    hyper = list(prec = list(initial = log(1), fixed=TRUE))) +
  beta.z + alpha.0 + alpha.z


model_simulation <- inla(formula, data = dd, scale = scale.vec,
               family = c("gaussian", "gaussian", "gaussian"),
               control.family = list(
                 list(hyper = list(prec = list(initial = log(1),
                                               param = c(0.5, 0.5),
                                               fixed = FALSE))),
                 list(hyper = list(prec = list(initial = log(prec.u_c),
                                               param = prior.prec.u_c,
                                               fixed = TRUE))),
                 list(hyper = list(prec = list(initial = log(prec.x),
                                               param = prior.prec.x,
                                               fixed = FALSE)))
               ),
               control.predictor = list(compute = TRUE),
               control.fixed = list(
                 mean = list(beta.0 = prior.beta[1],
                             beta.z = prior.beta[1],
                             alpha.0 = prior.alpha[1],
                             alpha.z = prior.alpha[1]),
                 prec = list(beta.0 = prior.beta[2],
                             beta.z = prior.beta[2],
                             alpha.0 = prior.alpha[2],
                             alpha.z = prior.alpha[2]))
)

usethis::use_data(model_simulation, overwrite = TRUE)
