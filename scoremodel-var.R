set.seed(0)
library(TMB)

# hacky way to create a virtual package
source("import.R")
import(from="util.R")


#######
### DATA

data <- c("teams", "rounds", "n", "m", "stats")
import(data, from="data.R")


# Phi <- matrix(c(.9,.1,-.2,.3),2,2)
# # Phi <- array(c(0.9,0.1,-0.2, 0.1,0.6,-0.1, -0.2,0.1,0.3), dim=c(3,3))

# theta <- util::eigen_decompose2D(Phi)$theta
# lambda <- util::eigen_decompose2D(Phi)$lambda
# Phi <- util::eigen_recompose2D(theta, lambda)
# Phi


#######
### PARAMETERS


n <- 16

rounds <- 2*(n-1)
matches <- n*(n-1)

A <- array(0.1, dim=c(rounds, n, 2))

mu <- sum(data::stats[T,5]) / (2*m)


Sigma <- matrix(c(1,0.2,0.2,2), ncol=2)
theta <- util::decompose_cov(Sigma)$theta
sds <- util::decompose_cov(Sigma)$sds

Phi <- matrix(c(.9,.1,-.2,.3),2,2)
eigtheta <- util::eigen_decompose2D(Phi)$theta
eiglambda <- util::eigen_decompose2D(Phi)$lambda


ztats = matrix(unlist(data::ztats), ncol=5)
stats = matrix(unlist(data::stats), ncol=5)

# Create datalist
data <- list(
    stats = ztats
)


# Define variables & parameters
parameters <- list(
    A     = A,
    gamma = 1,
    mu    = log(mu),
    theta = theta,
    log_sds = log(sds),
    eigtheta = c(0, pi/2),
    logit_eiglambda = qlogis((eiglambda+1)/2)
)


#######
### OBJECTIVE FUNCTION

MODEL = "var_tmb"

# Compile and link the template
# paste0 concatenates strings (without separator)
. <- TMB::compile(paste0("models-tmb/", MODEL, ".cpp")) # Only needed once
dyn.load(TMB::dynlib(paste0("models-tmb/", MODEL)))


# Make Automatic Differentiation Function
obj <- TMB::MakeADFun(data, parameters, random=c("A"), DLL=MODEL, silent=T)


# NonLinear MINimization subject to Box constraints
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/nlminb
# deprecated, but alternatives are worse:
# nlm(obj$fn, obj$par), optim(obj$par, obj$fn, obj$gr)
system.time(opt <- nlminb(obj$par, obj$fn, obj$gr))



#######
### RESULT

# https://www.rdocumentation.org/packages/TMB/versions/1.7.16/topics/sdreport
report <-TMB::sdreport(obj)

result <- util::unlog(obj$env$parList())

# Phi = util::eigen_recompose2D(result$eigtheta, result$eiglambda)


# result$cov <- recompose_cov(result$theta, result$sds)
# result$cov


# # Gets the result values matching the function signature + stats
values <- result[intersect(names(result), formalArgs(util::calc_VAR_ratings))]
values$stats <- stats


# Sort teams by points
points <- do.call(util::calc_VAR_points, values)

util::rankings(points, as.character(teams$name))
