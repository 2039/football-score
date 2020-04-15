set.seed(0)
library(tmbstan)

# hacky way to create a virtual package
source("import.R")
import(from="util.R")


#######
### DATA

data <- c("teams","n", "m", "home_scores", "away_scores")
import(data, from="data.R")



#######
### PARAMETERS

mu <- sum(home_scores + away_scores, na.rm=TRUE) / (2*m)

Sigma <- matrix(c(1,0.2,0.2,2), ncol=2)

theta <- util::decompose_cov(Sigma)$theta
sds <- util::decompose_cov(Sigma)$sds


# Create datalist
data <- list(
    HOME = home_scores,
    AWAY = away_scores
)


# Define variables & parameters
parameters <- list(
    alpha = rep(0, n),
    beta  = rep(0, n),
    gamma = 1,
    mu    = log(mu),
    theta = theta,
    log_sds = log(sds)
)



#######
### OBJECTIVE FUNCTION

# Compile and link the template
. <- TMB::compile("models-tmb/poisson_tmb.cpp") # Only needed once
dyn.load(TMB::dynlib("models-tmb/poisson_tmb"))


# Make Automatic Differentiation Function
obj <- TMB::MakeADFun(data, parameters, random=c("alpha", "beta"), DLL="poisson_tmb", silent=TRUE)

cores <- parallel::detectCores()-1
options(mc.cores = cores)

# https://cran.r-project.org/web/packages/tmbstan/tmbstan.pdf
system.time(opt <- tmbstan(obj, laplace=TRUE))



#######
### RESULT

# https://www.rdocumentation.org/packages/TMB/versions/1.7.16/topics/sdreport
report <-TMB::sdreport(obj)


# :: NOTE
# There are a lot of ways to obtain the results; the documentation
# is not clear on which is appropriate.

# opt$par == rep$par.fixed
# obj$report()

# obj$env$last.par
# obj$env$last.par.best
# obj$env$random

# attributes(rep)
# attributes(obj[names(obj) == "report"])

# unlog(groupby(report$par.fixed))
# groupby(report$par.random)

result <- util::unlog(obj$env$parList())

# result$cov <- recompose_cov(result$theta, result$sds)
# result$cov


# Gets the values matching the function signature
values <- result[formalArgs(util::calc_ratings)]


# Sort teams by score rating
ratings <- do.call(util::calc_ratings, values)
util::rankings(ratings, teams$name)


# Sort teams by point system
points <- do.call(util::calc_points, values)
util::rankings(points, teams$name)
