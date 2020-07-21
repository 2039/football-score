set.seed(0)
library(tmbstan)

# hacky way to create a virtual package
source("import.R")
import(from="util.R")


#######
### DATA

data <- c("teams", "matches", "team", "home_score", "away_score", "mu")
import(data, from="data.R")



#######
### PARAMETERS

Sigma <- matrix(c(1,0.2,0.2,2), ncol=2)

theta <- util::decompose_cov(Sigma)$theta
sds <- util::decompose_cov(Sigma)$sds


# Create datalist
data <- list(
    HOME = home_score,
    AWAY = away_score
)


# Define variables & parameters
parameters <- list(
    alpha = rep(0, teams),
    beta  = rep(0, teams),
    gamma = array(1, dim=teams),
    mu    = array(log(mu), dim=teams),
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


result <- util::unlog(obj$env$parList())


# Gets the values matching the function signature
values <- result[formalArgs(util::calc_ratings)]


# Sort teams by score rating
ratings <- do.call(util::calc_ratings, values)
print(util::rankings(ratings, team$name))


# Sort teams by point system
points <- do.call(util::calc_points, values)
print(util::rankings(points, team$name))
