set.seed(0)
library(TMB)

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
    alpha = array(0, dim=teams),
    beta  = array(0, dim=teams),
    gamma = 1,
    mu    = log(mu),
    theta = theta,
    log_sds = log(sds)
)



#######
### OBJECTIVE FUNCTION

MODEL = "poisson_tmb"

# Compile and link the template
# paste0 concatenates strings (without separator)
. <- TMB::compile(paste0("models-tmb/", MODEL, ".cpp")) # Only needed once
dyn.load(TMB::dynlib(paste0("models-tmb/", MODEL)))



# Make Automatic Differentiation Function
obj <- TMB::MakeADFun(data, parameters, random=c("alpha", "beta"), DLL=MODEL, silent=TRUE)

# NonLinear MINimization subject to Box constraints
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/nlminb
system.time(opt <- nlminb(obj$par, obj$fn, obj$gr))



#######
### RESULT

# https://www.rdocumentation.org/packages/TMB/versions/1.7.16/topics/sdreport
report <-TMB::sdreport(obj)

result <- util::unlog(obj$env$parList())


# Get values matching the function signature
values <- result[formalArgs(util::calc_ratings)]


# Sort teams by score rating
ratings <- do.call(util::calc_ratings, values)
print(util::rankings(ratings, team$name))


# Sort teams by point system
points <- do.call(util::calc_points, values)
print(util::rankings(points, team$name))
