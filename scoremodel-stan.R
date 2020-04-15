set.seed(0)
library(rstan)

# base::options(mc.cores = parallel::detectCores())


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

Sigma <- matrix(c(1,0.2,0.2,1), ncol=2)
Sigma <- diag(2)
#Sigma
#sigma <- c(1, 0.2, 1)


# Create datalist
data <- list(
    N = n,
    home_scores = home_scores,
    away_scores = away_scores
)


# Define variables & parameters
parameters <- list(
    alpha = rep(0, n),
    beta  = rep(0, n),
    #AB    = matrix(0.1, 2, n),
    gamma = 1,
    mu    = mu,
    Sigma = Sigma
)



#######
### OBJECTIVE FUNCTION

fit <- rstan::stan(file="models-stan/scoremodel.stan", data=data)



#######
### RESULT

pars <- c("alpha", "beta", "gamma", "mu", "Sigma")
report <- summary(fit, pars = pars)$summary[TRUE, "mean"]

result <- util::groupby(util::unindex(report))

# result
# graphics::pairs(fit)


# Gets the values matching the function signature
values <- result[formalArgs(util::calc_ratings)]


# Sort teams by score rating
ratings <- do.call(util::calc_ratings, values)
util::rankings(ratings, teams$name)


# Sort teams by point system
points <- do.call(util::calc_points, values)
util::rankings(points, teams$name)
