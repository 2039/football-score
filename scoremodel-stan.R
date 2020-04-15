set.seed(0)
library(rstan)

# base::options(mc.cores = parallel::detectCores())


# hacky way to create a virtual package
source("import.R")
import(from="util.R")


#######
### DATA

data <- c("teams","matches", "team", "home_score", "away_score", "mu")
import(data, from="data.R")



#######
### PARAMETERS

Sigma <- matrix(c(1,0.2,0.2,1), ncol=2)
Sigma <- diag(2)
#Sigma
#sigma <- c(1, 0.2, 1)


# Create datalist
data <- list(
    N = teams,
    home_scores = home_score,
    away_scores = away_score
)


# Define variables & parameters
parameters <- list(
    alpha = rep(0, teams),
    beta  = rep(0, teams),
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
print(util::rankings(ratings, team$name))


# Sort teams by point system
points <- do.call(util::calc_points, values)
print(util::rankings(points, team$name))
