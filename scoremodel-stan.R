set.seed(0)
library(rstan)
source("util.R")

# base::options(mc.cores = parallel::detectCores())



teams <- read.csv("data/teams.csv", header=TRUE)
scores <- read.csv("data/scores.csv", header=TRUE)


# number of teams & matches
n <- length(teams$key)
m <- n*(n-1)

indexes <- as.matrix(scores[1:2]) +1 # +1 shifts from 0-indexing to 1-indexing
default <- 0 # NA would break TMB

home_scores <- matrix(default, ncol=n, nrow=n)
home_scores[indexes] <- scores[TRUE, 3] # selects the third col; aka scores[, 3]

away_scores <- matrix(default, ncol=n, nrow=n)
away_scores[indexes] <- scores[TRUE, 4]


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


fit <- rstan::stan(file="models-stan/scoremodel.stan", data=data)


pars <- c("alpha", "beta", "gamma", "mu", "Sigma")
report <- summary(fit, pars = pars)$summary[TRUE, "mean"]

result <- groupby(unindex(report))

result
# graphics::pairs(fit)


# Gets the values matching the function signature
values <- result[formalArgs(calc_ratings)]


# Sort teams by score rating
ratings <- do.call(calc_ratings, values)
for (team in teams$name[order(ratings, decreasing=TRUE)]) print(team)


# Sort teams by point system
points <- do.call(calc_points, values)
for (point_index in order(points, decreasing=TRUE))
    print(paste(teams$name[point_index], points[point_index]))
