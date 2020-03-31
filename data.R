exports <- c("teams", "n", "m", "home_scores", "away_scores")

teams <- read.csv("data/teams.csv", header=TRUE, encoding="UTF-8")
scores <- read.csv("data/scores.csv", header=TRUE, encoding="UTF-8")


# number of teams & matches
n <- length(teams$key)
m <- n*(n-1)

indexes <- as.matrix(scores[1:2]) +1 # +1 shifts from 0-indexing to 1-indexing
default <- 0 # NA would break TMB

home_scores <- matrix(default, ncol=n, nrow=n)
home_scores[indexes] <- scores[TRUE, 3] # selects the third col; aka scores[, 3]

away_scores <- matrix(default, ncol=n, nrow=n)
away_scores[indexes] <- scores[TRUE, 4]
