exports <- c("teams", "n", "m", "home_scores", "away_scores", "indexes", "time_indexes", "stats", "ztats")

teams <- read.csv("data/teams.csv", header=TRUE, encoding="UTF-8")
scores <- read.csv("data/scores.csv", header=TRUE, encoding="UTF-8")
scores_full <- read.csv("data/scores_full.csv", header=TRUE, encoding="UTF-8")


# number of teams & matches
n <- length(teams$key)
m <- n*(n-1)

indexes <- as.matrix(scores[1:2]) +1 # +1 shifts from 0-indexing to 1-indexing
default <- 0 # NA would break TMB

home_scores <- matrix(default, ncol=n, nrow=n)
home_scores[indexes] <- scores[TRUE, 3] # selects the third col; aka scores[, 3]

away_scores <- matrix(default, ncol=n, nrow=n)
away_scores[indexes] <- scores[TRUE, 4]

time_indexes <- as.matrix(scores_full[c(1, 2, 5)])
time_indexes[T, 1] <- teams[match(time_indexes[T, 1], teams[T, 2]), 1] + 1
time_indexes[T, 2] <- teams[match(time_indexes[T, 2], teams[T, 2]), 1] + 1
time_indexes <- matrix(as.numeric(time_indexes), nrow=nrow(time_indexes))


indexes <- merge(time_indexes, c(-1, 1), all=TRUE)
names(indexes) <- c("home_team", "away_team", "round", "which")


# TODO find a prettier way to write this
stats <- array(0, dim=c(2*m, 5))
stats[T,1:4] <- unlist(indexes)
stats[1:m,5] <- unlist(scores_full[3])
stats[((m+1):(2*m)),5] <- unlist(scores_full[4])
stats

# zero-index
ztats <- stats
ztats[T,1:3] <- ztats[T,1:3] - 1
ztats
