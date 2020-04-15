exports <- c("n", "teams", "m", "matches", "r", "rounds", "indexes", "stats", "ztats", "mu", "team", "score_full", "home_score", "away_score")

# fixes default arguments
read.csv.better <- function (file, ...)
    read.csv(file, header=TRUE, encoding="UTF-8", stringsAsFactors=FALSE, ...)

team       <- read.csv.better("data/teams.csv")
score      <- read.csv.better("data/scores.csv")
score_full <- read.csv.better("data/scores_full.csv")


# Replace team names (strings) with ordinals
score_full[[1]] <- team[match(score_full[[1]], team[T, 2]), 1] + 1
score_full[[2]] <- team[match(score_full[[2]], team[T, 2]), 1] + 1


# number of teams & matches
n <- teams   <- nrow(team)
m <- matches <- n*(n-1) # alias for m
r <- rounds  <- 2*(n-1)


# gief tuple unpack pl0x
home_team  <- score_full[[1]]
away_team  <- score_full[[2]]
home_score <- score_full[[3]]
away_score <- score_full[[4]]
round      <- score_full[[5]]

# score keys
indexes <- data.frame(
    team      = c(home_team, away_team),
    opponent  = c(away_team, home_team),
    round     = rep(round, 2),
    advantage = rep(c(1, -1), each=nrow(score_full))
)

# scores
scores <- data.frame(score = c(home_score, away_score))


# indexes + scores
stats <- cbind(indexes, scores)

# zero-indexes + scores
ztats <- stats
ztats[T,1:3] <- ztats[T,1:3] - 1


# average score
mu <- mean(stats$score)


# scores as matrix
.indexes <- as.matrix(score[1:2]) + 1 # add for for 1-indexing

home_score <- array(0, dim=c(n, n))
home_score[.indexes] <- score[T, 3]

away_score <- array(0, dim=c(n, n))
away_score[.indexes] <- score[T, 4]
