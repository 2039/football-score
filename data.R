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
round      <- score_full[[5]] # NOT IN CHRONOLOGICAL ORDER
date       <- score_full[[6]]



score_ordered <- score_full[order(as.Date(date, format="%d.%m.%Y")), c(1, 2)]


home_round <- array(0, dim=nrow(score_full))
away_round <- array(0, dim=nrow(score_full))


team_round <- function(team)
    which(unname(apply(score_ordered[c(1,2)], 1, function(r) any(team %in% r))))

team_home <- function(team)
    which(score_ordered[[1]] %in% team)

team_away <- function(team)
    which(score_ordered[[2]] %in% team)

for (i in 1:n) home_round[team_home(i)] <- match(team_home(i), team_round(i))
for (i in 1:n) away_round[team_away(i)] <- match(team_away(i), team_round(i))


indexes <- data.frame(home_team, away_team, home_round, away_round)


advantage <- function(indexes) {
    #' Adds advantage column to indexes,
    #'   depending on whether or not the team plays at home

    # This is really ugly
    exposed <- c("home_team", "away_team", "home_round", "away_round")
    list2env(indexes[exposed], environment())

    data.frame(
        team           = c(home_team, away_team),
        opponent       = c(away_team, home_team),
        team_round     = c(home_round, away_round),
        opponent_round = c(away_round, home_round),
        advantage      = rep(c(1, -1), each=length(home_team))
    )
}

# score keys
indexes <- advantage(indexes)


# scores
scores <- data.frame(score = c(home_score, away_score))


# indexes + scores
stats <- cbind(indexes, scores)

# zero-indexes + scores
ztats <- stats
ztats[T,1:4] <- ztats[T,1:4] - 1


# average score
mu <- mean(stats$score)


# scores as matrix
.indexes <- as.matrix(score[1:2]) + 1 # add for for 1-indexing

home_score <- array(0, dim=c(n, n))
home_score[.indexes] <- score[T, 3]

away_score <- array(0, dim=c(n, n))
away_score[.indexes] <- score[T, 4]

