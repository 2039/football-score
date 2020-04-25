exports <- c(
    "n", "teams", "m", "matches", "r", "rounds",
    "team_indexes", "match_indexes", "score_indexes",
    "advantage",
    "stats", "ztats",
    "mu",
    "team", "score_full", "home_score", "away_score"
)

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


date <- score_full[[6]]
date_order <- order(as.Date(date, format="%d.%m.%Y"))

score_ordered <- score_full[date_order, T]


# gief tuple unpack pl0x
home_team  <- score_ordered[[1]]
away_team  <- score_ordered[[2]]
home_score <- score_ordered[[3]]
away_score <- score_ordered[[4]]
round      <- score_ordered[[5]]



# these functions gets the rounds of teams sorted by date
team_round <- function(team)
    # indexes of team (either home or away)
    which(unname(apply(score_ordered[c(1,2)], 1, function(r) any(team %in% r))))

team_home <- function(team)
    # indexes of home team
    which(score_ordered[[1]] %in% team)

team_away <- function(team)
    # indexes of away team
    which(score_ordered[[2]] %in% team)

team_rounds <- function(team) {
    .team_round <- array(0, dim=nrow(score_ordered))

    for (i in 1:n) .team_round[team(i)] <- match(team(i), team_round(i))

    return(.team_round)
}

# team rounds in ordered by date
home_round <- team_rounds(team_home)
away_round <- team_rounds(team_away)


advantage <- function(indexes) {
    #' Adds advantage column to indexes,
    #'   depending on whether or not the team plays at home

    unpack <- function(l, k, e) list2env(l[k], e)

    # This is really ugly
    exposed <- c("home_team", "away_team", "home_round", "away_round")
    unpack(indexes, exposed, environment())

    # print(merge(indexes, c(1, -1)))

    data.frame(
        team           = c(home_team, away_team),
        opponent       = c(away_team, home_team),
        team_round     = c(home_round, away_round),
        opponent_round = c(away_round, home_round),
        advantage      = rep(c(1, -1), each=length(home_team))
    )
}


# row(home, away)
team_indexes <- data.frame(home_team, away_team)

# team_indexes + row(home_round, away_round)
match_indexes <- data.frame(home_team, away_team, home_round, away_round)

# row(team, opponent, team_round, opponent_round, advantage)
score_indexes <- advantage(match_indexes)


# row(score)
scores <- data.frame(score = c(home_score, away_score))

# indexes + scores
stats <- cbind(score_indexes, scores)

# zero-indexes + scores
ztats <- stats
ztats[T,1:4] <- ztats[T,1:4] - 1


# statistical data
# average score
mu <- mean(stats$score)


# deprecate?
# scores as matrix
.indexes <- as.matrix(score[1:2]) + 1 # add for for 1-indexing

home_score <- array(0, dim=c(n, n))
home_score[.indexes] <- score[T, 3]

away_score <- array(0, dim=c(n, n))
away_score[.indexes] <- score[T, 4]

