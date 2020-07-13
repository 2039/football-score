exports <- c(
    "n", "teams", "m", "matches", "r", "rounds",
    "team_indexes", "match_indexes", "score_indexes",
    "time_diffs",
    "match_stats", "time",
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

# format date
score_full[[6]] <- as.Date(score_full[[6]], format="%d.%m.%Y")


# number of teams & matches
n <- teams   <- nrow(team)
m <- matches <- n*(n-1) # alias for m
r <- rounds  <- 2*(n-1)


date_order <- order(score_full[[6]])

score_ordered <- score_full[date_order, T]


# gief tuple unpack pl0x
home_team  <- score_ordered[[1]]
away_team  <- score_ordered[[2]]
home_score <- score_ordered[[3]]
away_score <- score_ordered[[4]]
round      <- score_ordered[[5]]
time       <- score_ordered[[6]] - min(score_ordered[[6]])


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

team_rounds <- function(team_indexes) {
    .team_round <- array(0, dim=nrow(score_ordered))

    for (i in 1:n) .team_round[team_indexes(i)] <- match(team_indexes(i), team_round(i))

    return(.team_round)
}


team_time_diff <- function(i)
    # Finds the time difference in days between rounds
    # The first round is -1
    c(-1, diff(time[team_round(i)]))

team_time_diffs <- function() {
    .team_time_diff <- array(0, dim=c(rounds, teams))

    for (i in 1:n) {
        .team_time_diff[T, i] <- team_time_diff(i)

    }

    return(.team_time_diff)
}


# team rounds in ordered by date
home_round <- team_rounds(team_home)
away_round <- team_rounds(team_away)

time_diffs <- team_time_diffs()

PR <- function(x) {
    print(x)
    return(x)
}

prev_match <- function(team, index=1)
    # index i corresponds to to the match-id of the i-1th match
    # [-i] is everything except element at index i
    c(-1, team_round(team)[-rounds]-1+index)

curr_role <- function(team)
    # index i is 0 if the match was home, 1 if the match was away for the team
    as.integer(team_round(team) %in% team_away(team))

prev_role <- function(team)
    c(-1, curr_role(team)[-rounds])

curr_score <- function(team)
    cbind(home_score, away_score)[team_index(team)]

team_index <- function(team)
    cbind(team_round(team), curr_role(team)+1)

construct_stats <- function(...) {
    s <- array(NA, dim=c(matches, 2, 3))


    for (team in 1:teams) {
        # clean
        indexes <- as.matrix(merge(team_index(team), 1:3))
        rows <- cbind(curr_score(team), prev_match(team, ...), prev_role(team))

        s[indexes] <- rows


        # with mx2 indexes; only for selection, not assignment
        # print(apply(s, 3, `[`, indexes))

        # this is ugly
        # indexes <- team_index(team)
        # rows <- cbind(curr_score(team), prev_match(team), prev_role(team))
        # for (i in 1:nrow(indexes)) for (j in 1:3)
        #     s[indexes[i,1], indexes[i,2], j] <- rows[i, j]

        # this is also ugly
        # indexes <- cbind(team_index(team)[rep(1:rounds, times=3),], rep(1:3, each=rounds))


        # failed attempts
        #print(indexes)
        #print(merge(team_index(team), 1:3))
        #print(array(s[indexes], dim=c(rounds, 3)))
        #print(apply(s, 3, `[`, team_index(team)))
    }

    return(s)
}


match_stats <- construct_stats(index=0)


advantage <- function(indexes) {
    #' Adds advantage column to indexes,
    #'   depending on whether or not the team plays at home

    unpack <- function(l, k, e) list2env(l[k], e)

    # This is really ugly
    exposed <- c("home_team", "away_team", "home_round", "away_round")
    unpack(indexes, exposed, environment())

    # this doesn't flip home/away team, but would be nice:
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

