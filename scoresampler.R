base::set.seed(0)

source("import.R")

data = c("teams", "matches", "rounds")
import(data, from="data.R")
import(from="options.R")
import(from="util.R")

. <- "
TODO
[o] split indexes -> score_indexes, match_indexes
[o] check is splitting indexes affects stats, ztats
[o] helper function join(match_indexes, array(scores, dim=c(2, nrow(match_indexes)))
[ ] table for scores? (or [h-o] pairs plot?)
[ ] write up math for geo/arimean of lambdas for site
[ ] figure out what kind of simulations are useful/how to plot
"

# load parameters given by the CLI interface
# [--load model-parameters --model simulation-model]
parameters <- options::load()


# team, opponent, team_round, opponent_round, advantage
indexes <- data::score_indexes


circle_round_robin_indexes <- function(k) {
    #' @param k integer
    #'
    #' @return array(dim=(k*(k-1)/2, 4))
    #'   first index is team, second is opponent,
    #'   third is team round, fourth is opponent round
    #'
    #' Implementation is based off the circle method
    #' https://en.wikipedia.org/wiki/Round-robin_tournament#Circle_method

    round <- array(0, dim=c(k*(k-1)/2, 3))

    # attempt to randomize order
    team <- sample(1:k)

    for (i in 1:(k-1)) {
        t = team[1:(k/2)]
        o = rev(team)[1:(k/2)]

        # nice indexing R
        round[((i-1)*k/2+1):(i*k/2), T] <- cbind(t, o, i)

        # rotate tail
        team <- c(team[1], tail(team), head(team[-1],-1))
    }

    # second part of rounds is just the first part flipped
    round2 <- round[nrow(round):1, c(2,1,3)]
    round2[T,3] <- round[T,3] + (k-1)

    round <- rbind(round, round2)
    round <- cbind(round, round[T, 3])

    round <- data.frame(round)
    names(round) <- c("home_team", "away_team", "home_round", "away_round")

    return(round)
}


# scrapped due to complex constraints on rounds
#
# round_robin_indexes <- function(k) {
#     # non-overlapping partitioning of a complete k-graph into k-1 subgraphs,
#     # each containing k/2 ordered pairings.
#     # A vertex may only occur first in a pairing k/2-1 or k/2 times

#     # R doesn't know what a 1-element vector is ...
#     sample <- function(x)
#         if (length(x) <= 1) x else base::sample(x, 1)

#     # Initialize valid pairings
#     pairings <- array(TRUE, dim=c(k,k))
#     diag(pairings) <- FALSE

#     # Initialize pairings array by round
#     matches <- array(0, dim=c(k/2, k-1, 2))

#     rounds <- k-1
#     pairs <- k/2

#     team <- 1:k

#     for (r in 1:rounds) {
#         # Initialize unmatched teams for the round
#         unmatched <- array(TRUE, dim=c(k))

#         for (p in 1:pairs) {
#             # blossom algorithm can be used
#             # but that would be make arbitrary pairings, not random ones

#             # this part is bugged
#
#             t1 <- sample(team[unmatched])
#             t2 <- sample(team[unmatched & pairings[T, t1] & pairings[t1, T]])

#             matches[p, r, T] <- c(t1, t2)

#             # cascading assignment
#             unmatched[t1] <- unmatched[t2] <- FALSE
#             pairings[t1,t2] <- FALSE
#         }
#     }

#     dim(matches) <- c((k-1)*k/2, 2)

#     matches
# }



model <- if (options::options$model == "var") {
    A     <- parameters$A
    mu    <- parameters$mu
    gamma <- parameters$gamma

    Phi    <- util::eigen_recompose2D(parameters$eigtheta, parameters$eiglambda)
    Sigma_w <- util::recompose_cov(parameters$theta, parameters$sds)
    Gamma0  <- util::AR_Gamma0(Phi, Sigma_w)

} else if(options::options$model == "rw") {
    A     <- parameters$A
    mu    <- parameters$mu
    gamma <- parameters$gamma

    Phi     <- diag(2)
    Sigma_w <- util::recompose_cov(parameters$theta, parameters$sds)
    Gamma0  <- Sigma_w
}


simulate_parameters <- function(Phi, Sigma_w, Gamma0) {
    w <- array(
        mvtnorm::rmvnorm(n=teams*(rounds-1), sigma=Sigma_w),
        dim=c(rounds-1, teams, 2)
    )

    # initial value
    A[T,T,1] <- t(mvtnorm::rmvnorm(n=teams, sigma=Gamma0))

    for (team in 1:teams) for (round in 2:rounds) {
        A[round, team, T] <- Phi %*% A[round-1, team, T] + w[round-1, team, T]
    }

    return(A)
}


simulate_lambdas <- function(indexes, Phi, Sigma_w, Gamma0) {
    A <- simulate_parameters(Phi, Sigma_w, Gamma0)

    lambda <- array(0, dim=nrow(indexes))

    for (i in 1:length(lambda)) {
        m <- indexes[i,T]

        team           <- m$team
        opponent       <- m$opponent
        team_round     <- m$team_round
        opponent_round <- m$opponent_round
        advantage      <- m$advantage

        lambda[i] <- exp(
            A[team_round, team, 1] - A[opponent_round, opponent, 2] + gamma*advantage + mu
        )
    }

    return(lambda)
}


simulate_scores <- function(indexes, Phi, Sigma_w, Gamma0) {
    lambda <- simulate_lambdas(indexes, Phi, Sigma_w, Gamma0)

    # weird signature
    scores <- rpois(length(lambda), lambda=lambda)

    return(scores)
}

simulate_points <- function(team_indexes, Phi, Sigma_w, Gamma0) {
    util::points_from_score(
        team_indexes,
        array(simulate_scores(
            #data::score_indexes,
            data::advantage(circle_round_robin_indexes(teams)),
            Phi,
            Sigma_w,
            Gamma0
        ), dim=c(240, 2))
    )
}



geometric_oco <- function(mu, gamma, alpha, beta) {
    #' GM(lambda)

    n = ncol(alpha)

    beta_sum = sum(beta)

    # compute *log*-lambda-average in everyone-against-everyone matches
    avg_llambdas <- sapply(1:n, function(i)
        mu + alpha[i] - (beta_sum - beta[i])/(n-1)
    )

    return(exp(avg_llambdas))
}


average_oco <- function(mu, gamma, alpha, beta) {
    #' AM(lambda)

    n <- length(alpha)

    beta_neg_exp_sum <- sum(exp(-beta))

    # compute lambda-average in everyone-against-everyone matches
    avg_lambdas <- sapply(1:n, function(i)
        exp(mu[i] + alpha[i])*cosh(gamma[i])*(beta_neg_exp_sum - exp(-beta[i]))/(n-1)
    )

    return(avg_lambdas)
}


temporal_oco <- function(mu, gamma, A) {

    # compute lambda-average in everyone-against-everyone matches
    # for each round
    round_oco <- sapply(1:rounds, function(r)
        average_oco(mu, gamma, A[r,T,1], A[r,T,2])
    )

    return(round_oco)
}


simulate_avg_scores <- function(k) {
    # Generate alpha-beta parameters
    As <- structure(
        sapply(1:k, function(...) simulate_parameters(Phi, Sigma_w, Gamma0)),
        dim=c(rounds, teams, 2, k)
    )

    # Compute lambda-averages
    lamdas <- structure(
        sapply(1:k, function(i) temporal_oco(mu, gamma, As[T,T,T,i])),
        dim=c(rounds, teams, k)
    )

    lamdas
}

########
# plotting

# "Simulating VAR(1) series"
k <- 1000
points <- sapply(1:k, function(...) simulate_points(
    circle_round_robin_indexes(16), #data::team_indexes,
    Phi,
    Sigma_w,
    Gamma0
))
rownames(points) <- data::team$name

# "MLE realization"
values <- parameters[intersect(names(parameters), formalArgs(util::calc_VAR_ratings))]
values$stats <- as.matrix(data::stats)
realization <- do.call(util::calc_VAR_points, values)

# "Actual score"
true_points <- util::points_from_score(
        data::team_indexes,
        array(data::stats$score, dim=c(240, 2))
    )

#windows() # Use X11() or quartz() if on linux or mac.
svg("image/point_round_shuffle.svg")

#plot(0,0, xlim=c(1,k), ylim=c(20,70), type="n")
par(mfrow=c(4,4))

color <- rainbow(teams)
for (t in 1:teams) {
    # v = vertical axis
    # col = color
    # lwd = line-width
    # lty = line-type
    hist(points[t, T], breaks=40, main=data::team$name[t], xlab="Points", col="orange", lty="blank")
    #abline(v=mean(points[t, T]), col=color[t], lwd=2)
    abline(v=true_points[t], col="dodgerblue", lwd=3)
    abline(v=realization[t], col="black", lty="dotted", lwd=3)
}

dev.off()

#. <- locator(1)

k <- 5
score_realizations <- simulate_avg_scores(k)
mean_realizations <- apply(score_realizations, c(1,2), mean)

windows() # Use X11() or quartz() if on linux or mac.
plot(0,0, xlim=c(1,rounds), ylim=c(1,2), type="n")

cl <- rainbow(teams)
for (t in 1:teams) #for (r in 1:k)
    lines(1:rounds, mean_realizations[T,t], col=cl[t], type='b')

. <- locator(1)
