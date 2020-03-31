library(Matrix)
# ?!! cannot silence library function
library(expm, warn.conflicts = FALSE, quietly = TRUE)

exports = c(
    "where", "ddiag", "chol", "inv",
    "decompose_cov", "recompose_cov", "groupby",
    "unlog", "unindex", "llambdas", "calc_ratings",
    "calc_points"
)

foo <- function(x) {2+x}

#' Returns the package name the object is contained in
#' @export
where <- function(obj) environmentName(environment(obj))

#' Returns the diagonal part of a matrix as a matrix.
#' ?!! improper signature/overloading
#' @export
ddiag <- function(M) diag(diag(M))

#' Adds option to return lower triangular cholesky matrix
#' ?!! upper triangular (hardcoded)
#' @export
chol <- function(M, lower=TRUE) {
    if(lower) t(base::chol(M)) else base::chol(M)
}

#' Adds an alias for matrix inverse, as `solve` is an odd name
#' ?!! misnomer
#' @export
inv  <- solve



#' @export
decompose_cov <- function(Sigma) {
    #' @description Rebuild covariance matrix from theta and sds
    #'
    #' @param Sigma matrix(ncol=n, nrow=n):
    #'     The corresponding covariance matrix
    #'
    #' @return list():
    #'     A list containing two keys:
    #'     theta : c()
    #'         A n*(n-1)-sized vector
    #'     sds : c()
    #'         A n-sized vector

    Rho <- sqrtm(inv(ddiag(Sigma))) %*% Sigma %*% sqrtm(inv(ddiag(Sigma)))
    L <- chol(Rho)
    Theta <- inv(ddiag(L)) %*% L

    params <- list(
        theta = Theta[lower.tri(Theta)],
        sds   = sqrt(diag(Sigma))
    )

    params
}



#' @export
recompose_cov <- function(theta, sds) {
    #' @description Rebuild covariance matrix from theta and sds
    #'
    #' @param theta c():
    #'     A n*(n-1)-sized vector
    #'
    #' @param sds c():
    #'     A n-sized vector
    #'
    #' @return matrix(ncol=n, nrow=n):
    #'     The corresponding covariance matrix

    n <- length(sds)
    Theta <- matrix(0, ncol=n, nrow=n)
    Theta[lower.tri(Theta, diag=TRUE)] <- 1
    Theta[lower.tri(Theta)] <- theta

    L   <- sqrtm(inv(ddiag(Theta %*% t(Theta)))) %*% Theta
    Rho <- L %*% t(L)
    Sigma <- diag(sds) %*% Rho %*% diag(sds)

    Sigma
}



#' @export
groupby <- function(x) {
    #' @description Group elements in a named vector by name
    #'
    #' @param x c() with names():
    #'     A named vector
    #'
    #' @return list(character = c())
    #'     A list with the names as keys and elements with
    #'     the same name in a vector

    result <- list()
    select <- function(x, name) unname(x[names(x) == name])

    for (name in unique(names(x))) {
        result[[name]] <- select(x, name)
    }

    return(result)
}



#' @export
unlog <- function(l) {
    #' @description Exponentiates logarithmed values with key starting with `log_`
    #' and remove the `log_` part of the key.
    #'
    #' @param l list():
    #'     A list with some (all) keys starting with `log_`
    #'
    #' @return list():
    #'     A list with no keys starting with `log_`

    for (name in names(l)) {
        if (startsWith(name, "log")) {
            expname <- substring(name, 5)
            l[[expname]] <- exp(l[[name]])
            l[[name]] <- NULL # ?!! assignment deletes component
        }
    }

    return(l)
}



#' @export
unindex <- function(v) {
    #' @description Removes index part of vector names
    #'
    #' @param v c():
    #'     A named vector with some names ending with [.*]
    #'
    #' @return list():
    #'     A named vector with no names ending with [.*]

    nnames <- lapply(names(v), function(s) strsplit(s, "\\[")[[1]][1])

    names(v) <- nnames

    return(v)
}



#' @export
llambdas <- function(alpha, beta, gamma, mu) {
    #' @description Calculates lambda for each match given the team parameters
    #'
    #' @param alpha numeric
    #' @param beta numeric
    #' @param gamma numeric
    #' @param mu numeric

    n <- length(alpha)

    loglambdas <- array(dim=c(n, n, 2))

    for (i in 1:n) for (j in 1:n) {
        if (i==j) next;

        loglambdas[i, j, 1] = alpha[i] - beta[j] + gamma + mu
        loglambdas[i, j, 2] = alpha[j] - beta[i] - gamma + mu
    }

    return(loglambdas)
}



#' @export
calc_ratings <- function(alpha, beta, gamma, mu) {
    #' @description calculates the rating for each team as the average lambda
    #'     score of each match the team plays in
    #'
    #' @param alpha numeric
    #' @param beta numeric
    #' @param gamma numeric
    #' @param mu numeric

    lambdas <- exp(llambdas(alpha, beta, gamma, mu))

    # match first axis to team instead of match
    lambdas[T,T,2] <- t(lambdas[T,T,2])

    ratings <- apply(lambdas, 1, function(x) mean(x, na.rm=TRUE))

    ### procedural version

    # n <- length(alpha)

    # # Calculate rankings by point system
    # ratings <- rep(0, n)

    # for (i in 1:n) for (j in 1:n) {
    #     if (i==j) next;

    #     ratings[i] = ratings[i] + exp(alpha[i] - beta[j] + gamma + mu)
    #     ratings[j] = ratings[j] + exp(alpha[j] - beta[i] - gamma + mu)
    # }

    # matches_by_team <- 2*(n-1)
    # ratings <- ratings / matches_by_team

    return(ratings)
}



#' @export
calc_points <- function(alpha, beta, gamma, mu) {
    #' @description calculates the points for each team as given by
    #'     the points system
    #'
    #' @param alpha numeric
    #' @param beta numeric
    #' @param gamma numeric
    #' @param mu numeric

    lambdas <- exp(llambdas(alpha, beta, gamma, mu))

    # skellam doesn't like NaN values, which is fine
    ties <- suppressWarnings(apply(lambdas, c(1,2), function(x) skellam::dskellam(0, x[1], x[2])))
    away <- suppressWarnings(apply(lambdas, c(1,2), function(x) skellam::pskellam(-1,x[1], x[2])))
    home <- suppressWarnings(apply(lambdas, c(1,2), function(x) skellam::pskellam(-1,x[2], x[1])))

    # score at home stadium plus score at opponent stadium
    X <- (0*away + 1*ties + 3*home) + (3*t(away) + 1*t(ties) + 0*t(home))

    points <- apply(X, 1, function(x) sum(x, na.rm=TRUE))

    ### procedural version

    # n <- length(alpha)

    # # Calculate rankings by point system
    # points <- rep(0, n)

    # for (i in 1:n) for (j in 1:n) {
    #     if (i==j) next;

    #     home_lambda = exp(alpha[i] - beta[j] + gamma + mu)
    #     away_lambda = exp(alpha[j] - beta[i] - gamma + mu)

    #     tie <- skellam::dskellam(0, home_lambda, away_lambda)
    #     away_win <- skellam::pskellam(-1, home_lambda, away_lambda)
    #     home_win <- skellam::pskellam(-1, away_lambda, home_lambda)

    #     points[i] = points[i] + 0*away_win + 1*tie + 3*home_win
    #     points[j] = points[j] + 3*away_win + 1*tie + 0*home_win
    # }

    return(points)
}
