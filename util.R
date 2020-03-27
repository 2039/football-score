library(Matrix)
# ?!! cannot silence library function
library(expm, warn.conflicts = FALSE, quietly = TRUE)


#' Returns the diagonal part of a matrix as a matrix.
#' ?!! improper signature/overloading
ddiag <- function(M) diag(diag(M))

#' Adds option to return lower triangular cholesky matrix
#' ?!! upper triangular (hardcoded)
chol <- function(M, lower=TRUE) {
    if(lower) t(base::chol(M)) else base::chol(M)
}

#' Adds an alias for matrix inverse, as `solve` is an odd name
#' ?!! misnomer
inv  <- solve

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


groupby <- function(x) {
    #' @description Group elements in a named vector by name
    #'
    #' @param c() with names():
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
