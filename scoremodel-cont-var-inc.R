set.seed(0)
library(TMB)

# hacky way to create a virtual package
source("import.R")
import(from="util.R")
import(from="options.R")



#######
### DATA

data <- c("teams", "rounds", "matches", "mu")
import(data, from="data.R")



#######
### OBJECTIVE FUNCTION

MODEL = "cvar2_tmb"

# Compile and link the template
# paste0 concatenates strings (without separator)
. <- TMB::compile(paste0("models-tmb/", MODEL, ".cpp")) # Only needed once
dyn.load(TMB::dynlib(paste0("models-tmb/", MODEL)))



# last match on a given date
match_date_indexes <- match(unique(data::time), data::time)-1
matches_per_date <- diff(c(match_date_indexes, matches))
lambda_table <- data.frame(array(NA, dim=c(matches, 2), dimnames=list(NULL, c("home", "away"))))

# number of matchdays
M <- length(unique(data::time))

for (i in 2:M) {
    md <- match_date_indexes[i]

    print(paste("Current day:", md, "---", "Match day", i, "of", M))

    #######
    ### PARAMETERS

    B <- array(0, dim=c(md, 2, 2))


    # Sigma, Theta and their parametrication parameters
    Theta     <- array(c(1,0.2,0.2,2), dim=c(2,2))
    L_theta   <- util::decompose_cov(Theta)$theta
    sds_theta <- util::decompose_cov(Theta)$sds

    D     <- array(c(1,0.2,0.2,2), dim=c(2,2))
    L_D   <- util::decompose_cov(D)$theta
    sds_D <- util::decompose_cov(D)$sds



    # # Phi and its parameters
    # Phi       <- array(c(.9,.1,-.2,.3), dim=c(2,2))
    # eigtheta  <- util::eigen_decompose2D(Phi)$theta
    # eiglambda <- util::eigen_decompose2D(Phi)$lambda


    # Create datalist
    data <- list(
        matches = data::match_stats[1:md,T,T],
        times = as.integer(data::time)[1:md]
    )

    # Define variables & parameters
    parameters <- list(
        B     = B,
        gamma = array(0, dim=1),
        mu    = array(log(mu), dim=1),

        L_theta       = L_theta,
        log_sds_theta = log(sds_theta),

        L_D       = L_D,
        log_sds_D = log(sds_D)
    )

    #######
    ### OBJECTIVE FUNCTION

    # Make Automatic Differentiation Function
    obj <- TMB::MakeADFun(data, parameters, random=c("B"), DLL=MODEL, silent=T)


    # NonLinear MINimization subject to Box constraints
    # https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/nlminb
    # deprecated, but alternatives are worse:
    # nlm(obj$fn, obj$par), optim(obj$par, obj$fn, obj$gr)
    system.time(opt <- nlminb(obj$par, obj$fn, obj$gr))


    # print(util::TMB_AIC(opt, n=480))
    # print(util::TMB_AIC(opt))


    #######
    ### RESULT

    # https://www.rdocumentation.org/packages/TMB/versions/1.7.16/topics/sdreport
    report <-TMB::sdreport(obj)


    result <- util::unlog(obj$env$parList())
    result <- util::vectorize_values(result, c("mu", "gamma"), teams)


    theta <- util::recompose_cov(result$L_theta, result$sds_theta)
    D <- util::recompose_cov(result$L_D, result$sds_D)

    for (mr in 1:matches_per_date[i]) {
        m <- md + mr


        lambdas <- util::calc_match_lambda(result$B, result$gamma, result$mu, theta, data::match_stats, data::time, m)

        lambda_table[m, T] <- lambdas
    }
}



# save to file if CLI arguments are used [--save filename]
if (util::is.nonempty(options::options$save)) options::save(lambda_table)


# # Gets the result values matching the function signature + stats
# values <- result[intersect(names(result), formalArgs(util::calc_VAR_ratings))]
# values$stats <- as.matrix(data::stats)


# # Sort teams by points
# points <- do.call(util::calc_VAR_points, values)

# # print rankings
# print(util::rankings(points, data::team$name))
