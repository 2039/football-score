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
### PARAMETERS

A <- array(0.1, dim=c(rounds, teams, 2))


# Sigma_w and its parameters
Sigma_w <- matrix(c(1,0.2,0.2,2), ncol=2)
theta   <- util::decompose_cov(Sigma_w)$theta
sds     <- util::decompose_cov(Sigma_w)$sds

# Phi and its parameters
Phi       <- matrix(c(.9,.1,-.2,.3),2,2)
eigtheta  <- util::eigen_decompose2D(Phi)$theta
eiglambda <- util::eigen_decompose2D(Phi)$lambda


# Create datalist
data <- list(
    stats = as.matrix(data::ztats)
)

# Define variables & parameters
parameters <- list(
    A     = A,
    gamma = array(0, dim=1),
    mu    = array(log(mu), dim=1),
    theta = theta,
    log_sds = log(sds),
    eigtheta = c(0, pi/2),
    logit_eiglambda = qlogis((eiglambda+1)/2)
)



#######
### OBJECTIVE FUNCTION

modelname <- options::options$model
MODEL <- if(util::is.nonempty(modelname)) modelname else "disc_var"

# Compile and link the template
# paste0 concatenates strings (without separator)
. <- TMB::compile(paste0("models-tmb/", MODEL, ".cpp")) # Only needed once
dyn.load(TMB::dynlib(paste0("models-tmb/", MODEL)))


# Make Automatic Differentiation Function
obj <- TMB::MakeADFun(data, parameters, random=c("A"), DLL=MODEL, silent=T)


# NonLinear MINimization subject to Box constraints
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/nlminb
# deprecated, but alternatives are worse:
# nlm(obj$fn, obj$par), optim(obj$par, obj$fn, obj$gr)
system.time(opt <- nlminb(obj$par, obj$fn, obj$gr))

util::TMB_AIC(opt, n=480)
util::TMB_AIC(opt)


#######
### RESULT

# https://www.rdocumentation.org/packages/TMB/versions/1.7.16/topics/sdreport
report <-TMB::sdreport(obj)

print(summary(report))


if (options::options$save) {
    result <- util::vecgroupby(summary(report))
    format <- options::options$format
    filename <- filename=paste0(MODEL, "_params")

    options::save(result, format=format, filename=filename)
}



result <- util::unlog(obj$env$parList())
result <- util::vectorize_values(result, c("mu", "gamma"), teams)

# Sigma <- util::recompose_cov(result$theta, result$sds)

# Gets the result values matching the function signature + stats
values <- result[intersect(names(result), formalArgs(util::calc_VAR_ratings))]
values$stats <- as.matrix(data::stats)


# Sort teams by points
points <- do.call(util::calc_VAR_points, values)

# print rankings
print(util::rankings(points, data::team$name))

alarm()
