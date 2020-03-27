#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  /* Data section */
  DATA_MATRIX(HOME);
  DATA_MATRIX(AWAY);

  /* Random effect section */
  PARAMETER_VECTOR(alpha);
  PARAMETER_VECTOR(beta);

  /* Parameter section */
  PARAMETER(gamma);
  PARAMETER(mu);

  // TODO: figure out how to properly make a cov-matrix
  PARAMETER_VECTOR(theta);   // rho = rho(theta)
  PARAMETER_VECTOR(log_sds); // sqrt-variance diagonal

  /* Procedure section */
  vector<Type> sds = exp(log_sds);

  // Subclass of MVNORM_t
  density::UNSTRUCTURED_CORR_t<Type> NEG_LOG_MVNORM_UNSCALED(theta);

  int matches = alpha.size();

  // Initialize value for negative-log-likelihood
  Type nll = 0;

  // log-likelihood of match scores
  for (int i=0; i < matches; i++) { for (int j=0; j < matches; j++) {
      if (i == j) { continue; /* A team never matches with themselves */ }

      // llambda == log-lambda
      Type llambda_home = alpha(i) - beta(j) + gamma + mu;
      Type llambda_away = alpha(j) - beta(i) - gamma + mu;

      // dpois(x: float, lambda: float, log: bool) -> float
      nll += -dpois(HOME(i, j), exp(llambda_home), true);
      nll += -dpois(AWAY(i, j), exp(llambda_away), true);
  }}

  // log-likelihood of latent variables
  for (int i=0; i < matches; i++) {
    vector<Type> x(2); x << alpha(i), beta(i);

    nll += density::VECSCALE(NEG_LOG_MVNORM_UNSCALED, sds)(x);
  }

  // Accessible by sdreport(model)$value, sdreport(model)$sd
  // ADREPORT(sds)
  // Accessible by model$report(); without sd
  // REPORT(sds)

  return nll;
}
