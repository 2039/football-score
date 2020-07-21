#include <TMB.hpp>
#include "DVAR.cpp"

// https://kaskr.github.io/adcomp/structarray.html#a73c905d02300879a7c8ed576f2c1d84f


template<class Type>
Type objective_function<Type>::operator() ()
{
  /* Data section */
  DATA_IMATRIX(stats);

  /* Random effect section */
  // A(round, team, strength)
  PARAMETER_ARRAY(A);

  /* Parameter section */
  PARAMETER(gamma);
  PARAMETER(mu);

  // TODO: figure out how to properly make a cov-matrix
  PARAMETER_VECTOR(theta);   // rho = rho(theta)
  PARAMETER_VECTOR(log_sds); // sqrt-variance diagonal

  // Parametrization based off https://github.com/jtufto/tmb-var1/
  // PARAMETER_VECTOR(eigtheta);
  // PARAMETER_VECTOR(logit_eiglambda); // Phi = Phi(eigentheta, eigenlambda)

  /* Variables (un-parameterized) */
  vector<Type> sds = exp(log_sds);

  // vector<Type> eiglambda = 2/(1+exp(-logit_eiglambda))-1;

  /* Procedure section */

  // We construct Sigma
  matrix<Type> Sigma_w = covariance(theta, sds);
  ADREPORT(Sigma_w);

  // // We construct Phi, the coefficient matrix of the VAR(1) series
  // // matrix<Type> Phi = VAR_coeff(eigtheta, eiglambda);
  // matrix<Type> Phi = Z<Type>(2);
  // ADREPORT(Phi);

  // // We construct the stationary covariance
  // matrix<Type> Gamma0 = VAR_covariance(Phi, Sigma_w);
  // ADREPORT(Gamma0);


  // Initialize value for negative-log-likelihood (nll)
  Type nll = 0;


  /* poisson(lambda) error */
  for (int s=0; s < stats.rows(); s++) {
    vector<int> round_stats = stats.row(s);

    // this would be so much prettier/readable with tuple assignment ...
    int team       = round_stats(0);
    int opponent   = round_stats(1);
    int team_round = round_stats(2);
    int opponent_round = round_stats(3);
    int advantage  = round_stats(4); // {-1, 1}
    Type score     = round_stats(5); //BUG dpois cant take int as first argument

    Type llambda =
      A(team_round, team, 0) - A(opponent_round, opponent, 1) + advantage * gamma + mu;

    nll += -dpois(score, exp(llambda), true);
  }


  // dimensions
  int rounds = A.rows();
  int teams = A.col(0).cols();

  /* VAR(1) noise error */
  for (int t=0; t < teams; t++) for (int r=0; r < rounds; r++) {
    vector<Type> x = A.row(r).row(t);

    nll += density::MVNORM(Sigma_w)(x);
  }

  return nll;
}
