#include <TMB.hpp>
#include "VAR.cpp"

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
  PARAMETER_VECTOR(eigtheta);
  PARAMETER_VECTOR(logit_eiglambda); // Phi = Phi(eigentheta, eigenlambda)

  /* Variables (un-parameterized) */
  vector<Type> sds = exp(log_sds);

  vector<Type> eiglambda = 2/(1+exp(-logit_eiglambda))-1;

  /* Procedure section */

  // We construct Sigma
  matrix<Type> Sigma_w = covariance(theta, sds);

  // We construct Phi, the coefficient matrix of the VAR(1) series
  matrix<Type> Phi = VAR_coeff(eigtheta, eiglambda);

  // We construct the stationary covariance
  matrix<Type> Gamma0 = VAR_covariance(Phi, Sigma_w);


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

  /* VAR(1) initialization error */
  for (int t=0; t < teams; t++) {
    // No array.row() method so we transpose to access inner dimension
    vector<Type> x0 = A.transpose().col(0).col(t);

    nll += density::MVNORM(Gamma0)(x0);
  }

  /* VAR(1) noise error */
  for (int t=0; t < teams; t++) for (int r=1; r < rounds; r++) {
    // No array.row() method so we transpose to access inner dimension
    vector<Type> xp = A.transpose().col(r-1).col(t);
    vector<Type> xn = A.transpose().col(r).col(t);

    vector<Type> w = xn - xp;

    nll += density::MVNORM(Sigma_w)(w);
  }

  return nll;
}
