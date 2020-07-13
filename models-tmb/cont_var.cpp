#include <TMB.hpp>
#include "CVAR.cpp"

// https://kaskr.github.io/adcomp/structarray.html#a73c905d02300879a7c8ed576f2c1d84f


template<class Type>
Type objective_function<Type>::operator() ()
{
  /* Data section */
  DATA_IMATRIX(stats);
  DATA_MATRIX(time_diffs);

  /* Random effect section */
  // A(round, team, strength)
  PARAMETER_ARRAY(A);

  /* Parameter section */
  PARAMETER(gamma);
  PARAMETER(mu);

  PARAMETER_VECTOR(L_theta);   // rho = rho(theta)
  PARAMETER_VECTOR(log_sds_theta); // sqrt-variance diagonal

  // Parametrization based off https://github.com/jtufto/tmb-var1/
  PARAMETER_VECTOR(L_D);
  PARAMETER_VECTOR(log_sds_D); // Phi = Phi(eigentheta, eigenlambda)

  /* Variables (un-parameterized) */
  vector<Type> sds_theta = exp(log_sds_theta);
  vector<Type> sds_D = exp(log_sds_D);


  /* Procedure section */

  matrix<Type> theta = covariance(L_theta, sds_theta);
  matrix<Type> D = covariance(L_D, sds_D);

  // continuous VAR class
  CVAR<Type> cvar {theta, D};


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

    matrix<Type> Gamma = cvar.Gamma();

    nll += density::MVNORM(Gamma)(x0);
  }


  /* VAR(1) noise error */
  for (int t=0; t < teams; t++) for (int r=1; r < rounds; r++) {
    // No array.row() method so we transpose to access inner dimension
    vector<Type> xp = A.transpose().col(r-1).col(t);
    vector<Type> xn = A.transpose().col(r).col(t);
    Type dt = time_diffs(r, t);

    vector<Type> mu = cvar.mu(xp, dt);
    matrix<Type> Gamma = cvar.Gamma(dt);

    nll += density::MVNORM(Gamma)(xn - mu);
  }

  return nll;
}
