#include <TMB.hpp>
#include "CVAR.cpp"

// https://kaskr.github.io/adcomp/structarray.html#a73c905d02300879a7c8ed576f2c1d84f

template<class Type>
Type objective_function<Type>::operator() ()
{
  /* Data section */
  DATA_IARRAY(matches);
  DATA_IVECTOR(times);

  /* Random effect section */
  // A(round, team, strength)
  PARAMETER_ARRAY(B);

  /* Parameter section */
  PARAMETER(gamma);
  PARAMETER(mu);

  // TODO: figure out how to properly make a cov-matrix
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
  for (int match=0; match < matches.rows(); match++) {
    // No array.row() method so we transpose to access inner dimension
    //vector<int> match_stats = matches.row(match);

    ////
    // Variables
    Type home_score = matches(match, 0, 0);
    Type away_score = matches(match, 1, 0);

    // this would be so much prettier/readable with tuple assignment ...
    Type home_lambda_log = B(match, 0, 0) - B(match, 1, 1) + gamma + mu;
    Type away_lambda_log = B(match, 1, 0) - B(match, 0, 1) - gamma + mu;

    nll -= dpois(home_score, exp(home_lambda_log), true);
    nll -= dpois(away_score, exp(away_lambda_log), true);


    ////
    // Random effects
    for (int role=0; role < 2; role++) {
      //vector<int> team = match_stats.col(role);

      int prev_match = matches(match, role, 1);
      int prev_role = matches(match, role, 2);

      // This should be allowed as long as variable`matches` is a DATA_* type
      if (prev_match == -1) /* => team's match */ {
        vector<Type> x0 = B.row(match).row(role);
        matrix<Type> Gamma = cvar.Gamma();

        nll += density::MVNORM(Gamma)(x0);

      } else /* => team has played a previous round */ {
        vector<Type> xp = B.row(prev_match).row(prev_role);
        vector<Type> xn = B.row(match).row(role);

        Type dt = times(match) - times(prev_match);

        // explicit typecasting is required
        vector<Type> mu = cvar.mu(xp, dt);
        matrix<Type> Gamma = cvar.Gamma(dt);

        nll += density::MVNORM(Gamma)(xn - mu);
      }
    }
  }

  return nll;
}
