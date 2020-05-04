#include <TMB.hpp>

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

  // TODO: figure out how to properly make a cov-matrix
  PARAMETER_VECTOR(L_theta);   // rho = rho(theta)
  PARAMETER_VECTOR(log_sds_theta); // sqrt-variance diagonal

  // Parametrization based off https://github.com/jtufto/tmb-var1/
  PARAMETER_VECTOR(L_D);
  PARAMETER_VECTOR(log_sds_D); // Phi = Phi(eigentheta, eigenlambda)

  /* Variables (un-parameterized) */
  vector<Type> sds_theta = exp(log_sds_theta);
  vector<Type> sds_D = exp(log_sds_D);

  int d = sds_theta.rows(); // 2

  /* Procedure section */

  // We construct (vec)Sigma,
  // the cov matrix of the gaussian noise in the VAR(1) series
  matrix<Type> Rho_theta(d, d);
  Rho_theta = density::UNSTRUCTURED_CORR(L_theta).cov();

  matrix<Type> Rho_D(d, d);
  Rho_D = density::UNSTRUCTURED_CORR(L_D).cov();

  matrix<Type> theta(d, d);
  vector<Type> vecD(d*d);

  // Matrix.reshaped() is not supported until Eigen 3.4
  // (unreleased as of 2020-04-13)
  for (int i=0; i<d; i++) for (int j=0; j<d; j++) {
    // cascading assignment
    theta(i,j) = sds_theta(i) * Rho_theta(i,j) * sds_theta(j);
    vecD(i+j*d) = sds_D(i) * Rho_D(i,j) * sds_D(j);
  }

  // We construct (vec)Gamma_0, where we have the relation
  // vec(Gamma0) = (I - Phi ⊗ Phi).inv() * vec(Sigma)
  // where ⊗ is the kronecker product

  // Initialize identity(4) matrix
  matrix<Type> I2(d, d); I2.setIdentity();
  matrix<Type> I4(d*d, d*d); I4.setIdentity();

  // A ⊕ B = A ⊗ I + I ⊗ B
  matrix<Type> two_theta = kronecker(theta, I2) + kronecker(I2, theta);
  matrix<Type> inv_two_theta = two_theta.inverse();


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

    matrix<Type> vecGamma = inv_two_theta * vecD;

    // Gamma0 = matricize(vec(Gamma0))
    // Matrix.reshaped() is not supported until Eigen 3.4
    // (unreleased as of 2020-04-13)
    matrix<Type> Gamma(d,d);
    for (int i=0; i<d; i++)
      for (int j=0; j<d; j++)
        Gamma(i,j) = vecGamma(i+j*d);


    nll += density::MVNORM(Gamma)(x0);
  }


  /* VAR(1) noise error */
  for (int t=0; t < teams; t++) for (int r=1; r < rounds; r++) {
    // No array.row() method so we transpose to access inner dimension
    vector<Type> xp = A.transpose().col(r-1).col(t);
    vector<Type> xn = A.transpose().col(r).col(t);
    Type dt = time_diffs(r, t);

    // explicit typecasting is required
    vector<Type> mu = expm((matrix<Type>)(-theta*dt)) * xp;
    vector<Type> vecGamma = (matrix<Type>)(inv_two_theta * (I4-expm((matrix<Type>)(-inv_two_theta*dt)))) * vecD;

    // Gamma0 = matricize(vec(Gamma0))
    // Matrix.reshaped() is not supported until Eigen 3.4
    // (unreleased as of 2020-04-13)
    matrix<Type> Gamma(d,d);
    for (int i=0; i<d; i++)
      for (int j=0; j<d; j++)
        Gamma(i,j) = vecGamma(i+j*d);

    nll += density::MVNORM(Gamma)(xn - mu);
  }

  return nll;
}
