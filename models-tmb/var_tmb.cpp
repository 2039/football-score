#include <TMB.hpp>

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

  int d = sds.rows();

  /* Procedure section */

  // We construct (vec)Sigma,
  // the cov matrix of the gaussian noise in the VAR(1) series
  matrix<Type> Rho(d, d);
  Rho = density::UNSTRUCTURED_CORR(theta).cov();

  matrix<Type> Sigma_w(d,d);
  vector<Type> vecSigma_w(d*d);

  // Matrix.reshaped() is not supported until Eigen 3.4
  // (unreleased as of 2020-04-13)
  for (int i=0; i<d; i++)
    for (int j=0; j<d; j++)
      // cascading assignment
      Sigma_w(i,j) = vecSigma_w(i+j*d) = sds(i)*Rho(i,j)*sds(j);


  // // We construct Phi, the coefficient matrix of the VAR(1) series
  matrix<Type> eigvec(d,d);
  eigvec.row(0) << cos(eigtheta(0)), cos(eigtheta(1));
  eigvec.row(1) << sin(eigtheta(0)), sin(eigtheta(1));

  matrix<Type> D(d,d);
  D.row(0) << eiglambda(0),            0;
  D.row(1) <<            0, eiglambda(1);

  matrix<Type> Phi = eigvec * D * eigvec.inverse(); ADREPORT(Phi);


  // We construct (vec)Gamma_0, where we have the relation
  // vec(Gamma0) = (I - Phi ⊗ Phi).inv() * vec(Sigma)
  // where ⊗ is the kronecker product

  // Initialize identity(4) matrix
  matrix<Type> I4(d*d, d*d); I4.setIdentity();

  vector<Type> vecGamma0 =
    (matrix<Type>)(I4 - kronecker(Phi, Phi)).inverse() * vecSigma_w;

  // Gamma0 = matricize(vec(Gamma0))
  // Matrix.reshaped() is not supported until Eigen 3.4
  // (unreleased as of 2020-04-13)
  matrix<Type> Gamma0(d,d);
  for (int i=0; i<d; i++)
    for (int j=0; j<d; j++)
      Gamma0(i,j) = vecGamma0(i+j*d);



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
