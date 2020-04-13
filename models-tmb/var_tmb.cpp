#include <TMB.hpp>

// https://kaskr.github.io/adcomp/structarray.html#a73c905d02300879a7c8ed576f2c1d84f

template<class Type>
Type objective_function<Type>::operator() ()
{
  /* Data section */
  DATA_IMATRIX(stats);

  /* Random effect section */
  // PARAMETER_VECTOR(alpha);
  // PARAMETER_VECTOR(beta);
  PARAMETER_ARRAY(A); // A[0,...] = alpha; A[1,...] = beta

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
  int d = 2;


  // Subclass of MVNORM_t
  // density::UNSTRUCTURED_CORR_t<Type> NEG_LOG_MVNORM_UNSCALED(theta);

  // vecSigma
  matrix<Type> Rho(d, d);
  Rho = density::UNSTRUCTURED_CORR(theta).cov();
  matrix<Type> Sigma_w(d,d);
  vector<Type> vecSigma_w(d*d);
  for (int i=0; i<d; i++)
    for (int j=0; j<d; j++)
      // cascading assignment
      Sigma_w(i, j) = vecSigma_w(i+j*d) = sds(i)*Rho(i,j)*sds(j);


  // Phi
  matrix<Type> eigvec(d,d);
  eigvec.row(0) << cos(eigtheta(0)), cos(eigtheta(1));
  eigvec.row(1) << sin(eigtheta(0)), sin(eigtheta(1));

  matrix<Type> D(d,d);
  D.row(0) << eiglambda(0),            0;
  D.row(1) <<            0, eiglambda(1);

  // TODO: verify if eigvec.inverse() == eigvec.transpose()
  matrix<Type> Phi = eigvec * D * eigvec.inverse(); ADREPORT(Phi);

  // vec(Gamma_0)(Phi, Sigma_w) = Ainv * vec(Sigma)
  // K = I_4 - kronecker(Phi, Phi)
  matrix<Type> K(d*d, d*d);


  // I_4
  for (int i=0; i<d*d; i++)
    K(i,i) += 1;

  for (int i=0; i<d; i++)
    for (int j=0; j<d; j++)
      for (int k=0; k<d; k++)
        for (int l=0; l<d; l++)
          K(i*d+k, j*d+l) = -Phi(i,j) * Phi(k,l);
  matrix<Type> Kinv = K.inverse();


  vector<Type> vecGamma0 = Kinv*vecSigma_w;

  // Gamma_0 = matricize(vec(Gamma_0))
  // TODO check if there exists reshape function in eigen
  matrix<Type> Gamma0(d,d);
  for (int i=0; i<d; i++)
    for (int j=0; j<d; j++)
      Gamma0(i,j) = vecGamma0(i+j*d);


  /* Procedure section */

  // dimensions
  // TODO assign dynamically
  int rounds = 30;
  int teams = 16;
  int pairs = 8; // teams/2

  // Initialize value for negative-log-likelihood (nll)
  Type nll = 0;

  /* poisson(lambda) error */
  for (int s=0; s < 2*rounds*pairs; s++) {
    vector<int> round_stats = stats.row(s);


    // this would be so much prettier/readable in python ...
    int home_team = round_stats(0);
    int away_team = round_stats(1);
    int round = round_stats(2);
    int is_home = round_stats(3); // :: {-1, 1}
    Type score = round_stats(4);  // BUG dpois cant take <int> in first argument


    Type llambda = A(round, home_team, 0) + A(round, away_team, 1) + is_home * gamma + mu;


    nll += -dpois(score, exp(llambda), true);
  }


  /* VAR(1) initialization error */
  for (int t=0; t < teams; t++) {
    // No array.row() method so we must transpose
    vector<Type> x0 = A.transpose().col(0).col(t);
    nll += density::MVNORM(Gamma0)( x0 );
  }

  /* VAR(1) error */
  for (int r=1; r < rounds; r++) for (int t; t < teams; t++) {
    // No array.row() method so we must transpose
    vector<Type> xp = A.transpose().col(r).col(t-1);
    // cascading assignment
    vector<Type> xn = A.transpose().col(r).col(t) = Phi * xn;

    vector<Type> w = xp - xn;

    nll += density::MVNORM(Sigma_w)(w);
  }

  return nll;
}
