#include "util.cpp"

template <class T>
matrix<T> VAR_coeff(vector<T> eigtheta, vector<T> eiglambda) {
  int d = eigtheta.rows();

  matrix<T> eigvec(d,d);

  eigvec.row(0) << cos(eigtheta(0)), cos(eigtheta(1));
  eigvec.row(1) << sin(eigtheta(0)), sin(eigtheta(1));

  matrix<T> D(d,d);
  D.row(0) << eiglambda(0),            0;
  D.row(1) <<            0, eiglambda(1);

  matrix<T> Phi = eigvec * D * eigvec.inverse();

  return Phi;
}


template <class T>
matrix<T> VAR_covariance(matrix<T> Phi, matrix<T> Sigma_w) {
  // We construct (vec)Gamma_0, where we have the relation
  // vec(Gamma0) = (I - Phi ⊗ Phi).inv() * vec(Sigma)
  // where ⊗ is the kronecker product
  vector<T> vecSigma_w = Sigma_w.vec();
  int d = Sigma_w.rows();

  matrix<T> Gamma0 = (matrix<T>)(I<T>(4) - kronecker(Phi, Phi)).inverse() * vecSigma_w;

  // Gamma0 = matricize(vec(Gamma0))
  Gamma0.resize(d,d);

  return Gamma0;
}
