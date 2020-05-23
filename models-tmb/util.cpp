template <class T>
matrix<T> I(int n) { matrix<T> I(n, n); I.setIdentity(); return I; }


template <class T>
matrix<T> covariance(vector<T> theta, vector<T> sds) {
  matrix<T> Rho = density::UNSTRUCTURED_CORR(theta).cov();
  int d = sds.rows();

  matrix<T> Sigma(d,d);
  for (int i=0; i<d; i++)
    for (int j=0; j<d; j++)
      Sigma(i,j) = sds(i)*Rho(i,j)*sds(j);

  return Sigma;
}
