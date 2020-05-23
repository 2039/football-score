#include "util.cpp"

template <class T>
struct CVAR {
  const matrix<T> theta;
  const matrix<T> vecD;
  const Eigen::Index d; // ???? error if int
  const matrix<T> inv_two_theta;
  const matrix<T> Idd;

  CVAR(matrix<T> theta, matrix<T> D) :
    theta{theta},
    vecD{D.vec()},
    d{D.rows()},
    inv_two_theta{make_inv_two_theta(theta)},
    Idd{I<T>(d*d)}
  {};

  matrix<T> make_inv_two_theta (matrix<T> theta) {
      // Initialize identity(2) matrix
      matrix<T> Id = I<T>(d);

      // A ⊕ B = A ⊗ I + I ⊗ B
      matrix<T> two_theta = kronecker(theta, Id) + kronecker(Id, theta);

      // invert
      return two_theta.inverse();
  }


  matrix<T> Gamma () {
    matrix<T> Gamma = inv_two_theta * vecD;

    Gamma.resize(d,d);

    return Gamma;
  }

  matrix<T> Gamma (T dt) {
    // explicit typecasting is required
    matrix<T> Gamma = (matrix<T>)(inv_two_theta * (Idd-expm((matrix<T>)(-inv_two_theta*dt)))) * vecD;

    Gamma.resize(d,d);

    return Gamma;
  }

  matrix<T> mu (vector<T> x0, T dt) {
    return expm((matrix<T>)(-theta*dt)) * x0;
  }
};
