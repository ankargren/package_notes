#include "RcppArmadillo.h"

inline void rcpparma_fill(arma::mat & m1, arma::mat & m2) {
  m1 = arma::eye<arma::mat>(3, 3);
  m2 = arma::eye<arma::mat>(3, 3);
}