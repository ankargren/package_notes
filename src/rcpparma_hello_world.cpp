#include "template.h"

// [[Rcpp::export]]
arma::mat rcpparma_hello_world() {
    arma::mat m1, m2;
    rcpparma_fill(m1, m2);
    return m1 + 3 * (m1 + m2);
}

