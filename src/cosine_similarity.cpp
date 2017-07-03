// [[Rcpp::depends(RcppEigen)]]
#include <Rcpp.h>
#include <RcppEigen.h>
#include "lime_types.h"

using namespace Rcpp;

// https://codereview.stackexchange.com/questions/159396/cosine-similarity-of-one-vector-with-many
// [[Rcpp::export]]
NumericVector rowSumsSq(MSpMat x) {
  int nrow = x.rows(), ncol = x.cols();
  NumericVector out(nrow);

  for (int j = 0; j < ncol; ++j) {
    for (InIterMat i_(x, j); i_; ++i_){
      out[i_.index()] += std::pow(i_.value(), 2);
    }
  }

  return out;
}


/*** R
m <- Matrix::Matrix(1:81, ncol = 9, sparse = TRUE)
testthat::expect_equal(rowSumsSq(m), c(17181, 17856, 18549, 19260, 19989, 20736, 21501, 22284, 23085))
*/
