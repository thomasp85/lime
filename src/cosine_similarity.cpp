// [[Rcpp::depends(RcppEigen)]]
#include <Rcpp.h>
#include <RcppEigen.h>
#include "lime_types.h"

using namespace Rcpp;

// https://codereview.stackexchange.com/questions/159396/cosine-similarity-of-one-vector-with-many
// Modified to manage sparse matrix with Eigen
// @param sparse_matrix the matrix to compute distance on.
// [[Rcpp::export]]
NumericVector rowSumsSq(MSpMat sparse_matrix) {
  int nrow = sparse_matrix.rows(), ncol = sparse_matrix.cols();
  NumericVector out(nrow);

  for (int col_index = 0; col_index < ncol; ++col_index) {
    for (InIterMat sparse_row_index(sparse_matrix, col_index); sparse_row_index; ++sparse_row_index){
      out[sparse_row_index.index()] += std::pow(sparse_row_index.value(), 2);
    }
  }
  return out;
}


/*** R
m <- Matrix::Matrix(1:81, ncol = 9, sparse = TRUE)
testthat::expect_equal(rowSumsSq(m), c(17181, 17856, 18549, 19260, 19989, 20736, 21501, 22284, 23085))
*/
