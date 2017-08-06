#include <Rcpp.h>
using namespace Rcpp;


// Generate permutations of a vector of indexes.
//
// First permutation is the original (full) document.
//
// @param original_document Index of words in a text document.
// @param number_permutations number of permutation to generate.
// @return a list of permutations. The permutation sizes are not all the same.
// [[Rcpp::export]]
List get_index_permutations(IntegerVector original_document, int number_permutations) {
  List result(number_permutations);
  IntegerVector nb = sample(original_document.size(), number_permutations, true);
  // Add original document as the first permutation
  result[0] = original_document;
  for(int i = 1; i < number_permutations; i++){
    IntegerVector v = sample(original_document, nb[i], false);
    std::sort(v.begin(), v.end());
    result[i] = v;
  }

  return result;
}



/*** R
system.time(a <- get_index_permutations(c(1,2,6), 1e6))
*/
