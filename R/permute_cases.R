permute_cases <- function(cases, n_permutations, ...) {
  UseMethod('permute_cases')
}
#' @importFrom stats rnorm
permute_cases.data.frame <- function(cases, n_permutations, feature_distribution) {
  nrows <- nrow(cases) * n_permutations
  perm <- as.data.frame(lapply(seq_along(cases), function(i) {
    if (is.numeric(cases[[i]])) {
      rnorm(nrows) * feature_distribution[[i]]['sd'] + feature_distribution[[i]]['mean']
    } else if (is.character(cases[[i]])) {
      sample(names(feature_distribution[[i]]), nrows, TRUE, as.numeric(feature_distribution[[i]]))
    } else if (is.factor(cases[[i]])) {
      x <- sample(names(feature_distribution[[i]]), nrows, TRUE, as.numeric(feature_distribution[[i]]))
      factor(x, levels = names(feature_distribution[[i]]))
    }
  }), stringsAsFactors = FALSE)
  names(perm) <- names(cases)
  perm[seq.int(1, by = n_permutations, length.out = nrow(cases)), ] <- cases
  perm
}
