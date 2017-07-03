permute_cases <- function(cases, n_permutations, ...) {
  UseMethod('permute_cases')
}
#' @importFrom stats rnorm runif
permute_cases.data.frame <- function(cases, n_permutations, feature_distribution, bin_continuous, bin_cuts) {
  nrows <- nrow(cases) * n_permutations
  perm <- as.data.frame(lapply(seq_along(cases), function(i) {
    if (is.numeric(cases[[i]]) && bin_continuous) {
      bin <- sample(seq_along(feature_distribution[[i]]), nrows, TRUE, as.numeric(feature_distribution[[i]]))
      diff(bin_cuts[[i]])[bin] * runif(nrows) + bin_cuts[[i]][bin]
    } else if (is.numeric(cases[[i]])) {
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

#' @importFrom Matrix Matrix sparseMatrix
#' @importFrom purrr map map2 flatten_chr set_names flatten flatten_int map_chr flatten_dbl accumulate
#' @importFrom stringdist seq_dist
#' @importFrom magrittr %>% set_colnames
permute_cases.character <- function(cases, n_permutations, tokenization, keep_word_position, dist_fun) {
  documents_tokens <- map(cases, tokenization) %>%
{d_tokens <- . ;
  map2(d_tokens, lengths(d_tokens) %>% cumsum() %>% head(., length(.) - 1) %>% c(0, .),
       ~ {if (keep_word_position) paste0(.x, "_",  seq_along(.x) + .y) %>% set_names(.x) else unique(.x) %>% set_names(., .)})}

  tokens <- documents_tokens %>%
    flatten_chr() %>%
    {.[!duplicated(.)]} # unique() would remove names

  tokens_for_external_model <- names(tokens)

  documents_tokens <- documents_tokens %>%
    map(~ which(tokens %in% .))

  number_permutations_per_document <- as.integer(n_permutations / length(cases))

  word_selections <- map(documents_tokens, ~ get_index_permutations(.x, number_permutations_per_document))

  word_selections_flatten <- flatten(word_selections)

  bow_matrix <- {
    to_repeat <- lengths(word_selections_flatten)
    rows_index <- seq(word_selections_flatten)
    i <- rep(rows_index, to_repeat)
    j <- flatten_int(word_selections_flatten)
    sparseMatrix(i, j, x = 1)
  } %>%
    set_colnames(tokens)

  permutation_candidates <- map_chr(word_selections_flatten, ~ paste(tokens_for_external_model[.x], collapse = " "))

  #permutation_distances_bis <- map2(word_selections, documents_tokens, ~ seq_dist(.x, .y, method = dist_fun)) %>%
    #flatten_dbl()

  word_indexes_2_logical_vector <- function(doc) seq(tokens) %in% doc

  bow_indexes_per_document <- length(cases) %>% rep(number_permutations_per_document, .) %>% purrr::accumulate(`+`) %>% map2(c(1, head(., -1) + 1), ., c)

  permutation_distances <- map2(documents_tokens, bow_indexes_per_document,
                                ~ word_indexes_2_logical_vector(.x) %>% cosine_distance_vector_to_matrix_rows(bow_matrix[.y[1]:.y[2],])) %>%
    flatten_dbl()

  list(tabular = bow_matrix,
       permutations = permutation_candidates,
       word_selections = word_selections,
       tokens = tokens,
       permutation_distances = permutation_distances)
}

#' Compute distances between a dense vector and a sparse matrix
#' @param vector dense integer vector
#' @param sparse_matrix a sparse matrix of permutations
cosine_distance_vector_to_matrix_rows  <- function(vector, sparse_matrix) {
  vector <- vector / c(sqrt(crossprod(vector))) # use c() to avoid a warning
  1 - as.vector(sparse_matrix %*% vector / sqrt(rowSumsSq(sparse_matrix)))
}

#' @useDynLib lime
#' @importFrom Rcpp sourceCpp
NULL

globalVariables(".")
