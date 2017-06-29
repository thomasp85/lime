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

#' @importFrom Matrix Matrix
#' @importFrom purrr map map2 flatten_chr set_names flatten flatten_int map_chr flatten_dbl
#' @importFrom stringdist seq_dist
#' @importFrom magrittr %>% set_colnames
permute_cases.character <- function(cases, n_permutations, tokenization, keep_word_position, dist_fun) {
  documents_tokens <- map(cases, tokenization) %>%
{d_tokens <- . ;
  map2(d_tokens, lengths(d_tokens) %>% cumsum() %>% head(., length(.) - 1) %>% c(0, .),
       ~ {if (keep_word_position) paste0(.x, "_",  seq_along(.) + .y) %>% set_names(.x) else unique(.x) %>% set_names(., .)})}

  tokens <- documents_tokens %>%
    flatten_chr() %>%
    {.[!duplicated(.)]} %>% # unique() would remove names
    sort(decreasing = FALSE)

  tokens_for_external_model <- names(tokens)

  documents_tokens <- documents_tokens %>%
    map(~ which(tokens %in% .))

  dict_size <- length(tokens)

  # Perf on this part should be improved
  word_selections <- documents_tokens %>%
    map(~ {document <- . ; sample(length(document), as.integer((n_permutations/length(cases)) - 1), replace = T) %>%
      map(~ sample(document, ., replace = F) %>% sort) %>%
      c(list(document), .)})

  word_selections_flatten <- flatten(word_selections)

  matrix_cols <- seq(dict_size)

  bow_matrix <- word_selections_flatten %>%
    map(~ matrix_cols %in% .) %>%
    flatten_int() %>%
    Matrix(ncol = dict_size, sparse = TRUE, byrow = TRUE) %>%
    set_colnames(tokens)

  permutation_candidates <- word_selections_flatten %>% map(~ tokens_for_external_model[.]) %>% map_chr(~ paste(., collapse = " "))

  permutation_distances <- map2(word_selections, documents_tokens,
                                ~ seq_dist(.x, .y, method = dist_fun)) %>%
    flatten_dbl()

  list(tabular = bow_matrix,
       permutations = permutation_candidates,
       word_selections = word_selections,
       tokens = tokens,
       permutation_distances = permutation_distances)
}

globalVariables(".")
