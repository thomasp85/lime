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
#' #' @importFrom stringi stri_split stri_join_list
#' #' @importFrom Matrix Matrix
#' permute_cases.character <- function(cases, n_permutations, split_by, bow) {
#'   tokens <- stri_split(cases, regex = split_by)
#'   doc_size <- lengths(tokens)
#'   if (bow) {
#'     tokens_lower <- split(tolower(unlist(tokens)), rep(seq_along(tokens), doc_size))
#'     tokens_lookup <- lapply(tokens_lower, unique)
#'     doc_size <- lengths(lapply(tokens_lookup, unique))
#'   } else {
#'     tokens_lookup <- tokens
#'   }
#'   perm <- lapply(seq_along(tokens_lookup), function(i) {
#'     remove_n <- sample(doc_size[i], n_permutations, replace = FALSE)
#'     remove <- lapply(remove_n, sample, x = doc_size[i] - 1)
#'     permutations <- lapply(remove, function(ii) tokens_lookup[[i]][-ii])
#'     if (bow) {
#'       permutations <- lapply(permutations, function(words) tokens[[i]][!tokens_lower[[i]] %in% words])
#'       var_names <- tokens_lookup[[i]]
#'     } else {
#'       var_names <- paste0(seq_along(tokens_lookup[[i]]), '_', tokens_lookup[[i]])
#'     }
#'     complete <- rep(1, length(tokens_lookup[[i]]))
#'     remove <- do.call(rbind, lapply(remove, function(ii) {complete[ii] <- 0; complete}))
#'     remove[1, ] <- 1
#'     remove <- Matrix(remove, sparse = TRUE)
#'     colnames(remove) <- tokens_lookup[[i]]
#'     list(permutations, remove)
#'   })
#'   removes <- lapply(perm, `[[`, 2)
#'   perm <- lapply(perm, `[[`, 1)
#'   perm <- stri_join_list(unlist(perm, recursive = FALSE), sep = ' ')
#'   perm[seq.int(1, by = n_permutations, length.out = length(cases))] <- cases
#'   list(permutations = perm, tabular = removes)
#' }


#' @importFrom Matrix Matrix
#' @importFrom purrr map map2 flatten_chr set_names flatten flatten_int set_colnames map_chr flatten_dbl
#' @importFrom stringdist seq_dist
#' @importFrom magrittr %>%
permute_cases.character <- function(cases, n_permutations, split_by, bow, dist_fun) {
  documents.tokens <- cases %>% map(~ stri_split_regex(str = ., pattern = split_by, simplify = TRUE) %>% as.character()) %>%
  {d.tokens <- . ;
  map2(d.tokens, lengths(d.tokens) %>% cumsum() %>% head(., length(.) - 1) %>% c(0, .),
       ~ {if (bow) paste0(.x, "_",  seq_along(.) + .y) else unique(.x)})}

  tokens <- documents.tokens %>%
    flatten_chr() %>%
    unique(.) %>%
    sort(decreasing = FALSE) %>%
    set_names(seq(.))

  documents.tokens %<>%
    map(~ {d <- . ; which(tokens %in% d) %>%
      set_names(d, nm = .)})

  dict_size <- length(tokens)

  # Perf on this part should be improved
  word.selections <- documents.tokens %>%
    map(~ names(.) %>% as.integer()) %>%
    map(~ {document <- . ; sample(length(document), as.integer((n_permutations/length(cases)) - 1), replace = T) %>%
      map(~ sample(document, ., replace = F) %>% sort) %>%
      c(list(document), .)})

  word.selections.flatten <- flatten(word.selections)

  matrix.cols <- seq(dict_size)

  bow.matrix <- word.selections.flatten %>%
    map(~ matrix.cols %in% .) %>%
    flatten_int() %>%
    Matrix(ncol = dict_size, sparse = TRUE, byrow = TRUE) %>%
    set_colnames(tokens)

  permutation.candidates <- word.selections.flatten %>% map(~ tokens[.]) %>% map_chr(~ paste(., collapse = " "))

  permutation.distances <- map2(word.selections, documents.tokens, ~ seq_dist(.x, names(.y) %>% as.integer(), method = dist_fun)) %>%
    flatten_dbl()

  list(tabular = bow.matrix,
       permutations = permutation.candidates,
       word.selections = word.selections,
       tokens = tokens,
       permutation.distances = permutation.distances)
}

#' @importFrom purrr set_names
#' @export
do.predict <- function(model, data) {
  switch(class(model),
         "xgb.Booster" = predict(model, data, type = "prob", reshape = TRUE) %>% data.frame %>% set_names(seq(ncol(.))),
         predict(model, data, type = "prob")
  )
}
