permute_cases <- function(cases, n_permutations, ...) {
  UseMethod('permute_cases')
}
#' @importFrom stats rnorm runif
permute_cases.data.frame <- function(cases, n_permutations, feature_distribution, bin_continuous, bin_cuts, use_density) {
  nrows <- nrow(cases) * n_permutations
  perm <- as.data.frame(lapply(seq_along(cases), function(i) {
    perms <- if (is.numeric(cases[[i]]) && identical(is.na(feature_distribution[[i]]), TRUE)) {
      rep(cases[[i]], each = n_permutations)
    } else if (is.numeric(cases[[i]]) && bin_continuous) {
      bin <- sample(seq_along(feature_distribution[[i]]), nrows, TRUE, as.numeric(feature_distribution[[i]]))
      diff(bin_cuts[[i]])[bin] * runif(nrows) + bin_cuts[[i]][bin]
    } else if (is.numeric(cases[[i]]) && use_density) {
      width <- diff(feature_distribution[[i]]$x[c(1, 2)]) / 2
      sample(feature_distribution[[i]]$x, nrows, TRUE, feature_distribution[[i]]$y) + runif(nrows, -width, width)
    } else if (is.numeric(cases[[i]])) {
      rnorm(nrows) * feature_distribution[[i]]['sd'] + feature_distribution[[i]]['mean']
    } else if (is.character(cases[[i]])) {
      sample(names(feature_distribution[[i]]), nrows, TRUE, as.numeric(feature_distribution[[i]]))
    } else if (is.logical(cases[[i]])) {
      sample(as.logical(names(feature_distribution[[i]])), nrows, TRUE, as.numeric(feature_distribution[[i]]))
    } else if (is.factor(cases[[i]])) {
      x <- sample(names(feature_distribution[[i]]), nrows, TRUE, as.numeric(feature_distribution[[i]]))
      factor(x, levels = names(feature_distribution[[i]]))
    } else if (inherits(cases[[i]], 'Date') || inherits(cases[[i]], 'POSIXt')) {
      rep(cases[[i]], each = n_permutations)
    }
    if (is.integer(cases[[i]])) {
      as.integer(round(perms))
    } else {
      perms
    }
  }), stringsAsFactors = FALSE)
  names(perm) <- names(cases)
  perm[seq.int(1, by = n_permutations, length.out = nrow(cases)), ] <- cases
  perm
}

#' @importFrom Matrix Matrix sparseMatrix
permute_cases.character <- function(cases, n_permutations, tokenization, keep_word_position) {
  documents_tokens <- local({
    tokenized_cases <- lapply(cases, tokenization)
    token_end_positions <- cumsum(lengths(tokenized_cases))
    token_start_positions <-  c(0, head(token_end_positions, -1))

    get_token_position <- function(tokenized_case, token_start_position) {
      if (keep_word_position) {
        tokens <- paste0(tokenized_case, "_",  seq_along(tokenized_case) + token_start_position)
        names(tokens) <- tokenized_case
        tokens
      } else {
        tokens <- unique(tokenized_case)
        names(tokens) <- tokens
        tokens
      }
    }

    mapply(get_token_position, tokenized_cases, token_start_positions, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  })

  tokens <- local({
    documents_tokens_flatten <- unlist(documents_tokens)
    # unique() would remove names
    documents_tokens_flatten[!duplicated(documents_tokens_flatten)]
  })

  tokens_for_external_model <- names(tokens)

  documents_tokens <- lapply(documents_tokens, function(x) which(tokens %in% x))

  word_selections <- lapply(documents_tokens, function(x) get_index_permutations(x, n_permutations))

  word_selections_flatten <- unlist(word_selections, recursive = FALSE)

  bow_matrix <- local({
    to_repeat <- lengths(word_selections_flatten)
    rows_index <- seq(word_selections_flatten)
    i <- rep(rows_index, to_repeat)
    j <- unlist(word_selections_flatten)
    sparseMatrix(i, j, x = 1)
  })

  colnames(bow_matrix) <- tokens

  permutation_candidates <- sapply(word_selections_flatten, function(x) paste(tokens_for_external_model[x], collapse = " "), USE.NAMES = FALSE)

  word_indexes_2_logical_vector <- function(doc) seq(tokens) %in% doc

  bow_indexes_per_document <-  local({
    document_end_positions <- cumsum(rep(n_permutations, length(cases)))
    document_start_positions <- c(1, head(document_end_positions, -1) + 1)
    mapply(c, document_start_positions, document_end_positions, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  })

  permutation_distances <- local({
    compute_distances <- function(documents, indexes) {
      boolean_document_representation <- word_indexes_2_logical_vector(documents)
      start_index <- indexes[1]
      end_index <- indexes[2]
      cosine_distance_vector_to_matrix_rows(boolean_document_representation, bow_matrix[start_index:end_index,])
    }
    distances <- mapply(compute_distances , documents_tokens,bow_indexes_per_document)
    unlist(distances)
  })

  list(tabular = bow_matrix,
       permutations = permutation_candidates,
       word_selections = word_selections,
       tokens = tokens,
       permutation_distances = permutation_distances)
}

# Compute distances between a dense vector and a sparse matrix
# @param vector dense integer vector
# @param sparse_matrix a sparse matrix of permutations
cosine_distance_vector_to_matrix_rows  <- function(vector, sparse_matrix) {
  vector <- vector / c(sqrt(crossprod(vector))) # uses c() to avoid a warning
  as.vector(sparse_matrix %*% vector / sqrt(rowSumsSq(sparse_matrix)))
}
