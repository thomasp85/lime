#' @rdname lime
#' @name lime
#' @param preprocess Function to transform a `character` vector to the format
#' expected from the model.
#' @param tokenization function used to tokenize text for the permutations.
#' @param keep_word_position set to `TRUE` if to keep order of words. Warning:
#' each word will be replaced by `word_position`.
#'
#' @examples
#' \dontrun{
#' # Explaining a model based on text data
#'
#' # Purpose is to classify sentences from scientific publications
#' # and find those where the team writes about their own work
#' # (category OWNX in the provided dataset).
#'
#' library(text2vec)
#' library(xgboost)
#'
#' data(train_sentences)
#' data(test_sentences)
#'
#' get_matrix <- function(text) {
#'   it <- itoken(text, progressbar = FALSE)
#'   create_dtm(it, vectorizer = hash_vectorizer())
#' }
#'
#' dtm_train = get_matrix(train_sentences$text)
#'
#' xgb_model <- xgb.train(list(max_depth = 7, eta = 0.1, objective = "binary:logistic",
#'                  eval_metric = "error", nthread = 1),
#'                  xgb.DMatrix(dtm_train, label = train_sentences$class.text == "OWNX"),
#'                  nrounds = 50)
#'
#' sentences <- head(test_sentences[test_sentences$class.text == "OWNX", "text"], 1)
#' explainer <- lime(train_sentences$text, xgb_model, get_matrix)
#' explanations <- explain(sentences, explainer, n_labels = 1, n_features = 2)
#'
#' # We can see that many explanations are based
#' # on the presence of the word `we` in the sentences
#' # which makes sense regarding the task.
#' print(explanations)
#' }
#' @importFrom assertthat assert_that is.flag
#' @export
lime.character <- function(x, model, preprocess = NULL, tokenization = default_tokenize, keep_word_position = FALSE, ...) {
  if (is.null(preprocess)) preprocess <- function(x) x
  assert_that(is.function(preprocess))
  assert_that(is.function(tokenization))
  assert_that(is.flag(keep_word_position))
  assert_that(!is.null(model))

  explainer <- c(as.list(environment()), list(...))
  explainer$x <- NULL

  structure(explainer, class = c('text_explainer', 'explainer', 'list'))
}
#' @rdname explain
#' @name explain
#'
#' @param single_explanation A boolean indicating whether to pool all text in
#' `x` into a single explanation.
#'
#' @importFrom assertthat assert_that is.count
#' @export
explain.character <- function(x, explainer, labels = NULL, n_labels = NULL,
                              n_features, n_permutations = 5000,
                              feature_select = 'auto',
                              single_explanation = FALSE, ...) {
  assert_that(is.text_explainer(explainer))
  m_type <- model_type(explainer)
  o_type <- output_type(explainer)
  if (m_type == 'regression') {
    if (!is.null(labels) || !is.null(n_labels)) {
      warning('"labels" and "n_labels" arguments are ignored when explaining regression models')
    }
    n_labels <- 1
    labels <- NULL
  }
  assert_that(is.null(labels) + is.null(n_labels) == 1, msg = "You need to choose between labels and n_labels parameters.")
  assert_that(is.count(n_features))
  assert_that(is.count(n_permutations))

  if (single_explanation) {
    perm_per_case <- ceiling(n_permutations/length(x))
    case_perm <- permute_cases(x, perm_per_case, explainer$tokenization, explainer$keep_word_position)
    assert_that(length(case_perm$permutations) == n_permutations, msg = "Incorrect number of permutations")
    case_ind <- list(seq_along(case_perm$permutations))
  } else {
    case_perm <- permute_cases(x, n_permutations, explainer$tokenization, explainer$keep_word_position)
    assert_that(length(case_perm$permutations) == n_permutations * length(x), msg = "Incorrect number of permutations")
    case_ind <- local({
      case_range <- seq_along(x)
      case_ids <- unlist(lapply(case_range, rep, n_permutations))
      split(seq_along(case_perm$permutations), case_ids)
    })
  }
  permutations_tokenized <- explainer$preprocess(case_perm$permutations)
  case_res <- predict_model(x = explainer$model, newdata = permutations_tokenized, type = o_type, ...)
  case_res <- set_labels(case_res, explainer$model)
  assert_that(all(!is.na(case_res)), msg = "Predictions contains some NAs")
  assert_that(nrow(case_res) == length(case_perm$permutations), msg = "Incorrect number of predictions")

  res <- lapply(seq_along(case_ind), function(ind) {
    i <- case_ind[[ind]]
    res <- model_permutations(case_perm$tabular[i, ], case_res[i, , drop = FALSE], case_perm$permutation_distances[i], labels, n_labels, n_features, feature_select)
    res$feature_value <- res$feature
    res$feature_desc <- res$feature
    res$case <- ind
    res$label_prob <- unname(as.matrix(case_res[i[1], ]))[match(res$label, colnames(case_res))]
    res$data <- x[ind]
    res$prediction <- list(as.list(case_res[i[1], ]))
    res$model_type <- m_type
    res
  })
  res <- do.call(rbind, res)
  res <- res[, c('model_type', 'case', 'label', 'label_prob', 'model_r2', 'model_intercept', 'model_prediction', 'feature', 'feature_value', 'feature_weight', 'feature_desc', 'data', 'prediction')]
  if (m_type == 'regression') {
    res$label <- NULL
    res$label_prob <- NULL
    res$prediction <- unlist(res$prediction)
  }
  as_tibble(res)
}
is.text_explainer <- function(x) inherits(x, 'text_explainer')
#' Default function to tokenize
#'
#' This tokenizer uses [stringi::stri_split_boundaries()] to tokenize a
#' `character` vector. To be used with [explain.character()`.
#'
#' @param text text to tokenize as a `character` vector
#' @return a `character` vector.
#' @importFrom stringi stri_split_boundaries
#' @export
#'
#' @examples
#' data('train_sentences')
#' default_tokenize(train_sentences$text[1])
#'
default_tokenize <- function(text) {
  unlist(stri_split_boundaries(text, type = "word", skip_word_none = TRUE))
}

#' Load an example text explanation
#'
#' @return A data.frame containing an example of a text explanation
#'
#' @keywords internal
#' @export
.load_text_example <- function() {
  readRDS(system.file('extdata', 'text_explanation.rds', package = 'lime'))
}
