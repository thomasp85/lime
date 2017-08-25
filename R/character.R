#' @describeIn lime Method for explaining text data
#' @param preprocess Function to transform [character] vector to feature
#' provided to the model to explain
#' @param tokenization function used to tokenize text
#' @param keep_word_position set to \code{\link{TRUE}} if to keep order of words.
#' @param n_permutations number of permutations to perform. More gives better explanation up to a point where it is not usefull and takes too much time. (5000)
#' @param number_features_explain number of features used in the explanation. (\code{5})
#' @param feature_selection_method method to select the best features. (\code{"auto"})
#'
#' One of:
#' \itemize{
#' \item{"auto"}{ : If n_features <= 6 use \code{"forward_selection"} else use \code{"highest_weights"}.}
#' \item{"none"} {Ignore \code{n_features} and use all features.}
#' \item{"forward_selection"} {: Add one feature at a time until \code{n_features} is
#'       reached, based on quality of a ridge regression model.}
#' \item{"highest_weights"} {: Fit a ridge regression and select the \code{n_features} with
#'       the highest absolute weight.}
#' \item{"lasso_path"} {: Fit a lasso model and choose the \code{n_features} whose lars
#'       path converge to zero the latest.}
#'  \item{"tree"} {: Fit a tree to select \code{n_features}. It requires XGBoost.}
#' }
#' @param labels name of the label to explain (use only when model to explain predictions includes names as \code{\link{data.frame}} column names, like with \code{link{caret}}). (\code{\link{NULL}}).
#' @param n_labels instead of labels, number of labels to explain. (\code{\link{NULL}})
#' @param prediction function used to perform the prediction. Should have 2 variables, first for the \code{\link{character}} vector, second for the \code{model}. Should return a \code{\link{data.frame}} with the predictions.
#' @param keep_word_position set to `TRUE` if to keep order of words. Warning:
#' each word will be replaced by `word_position`.
#'
#' @examples
#' \dontrun{
#' # Explaining a model based on text data
#'
#' library(text2vec)
#' library(lime)
#' library(xgboost)
#'
#' data(train_sentences)
#' data(test_sentences)
#'
#' get.matrix <- function(text) {
#'   it <- itoken(text, progressbar = FALSE)
#'   create_dtm(it, vectorizer = hash_vectorizer())
#' }
#'
#' dtm_train = get.matrix(train_sentences$text)
#'
#' bst <- xgb.train(list(max_depth = 7, eta = 0.1, objective = "binary:logistic",
#'                  eval_metric = "error", nthread = 1),
#'                  xgb.DMatrix(dtm_train, label = train_sentences$class.text == "OWNX"),
#'                  nrounds = 50)
#'
#' lime(test_sentences[5, text], bst, get.matrix, n_labels = 1,
#'      number_features_explain = 2, keep_word_position = FALSE)()
#' }
#'
#' @return Return a function. To make only one call you can perform a currying like in \code{lime(...)(...)}.
#'
#' @importFrom dplyr bind_rows
#' @importFrom purrr is_empty is_scalar_logical is_null flatten_int
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that not_empty is.flag is.number is.count
#' @export
lime.character <- function(x, model, preprocess, tokenization = default_tokenize, keep_word_position = FALSE, ...) {

  assert_that(is.function(preprocess))
  assert_that(is.function(tokenization))
  assert_that(is.flag(keep_word_position))
  assert_that(!is.null(model))
  assert_that(not_empty(x))
  #assert_that(feature_selection_method %in% feature_selection_method())

  m_type <- model_type(model)
  output_type <- switch(
    m_type,
    classification = 'prob',
    regression = 'raw',
    stop(m_type, ' models are not supported yet', call. = FALSE)
  )

  function(cases, labels = NULL, n_labels = NULL, n_features, n_permutations = 5000, feature_select = 'auto') {
    if (m_type == 'regression') {
      if (!is.null(labels) || !is.null(n_labels)) {
        warning('"labels" and "n_labels" arguments are ignored when explaining regression models')
        n_labels <- 1
        labels <- NULL
      }
    }
    assert_that(is.null(labels) + is.null(n_labels) == 1, msg = "You need to choose between labels and n_labels parameters.")
    assert_that(is.count(n_features))
    assert_that(is.count(n_permutations))

    case_perm <- permute_cases(cases, n_permutations, tokenization, keep_word_position)
    case_res <- preprocess(case_perm$permutations) %>%
      predict_model(x = model, newdata = ., type = output_type)
    assert_that(length(case_perm$permutations) == n_permutations * length(cases))
    case_ind <- length(cases) %>% seq() %>% map(~ rep(.x, n_permutations)) %>% flatten_int() %>% split(seq_along(case_perm$permutations), .)

    res <- lapply(seq_along(case_ind), function(ind) {
      i <- case_ind[[ind]]
      res <- model_permutations(case_perm$tabular[i, ], case_res[i, ], case_perm$permutation_distances[i], labels, n_labels, n_features, feature_select)
      res$feature_value <- res$feature
      res$feature_desc <- res$feature
      res$case <- cases[ind]
      res$label_prob <- unname(as.matrix(case_res[i[1], ]))[match(res$label, colnames(case_res))]
      res$data <- cases[ind]
      res$prediction <- list(as.list(case_res[i[1], ]))
      res$model_type <- m_type
      res
    })
    res <- bind_rows(res)
    res <- res[, c('model_type', 'case', 'label', 'label_prob', 'model_r2', 'model_intercept', 'feature', 'feature_value', 'feature_weight', 'feature_desc', 'data', 'prediction')]
    if (m_type == 'regression') {
      res$label <- NULL
      res$label_prob <- NULL
      res$prediction <- unlist(res$prediction)
    }
    if (m_type == 'classification') {
      attr(res, "original_text") <- cases
    }
    res
  }
}

#' Default function to tokenize
#'
#' @description Use simple regex to tokenize a \code{\link{character}} vector. To be used with \code{\link{lime.character}}.
#' @param text text to tokenize as a \code{\link{character}} vector
#' @return a \code{\link{character}} vector.
#' @importFrom stringi stri_split_boundaries
#' @export
default_tokenize <- function(text) {
  stri_split_boundaries(text, type = "word", skip_word_none = TRUE) %>%
    flatten_chr()
}
