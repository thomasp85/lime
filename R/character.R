#' @describeIn lime Method for explaining text data
#' @param preprocess Function to transform \code{\link{character}} vector to feature provided to the model to explain
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
#' @importFrom purrr is_empty is_scalar_logical is_null
#' @importFrom magrittr %>%
#' @importFrom assertthat validate_that not_empty is.scalar
#' @export
lime.character <- function(x, model, preprocess, tokenization = default_tokenize, keep_word_position = FALSE, kernel_width = 25,
                           n_permutations = 5000, number_features_explain = 5, feature_selection_method = "auto",
                           labels = NULL, n_labels = NULL,  prediction = default_predict, ...) {

  validate_that("function" %in% class(preprocess))
  validate_that("function" %in% class(tokenization))
  validate_that("function" %in% class(prediction))
  validate_that(is_scalar_logical(keep_word_position))
  validate_that(is_null(labels) + is.null(n_labels) == 1, msg = "You need to choose between labels and n_labels parameters.")
  validate_that(!is_null(model))
  not_empty(x)
  validate_that(feature_selection_method %in% feature_selection_method())
  is.scalar(number_features_explain)
  validate_that(number_features_explain >= 1)
  is.scalar(n_permutations)
  validate_that(n_permutations >= 1)
  is.scalar(kernel_width)
  validate_that(kernel_width >= 1)

  function() {
    permutation_cases <- permute_cases(x, n_permutations, tokenization, keep_word_position)
    predicted_labels_dt <- preprocess(permutation_cases$permutations) %>% prediction(model)
    validate_that("data.frame" %in% class(predicted_labels_dt))
    tib <- model_permutations(x = permutation_cases$tabular, y = predicted_labels_dt,
                       weights = exp_kernel(kernel_width)(permutation_cases$permutation_distances),
                       labels = labels, n_labels = n_labels, n_features = number_features_explain,
                       feature_method = feature_selection_method)
    attr(tib, "original_text") <- x
    tib
  }
}

#' @title Default function to perform the prediction
#'
#' @description Takes care of performing the prediction and adapting the format of the result. To be used with \code{\link{lime.character}}.
#' @param data data to be explained (as \code{\link{character}} vector).
#' @param model model to be explained
#' @importFrom purrr set_names
#' @export
default_predict <- function(data, model) {
  switch(class(model),
         "xgb.Booster" = predict(model, data, type = "prob", reshape = TRUE) %>%
           data.frame %>% set_names(seq(ncol(.))),
         predict(model, data, type = "prob")
  )
}

#' @title Default function to tokenize
#'
#' @description Use simple regex to tokenize a \code{\link{character}} vector. To be used with \code{\link{lime.character}}.
#' @param text text to tokenize as a \code{\link{character}} vector
#' @return a \code{\link{character}} vector.
#' @importFrom stringi stri_split_boundaries
#' @importFrom magrittr %>% set_colnames
#' @export
default_tokenize <- function(text) {
  stri_split_boundaries(text, type = "word", skip_word_none = TRUE) %>%
    flatten_chr()
}

globalVariables(".")
