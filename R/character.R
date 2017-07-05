#' @describeIn lime Method for explaining text data
#' @param preprocess Function to transform \code{\link{character}} vector to feature provided to the model to explain
#' @param tokenization function used to tokenize text
#' @param keep_word_position set to \code{\link{TRUE}} if to keep order of words. Warning: each word will be replaced by \code{word_position}.
#' @param n_permutations number of permutations to perform. More gives better explanation up to a point where it is not usefull and takes too much time. (5000)
#' @param number_features_explain number of features used in the explanation. (5)
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
#' }
#' @param labels name of the label to explain (use only when model to explain predictions includes names as \code{\link{data.frame}} column names, like with \code{link{caret}}). (\code{\link{NULL}}).
#' @param n_labels instead of labels, number of labels to explain. (\code{\link{NULL}})
#' @param prediction function used to perform the prediction. Should have 2 variables, first for the \code{\link{character}} vector, second for the \code{model}. Should return a \code{\link{data.frame}} with the predictions.
#' @return Return a function. To make only one call you can perform a currying like in \code{lime(...)(...)}.
#'
#' TODO : add example
#' TODO : for keep_word_position, make 2 versions of the text, one with _position and one without for the model to predict
#'
#' @importFrom purrr is_empty is_scalar_logical
#' @importFrom stringdist seq_dist
#' @importFrom magrittr %>%
#' @importFrom assertthat validate_that not_empty is.scalar
#' @export
lime.character <- function(x, model, preprocess, tokenization = default_tokenize, keep_word_position = FALSE, kernel_width = 25,
                           n_permutations = 5000, number_features_explain = 5, feature_selection_method = "auto",
                           labels = NULL, n_labels = NULL,  prediction = default_predict, ...) {

  validate_that(class(preprocess) == "function")
  validate_that(class(tokenization) == "function")
  validate_that(class(prediction) == "function")
  validate_that(is_scalar_logical(keep_word_position))
  validate_that(is.null(labels) + is.null(n_labels) == 1, msg = "You need to choose between labels and n_labels parameters.")
  validate_that(!is.null(model))
  not_empty(x)
  validate_that(feature_selection_method %in% feature_selection_method())
  validate_that(number_features_explain >= 1)
  is.scalar(number_features_explain)
  validate_that(n_permutations >= 1)
  is.scalar(n_permutations)
  validate_that(kernel_width >= 1)
  is.scalar(kernel_width)

  function() {
    permutation_cases <- permute_cases(x, n_permutations, tokenization, keep_word_position)
    predicted_labels_dt <- preprocess(permutation_cases$permutations) %>% prediction(model)
    model_permutations(x = permutation_cases$tabular, y = predicted_labels_dt,
                       weights = exp_kernel(kernel_width)(permutation_cases$permutation_distances),
                       labels = labels, n_labels = n_labels, n_features = number_features_explain,
                       feature_method = feature_selection_method)
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
#' @importFrom stringi stri_split_regex
#' @importFrom magrittr %>% set_colnames
#' @export
default_tokenize <- function(text) {
  stri_split_regex(str = text, pattern = "\\W+", simplify = TRUE) %>% as.character()
}

globalVariables(".")
