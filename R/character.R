#' @describeIn lime Method for explaining text data
#' @param preprocess Function to transform \code{\link{character}} vector to feature provided to the model to explain
#' @param tokenization function used to tokenize text
#' @param bow set to \code{\link{TRUE}} if to keep order of words. Warning: each word will be replaced by \code{word_position}, this need to be managed by \code{preprocess} function.
#' @param n_permutations number of permutations to perform. More gives better explanation up to a point where it is not usefull and takes too much time. (5000)
#' @param number_features_explain number of features used in the explanation. (5)
#' @param feature_selection_method method to select the best features. ("auto")
#' @param labels name of the label to explain (use only when model to explain predictions includes names as \code{\link{data.frame}} column names, like with \code{link{caret}}). (\code{\link{NULL}}).
#' @param n_labels instead of labels, number of labels to explain. (\code{\link{NULL}})
#' @param dist_fun function to measure distance between original the datum and its permultations. Used for weighting the permutations in the explanation model. (cosine)
#' @param prediction function used to perform the prediction. Should have 2 variables, first for the \code{\link{character}} vector, second for the \code{model}. Should return a \code{\link{data.frame}} with the predictions.
#'
#' TODO : add example
#' TODO : check parameter labels != NULL OR n_labels != NULL
#' @importFrom stringdist seq_dist
#' @importFrom magrittr %>%
#' @export
lime.character <- function(x, model, preprocess, tokenization = default_tokenize, bow = FALSE, kernel_width = 25,
                           n_permutations = 5000, number_features_explain = 5, feature_selection_method = "auto",
                           labels = NULL, n_labels = NULL,  dist_fun = "cosine", prediction = default_predict, ...) {
  function() {
  permutation_cases <- permute_cases.character(x, n_permutations, tokenization, bow, dist_fun)
  predicted_labels_dt <- preprocess(permutation_cases$permutations) %>% prediction(model)
  model_permutations(x = permutation_cases$tabular, y = predicted_labels_dt,
                            weights = exp_kernel(kernel_width)(1 - permutation_cases$permutation_distances),
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
