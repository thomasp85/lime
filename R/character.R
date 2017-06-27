#' @describeIn lime Method for explaining text data
#' @param preprocess Function to transform character vector to feature provided to the model to explain
#' @param tokenization function used to tokenize text
#' @param bow set to TRUE if you want to keep order of words. Warning: each word is replaced by word_position, this need to be managed by preprocess function
#' @param n_permutations number of permutations to perform. More gives better explanation until it is not usefull.
#' @param number_features_explain as the name says
#' @param feature_selection_method method to select the best features
#' @param labels name of the label to explain (use only when model to explain predictions includes names as data.frame column names, like with caret)
#' @param n_labels instead of labels, number of labels to explain.
#' @param dist_fun function measure distance between original text and the permultation.
#' @param prediction function used to perform the prediction. Should have 2 variables, first for the character vector, second for the model. Should return a data.table with the predictions.
#'
#' TODO : add example
#' TODO : check parameter labels != NULL OR n_labels != NULL
#' @importFrom stringdist seq_dist
#' @importFrom magrittr %>%
#' @export
lime.character <- function(x, model, preprocess, tokenization = default_tokenization, bow = FALSE, kernel_width = 25,
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
#' @description Takes care of performing the prediction and adapting the format
#' @param data data to be explained (character vector)
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
#' @description Use simple regex to tokenize a String.
#' @param text text to tokenize as a character vector
#' @importFrom stringi stri_split_regex
#' @importFrom magrittr %>% set_colnames
#' @export
default_tokenization <- function(text) {
  stri_split_regex(str = text, pattern = "\\W+", simplify = TRUE) %>% as.character()
}

globalVariables(".")
