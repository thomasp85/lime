## TODO Add warning about bow when set to T, must manage new words
## Check parameter labels != NULL OR n_labels != NULL
#' @importFrom stringdist seq_dist
#' @importFrom magrittr %>%
#' @export
lime.character <- function(x, model, preprocess, split_by = "\\W+", bow = FALSE, kernel_width = 25,
                           n_permutations = 5000, number_features_explain = 5, feature_selection_method = "auto",
                           labels = NULL, n_labels = NULL,  dist_fun = "cosine", prediction = do_predict, ...) {
  permutation.cases <- permute_cases.character(x, n_permutations, split_by, bow, dist_fun)
  predicted.labels.dt <- preprocess(permutation.cases$permutations) %>% prediction(model, .)
  model_permutations(x = permutation.cases$tabular, y = predicted.labels.dt,
                            weights = exp_kernel(kernel_width)(1 - permutation.cases$permutation.distances),
                            labels = labels, n_labels = n_labels, n_features = number_features_explain,
                            feature_method = feature_selection_method)
}
