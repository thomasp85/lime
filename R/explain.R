#' @export
explain <- function(x, explainer, labels, n_labels = NULL, n_features,
                    n_permutations = 5000, dist_fun = 'euclidean',
                    feature_select = 'auto') {
  UseMethod('explain')
}
model_type.explainer <- function(x) {
  model_type(x$model)
}
output_type <- function(x) {
  switch(
    model_type(x),
    classification = 'prob',
    regression = 'raw',
    stop(model_type(x), ' models are not supported yet', call. = FALSE)
  )
}
