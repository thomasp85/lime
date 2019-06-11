#' Explain model predictions
#'
#' Once an explainer has been created using the [lime()] function it can be used
#' to explain the result of the model on new observations. The `explain()`
#' function takes new observation along with the explainer and returns a
#' data.frame with prediction explanations, one observation per row. The
#' returned explanations can then be visualised in a number of ways, e.g. with
#' [plot_features()].
#'
#' @param x New observations to explain, of the same format as used when
#' creating the explainer
#'
#' @param explainer An `explainer` object to use for explaining the observations
#'
#' @param labels The specific labels (classes) to explain in case the model is
#' a classifier. For classifiers either this or `n_labels` must be given.
#'
#' @param n_labels The number of labels to explain. If this is given for
#' classifiers the top `n_label` classes will be explained.
#'
#' @param n_features The number of features to use for each explanation.
#'
#' @param n_permutations The number of permutations to use for each explanation.
#'
#' @param feature_select The algorithm to use for selecting features. One of:
#'
#' - `"auto"`: If `n_features <= 6` use `"forward_selection"` else use `"highest_weights"`.
#' - `"none"`: Ignore `n_features` and use all features.
#' - `"forward_selection"`: Add one feature at a time until `n_features` is
#'   reached, based on quality of a ridge regression model.
#' - `"highest_weights"`: Fit a ridge regression and select the `n_features` with
#'   the highest absolute weight.
#' - `"lasso_path"`: Fit a lasso model and choose the `n_features` whose lars
#'   path converge to zero the latest.
#' - `"tree"` : Fit a tree to select `n_features` (which needs to be a power of
#'   2). It requires last version of `XGBoost`.
#'
#' @param ... Parameters passed on to the `predict_model()` method
#'
#' @return A data.frame encoding the explanations one row per explained
#' observation. The columns are:
#'
#' - `model_type`: The type of the model used for prediction.
#' - `case`: The case being explained (the rowname in `cases`).
#' - `model_r2`: The quality of the model used for the explanation
#' - `model_intercept`: The intercept of the model used for the explanation
#' - `model_prediction`: The prediction of the observation based on the model
#'   used for the explanation.
#' - `feature`: The feature used for the explanation
#' - `feature_value`: The value of the feature used
#' - `feature_weight`: The weight of the feature in the explanation
#' - `feature_desc`: A human readable description of the feature importance.
#' - `data`: Original data being explained
#' - `prediction`: The original prediction from the model
#'
#' Furthermore classification explanations will also contain:
#'
#' - `label`: The label being explained
#' - `label_prob`: The probability of `label` as predicted by `model`
#'
#' @export
#'
#' @examples
#' # Explaining a model and an explainer for it
#' library(MASS)
#' iris_test <- iris[1, 1:4]
#' iris_train <- iris[-1, 1:4]
#' iris_lab <- iris[[5]][-1]
#' model <- lda(iris_train, iris_lab)
#' explanation <- lime(iris_train, model)
#'
#' # This can now be used together with the explain method
#' explain(iris_test, explanation, n_labels = 1, n_features = 2)
#'
explain <- function(x, explainer, labels, n_labels = NULL, n_features,
                    n_permutations = 5000, feature_select = 'auto', ...) {
  if (is.character(x) && is.image_file(x)) class(x) <- 'imagefile'
  UseMethod('explain', x)
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
