#' Methods for extending limes model support
#'
#' In order to have `lime` support for your model of choice `lime` needs to be
#' able to get predictions from the model in a standardised way, and it needs to
#' be able to know whether it is a classification or regression model. For the
#' former it calls the `predict_model()` generic which the user is free to
#' supply methods for without overriding the standard `predict()` method. For
#' the latter the model must respond to the `model_type()` generic. Out of the
#' box `lime` supports models from `caret` and `mlr` as well as `xgboost`. See
#' the details for how to provide compliant methods for other models.
#'
#' @param x A model object
#'
#' @param newdata The new observations to predict
#'
#' @param type Either `'raw'` to indicate predicted values, or `'prob'` to
#' indicate class probabilities
#'
#' @param ... passed on to `predict` method
#'
#' @return A data.frame in the case of `predict_model()`. If `type = 'raw'` it
#' will contain one column named `'Response'` holding the predicted values. If
#' `type = 'prob'` it will contain a column for each of the possible classes
#' named after the class, each column holding the probability score for class
#' membership. For `model_type()` a character string. Either `'regression'` or
#' `'classification'` is currently supported.
#'
#' @name model_support
#' @rdname model_support
#'
#' @examples
#' # Example of adding support for lda models (already available in lime)
#' predict_model.lda <- function(x, newdata, type, ...) {
#'   res <- predict(x, newdata = newdata, ...)
#'   switch(
#'     type,
#'     raw = data.frame(Response = res$class, stringsAsFactors = FALSE),
#'     prob = as.data.frame(res$posterior, check.names = FALSE)
#'   )
#' }
#'
#' model_type.lda <- function(x, ...) 'classification'
#'
NULL

#' @rdname model_support
#' @export
predict_model <- function(x, newdata, type, ...) {
  UseMethod('predict_model')
}
predict_model.default <- function(x, newdata, type, ...) {
  p <- predict(x, newdata = newdata, type = type, ...)
  if (type == 'raw') p <- data.frame(Response = p, stringsAsFactors = FALSE)
  as.data.frame(p)
}
predict_model.WrappedModel <- function(x, newdata, type, ...) {
  type2 <- switch(
    type,
    raw = 'response',
    prob = 'prob',
    stop('Type must be either "raw" or "prob"', call. = FALSE)
  )
  x$learner <- mlr::setPredictType(x$learner, type2)
  p <- predict(x, newdata = newdata, ...)
  if (type == 'raw') p <- data.frame(Response = p, stringsAsFactors = FALSE)
  p
}
predict_model.xgb.Booster <- function(x, newdata, type, ...) {
  p <- data.frame(predict(x, newdata = newdata, reshape = TRUE, ...), stringsAsFactors = FALSE)
  if (type == 'raw') {
    names(p) <- 'Response'
  } else if (type == 'prob') {
    if (ncol(p) == 1) { # Binary classification
      names(p) = '1'
      p[['0']] <- 1 - p[['1']]
    } else {
      names(p) <- as.character(seq_along(p))
    }
  }
  p
}
predict_model.lda <- function(x, newdata, type, ...) {
  res <- predict(x, newdata = newdata, ...)
  switch(
    type,
    raw = data.frame(Response = res$class, stringsAsFactors = FALSE),
    prob = as.data.frame(res$posterior, check.names = FALSE)
  )
}
#' @rdname model_support
#' @export
model_type <- function(x, ...) {
  UseMethod('model_type')
}
model_type.default <- function(x, ...) {
  stop('The class of model must have a model_type method. Models other than those from `caret` and `mlr` must have a `model_type` method defined manually e.g. model_type.mymodelclass <- function(x, ...) "classification"', call. = FALSE)
}
model_type.train <- function(x, ...) {
  tolower(x$modelType)
}
model_type.WrappedModel <- function(x, ...) {
  switch(
    x$learner$type,
    classif = 'classification',
    regr = 'regression',
    surv = 'survival',
    cluster = 'clustering',
    multilabel = 'multilabel'
  )
}
model_type.xgb.Booster <- function(x, ...) {
  obj <- x$params$objective
  type <- strsplit(obj, ':')[[1]][1]
  switch(
    type,
    reg = 'regression',
    binary = 'classification',
    multi = 'classification',
    stop('Unsupported model type', call. = FALSE)
  )
}
model_type.lda <- function(x, ...) 'classification'
