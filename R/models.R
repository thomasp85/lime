#' Methods for extending limes model support
#'
#' In order to have `lime` support for your model of choice `lime` needs to be
#' able to get predictions from the model in a standardised way, and it needs to
#' be able to know whether it is a classification or regression model. For the
#' former it calls the `predict_model()` generic which the user is free to
#' supply methods for without overriding the standard `predict()` method. For
#' the latter the model must respond to the `model_type()` generic.
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
#' @section Supported Models:
#' Out of the box, `lime` supports the following model objects:
#'
#' - `train` from caret
#' - `WrappedModel` from mlr
#' - `xgb.Booster` from xgboost
#' - `H2OModel` from h2o
#' - `keras.engine.training.Model` from keras
#' - `lda` from MASS (used for low-dependency examples)
#'
#' If your model is not one of the above you'll need to implement support
#' yourself. If the model has a predict interface mimicking that of
#' `predict.train()` from `caret`, it will be enough to wrap your model in
#' [as_classifier()]/[as_regressor()] to gain support. Otherwise you'll need
#' need to implement a `predict_model()` method and potentially a `model_type()`
#' method (if the latter is omitted the model should be wrapped in
#' [as_classifier()]/[as_regressor()], everytime it is used in [lime()]).
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

#' Indicate model type to lime
#'
#' `lime` requires knowledge about the type of model it is dealing with, more
#' specifically whether the model is a regressor or a classifier. If the model
#' class has a [model_type()] method defined lime can figure it out on its own
#' but if not, you can wrap your model in either of these functions to indicate
#' what type of model lime is dealing with. This can also be used to overwrite
#' the output from [model_type()] if the implementation uses some heuristic that
#' doesn't work for your particular model (e.g. keras models types are found by
#' checking if the activation in the last layer is linear or not - this is
#' rather crude). In addition `as_classifier` can be used to overwrite the
#' returned class labels - this is handy if the model does not store the labels
#' (again, keras springs to mind).
#'
#' @param x The model object
#' @param labels An optional character vector giving labels for each class
#'
#' @return A model augmented with information about the model type and
#' (potentially) the class labels.
#'
#' @export
as_classifier <- function(x, labels = NULL) {
  class(x) <- c('lime_classifier', class(x))
  attr(x, 'lime_labels') <- labels
  x
}
#' @rdname as_classifier
#' @export
as_regressor <- function(x) {
  class(x) <- 'lime_regressor'
  x
}
set_labels <- function(res, model) {
  labels <- attr(model, 'lime_labels')
  if (model_type(model) == 'classification' && !is.null(labels)) {
    if (length(labels) != ncol(res)) {
      warning('Ignoring provided class labels as length differs from model output')
    } else {
      names(res) <- labels
    }
  }
  res
}

#' @rdname model_support
#' @export
predict_model <- function(x, newdata, type, ...) {
  UseMethod('predict_model')
}
#' @export
predict_model.default <- function(x, newdata, type, ...) {
  p <- predict(x, newdata = newdata, type = type, ...)
  if (type == 'raw') p <- data.frame(Response = p, stringsAsFactors = FALSE)
  as.data.frame(p)
}
#' @export
predict_model.WrappedModel <- function(x, newdata, type, ...) {
  if (!requireNamespace('mlr', quietly = TRUE)) {
    stop('mlr must be available when working with WrappedModel models')
  }
  p <- predict(x, newdata = newdata, ...)
  type2 <- switch(
    type,
    raw = data.frame(Response = mlr::getPredictionResponse(p), stringsAsFactors = FALSE),
    prob = mlr::getPredictionProbabilities(p, p$task.desc$class.levels),
    stop('Type must be either "raw" or "prob"', call. = FALSE)
  )
}
#' @export
predict_model.xgb.Booster <- function(x, newdata, type, ...) {
  if (!requireNamespace('xgboost', quietly = TRUE)) {
    stop('The xgboost package is required for predicting xgboost models')
  }
  if(is.data.frame(newdata)){
    newdata <- xgboost::xgb.DMatrix(as.matrix(newdata))
  }
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
#' @export
predict_model.lda <- function(x, newdata, type, ...) {
  res <- predict(x, newdata = newdata, ...)
  switch(
    type,
    raw = data.frame(Response = res$class, stringsAsFactors = FALSE),
    prob = as.data.frame(res$posterior, check.names = FALSE)
  )
}
#' @export
predict_model.keras.engine.training.Model <- function(x, newdata, type, ...) {
  if (!requireNamespace('keras', quietly = TRUE)) {
    stop('The keras package is required for predicting keras models')
  }
  res <- predict(x, as.matrix(newdata))
  if (type == 'raw') {
    data.frame(Response = res[, 1])
  } else {
    if (ncol(res) == 1) {
      res <- cbind(1 - res, res)
    }
    colnames(res) <- as.character(seq_len(ncol(res)))
    as.data.frame(res, check.names = FALSE)
  }
}
#' @export
predict_model.H2OModel <- function(x, newdata, type, ...){
  if (!requireNamespace('h2o', quietly = TRUE)) {
      stop('The h2o package is required for predicting h2o models')
  }
  pred <- h2o::h2o.predict(x, h2o::as.h2o(newdata))
  h2o_model_class <- class(x)[[1]]
  if (h2o_model_class %in% c("H2OBinomialModel", "H2OMultinomialModel")) {
      return(as.data.frame(pred[,-1]))
  } else if (h2o_model_class == "H2ORegressionModel") {
      ret <- as.data.frame(pred[,1])
      names(ret) <- "Response"
      return(ret)
  } else {
      stop('This h2o model is not currently supported.')
  }
}
#' @rdname model_support
#' @export
model_type <- function(x, ...) {
  UseMethod('model_type')
}
#' @export
model_type.default <- function(x, ...) {
  stop('The class of model must have a model_type method. See ?model_type to get an overview of models supported out of the box', call. = FALSE)
}
#' @export
model_type.lime_classifier <- function(x, ...) 'classification'
#' @export
model_type.lime_regressor <- function(x, ...) 'regression'
#' @export
model_type.train <- function(x, ...) {
  tolower(x$modelType)
}
#' @export
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
#' @export
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
#' @export
model_type.lda <- function(x, ...) 'classification'
#' @export
model_type.keras.engine.training.Model <- function(x, ...) {
  if (!requireNamespace('keras', quietly = TRUE)) {
    stop('The keras package is required for predicting keras models')
  }
  if (keras::get_layer(x, index = -1)$activation$__name__ == 'linear') {
    'regression'
  } else {
    'classification'
  }
}
#' @export
model_type.H2OModel <- function(x, ...) {
  h2o_model_class <- class(x)[[1]]
  if (h2o_model_class %in% c("H2OBinomialModel", "H2OMultinomialModel")) {
      return('classification')
  } else if (h2o_model_class == "H2ORegressionModel") {
      return('regression')
  } else {
      stop('This h2o model is not currently supported.')
  }
}
