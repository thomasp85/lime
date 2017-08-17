#' Methods for extending limes model support
#'
#' In order to have lime support for your model of choice lime needs to be able
#' to get predictions from the model in a standardised way, and it needs to be
#' able to know whether it is a classification or regression model. For the
#' former it defaults to calling predict
#'
NULL
#' @export
predict_model <- function(x, type, ...) {
  UseMethod('predict_model')
}
predict_model.default <- function(x, type, ...) {
  p <- predict(x, type = type, ...)
  if (type == 'raw') p <- data.frame(Response = p, stringsAsFactors = FALSE)
  p
}
predict_model.WrappedModel <- function(x, type, ...) {
  type2 <- switch(
    type,
    raw = 'response',
    prob = 'prob',
    stop('Type must be either "raw" or "prob"', call. = FALSE)
  )
  x$learner <- setPredictType(x$learner, type2)
  p <- predict(x, ...)
  if (type == 'raw') p <- data.frame(Response = p, stringsAsFactors = FALSE)
  p
}
predict_model.xgb.Booster <- function(x, type, ...) {
  p <- data.frame(predict(x, ...), stringsAsFactors = FALSE)
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
  type <- strsplit(obj, ':')[[1]]
  switch(
    type,
    reg = 'regression',
    binary = ,
    multi = 'classification',
    stop('Unsupported model type', call. = FALSE)
  )
}
