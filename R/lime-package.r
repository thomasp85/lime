#' @details
#' This package is a port of the original Python lime package implementing the
#' prediction explanation framework laid out Ribeiro *et al.* (2016). The
#' package supports models from `caret` and `mlr` natively, but see
#' [the docs][model_support] for how to make it work for any model.
#'
#' **Main functions:**
#'
#' Use of `lime` is mainly through two functions. First you create an
#' `explainer` object using the [lime()] function based on the training data and
#' the model, and then you can use the [explain()] function along with new data
#' and the explainer to create explanations for the model output.
#'
#' Along with these two functions, `lime` also provides the [plot_features()]
#' and [plot_text_explanations()] function to visualise the explanations
#' directly.
#'
#' @references Ribeiro, M.T., Singh, S., Guestrin, C. *"Why Should I Trust You?": Explaining the Predictions of Any Classifier*. 2016, <https://arxiv.org/abs/1602.04938>
#'
#' @aliases lime-package
#' @useDynLib lime
#' @importFrom Rcpp sourceCpp
'_PACKAGE'

