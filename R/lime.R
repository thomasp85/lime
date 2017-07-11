#' Create a model explanation function based on training data
#'
#' This is the main function of the `lime` package. It is a factory function
#' that returns a new function that can be used to explain the predictions made
#' by black box models. This is a generic with methods for the different data
#' types supported by lime.
#'
#' @param x The training data used for training the model that should be
#' explained.
#'
#' @param model The model whose output should be explained
#'
#' @param ... Arguments passed on to methods
#'
#' @return A function taking the following arguments:
#'
#' * `cases`: Data of the same format as `x` that needs to be explained
#' * `labels`: The prediction(s) that needs to be explained
#' * `n_labels`: Alternative to `labels`, the number of predictions to explain,
#'   selected by their probability.
#' * `n_features`: The number of features to use in the explanaition.
#' * `n_permutations`: The number of permutations to make on each row in `cases`
#' * `dist_fun`: The distance measure to use for weighting the permutations
#' * `feature_select`: The method to use for feature selection. One of:
#'   - `"auto"`: If `n_features <= 6` use `"forward_selection"` else use `"highest_weights"`.
#'   - `"none"`: Ignore `n_features` and use all features.
#'   - `"forward_selection"`: Add one feature at a time until `n_features` is
#'     reached, based on quality of a ridge regression model.
#'   - `"highest_weights"`: Fit a ridge regression and select the `n_features` with
#'     the highest absolute weight.
#'   - `"lasso_path"`: Fit a lasso model and choose the `n_features` whose lars
#'     path converge to zero the latest.
#'
#' The return value of the returned function will be a `tibble` encoding the
#' explanations in a tidy format. The columns are:
#'
#' * `case`: The case being explained (the rowname in `cases`)
#' * `predict_label`: The label with the highest probability as predicted by `model`
#' * `predict_prob`: The probability of `predict_label`
#' * `label`: The label being explained
#' * `label_prob`: The probability of `label` as predicted by `model`
#' * `feature`: The feature used for the explanation
#' * `weight`: The weight of the feature in the explanation
#' * `model_r2`: The quality of the model used for the explanation
#' * `model_intercept`: The intercept of the model used for the explanation
#'
#' @export
lime <- function(x, model, ...) {
  UseMethod('lime')
}

# Helpers -----------------------------------------------------------------

#' @importFrom glmnet cv.glmnet coef.cv.glmnet
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom stats coef
#' @importFrom stats glm.fit
#' @importFrom stats gaussian
model_permutations <- function(x, y, weights, labels, n_labels, n_features, feature_method) {
  if (!is.null(n_labels)) {
    labels <- names(y)[order(y[1,], decreasing = TRUE)[seq_len(n_labels)]]
  }
  x <- x[, apply(x, 2, var) != 0, drop = FALSE]
  res <- lapply(labels, function(label) {
    features <- select_features(feature_method, x, y[[label]], weights, n_features)

    # glmnet does not allow n_features=1
    if(n_features==1){
      x_fit = cbind("(Intercept)" = rep(1, nrow(x)), x[, features, drop=FALSE])
      fit <- glm.fit(x = x_fit, y = y[[label]],  weights = weights, family = gaussian())
      r2 <- fit$deviance / fit$null.deviance
      coefs <- coef(fit)
      intercept <- coefs[1]
      coefs <- coefs[-1]
    } else {
      fit <- glmnet(x[, features], y[[label]], weights = weights, alpha = 0, lambda = 0.001)
      r2 <- fit$dev.ratio
      coefs <- coef(fit)
      intercept <- coefs[1, 1]
      coefs <- coefs[-1, 1]
    }

    tibble(label = label, feature = names(coefs), feature_weight = unname(coefs), model_r2 = r2, model_intercept = intercept)
  })
  bind_rows(res)
}

feature_selection_method <- function() c("auto", "none", "forward_selection", "highest_weights", "lasso_path")

select_features <- function(method, x, y, weights, n_features) {
  if (n_features >= ncol(x)) {
    return(seq_len(ncol(x)))
  }
  method <- match.arg(method, feature_selection_method())
  switch(
    method,
    auto = if (n_features <= 6) {
      select_features("forward_selection", x, y, weights, n_features)
    } else {
      select_features("highest_weights", x, y, weights, n_features)
    },
    none = seq_len(nrow(x)),
    forward_selection = select_f_fs(x, y, weights, n_features),
    highest_weights = select_f_hw(x, y, weights, n_features),
    lasso_path = select_f_lp(x, y, weights, n_features),
    stop("Method not implemented", call. = FALSE)
  )
}
#' @importFrom glmnet cv.glmnet
select_f_fs <- function(x, y, weights, n_features) {
  features <- c()
  for (i in seq_len(n_features)) {
    max <- -100000
    best <- 0
    for (j in seq_len(ncol(x))) {
      if (j %in% features) next
      #                                          is this ok?
      try_features <- if (length(features) == 0) c(j, j) else c(features, j)
      fit <- glmnet(x[, try_features, drop = FALSE], y, weights = weights, alpha = 0, lambda = 0)
      r2 <- fit$dev.ratio
      if (r2 > max) {
        max <- r2
        best <- j
      }
    }
    features <- c(features, best)
  }
  features
}
#' @importFrom glmnet cv.glmnet coef.cv.glmnet
#' @importFrom stats coef
#' @importFrom utils head
select_f_hw <- function(x, y, weights, n_features) {
  fit <- glmnet(x, y, weights = weights, alpha = 0, lambda = 0)
  head(order(abs(coef(fit)[-1, 1] * x[1,]), decreasing = TRUE), n_features)
}
#' @importFrom glmnet glmnet coef.glmnet
#' @importFrom stats coef
select_f_lp <- function(x, y, weights, n_features) {
  fit <- glmnet(x, y, weights = weights, alpha = 1, nlambda=300)
  # In case that no model with correct n_feature size was found
  if(all(fit$df != n_features)){
    stop(sprintf("No model with %i features found with lasso_path. Try a different method.", n_features))
  }
  has_value <- apply(coef(fit)[-1, ], 2, function(x) x != 0)
  f_count <- apply(has_value, 2, sum)
  row <- which(f_count >= n_features)[1]
  features <- which(has_value[, row])
  if (length(features) > n_features) {
    features <- features[sample(seq_along(features), n_features)]
  }
  features
}
exp_kernel <- function(width) {
  function(x) sqrt(exp(-(x^2) / (width^2)))
}

globalVariables(c("var", "."))
