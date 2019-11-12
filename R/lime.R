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
#' @return Return an explainer which can be used together with [explain()] to
#' explain model predictions.
#'
#' @name lime
#' @export
lime <- function(x, model, ...) {
  if (is.character(x) && is.image_file(x)) class(x) <- 'imagefile'
  UseMethod('lime', x)
}

# Helpers -----------------------------------------------------------------

#' @importFrom glmnet cv.glmnet
#' @importFrom stats coef glm.fit gaussian var
#' @importFrom Matrix colSums
model_permutations <- function(x, y, weights, labels, n_labels, n_features, feature_method) {
  if (all(weights[-1] == 0)) {
    stop('All permutations have no similarity to the original observation. Try setting bin_continuous to TRUE and/or increase kernel_size', call. = FALSE)
  }
  if (!is.null(n_labels)) {
    labels <- names(y)[order(as.data.frame(y)[1,], decreasing = TRUE)[seq_len(n_labels)]]
  }
  x <- x[, colSums(is.na(x)) == 0 & apply(x, 2, var) != 0, drop = FALSE]
  res <- lapply(labels, function(label) {

    if (length(unique(y[[label]])) == 1) {
      stop("Response is constant across permutations. Please check your model", call. = FALSE)
    }

    features <- select_features(feature_method, x, y[[label]], weights, n_features)
    # glmnet does not allow n_features=1
    if (length(features) == 1) {
      x_fit = cbind("(Intercept)" = rep(1, nrow(x)), x[, features, drop = FALSE])
      fit <- glm.fit(x = x_fit, y = y[[label]],  weights = weights, family = gaussian())
      r2 <- 1 - fit$deviance / fit$null.deviance
      coefs <- coef(fit)
      intercept <- coefs[1]
      coefs <- coefs[-1]
      model_pred <- fit$fitted.values[1]
    } else {
      shuffle_order <- sample(length(y[[label]])) # glm is sensitive to the order of the examples
      fit <- glmnet(x[shuffle_order, features], y[[label]][shuffle_order], weights = weights[shuffle_order], alpha = 0, lambda = 2 / length(y[[label]]))
      r2 <- fit$dev.ratio
      coefs <- coef(fit)
      intercept <- coefs[1, 1]
      coefs <- coefs[-1, 1]
      model_pred <- predict(fit, x[1, features, drop = FALSE])[1]
    }

    data.frame(
      label = label,
      feature = names(coefs),
      feature_weight = unname(coefs),
      model_r2 = r2,
      model_intercept = intercept,
      model_prediction = model_pred,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, res)
}


feature_selection_method <- function() c("auto", "none", "forward_selection", "highest_weights", "lasso_path", "tree")


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
    none = seq_len(ncol(x)),
    forward_selection = select_f_fs(x, y, weights, n_features),
    highest_weights = select_f_hw(x, y, weights, n_features),
    lasso_path = select_f_lp(x, y, weights, n_features),
    tree = select_tree(x, y, weights, n_features),
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
      if (length(features) == 0) {
        x_fit = cbind("(Intercept)" = rep(1, nrow(x)), x[, j, drop = FALSE])
        fit <- glm.fit(x = x_fit, y = y,  weights = weights, family = gaussian())
        r2 <- 1 - fit$deviance / fit$null.deviance
      } else {
        fit <- glmnet(x[, c(features, j), drop = FALSE], y, weights = weights, alpha = 0, lambda = 0)
        r2 <- fit$dev.ratio
      }
      if (is.finite(r2) && r2 > max) {
        max <- r2
        best <- j
      }
    }
    if (best == 0) {
      stop('Failed to select features with forward selection. Please choose another feature selector', call. = FALSE)
    }
    features <- c(features, best)
  }
  features
}

#' @importFrom glmnet glmnet
#' @importFrom stats coef
#' @importFrom utils head
select_f_hw <- function(x, y, weights, n_features) {
  shuffle_order <- sample(length(y)) # glm is sensitive to the order of the examples
  fit_model <- glmnet(x[shuffle_order,], y[shuffle_order], weights = weights[shuffle_order], alpha = 0, lambda = 0)
  features <- coef(fit_model)[-1, 1]
  features_order <- order(abs(features), decreasing = TRUE)
  head(features_order, n_features)
}

# Tree model for feature selection
# Based on the latest XGBoost version.
# May require the Drat package because of a bug in old version of xgb.model.dt.tree
# @param x the data as a sparse matrix
# @param y the labels
# @param weights distance of the sample with the original datum
# @param n_features number of features to take
#' @importFrom utils packageVersion
select_tree <- function(x, y, weights, n_features) {
  xgb_version <- packageVersion("xgboost")
  if (xgb_version < "0.6.4.6") stop("You need to install latest xgboost (version >= \"0.6.4.6\") from Xgboost Drat repository to use tree mode for feature selection.\nMore info on http://xgboost.readthedocs.io/en/latest/R-package/xgboostPresentation.html")
  number_trees <- max(trunc(log2(n_features)), 2)
  if (log2(n_features) != number_trees) message("In \"tree\" mode, number of features should be a power of 2 and at least of 4 (= deepness of the binary tree of 2), setting was set to [", n_features, "], it has been replaced by [", 2^number_trees, "].")
  mat <- xgboost::xgb.DMatrix(x, label = y, weight = weights)
  bst.bow <- xgboost::xgb.train(params = list(max_depth = number_trees, eta = 1, silent = 1, objective = "binary:logistic"), data = mat, nrounds = 1, lambda = 0)
  dt <- xgboost::xgb.model.dt.tree(model = bst.bow)
  selected_words <- head(dt[["Feature"]], n_features)
  which(colnames(mat) %in% selected_words)
}

#' @importFrom glmnet glmnet coef.glmnet
#' @importFrom stats coef
select_f_lp <- function(x, y, weights, n_features) {
  shuffle_order <- sample(length(y)) # glm is sensitive to the order of the examples
  fit <- glmnet(x[shuffle_order,], y[shuffle_order], weights = weights[shuffle_order], alpha = 1, nlambda = 300)
  has_value <- apply(coef(fit)[-1, ], 2, function(x) x != 0)
  f_count <- apply(has_value, 2, sum)
  # In case that no model with correct n_feature size was found return features <= n_features
  row <- rev(which(f_count <= n_features))[1]
  features <- which(has_value[, row])
  features
}
exp_kernel <- function(width) {
  function(x) sqrt(exp(-(x^2) / (width^2)))
}
