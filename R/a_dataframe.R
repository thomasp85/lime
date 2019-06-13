#' @rdname lime
#' @name lime
#' @param bin_continuous Should continuous variables be binned when making the explanation
#' @param n_bins The number of bins for continuous variables if `bin_continuous = TRUE`
#' @param quantile_bins Should the bins be based on `n_bins` quantiles or spread evenly over the range of the training data
#' @param use_density If `bin_continuous = FALSE` should continuous data be sampled using a kernel density estimation. If not, continuous features are expected to follow a normal distribution.
#' @importFrom stats predict sd quantile density
#' @export
#'
#' @examples
#' # Explaining a model based on tabular data
#' library(MASS)
#' iris_test <- iris[1, 1:4]
#' iris_train <- iris[-1, 1:4]
#' iris_lab <- iris[[5]][-1]
#' # Create linear discriminant model on iris data
#' model <- lda(iris_train, iris_lab)
#' # Create explanation object
#' explanation <- lime(iris_train, model)
#'
#' # This can now be used together with the explain method
#' explain(iris_test, explanation, n_labels = 1, n_features = 2)
#'
lime.data.frame <- function(x, model, preprocess = NULL, bin_continuous = TRUE, n_bins = 4, quantile_bins = TRUE, use_density = TRUE, ...) {
  if (is.null(preprocess)) preprocess <- function(x) x
  assert_that(is.function(preprocess))
  explainer <- c(as.list(environment()), list(...))
  explainer$x <- NULL
  explainer$feature_type <- setNames(sapply(x, function(f) {
    if (is.integer(f)) {
      if (length(unique(f)) == 1) 'constant' else 'integer'
    } else if (is.numeric(f)) {
      if (length(unique(f)) == 1) 'constant' else 'numeric'
    } else if (is.character(f)) {
      'character'
    } else if (is.factor(f)) {
      'factor'
    } else if (is.logical(f)) {
      'logical'
    } else if (inherits(f, 'Date') || inherits(f, 'POSIXt')) {
      'date_time'
    } else {
      stop('Unknown feature type', call. = FALSE)
    }
  }), names(x))
  if (any(explainer$feature_type == 'constant')) {
    warning('Data contains numeric columns with zero variance', call. = FALSE)
  }
  explainer$bin_cuts <- setNames(lapply(seq_along(x), function(i) {
    if (explainer$feature_type[i] %in% c('numeric', 'integer')) {
      if (quantile_bins) {
        bins <- quantile(x[[i]], seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
        bins <- bins[!duplicated(bins)]
        if (length(bins) < 3) {
          warning(names(x)[i], ' does not contain enough variance to use quantile binning. Using standard binning instead.', call. = FALSE)
          d_range <- range(x[[i]], na.rm = TRUE)
          bins <- seq(d_range[1], d_range[2], length.out = n_bins + 1)
        }
        bins
      } else {
        d_range <- range(x[[i]], na.rm = TRUE)
        seq(d_range[1], d_range[2], length.out = n_bins + 1)
      }
    }
  }), names(x))
  explainer$feature_distribution <- setNames(lapply(seq_along(x), function(i) {
    switch(
      explainer$feature_type[i],
      integer = ,
      numeric = if (bin_continuous) {
        table(cut(x[[i]], unique(explainer$bin_cuts[[i]]), labels = FALSE, include.lowest = TRUE))/nrow(x)
      } else if (use_density) {
        density(x[[i]])
      } else {
        c(mean = mean(x[[i]], na.rm = TRUE), sd = sd(x[[i]], na.rm = TRUE))
      },
      character = ,
      logical = ,
      factor = table(x[[i]])/nrow(x),
      NA
    )
  }), names(x))
  structure(explainer, class = c('data_frame_explainer', 'explainer', 'list'))
}
#' @rdname explain
#' @name explain
#'
#' @param dist_fun The distance function to use for calculating the distance
#' from the observation to the permutations. If `dist_fun = 'gower'` (default)
#' it will use [gower::gower_dist()]. Otherwise it will be forwarded to
#' [stats::dist()]
#' @param kernel_width The width of the exponential kernel that will be used to
#' convert the distance to a similarity in case `dist_fun != 'gower'`.
#' @param gower_pow A modifier for gower distance. The calculated distance will
#' be raised to the power of this value.
#'
#' @importFrom gower gower_dist
#' @importFrom stats dist
#' @export
explain.data.frame <- function(x, explainer, labels = NULL, n_labels = NULL,
                               n_features, n_permutations = 5000,
                               feature_select = 'auto', dist_fun = 'gower',
                               kernel_width = NULL, gower_pow = 1, ...) {
  assert_that(is.data_frame_explainer(explainer))
  m_type <- model_type(explainer)
  o_type <- output_type(explainer)
  if (m_type == 'regression') {
    if (!is.null(labels) || !is.null(n_labels)) {
      warning('"labels" and "n_labels" arguments are ignored when explaining regression models')
    }
    n_labels <- 1
    labels <- NULL
  }
  assert_that(is.null(labels) + is.null(n_labels) == 1, msg = "You need to choose between labels and n_labels parameters.")
  assert_that(is.count(n_features))
  assert_that(is.count(n_permutations))

  if (is.null(kernel_width)) {
    kernel_width <- sqrt(ncol(x)) * 0.75
  }
  kernel <- exp_kernel(kernel_width)

  case_perm <- permute_cases(x, n_permutations, explainer$feature_distribution,
                             explainer$bin_continuous, explainer$bin_cuts,
                             explainer$use_density)
  case_res <- predict_model(explainer$model, explainer$preprocess(case_perm), type = o_type, ...)
  case_res <- set_labels(case_res, explainer$model)
  case_ind <- split(seq_len(nrow(case_perm)), rep(seq_len(nrow(x)), each = n_permutations))
  res <- lapply(seq_along(case_ind), function(ind) {
    i <- case_ind[[ind]]
    if (dist_fun == 'gower') {
      sim <- 1 - (gower_dist(case_perm[i[1], , drop = FALSE], case_perm[i, , drop = FALSE])) ^ gower_pow
    }
    perms <- numerify(case_perm[i, ], explainer$feature_type, explainer$bin_continuous, explainer$bin_cuts)
    if (dist_fun != 'gower') {
      sim <- kernel(c(0, dist(feature_scale(perms, explainer$feature_distribution, explainer$feature_type, explainer$bin_continuous),
                        method = dist_fun)[seq_len(n_permutations-1)]))
    }
    res <- model_permutations(as.matrix(perms), case_res[i, , drop = FALSE], sim, labels, n_labels, n_features, feature_select)
    res$feature_value <- unlist(case_perm[i[1], res$feature])
    res$feature_desc <- describe_feature(res$feature, case_perm[i[1], ], explainer$feature_type, explainer$bin_continuous, explainer$bin_cuts)
    guess <- which.max(abs(case_res[i[1], ]))
    res$case <- rownames(x)[ind]
    res$label_prob <- unname(as.matrix(case_res[i[1], ]))[match(res$label, colnames(case_res))]
    res$data <- list(as.list(case_perm[i[1], ]))
    res$prediction <- list(as.list(case_res[i[1], ]))
    res$model_type <- m_type
    res
  })
  res <- do.call(rbind, res)
  res <- res[, c('model_type', 'case', 'label', 'label_prob', 'model_r2', 'model_intercept', 'model_prediction', 'feature', 'feature_value', 'feature_weight', 'feature_desc', 'data', 'prediction')]
  if (m_type == 'regression') {
    res$label <- NULL
    res$label_prob <- NULL
    res$prediction <- unlist(res$prediction)
  }
  as_tibble(res)
}
is.data_frame_explainer <- function(x) inherits(x, 'data_frame_explainer')
#' @importFrom stats setNames
numerify <- function(x, type, bin_continuous, bin_cuts) {
  setNames(as.data.frame(lapply(seq_along(x), function(i) {
    if (type[i] %in% c('character', 'factor', 'logical')) {
      as.numeric(x[[i]] == x[[i]][1])
    } else if (type[i] == 'date_time' || type[i] == 'constant') {
      rep(0, nrow(x))
    } else {
      if (bin_continuous) {
        cuts <- bin_cuts[[i]]
        cuts[1] <- -Inf
        cuts[length(cuts) + 1] <- Inf
        xi <- cut(x[[i]], unique(cuts), include.lowest = T)
        as.numeric(xi == xi[1])
      } else {
        x[[i]]
      }
    }
  }), stringsAsFactors = FALSE), names(x))
}
#' @importFrom stats setNames
feature_scale <- function(x, distribution, type, bin_continuous) {
  setNames(as.data.frame(lapply(seq_along(x), function(i) {
    if (type[i] == 'numeric' && !bin_continuous) {
      scale(x[, i], distribution[[i]]['mean'], distribution[[i]]['sd'])
    } else {
      x[, i]
    }
  }), stringsAsFactors = FALSE), names(x))
}
describe_feature <- function(feature, case, type, bin_continuous, bin_cuts) {
  sapply(feature, function(f) {
    if (type[[f]] == 'logical') {
      paste0(f, ' is ', tolower(as.character(case[[f]])))
    } else if (type[[f]] %in% c('character', 'factor')) {
      paste0(f, ' = ', as.character(case[[f]]))
    } else if (bin_continuous) {
      cuts <- bin_cuts[[f]]
      cuts[1] <- -Inf
      cuts[length(cuts)] <- Inf
      bin <- cut(case[[f]], unique(cuts), labels = FALSE, include.lowest = TRUE)
      cuts <- trimws(format(cuts, digits = 3))
      if (bin == 1) {
        paste0(f, ' <= ', cuts[bin + 1])
      } else if (bin == length(cuts) - 1) {
        paste0(cuts[bin], ' < ', f)
      } else {
        paste0(cuts[bin], ' < ', f, ' <= ', cuts[bin + 1])
      }
    } else {
      f
    }
  })
}
