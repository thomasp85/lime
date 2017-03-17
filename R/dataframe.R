#' @describeIn lime Method for explaining tabular data
#' @param bin_continuous Should continuous variables be binned when making the explanation
#' @param n_bins The number of bins for continuous variables if `bin_continuous = TRUE`
#' @param kernel_width The width of the kernel used for converting the distances to permutations into weights
#' @importFrom dplyr bind_rows
#' @importFrom stats predict sd
#' @export
lime.data.frame <- function(x, bin_continuous = TRUE, n_bins = 4, kernel_width = NULL, ...) {
  feature_type <- sapply(x, function(f) {
    if (is.numeric(f)) {
      'numeric'
    } else if (is.character(f)) {
      'character'
    } else if (is.factor(f)) {
      'factor'
    } else {
      stop('Unknown feature type', call. = FALSE)
    }
  })
  feature_distribution <- lapply(seq_along(x), function(i) {
    switch(
      feature_type[i],
      numeric = c(mean = mean(x[[i]], na.rm = TRUE), sd = sd(x[[i]], na.rm = TRUE)),
      character,
      factor = table(x[[i]])/nrow(x)
    )
  })
  if (is.null(kernel_width)) {
    kernel_width <- sqrt(ncol(x)) * 0.75
  }
  kernel <- function(x) {
    sqrt(exp(-(x^2) / (kernel_width^2)))
  }
  function(cases, model, labels, n_labels, n_features, n_permutations, dist_fun = 'euclidean', feature_select = 'auto') {
    case_perm <- permute_cases(cases, n_permutations, feature_distribution)
    case_res <- predict(model, case_perm, type = 'prob')
    case_ind <- split(seq_len(nrow(case_perm)), rep(seq_len(nrow(cases)), each = n_permutations))
    res <- lapply(seq_along(case_ind), function(ind) {
      i <- case_ind[[ind]]
      perms <- numerify(case_perm[i, ], feature_type)
      dist <- c(0, dist(feature_scale(perms, feature_distribution, feature_type),
                        method = dist_fun)[seq_len(n_permutations-1)])
      res <- model_permutations(as.matrix(perms), case_res[i, ], kernel(dist), labels, n_labels, n_features, feature_select)
      guess <- which.max(abs(case_res[i[1], ]))
      res$case <- rownames(cases)[ind]
      res$label_prob <- unname(as.matrix(case_res[i[1], ]))[match(res$label, names(case_res))]
      res$predict_label <- names(case_res)[guess]
      res$predict_prob <- case_res[i[1], guess]
      res
    })
    bind_rows(res)
  }
}
#' @importFrom stats setNames
numerify <- function(x, type) {
  setNames(as.data.frame(lapply(seq_along(x), function(i) {
    if (type[i] %in% c('character', 'factor')) {
      as.numeric(x[, i] == x[1, i])
    } else {
      x[, i]
    }
  }), stringsAsFactors = FALSE), names(x))
}
#' @importFrom stats setNames
feature_scale <- function(x, distribution, type) {
  setNames(as.data.frame(lapply(seq_along(x), function(i) {
    if (type[i] == 'numeric') {
      scale(x[, i], distribution[[i]]['mean'], distribution[[i]]['sd'])
    } else {
      x[, i]
    }
  }), stringsAsFactors = FALSE), names(x))
}
