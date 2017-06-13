#' @importFrom stringdist seq_dist
#' @importFrom dplyr bind_rows
#' @export
lime.character <- function(x, model, preprocess = NULL, split_at = '\\W+', bow = TRUE, kernel_width = 25, ...) {
  kernel <- exp_kernel(kernel_width)
  if (is.null(preprocess)) preprocess <- function(x, ...) x
  function(cases, labels, n_labels = NULL, n_features, n_permutations = 5000, dist_fun = 'cosine', feature_select = 'auto') {
    case_perm <- permute_cases(cases, n_permutations, split_at, bow)
    case_perm_mat <- case_perm$tabular
    case_perm <- case_perm$permutations
    case_res <- predict(model, preprocess(case_perm, ...), type = 'prob')
    case_ind <- split(seq_len(nrow(case_perm)), rep(seq_len(nrow(cases)), each = n_permutations))
    res <- lapply(seq_along(case_ind), function(ind) {
      i <- case_ind[[ind]]
      dist <- seq_dist(list(case_perm_mat[[ind]][1, ]),
                       split(case_perm_mat[[ind]], seq_along(i)),
                       method = dist_fun)
      res <- model_permutations(as.matrix(case_perm_mat[[ind]]), case_res[i, ], kernel(dist), labels, n_labels, n_features, feature_select)
      res$feature_value <- unlist(case_perm[i[1], res$feature])
      res$feature_desc <- res$feature
      guess <- which.max(abs(case_res[i[1], ]))
      res$case <- rownames(cases)[ind]
      res$label_prob <- unname(as.matrix(case_res[i[1], ]))[match(res$label, names(case_res))]
      res$data <- list(as.list(case_perm[i[1], ]))
      res$prediction <- list(as.list(case_res[i[1], ]))
      res
    })
    res <- bind_rows(res)
    res[, c('case', 'label', 'label_prob', 'model_r2', 'model_intercept', 'feature', 'feature_value', 'feature_weight', 'feature_desc', 'data', 'prediction')]
  }
}
