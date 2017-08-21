#' @describeIn lime Method for explaining text data
#' @param preprocess Function to transform [character] vector to feature
#' provided to the model to explain
#' @param tokenization function used to tokenize text
#' @param keep_word_position set to `TRUE` if to keep order of words. Warning:
#' each word will be replaced by `word_position`.
#'
#' TODO : add example
#' TODO : for keep_word_position, make 2 versions of the text, one with _position and one without for the model to predict
#'
#' @importFrom purrr is_empty is_scalar_logical
#' @importFrom stringdist seq_dist
#' @importFrom magrittr %>%
#' @importFrom assertthat validate_that not_empty is.scalar
#' @export
lime.character <- function(x, model, preprocess, tokenization = default_tokenize, keep_word_position = FALSE, kernel_width = 25, ...) {

  validate_that("function" %in% class(preprocess))
  validate_that("function" %in% class(tokenization))
  validate_that(is_scalar_logical(keep_word_position))
  validate_that(!is.null(model))
  not_empty(x)
  validate_that(feature_selection_method %in% feature_selection_method())
  validate_that(kernel_width >= 1)
  is.scalar(kernel_width)

  m_type <- model_type(model)
  output_type <- switch(
    m_type,
    classification = 'prob',
    regression = 'raw',
    stop(m_type, ' models are not supported yet', call. = FALSE)
  )

  function(cases, labels, n_labels = NULL, n_features, n_permutations = 5000, dist_fun = 'euclidean', feature_select = 'auto') {
    if (m_type == 'regression') {
      if (!missing(labels) || !is.null(n_labels)) {
        warning('"labels" and "n_labels" arguments are ignored when explaining regression models')
        n_labels <- 1
        labels <- NULL
      }
    }
    validate_that(is.null(labels) + is.null(n_labels) == 1, msg = "You need to choose between labels and n_labels parameters.")
    validate_that(n_features >= 1)
    is.scalar(n_features)
    validate_that(n_permutations >= 1)
    is.scalar(n_permutations)

    case_perm <- permute_cases(cases, n_permutations, tokenization, keep_word_position)
    case_res <- preprocess(case_perm$permutations) %>%
      predict_model(model, type = output_type)
    case_ind <- split(seq_along(case_perm$permutations), rep(seq_along(case_perm$permutations), each = n_permutations))
    kernel <- exp_kernel(kernel_width)
    res <- lapply(seq_along(case_ind), function(ind) {
      i <- case_ind[[ind]]
      res <- model_permutations(as.matrix(case_perm$tabular[i, ]), case_res[i, ], kernel(case_perm$permutation_distances[i]), labels, n_labels, n_features, feature_select)
      res$feature_value <- unlist(case_perm[i[1], res$feature])
      res$feature_desc <- res$feature
      guess <- which.max(abs(case_res[i[1], ]))
      res$case <- rownames(cases)[ind]
      res$label_prob <- unname(as.matrix(case_res[i[1], ]))[match(res$label, colnames(case_res))]
      res$data <- list(as.list(cases[ind]))
      res$prediction <- list(as.list(case_res[i[1], ]))
      res$model_type <- m_type
      res
    })
    res <- bind_rows(res)
    res <- res[, c('model_type', 'case', 'label', 'label_prob', 'model_r2', 'model_intercept', 'feature', 'feature_value', 'feature_weight', 'feature_desc', 'data', 'prediction')]
    if (m_type == 'regression') {
      res$label <- NULL
      res$label_prob <- NULL
      res$prediction <- unlist(res$prediction)
    }
    res
  }
}

#' Default function to tokenize
#'
#' Use simple regex to tokenize a [character] vector. To be used with
#' [lime.character].
#'
#' @param text text to tokenize as a [character] vector
#'
#' @return A character vector
#'
#' @importFrom stringi stri_split_regex
#' @importFrom magrittr %>% set_colnames
#' @export
default_tokenize <- function(text) {
  stri_split_regex(str = text, pattern = "\\W+", simplify = TRUE) %>% as.character()
}
