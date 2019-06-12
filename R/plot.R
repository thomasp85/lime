#' Plot the features in an explanation
#'
#' This functions creates a compact visual representation of the explanations
#' for each case and label combination in an explanation. Each extracted feature
#' is shown with its weight, thus giving the importance of the feature in the
#' label prediction.
#'
#' @param explanation A `data.frame` as returned by [explain()].
#'
#' @param ncol The number of columns in the facetted plot
#'
#' @param cases An optional vector with case names to plot. `explanation` will
#' be filtered to only include these cases prior to plotting
#'
#' @return A `ggplot` object
#'
#' @import ggplot2
#' @export
#'
#' @family explanation plots
#'
#' @examples
#' # Create some explanations
#' library(MASS)
#' iris_test <- iris[1, 1:4]
#' iris_train <- iris[-1, 1:4]
#' iris_lab <- iris[[5]][-1]
#' model <- lda(iris_train, iris_lab)
#' explanation <- lime(iris_train, model)
#' explanations <- explain(iris_test, explanation, n_labels = 1, n_features = 2)
#'
#' # Get an overview with the standard plot
#' plot_features(explanations)
#'
plot_features <- function(explanation, ncol = 2, cases = NULL) {
  type_pal <- c('Supports', 'Contradicts')

  if (!is.null(cases)) {
    explanation <- explanation[explanation$case %in% cases, , drop = FALSE]
  }
  if (nrow(explanation) == 0) stop("No explanations to plot", call. = FALSE)

  if (explanation$model_type[1] == 'regression') {
    type_pal <- c('Positive', 'Negative')
    binned_feature <- grepl("=|>|<", explanation$feature_desc)
    explanation[!binned_feature, "feature_weight"] <- as.numeric(explanation$feature_value[!binned_feature]) * explanation$feature_weight[!binned_feature]
  }

  explanation$type <- factor(ifelse(sign(explanation$feature_weight) == 1, type_pal[1], type_pal[2]), levels = type_pal)
  description <- paste0(explanation$case, '_', explanation[['label']])
  desc_width <- max(nchar(description)) + 1
  description <- paste0(format(description, width = desc_width), explanation$feature_desc)
  explanation$description <- factor(description, levels = description[order(abs(explanation$feature_weight))])
  explanation$case <- factor(explanation$case, unique(explanation$case))
  explanation$`Explanation fit` <- format(explanation$model_r2, digits = 2)

  if (explanation$model_type[1] == 'classification') {
    explanation$probability <- format(explanation$label_prob, digits = 2)
    explanation$label <- factor(explanation$label, unique(explanation$label[order(explanation$label_prob, decreasing = TRUE)]))
    p <- ggplot(explanation) +
      facet_wrap(~ case + label + probability + `Explanation fit`, labeller = label_both_upper, scales = 'free_y', ncol = ncol)
  } else if (explanation$model_type[1] == 'regression') {
    p <- ggplot(explanation) +
      facet_wrap(~ case + prediction + `Explanation fit`, labeller = label_both_upper, scales = 'free_y', ncol = ncol)
  }
  p +
    geom_col(aes_(~description, ~feature_weight, fill = ~type)) +
    coord_flip() +
    scale_fill_manual(values = c('steelblue', 'firebrick'), drop = FALSE) +
    scale_x_discrete(labels = function(lab) substr(lab, desc_width + 1, nchar(lab))) +
    labs(y = 'Weight', x = 'Feature', fill = '') +
    theme_lime()
}
#' Plot a condensed overview of all explanations
#'
#' This function produces a facetted heatmap visualisation of all
#' case/label/feature combinations. Compared to [plot_features()] it is much
#' more condensed, thus allowing for an overview of many explanations in one
#' plot. On the other hand it is less useful for getting exact numerical
#' statistics of the explanation.
#'
#' @param explanation A `data.frame` as returned by [explain()].
#' @param ... Parameters passed on to [ggplot2::facet_wrap()]
#'
#' @return A `ggplot` object
#'
#' @import ggplot2
#' @export
#'
#' @family explanation plots
#'
#' @examples
#' # Create some explanations
#' library(MASS)
#' iris_test <- iris[c(1, 51, 101), 1:4]
#' iris_train <- iris[-c(1, 51, 101), 1:4]
#' iris_lab <- iris[[5]][-c(1, 51, 101)]
#' model <- lda(iris_train, iris_lab)
#' explanation <- lime(iris_train, model)
#' explanations <- explain(iris_test, explanation, n_labels = 1, n_features = 2)
#'
#' # Get an overview with the standard plot
#' plot_explanations(explanations)
plot_explanations <- function(explanation, ...) {
  num_cases <- unique(suppressWarnings(as.numeric(explanation$case)))
  if (!anyNA(num_cases)) {
    explanation$case <- factor(explanation$case, levels = as.character(sort(num_cases)))
  }
  explanation$feature_desc <- factor(
    explanation$feature_desc,
    levels = rev(unique(explanation$feature_desc[order(explanation$feature, explanation$feature_value)]))
  )
  p <- ggplot(explanation, aes_(~case, ~feature_desc)) +
    geom_tile(aes_(fill = ~feature_weight)) +
    scale_x_discrete('Case', expand = c(0, 0)) +
    scale_y_discrete('Feature', expand = c(0, 0)) +
    scale_fill_gradient2('Feature\nweight', low = 'firebrick', mid = '#f7f7f7', high = 'steelblue') +
    theme_lime() +
    theme(panel.border = element_rect(fill = NA, colour = 'grey60', size = 1),
          panel.grid = element_blank(),
          legend.position = 'right',
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  if (is.null(explanation$label)) {
    p
  } else {
    p + facet_wrap(~label, ...)
  }
}

theme_lime <- function(...) {
  theme_minimal() +
    theme(
      strip.text = element_text(face = 'bold', size = 9),
      plot.margin = margin(15, 15, 15, 15),
      legend.background = element_blank(),
      legend.key = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = 'bottom',
      panel.spacing.y = unit(15, 'pt'),
      strip.text.x = element_text(margin = margin(t = 2, b = 2), hjust = 0),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),
      ...
    )
}
#' @importFrom tools toTitleCase
label_both_upper <- function(labels, multi_line = TRUE, sep = ': ') {
  names(labels) <- toTitleCase(names(labels))
  label_both(labels, multi_line, sep)
}
