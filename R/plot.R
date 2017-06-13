#' Plot the features in an explanation
#'
#' This functions creates a compact visual representation of the explanations
#' for each case and label combination in an explanation. Each extracted feature
#' is shown with its weight, thus giving the importance of the feature in the
#' label prediction.
#'
#' @param explanation A `data.frame` as returned by an explanation function.
#'
#' @param ncol The number of columns in the facetted plot
#'
#' @return A ggplot2 object
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' library(caret)
#' iris_test <- iris[1, 1:4]
#' iris_train <- iris[-1, 1:4]
#' iris_lab <- iris[[5]][-1]
#'
#' # Create Random Forest model on iris data
#' model <- train(iris_train, iris_lab, method = 'rf')
#'
#' # Create explanation function
#' expl <- lime(iris_train, model)
#' explanation <- expl(iris_test, n_labels = 1, n_features = 2)
#'
#' plot_features(explanation)
#' }
plot_features <- function(explanation, ncol = 2) {
  type_pal <- c('Supports', 'Contradicts')
  explanation$type <- factor(ifelse(sign(explanation$feature_weight) == 1, type_pal[1], type_pal[2]), levels = type_pal)
  description <- paste0(explanation$case, '_', explanation$label)
  desc_width <- max(nchar(description)) + 1
  description <- paste0(format(description, width = desc_width), explanation$feature_desc)
  explanation$description <- factor(description, levels = description[order(abs(explanation$feature_weight))])
  explanation$probability <- format(explanation$label_prob, digits = 2)
  ggplot(explanation) +
    geom_col(aes_(~description, ~feature_weight, fill = ~type)) +
    coord_flip() +
    facet_wrap(~ case + label + probability, labeller = label_both_upper, scales = 'free', ncol = ncol) +
    scale_fill_manual(values = c('forestgreen', 'firebrick'), drop = FALSE) +
    scale_x_discrete(labels = function(lab) substr(lab, desc_width+1, nchar(lab))) +
    labs(y = 'Weight', x = 'Feature', fill = '') +
    theme_lime()
}

#' @importFrom hrbrthemes theme_ipsum
theme_lime <- function(...) {
  theme_ipsum(strip_text_size = 9, strip_text_face = 'bold', grid = 'Xx',
              plot_margin = margin(15, 15, 15, 15),
              axis_title_just = 'm') +
    theme(legend.position = 'bottom',
          panel.spacing.y = unit(15, 'pt'),
          strip.text.x = element_text(margin = margin(t = 2, b = 2)),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10)),
          ...)
}
#' @importFrom tools toTitleCase
label_both_upper <- function(labels, multi_line = TRUE, sep = ': ') {
  names(labels) <- toTitleCase(names(labels))
  label_both(labels, multi_line, sep)
}
