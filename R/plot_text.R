#' View HTML rendering of regular expression match.
#'
#' \code{plot_text_explanations} shows the first match;
#'
#' @param explanations object returned by the \code{lime.character} function.
#' @examples
#' \dontrun{
#' # Explaining a model based on text data
#'
#' library(lime)
#' library(text2vec)
#' library(xgboost)
#'
#' data(train_sentences)
#' data(test_sentences)
#'
#' get.matrix <- function(text) {
#'   it <- itoken(text, progressbar = FALSE)
#'   create_dtm(it, vectorizer = hash_vectorizer())
#' }
#'
#' dtm_train = get.matrix(train_sentences$text)
#'
#' bst <- xgb.train(list(max_depth = 7, eta = 0.1, objective = "binary:logistic",
#'                  eval_metric = "error", nthread = 1),
#'                  xgb.DMatrix(dtm_train, label = train_sentences$class.text == "OWNX"),
#'                  nrounds = 50)
#'
#' r <- lime(test_sentences[5, text], bst, get.matrix, n_labels = 1,
#'      number_features_explain = 2, keep_word_position = FALSE)()
#'
#'  print(r)
#'
#'  plot_text_explanations(r)
#' }
#'
#' @importFrom assertthat validate_that
#' @importFrom htmlwidgets createWidget
#' @importFrom purrr map_if map
#' @importFrom dplyr mutate
#' @export
plot_text_explanations <- function(explanations) {
  validate_that("data.frame" %in% class(explanations))
  validate_that(!attr(explanations, "original_text") %>% is.null())
  original_text <- attr(explanations, "original_text")

  results_percent <- explanations %>% mutate(weight_percent = abs(feature_weight) / sum(abs(feature_weight)))

  text_highlighted <- get_html_span(original_text, results_percent)

  createWidget("plot_text_explanations", list(html = text_highlighted),
                              sizingPolicy = htmlwidgets::sizingPolicy(
                                knitr.figure = FALSE,
                                defaultHeight = "auto"
                              ),
                              package = "lime")
}

#' @importFrom purrr map_if map
#' @importFrom magrittr %>%
get_html_span <- function(text, results_percent) text %>%
  map(~ default_tokenize(.x) %>%
        map_if(.p = ~ .x %in% results_percent$feature, .f = ~ paste0("<span class='", get_color_code(.x, results_percent), "'>", .x, "</span>"))
      %>% paste(collapse = " ")
  ) %>% paste(collapse = "<br/><br/>\n")

#' @importFrom dplyr filter select
#' @importFrom magrittr %>%
get_color_code <- function(word, results_percent) {
  weight_level <- results_percent %>% filter(word == feature) %>% select(weight_percent) %>% as.numeric()
  sign <- results_percent %>% filter(word == feature) %>% select(feature_weight) %>% as.numeric() > 0
  code_level <- (1 + (weight_level %/% 0.25)) * sign
  switch(as.character(code_level),
         "-4" = "negative_4",
         "-3" = "negative_3",
         "-2" = "negative_2",
         "-1" = "negative_1",
         "1" = "positive_1",
         "2" = "positive_2",
         "3" = "positive_3",
         "4" = "positive_4")
}

globalVariables(c("feature_weight", "feature", "weight_percent"))
