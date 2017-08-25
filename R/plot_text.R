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
#' r <- lime(test_sentences[5, text], bst, get.matrix)(n_labels = 1, n_features = 2)
#' print(r)
#'
#' plot_text_explanations(r)
#' }
#'
#' @importFrom assertthat assert_that
#' @importFrom htmlwidgets createWidget
#' @importFrom purrr map_if map
#' @importFrom dplyr mutate filter select
#' @export
plot_text_explanations <- function(explanations) {
  assert_that("data.frame" %in% class(explanations))
  assert_that(!attr(explanations, "original_text") %>% is.null())
  original_text <- attr(explanations, "original_text")

  text_highlighted <- lapply(unique(results$case), function(id) {
    current_case_df <- results %>% filter(case == id)
    original_text <- current_case_df %>% select(data) %>% unique
    results_percent <- current_case_df %>% mutate(weight_percent = abs(feature_weight) / sum(abs(feature_weight)),
                                                 sign = ifelse(feature_weight > 0, 1, -1),
                                                 code_level = sign * (1 + weight_percent %/% 0.2),
                                                 color = sapply(code_level, function(x) get_color_code(x)))

    get_html_span(original_text, results_percent)
  }) %>%
    flatten_chr() %>%
    paste(collapse = "<br/><br/>\n")

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
  default_tokenize() %>%
  map_if(.p = ~ .x %in% results_percent$feature, .f = ~ paste0("<span class='", filter(results_percent, feature == .x) %>% select(color) %>% as.character(), "'>", .x, "</span>")) %>%
  paste(collapse = " ")


get_color_code <- function(code_level) {
  switch(as.character(code_level),
         "-6" = "negative_5", # for -100%
         "-5" = "negative_5",
         "-4" = "negative_4",
         "-3" = "negative_3",
         "-2" = "negative_2",
         "-1" = "negative_1",
         "1" = "positive_1",
         "2" = "positive_2",
         "3" = "positive_3",
         "4" = "positive_4",
         "5" = "positive_5",
         "6" = "positive_5") # for 100%
}

globalVariables(c("feature_weight", "feature", "weight_percent", "code_level", "case", "data", "results"))
