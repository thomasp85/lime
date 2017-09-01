#' View HTML rendering of regular expression match.
#'
#' \code{plot_text_explanations} shows the first match;
#'
#' @param explanations object returned by the \code{lime.character} function.
#' @examples
#' \dontrun{
#' # Explaining a model based on text data
#'
#' # Purpose is to classify sentences from scientific publications
#' # and find those where the team writes about their own work
#' # (category OWNX in the provided dataset).
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
#' sentences <- head(test_sentences[test_sentences$class.text == "OWNX", "text"], 5)
#' explainer <- lime(sentences, xgb_model, get_matrix)
#' explanations <- explain(sentences, explainer, n_labels = 1, n_features = 2)
#'
#' # We can see that many explanations are based
#' # on the presence of the word `we` in the sentences
#' # which makes sense regarding the task.
#' print(explanations)
#'
#' # We can also print the explanations to see the selected words in their context.
#' plot_text_explanations(explanations)
#' }
#'
#' @importFrom assertthat assert_that
#' @importFrom htmlwidgets createWidget
#' @importFrom purrr map_if map
#' @importFrom dplyr mutate filter select
#' @export
plot_text_explanations <- function(explanations) {
  assert_that(is.data.frame(explanations))
  assert_that(!is.null(explanations$data))
  original_text <- explanations$data

  text_highlighted <- lapply(unique(explanations$case), function(id) {
    current_case_df <- explanations %>% filter(case == id)
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

globalVariables(c("feature_weight", "feature", "weight_percent", "code_level", "case", "data"))
