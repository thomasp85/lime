#' Plot text explanations
#'
#' Highlight words which explains a prediction.
#'
#' @param explanations object returned by the [lime.character] function.
#' @param ... parameters passed to [sizingPolicy]
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
#' get_matrix <- function(text) {
#'   it <- itoken(text, progressbar = FALSE)
#'   create_dtm(it, vectorizer = hash_vectorizer())
#' }
#'
#' dtm_train = get_matrix(train_sentences$text)
#'
#' xgb_model <- xgb.train(list(max_depth = 7, eta = 0.1, objective = "binary:logistic",
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
#' @importFrom assertthat assert_that is.number is.string
#' @importFrom htmlwidgets createWidget
#' @rdname text_explanations
#' @export
plot_text_explanations <- function(explanations, ...) {
  assert_that(is.data.frame(explanations))
  assert_that(!is.null(explanations$data))
  original_text <- explanations$data

  text_highlighted_raw <- lapply(unique(explanations$case), function(id) {
    current_case_df <- explanations[explanations$case == id,]
    original_text <- unique(current_case_df[["data"]])
    predicted_label <- unique(current_case_df[["label"]])
    predicted_label_prob <- unique(current_case_df[["label_prob"]])
    assert_that(is.string(original_text))
    assert_that(is.string(predicted_label))
    assert_that(is.number(predicted_label_prob))
    info_prediction_text <- paste0('<sub>Label predicted: ', predicted_label, ' (', round(predicted_label_prob * 100, 2), '%)</sub>')

    current_case_df$weight_percent <- abs(current_case_df$feature_weight) / sum(abs(current_case_df$feature_weight))

    current_case_df$sign <- ifelse(current_case_df$feature_weight > 0, 1, -1)
    current_case_df$code_level <- current_case_df$sign * (1 + current_case_df$weight_percent %/% 0.2)
    current_case_df$color <- sapply(current_case_df$code_level, get_color_code)

    list(get_html_span(original_text, current_case_df), info_prediction_text)
  })

  text_highlighted <- paste('<div style="word-wrap: break-word">',
                            paste("<p><pre>", unlist(text_highlighted_raw, recursive = TRUE), "</pre></p>", collapse = "\n"),
                            "</div>")

  createWidget("plot_text_explanations", list(html = text_highlighted),
                              sizingPolicy = htmlwidgets::sizingPolicy(
                                knitr.figure = FALSE,
                                defaultHeight = "auto",
                                knitr.defaultWidth = "100%",
                                ...
                              ),
                              package = "lime")
}

#' @importFrom stringi stri_replace_all_regex
get_html_span <- function(text, current_case_df) {
  result <- text
  for (word in current_case_df$feature) {
    color <- as.character(current_case_df[current_case_df$feature == word, "color"])
    text_searched <- paste0("(\\b", word, "\\b)")
    replace_expression <- paste0("<span class='", color, "'>", "$1", "</span>")
    result <- stri_replace_all_regex(result, text_searched, replace_expression)
  }
  result
}

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
