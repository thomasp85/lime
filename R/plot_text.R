#' Plot text explanations
#'
#' Highlight important words
#'
#' @param explanations object returned by the [lime.character] function.
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
#' @importFrom assertthat assert_that
#' @importFrom htmlwidgets createWidget
#' @export
plot_text_explanations <- function(explanations) {
  assert_that(is.data.frame(explanations))
  assert_that(!is.null(explanations$data))
  original_text <- explanations$data

  text_highlighted_raw <- lapply(unique(explanations$case), function(id) {
    current_case_df <- explanations[explanations$case == id,]
    original_text <- unique(current_case_df[["data"]])

    current_case_df$weight_percent <- abs(current_case_df$feature_weight) / sum(abs(current_case_df$feature_weight))

    current_case_df$sign <- ifelse(current_case_df$feature_weight > 0, 1, -1)
    current_case_df$code_level <- current_case_df$sign * (1 + current_case_df$weight_percent %/% 0.2)
    current_case_df$color <- sapply(current_case_df$code_level, get_color_code)

    get_html_span(original_text, current_case_df)
  })

  text_highlighted <- paste(unlist(text_highlighted_raw), collapse = "<br/><br/>\n")

  createWidget("plot_text_explanations", list(html = text_highlighted),
                              sizingPolicy = htmlwidgets::sizingPolicy(
                                knitr.figure = FALSE,
                                defaultHeight = "auto"
                              ),
                              package = "lime")
}

get_html_span <- function(text, current_case_df) {
  tokenized_text <- default_tokenize(text)
  tokenized_text_with_span <- add_span_tag(current_case_df, tokenized_text)
  paste(tokenized_text_with_span, collapse = " ")
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

add_span_tag <- function(results_percent, tokenized_text) {
  colors <- sapply(tokenized_text, function(word) if (word %in% results_percent$feature) as.character(results_percent[results_percent$feature == word, "color"]) else "", USE.NAMES = FALSE)

  ifelse(colors == "", tokenized_text, paste0("<span class='", colors, "'>", tokenized_text, "</span>"))
}

#' Shiny widget output
#'
#' Create an output to insert text explanation plot in Shiny application.
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit or a number, which will be coerced to a string and have "px" appended.
#' @return An output function that enables the use of the widget within Shiny applications.
#' @importFrom htmlwidgets shinyWidgetOutput
#' @export
text_explanations_output <- function(outputId, width = "100%", height = "400px") {
  shinyWidgetOutput(outputId, "plot_text_explanations", width, height, package = "lime")
}

#' Shiny widget render
#'
#' Render the text explanations in Shiny application.
#' @param expr An expression that generates an HTML widget
#' @param env The environment in which to evaluate `expr`.
#' @param quoted Is `expr` a quoted expression (with [quote()])? This
#'   is useful if you want to save an expression in a variable.
#' @return A render function that enables the use of the widget within Shiny applications.
#' @importFrom htmlwidgets shinyRenderWidget
#' @export
render_text_explanations <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, text_explanations_output, env, quoted = TRUE)
}

globalVariables(c("feature_weight", "feature", "weight_percent", "code_level", "case", "data"))
