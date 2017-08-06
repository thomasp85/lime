#' View HTML rendering of regular expression match.
#'
#' \code{plot_text_explanations} shows the first match;
#'
#' @param explanations object returned by the \code{lime.character} function.
#' @examples
#' \dontrun{
#' # Explaining a model based on text data
#'
#' library(text2vec)
#' library(lime)
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
#' @export
plot_text_explanations <- function(explanations) {
  validate_that("data.frame" %in% class(explanations))
  validate_that(!attr(explanations, "original_text") %>% is.null())
  original_text <- attr(explanations, "original_text")

  text_highlighted <- original_text %>%
    map(~ default_tokenize(.x) %>%
          map_if(.p = ~ .x %in% explanations$feature, .f = ~ paste0("<span class='match'>", .x, "</span>"))
        %>% paste(collapse = " ")
        ) %>% paste(collapse = "<br/><br/>\n")

  createWidget("plot_text_explanations", list(html = text_highlighted),
                              sizingPolicy = htmlwidgets::sizingPolicy(
                                knitr.figure = FALSE,
                                defaultHeight = "auto"
                              ),
                              package = "lime")
}
