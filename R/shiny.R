#' Interactive explanations
#'
#' Display text explanation in an interactive way.
#' You can :
#' * send a new sentence
#' * update the parameters of the explainer
#' @param explainer parameters
#' @param window_title,title,place_holder,minimum_lentgh_error text to be displayed on the page
#' @param minimum_lentgh don't update display if text is shorter than this parameter
#' @examples
#' \dontrun{
#' library(lime)
#' library(text2vec)
#' library(xgboost)
#' library(stringi)
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
#'                             eval_metric = "error", nthread = 1),
#'                        xgb.DMatrix(dtm_train, label = train_sentences$class.text == "OWNX"),
#'                        nrounds = 50)
#'
#' sentences <- head(test_sentences[test_sentences$class.text != "OWNX", "text"], 5)
#' explainer <- lime(sentences, xgb_model, get_matrix)
#'
#' interactive_text_explanations(explainer)
#' }
#' @rdname interactive_text_explanations
#' @importFrom shiny fluidPage textAreaInput shinyApp sliderInput mainPanel titlePanel hr need validate
#' @importFrom stringi stri_count_words
#' @importFrom shinythemes shinytheme
#' @importFrom assertthat assert_that is.string is.count
#' @export
interactive_text_explanations <- function(explainer, window_title = "Text model explainer", title = "Local Interpretable Model-agnostic Explanations", place_holder = "Put here the text to explain", minimum_lentgh = 5, minimum_lentgh_error = "Text is too short to be displayed (>= 5).", max_feature_to_select = 10) {
  assert_that(is.list(explainer))
  assert_that(is.string(window_title))
  assert_that(is.string(title))
  assert_that(is.string(place_holder))
  assert_that(is.count(minimum_lentgh))
  assert_that(is.count(max_feature_to_select))

  ui <- fluidPage(title = window_title,
                  theme = shinytheme("united"),
                  mainPanel(
                    titlePanel(title = title),
                    hr(),
                    textAreaInput("text_to_explain", label = NULL, resize = "both", placeholder = place_holder, width = "200%", height = "200px"),
                    sliderInput("number_features_to_explain", label = "number of words to select", min = 1, max = max_feature_to_select, value = 1, ticks = FALSE),
                    text_explanations_output("text_explanations_plot")
                  ))

  # Define server logic for slider examples ----
  server <- function(input, output) {
    output$text_explanations_plot <- render_text_explanations({
      validate(
        need(stri_count_words(input$text_to_explain) >= minimum_lentgh, message = minimum_lentgh_error)
      )
      explanations <- explain(input$text_to_explain, explainer, n_labels = 1, n_features = input$number_features_to_explain)
      plot_text_explanations(explanations)
    })
  }
  shinyApp(ui, server)
}

#' Shiny widget output
#'
#' Create an output to insert text explanation plot in Shiny application.
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit or a number, which will be coerced to a string and have "px" appended.
#' @return An output function that enables the use of the widget within Shiny applications.
#' @importFrom htmlwidgets shinyWidgetOutput
#' @rdname interactive_text_explanations
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
#' @rdname interactive_text_explanations
#' @export
render_text_explanations <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, text_explanations_output, env, quoted = TRUE)
}

