library(shiny)
library(lime)
library(text2vec)
library(xgboost)
library(assertthat)
library(stringi)
library(shinythemes)

#' Interactive explanations
#'
#' Display text explanation in an interactive way.
#' You can :
#' * send a new sentence
#' * update the parameters of the explainer
#' @param explainer parameters
#' @param window_title,title,place_holder,minimum_lentgh_error text to be displayed on the page
#' @param minimum_lentgh don't update display if text is shorter than this parameter
#' @importFrom shiny fixedPage fixedRow textAreaInput text_explanations_output shinyApp sliderInput
#' @importFrom stringi stri_count_words
#' @importFrom shinythemes shinytheme
#' @export
interactive_text_explanations <- function(explainer, window_title = "Text model explainer", title = "Local Interpretable Model-agnostic Explanations", place_holder = "Put here the text to explain", minimum_lentgh = 5, minimum_lentgh_error = "Text is too short to be displayed.", max_feature_to_select = 10) {
  assert_that(is.list(explainer))
  assert_that(is.string(window_title))
  assert_that(is.string(title))
  assert_that(is.string(place_holder))
  assert_that(is.count(minimum_lentgh))
  assert_that(is.count(max_feature_to_select))

  ui <- fixedPage(title = window_title,
                  mainPanel(
                    fixedRow(align = "center", titlePanel(title = title)),
                    hr(),
                    fixedRow(align = "center", textAreaInput("text_to_explain", label = NULL, resize = "vertical", placeholder = place_holder)),
                    fixedRow(align = "center", sliderInput("number_features_to_explain", label = "number of words to select", min = 1, max = max_feature_to_select, value = 1, ticks = FALSE)),
                    fixedRow(align = "center", text_explanations_output("text_explanations_plot"))
                  ))

  # Define server logic for slider examples ----
  server <- function(input, output) {
    output$text_explanations_plot <- render_text_explanations({
      validate(
        need(stri_count_words(input$text_to_explain) > 5, message = minimum_lentgh_error)
      )
      explanations <- explain(input$text_to_explain, explainer, n_labels = 1, n_features = input$number_features_to_explain)
      plot_text_explanations(explanations)
    })
  }
  shinyApp(ui, server)
}

data(train_sentences)
data(test_sentences)

get_matrix <- function(text) {
  it <- itoken(text, progressbar = FALSE)
  create_dtm(it, vectorizer = hash_vectorizer())
}

dtm_train = get_matrix(train_sentences$text)

xgb_model <- xgb.train(list(max_depth = 7, eta = 0.1, objective = "binary:logistic",
                            eval_metric = "error", nthread = 1),
                       xgb.DMatrix(dtm_train, label = train_sentences$class.text == "OWNX"),
                       nrounds = 50)

#sentences <- head(test_sentences[test_sentences$class.text != "OWNX", "text"], 5)
explainer <- lime(sentences, xgb_model, get_matrix)

interactive_text_explanations(explainer)
