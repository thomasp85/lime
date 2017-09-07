library(shiny)
library(lime)
library(text2vec)
library(xgboost)

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

sentences <- head(test_sentences[test_sentences$class.text == "OWNX", "text"], 5)
explainer <- lime(sentences, xgb_model, get_matrix)

ui <- fixedPage(title = "Text model explainer",
  mainPanel(
    fixedRow(align = "center", titlePanel(title = "Local Interpretable Model-agnostic Explanations")),
    hr(),
    fixedRow(align = "center", textAreaInput("text_to_explain", label = NULL, resize = "vertical", placeholder = "Put here the text to explain")),
    fixedRow(align = "center", text_explanations_output("text_explanations_plot"))
  ))

# Define server logic for slider examples ----
server <- function(input, output) {
  output$text_explanations_plot <- render_text_explanations({
    explanations <- explain(input$text_to_explain, explainer, n_labels = 1, n_features = 2)
    plot_text_explanations(explanations)
  })
}

# Create Shiny app ----
shinyApp(ui, server)
