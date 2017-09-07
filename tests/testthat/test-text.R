context("Text explanation")

library(xgboost)
library(text2vec)

data(train_sentences)

test_that("lime text explanation results has expecatated properties", {
  # Tokenize data
  get_matrix <- function(text) {
    it <- itoken(text, progressbar = FALSE)
    create_dtm(it, vectorizer = hash_vectorizer())
  }

  dtm_train = get_matrix(train_sentences$text)

  # Create boosting model
  xgb_model <- xgb.train(list(max_depth = 7, eta = 0.1, objective = "binary:logistic",
                              eval_metric = "error", nthread = 1),
                         xgb.DMatrix(dtm_train, label = train_sentences$class.text == "OWNX"),
                         nrounds = 50)

  to_explain <- "Since our motivation is an application in bioinformatics, our notation and terminology will be drawn from that area"

  # Check that the model works as expected
  expect_gt(predict(xgb_model, get_matrix(to_explain)), 0.5)

  explainer <- lime(x = to_explain, model = xgb_model, preprocess = get_matrix)
  explanation <- explain(to_explain, explainer, n_labels = 1, n_features = 2)

  # Checkes the content of the explanation - 1 example
  expect_length(explanation, 12)
  expect_equal(nrow(explanation), 2)
  expect_type(explanation, "list")
  expect_true("our" %in% explanation$feature)
  expect_gt(sum(explanation[explanation$feature == "our", "feature_weight"]), 0)

  # Checkes the content of the explanation - several examples
  sentences <- head(test_sentences[test_sentences$class.text == "OWNX", "text"], 5)
  explainer_bis <- lime(sentences, xgb_model, get_matrix)
  explanation_bis <- explain(sentences, explainer, n_labels = 1, n_features = 2)
  expect_gte(sum(tolower(explanation_bis$feature) == "we"), 3)
  expect_true("our" %in% explanation_bis$feature)
  expect_equal(nrow(explanation_bis), 5 * 2)
  expect_true(all(explanation_bis[explanation_bis$feature == "our", "feature_weight"] > 0))
  expect_gt(sum(explanation_bis[explanation_bis$feature == "we", "feature_weight"]), 0)
})
