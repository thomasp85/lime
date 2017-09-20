context("Text explanation")

library(xgboost)
library(text2vec)

data(train_sentences)

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


test_that("single sentence explanation", {
  to_explain <- "Since our motivation is an application in bioinformatics, our notation and terminology will be drawn from that area"
  expect_gt(predict(xgb_model, get_matrix(to_explain)), 0.5)
  explainer <- lime(x = to_explain, model = xgb_model, preprocess = get_matrix)
  explanation <- explain(to_explain, explainer, n_labels = 1, n_features = 2)
  expect_length(explanation, 13)
  expect_equal(nrow(explanation), 2)
  expect_type(explanation, "list")
  expect_true("our" %in% explanation$feature)
  expect_gt(sum(explanation[explanation$feature == "our", "feature_weight"]), 0)
})

test_that("multiple sentences, multiple explanations", {
  sentences <- head(test_sentences[test_sentences$class.text == "OWNX", "text"], 5)
  explainer <- lime(sentences, xgb_model, get_matrix)
  explanation <- explain(sentences, explainer, n_labels = 1, n_features = 2)
  expect_gte(sum(tolower(explanation$feature) == "we"), 3)
  expect_true("our" %in% explanation$feature)
  expect_equal(nrow(explanation), 5 * 2)
  expect_true(all(explanation[explanation$feature == "our", "feature_weight"] > 0))
  expect_gt(sum(explanation[explanation$feature == "we", "feature_weight"]), 0)
})

test_that("multiple sentences, single explanation", {
  sentences <- head(test_sentences[test_sentences$class.text == "OWNX", "text"], 5)
  explainer <- lime(sentences, xgb_model, get_matrix)
  explanation <- explain(sentences, explainer, n_labels = 1, n_features = 5, single_explanation = TRUE)
  expect_true(all(explanation[explanation$feature == "Section", "feature_weight"] > 0))
  expect_gt(sum(explanation[explanation$feature == "we", "feature_weight"]), 0)
})
