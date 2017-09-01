library(lime)

context("Permutations")

test_that("lime explanation only produces one entry per case and feature", {

  library(caret)
  # Split up the data set
  iris_train <- iris[, 1:4]
  iris_lab <- iris[[5]]

  # Create Random Forest model on iris data
  model <- train(iris_train, iris_lab, method = 'rf')

  # Create explanation function
  explain <- lime(iris_train, model)

  # Explain new observation. This should yield a tibble with one row, because it's one case, one feature, one label
  explanations <- explain(iris_train[1,], n_labels = 1, n_features = 1, feature_select = 'forward_selection')
  expect_equal(nrow(explanations), 1)
})

test_that("lime text explanation results has expecatated properties", {

  library(xgboost)
  library(text2vec)

  data(train_sentences)

  # Tokenize data
  get.matrix <- function(text) {
    it <- itoken(text, progressbar = FALSE)
    create_dtm(it, vectorizer = hash_vectorizer())
  }

  dtm_train = get.matrix(train_sentences$text)

  # Create boosting model
  bst <- xgb.train(list(max_depth = 7, eta = 0.1, objective = "binary:logistic",
                        eval_metric = "error", nthread = 1),
                   xgb.DMatrix(dtm_train, label = train_sentences$class.text == "OWNX"),
                   nrounds = 50)

  to_explain <- "Since our motivation is an application in bioinformatics, our notation and terminology will be drawn from that area"

  # Check that the model works as expected
  expect_gt(predict(bst, get.matrix(to_explain)), 0.5)

  r <- lime(x = to_explain, model = bst, preprocess = get.matrix)(cases = to_explain, n_labels = 1, n_features = 2)

  # Checkes the content of the explanation
  expect_length(r, 12)
  expect_equal(nrow(r), 2)
  expect_type(r, "list")
  expect_true("our" %in% r$feature)
})
