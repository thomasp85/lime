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
