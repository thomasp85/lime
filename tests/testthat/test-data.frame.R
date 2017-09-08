context("data.frame")
library(MASS)

test_that("lime explanation only produces one entry per case and feature", {
  # Split up the data set
  iris_train <- iris[, 1:4]
  iris_lab <- iris[[5]]

  # Create Random Forest model on iris data
  model <- lda(iris_train, iris_lab)

  # Create explanation function
  explainer <- lime(iris_train, model)

  # Explain new observation. This should yield a tibble with one row, because it's one case, one feature, one label
  explanations <- explain(iris_train[1,], explainer, n_labels = 1, n_features = 1, feature_select = 'forward_selection')
  expect_equal(nrow(explanations), 1)
})
