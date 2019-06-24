context("h2o")

skip_on_cran()
library(h2o)
h2o.init()
h2o.no_progress()

# BINOMIAL CLASSIFICATION MODEL --------------
test_that("H2OBinomialClassification: lime explanation only produces one entry per case and feature", {

    # Get data
    path <- system.file("extdata", "prostate.csv", package = "h2o")
    df_h2o <- h2o.importFile(path)

    # Prep data
    df_h2o$CAPSULE <- h2o::as.factor(df_h2o$CAPSULE)
    df_h2o$RACE    <- h2o::as.factor(df_h2o$RACE)
    df_h2o$DCAPS   <- h2o::as.factor(df_h2o$DCAPS)
    df_h2o$DPROS   <- h2o::as.factor(df_h2o$DPROS)
    df_h2o$ID      <- NULL

    predictors <- c("AGE", "RACE", "VOL", "GLEASON")
    response <- "CAPSULE"

    # Binomial Classification
    model.rf <- h2o.randomForest(x = predictors, y = response, training_frame = df_h2o, ntrees = 50, stopping_rounds = 2)

    # Check class
    expect_s4_class(model.rf, "H2OBinomialModel")

    # Check explainer
    explainer <- lime(as.data.frame(df_h2o), model = model.rf)
    expect_s3_class(explainer, "data_frame_explainer")

    # Check explanation
    explanation <- lime::explain(as.data.frame(df_h2o[1,]), explainer, n_labels = 1, n_features = 1, kernel_width = 0.5)
    expect_equal(nrow(explanation), 1)
})


# MULTINOMIAL CLASSIFICATION MODEL --------------
test_that("H2OMultinomialClassification: lime explanation only produces one entry per case and feature", {

    # Get data
    path <- system.file("extdata", "iris.csv", package = "h2o")
    df_h2o <- h2o.importFile(path)

    # Prep data
    df_h2o$C5 <- h2o::as.factor(df_h2o$C5)

    response <- "C5"
    predictors <- base::setdiff(names(df_h2o), response)

    # Multinomial Classification
    model.rf <- h2o.randomForest(x = predictors, y = response, training_frame = df_h2o, ntrees = 50, stopping_rounds = 2)

    # Check class
    expect_s4_class(model.rf, "H2OMultinomialModel")

    # Check explainer
    explainer <- lime(as.data.frame(df_h2o), model = model.rf)
    expect_s3_class(explainer, "data_frame_explainer")

    # Check explanation
    explanation <- lime::explain(as.data.frame(df_h2o[1,]), explainer, n_labels = 1, n_features = 1, kernel_width = 0.5)
    expect_equal(nrow(explanation), 1)
})

test_that("H2ORegression: lime explanation only produces one entry per case and feature", {

    # Get data
    path <- system.file("extdata", "australia.csv", package = "h2o")
    df_h2o <- h2o.importFile(path)

    # Prep data
    response <- "premax"
    predictors <- base::setdiff(names(df_h2o), response)

    # Regression Classification
    model.rf <- h2o.randomForest(x = predictors, y = response, training_frame = df_h2o, ntrees = 50, stopping_rounds = 2)

    # Check class
    expect_s4_class(model.rf, "H2ORegressionModel")

    # Check explainer
    explainer <- lime(as.data.frame(df_h2o), model = model.rf)
    expect_s3_class(explainer, "data_frame_explainer")

    # Check explanation
    explanation <- lime::explain(as.data.frame(df_h2o[1,]), explainer, n_features = 1, kernel_width = 0.5)
    expect_equal(nrow(explanation), 1)
})
