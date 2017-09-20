
<!-- README.md is generated from README.Rmd. Please edit that file -->
lime <img src="man/figures/lime_logo.jpg" align="right" />
==========================================================

[![Travis-CI Build Status](https://travis-ci.org/thomasp85/lime.svg?branch=master)](https://travis-ci.org/thomasp85/lime) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/thomasp85/lime?branch=master&svg=true)](https://ci.appveyor.com/project/thomasp85/lime) [![CRAN\_Release\_Badge](http://www.r-pkg.org/badges/version-ago/lime)](https://CRAN.R-project.org/package=lime) [![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/lime)](https://CRAN.R-project.org/package=lime) [![Coverage Status](https://img.shields.io/codecov/c/github/thomasp85/lime/master.svg)](https://codecov.io/github/thomasp85/lime?branch=master)

*This is an R port of the Python lime package (<https://github.com/marcotcr/lime>) developed by the authors of the lime (Local Interpretable Model-agnostic Explanations) approach for black-box model explanations. All credits for the invention of the approach goes to the original developers.*

The purpose of `lime` is to explain the predictions of black box classifiers. What this means is that for any given prediction and any given classifier it is able to determine a small set of features in the original data that has driven the outcome of the prediction. To learn more about the methodology of `lime` read the [paper](https://arxiv.org/abs/1602.04938) and visit the repository of the [original implementation](https://github.com/marcotcr/lime).

The `lime` package for R does not aim to be a line-by-line port of its Python counterpart. Instead it takes the ideas laid out in the original code and implements them in an API that is idiomatic to R.

An example
----------

Out of the box `lime` supports models created using the `caret` and `mlr` frameworks. Support for other models are easy to achieve by adding a `predict_model` and `model_type` method for the given model.

The following shows how a random forest model is trained on the iris data set and how `lime` is then used to explain a set of new observations:

``` r
library(caret)
library(lime)

# Split up the data set
iris_test <- iris[1:5, 1:4]
iris_train <- iris[-(1:5), 1:4]
iris_lab <- iris[[5]][-(1:5)]

# Create Random Forest model on iris data
model <- train(iris_train, iris_lab, method = 'rf')

# Create an explainer object
explainer <- lime(iris_train, model)

# Explain new observation
explanation <- explain(iris_test, explainer, n_labels = 1, n_features = 2)

# The output is provided in a consistent tabular format and includes the
# output from the model.
head(explanation)
#>       model_type case  label label_prob  model_r2 model_intercept
#> 1 classification    1 setosa          1 0.3928161       0.2506065
#> 2 classification    1 setosa          1 0.3928161       0.2506065
#> 3 classification    2 setosa          1 0.4073850       0.2430483
#> 4 classification    2 setosa          1 0.4073850       0.2430483
#> 5 classification    3 setosa          1 0.3991240       0.2463829
#> 6 classification    3 setosa          1 0.3991240       0.2463829
#>   model_prediction      feature feature_value feature_weight
#> 1        0.7192690 Sepal.Length           5.1    0.008395256
#> 2        0.7192690 Petal.Length           1.4    0.460267268
#> 3        0.7293933 Sepal.Length           4.9    0.012021577
#> 4        0.7293933 Petal.Length           1.4    0.474323339
#> 5        0.7215491 Sepal.Length           4.7    0.011426553
#> 6        0.7215491 Petal.Length           1.3    0.463739592
#>          feature_desc               data prediction
#> 1 Sepal.Length <= 5.2 5.1, 3.5, 1.4, 0.2    1, 0, 0
#> 2 Petal.Length <= 1.6 5.1, 3.5, 1.4, 0.2    1, 0, 0
#> 3 Sepal.Length <= 5.2 4.9, 3.0, 1.4, 0.2    1, 0, 0
#> 4 Petal.Length <= 1.6 4.9, 3.0, 1.4, 0.2    1, 0, 0
#> 5 Sepal.Length <= 5.2 4.7, 3.2, 1.3, 0.2    1, 0, 0
#> 6 Petal.Length <= 1.6 4.7, 3.2, 1.3, 0.2    1, 0, 0

# And can be visualised directly
plot_features(explanation)
```

![](man/figures/README-unnamed-chunk-2-1.png)

`lime` also supports explaining text model and putting the explanation in the context of the original text input. It even includes a `shiny` application for interactively exploring text models:

![interactive text explainer](man/figures/shine_text_explanations.gif)

Installation
------------

`lime` is still a work in progress and is thus not available on CRAN yet. In order to try it out install it directly from GitHub:

``` r
# install.packages('devtools')
devtools::install_github('thomasp85/lime')
```

Scope
-----

The current version of `lime` has support for tabular and text data. The Python implementation has additional support for image data, which will be added to this package in time. In addition to the capabilities discussed in the *"Why Should I Trust You?": Explaining the Predictions of Any Classifier* article, this package also support regression model explanations (this has been added to the Python library as well). The global model explanation using submodular picks is not supported in either packages.
