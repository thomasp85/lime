
<!-- README.md is generated from README.Rmd. Please edit that file -->
lime
====

[![Travis-CI Build Status](https://travis-ci.org/thomasp85/lime.svg?branch=master)](https://travis-ci.org/thomasp85/lime) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/thomasp85/lime?branch=master&svg=true)](https://ci.appveyor.com/project/thomasp85/lime) [![CRAN\_Release\_Badge](http://www.r-pkg.org/badges/version-ago/lime)](https://CRAN.R-project.org/package=lime) [![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/lime)](https://CRAN.R-project.org/package=lime)

*This is an R port of the Python lime package (<https://github.com/marcotcr/lime>) developed by the authors of the lime (Local Interpretable Model-agnostic Explanations) approach for black-box model explanations. All credits goes to the original developers.*

The purpose of `lime` is to explain the predictions of black box classifiers. What this means is that for any given prediction and any given classifier it is able to determine a small set of features in the original data that has driven the outcome of the prediction. To learn more about the methodology of `lime` read the [paper](https://arxiv.org/abs/1602.04938) and visit the repository of the [original implementation](https://github.com/marcotcr/lime).

The `lime` package for R does not aim to be a line-by-line port of its Python counterpart. Instead it takes the ideas laid out in the original code and implements them in an API that is idiomatic to R.

An example
----------

The only requirement for the classifier that `lime` imposes is that it must implement a `predict()` method accepting a `type = 'prob'` argument (which outputs the probability for each class). Conveniently, this covers all classifiers available through [`caret`](https://CRAN.R-project.org/package=caret), and if your model of choice is not covered you have the possibility of writing your own predict method.

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

# Create explanation function
explain <- lime(iris_train, model)

# Explain new observation
explain(iris_test, n_labels = 1, n_features = 2)
#> # A tibble: 10 × 11
#>     case  label label_prob  model_r2 model_intercept      feature
#>    <chr>  <chr>      <dbl>     <dbl>           <dbl>        <chr>
#> 1      1 setosa          1 0.7201131      0.10770287 Petal.Length
#> 2      1 setosa          1 0.7201131      0.10770287  Petal.Width
#> 3      2 setosa          1 0.7385668      0.09752498 Petal.Length
#> 4      2 setosa          1 0.7385668      0.09752498  Petal.Width
#> 5      3 setosa          1 0.7212672      0.09894034 Petal.Length
#> 6      3 setosa          1 0.7212672      0.09894034  Petal.Width
#> 7      4 setosa          1 0.7325818      0.10247722 Petal.Length
#> 8      4 setosa          1 0.7325818      0.10247722  Petal.Width
#> 9      5 setosa          1 0.7358051      0.10044829 Petal.Length
#> 10     5 setosa          1 0.7358051      0.10044829  Petal.Width
#> # ... with 5 more variables: feature_value <dbl>, feature_weight <dbl>,
#> #   feature_desc <chr>, data <list>, prediction <list>
```

Installation
------------

`lime` is still a work in progress and is thus not available on CRAN yet. In order to try it out install it directly from GitHub:

``` r
# install.packages('devtools')
devtools::install_github('thomasp85/lime')
```

Scope
-----

In addition to standard tabular data the Python implementation also provides specialized explainers for text and image data. Furthermore, the article also discusses how `lime` can be used for explaining *models* rather than *predictions*, using an approach called submodular picks. Lastly, there's obvious extensions to the approach such as using different algorithms for the local fit step, supporting regressors in addition to classifiers, and providing additional feature selection algorithms. All of the above will hopefully find its way to this port in due time...
