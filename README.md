
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lime <img src="man/figures/logo.png" width="131px" height="140px" align="right" style="padding-left:10px;background-color:white;" />

[![Travis-CI Build
Status](https://travis-ci.org/thomasp85/lime.svg?branch=master)](https://travis-ci.org/thomasp85/lime)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/thomasp85/lime?branch=master&svg=true)](https://ci.appveyor.com/project/thomasp85/lime)
[![CRAN\_Release\_Badge](http://www.r-pkg.org/badges/version-ago/lime)](https://CRAN.R-project.org/package=lime)
[![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/lime)](https://CRAN.R-project.org/package=lime)
[![Coverage
Status](https://img.shields.io/codecov/c/github/thomasp85/lime/master.svg)](https://codecov.io/github/thomasp85/lime?branch=master)

> There once was a package called lime,
> 
> Whose models were simply sublime,
> 
> It gave explanations for their variations,
> 
> one observation at a time.

*lime-rick by Mara Averick*

-----

*This is an R port of the Python lime package
(<https://github.com/marcotcr/lime>) developed by the authors of the
lime (Local Interpretable Model-agnostic Explanations) approach for
black-box model explanations. All credits for the invention of the
approach goes to the original developers.*

The purpose of `lime` is to explain the predictions of black box
classifiers. What this means is that for any given prediction and any
given classifier it is able to determine a small set of features in the
original data that has driven the outcome of the prediction. To learn
more about the methodology of `lime` read the
[paper](https://arxiv.org/abs/1602.04938) and visit the repository of
the [original implementation](https://github.com/marcotcr/lime).

The `lime` package for R does not aim to be a line-by-line port of its
Python counterpart. Instead it takes the ideas laid out in the original
code and implements them in an API that is idiomatic to R.

## An example

Out of the box `lime` supports a long range of models, e.g. those
created with caret, parsnip, and mlr. Support for unsupported models are
easy to achieve by adding a `predict_model` and `model_type` method for
the given model.

The following shows how a random forest model is trained on the iris
data set and how `lime` is then used to explain a set of new
observations:

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
explanation
#> # A tibble: 10 x 13
#>    model_type case  label label_prob model_r2 model_intercept
#>    <chr>      <chr> <chr>      <dbl>    <dbl>           <dbl>
#>  1 classific… 1     seto…          1    0.680           0.120
#>  2 classific… 1     seto…          1    0.680           0.120
#>  3 classific… 2     seto…          1    0.675           0.125
#>  4 classific… 2     seto…          1    0.675           0.125
#>  5 classific… 3     seto…          1    0.682           0.122
#>  6 classific… 3     seto…          1    0.682           0.122
#>  7 classific… 4     seto…          1    0.667           0.128
#>  8 classific… 4     seto…          1    0.667           0.128
#>  9 classific… 5     seto…          1    0.678           0.121
#> 10 classific… 5     seto…          1    0.678           0.121
#> # … with 7 more variables: model_prediction <dbl>, feature <chr>,
#> #   feature_value <dbl>, feature_weight <dbl>, feature_desc <chr>,
#> #   data <list>, prediction <list>

# And can be visualised directly
plot_features(explanation)
```

![](man/figures/README-unnamed-chunk-2-1.png)<!-- -->

`lime` also supports explaining image and text models. For image
explanations the relevant areas in an image can be highlighted:

``` r
explanation <- .load_image_example()

plot_image_explanation(explanation)
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

Here we see that the second most probably class is hardly true, but is
due to the model picking up waxy areas of the produce and interpreting
them as wax-light surface.

For text the explanation can be shown by highlighting the important
words. It even includes a `shiny` application for interactively
exploring text models:

![interactive text explainer](man/figures/shine_text_explanations.gif)

## Installation

`lime` is available on CRAN and can be installed using the standard
approach:

``` r
install.packages('lime')
```

To get the development version, install from GitHub instead:

``` r
# install.packages('devtools')
devtools::install_github('thomasp85/lime')
```

## Code of Conduct

Please note that the ‘lime’ project is released with a [Contributor Code
of Conduct](https://lime.data-imaginist.com/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
