# lime 0.5.1

* Fixed namespace import from glmnet following changes there

# lime 0.5.0

* `explain()` will now pass `...` on to the relevant `predict()` method (#150)
* `explain.data.frame()` gains a `gower_pow` argument to modify the calculated 
  gower distance before use by raising it to the power of the given value (#158)
* Fixed a bug when calculating R^2 on single feature explanations (@pkopper, #157)
* Fixed formatting of text prediction html presentation (#145)
* Fixed a bug when setting feature select method to "none" (#141)
* Changes default colouring from green-red to blue-red (#137)
* `lime()` now warns when quantile binning is not feasible and uses standard 
  binning instead (#154)
* Changed the `lambda` value in the local model fit to match the one used in the
  Python version according to the relationship given here:
  https://stats.stackexchange.com/a/270705
* Added pkgdown site at https://lime.data-imaginist.com
* Fixed a bug when using a proprocessor with data.frame explanations

# lime 0.4.1

* Add build-in support for `parsnip` and `ranger`
* Add `preprocess` argument to `lime.data.frame` to keep it in line with the 
  other types. Use it to transform your data.frame into a new input that your
  model expects after permutations
* `magick` is now only in suggest to cut down on heavy hard dependencies
* `explain` now returns a `tbl_df` so you get pretty printing if you have 
  `tibble` loaded
* When plotting regression explanations of non-binned features the 
  feature weight is now multiplied by its value
* More consistent support for keras
* Fix bug when xgboost was used with with default objective
* Better errors when handling bad models
* `plot_features` now has a `cases` argument for subsetting the data before 
  plotting


# lime 0.4

* Add support for image explanation. The dispatch will be on paths pointing to
  valid image files. Image explanations can be visualised using 
  `plot_image_explanation` (#35)
* Add support for neural networks from the `keras` package
* Add `as_classifier()` and `as_regressor()` for ad-hoc specification of the 
  model type in case the heuristic implemented in `lime` doesn't hold. 
  `as_classifier()` also lets you add/overwrite the class labels.
* Use `gower` as the new default similarity measure for tabular data
* If `bin_continuous = FALSE` the default behavior is now to sample from a 
  kernel density estimation rather than assume a normal distribution.
* Fix bug when numeric features in the training data were constant (#56)
* Fix bug when plotting regression explanations with `plot_explanations()` (#60)
* Logical columns in tabular data is now supported (#75)
* Overhaul of `plot_text_explanation()` with better formatting and scrolling
  support for many explanations
* All plots now show the fit of the explainer so the user can assess the quality
  of the explanation

# lime 0.3.1

* Added a `NEWS.md` file to track changes to the package.
* Fixed bug when explaining regression models, due to drop=TRUE defaults (#33)
* Integer features are no longer converted to numeric during permutations (#32)
* Fix bug when working with xgboost and tabular predictions (@martinju #1)
* Training data can now contain `NA` values (#8) 
* Keep ordering when plotting with `plot_features()` (#38)
* Fix support for mlr by extracting predictions correctly
* Added support for `h2o` (@mdancho84) (#40)
* Throws meaningful error when all permutations have 0 similarity to original
  observation (#47)
* Explaining data can now contain `NA` values (#45)
* Support for `Date` and `POSIXt` columns. They will be kept constant during
  permutations so that `lime` will explain the model behaviour at the given 
  timepoint based on the remaining features (#39).
* Add `plot_explanations()` for an overview plot of a large explanation set
