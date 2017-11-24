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
