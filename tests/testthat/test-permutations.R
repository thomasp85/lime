library(lime)
library(magrittr)
library(purrr)

context("Permutations")

test_that("properties of permutations are as expected", {
  set.seed(2000)
  original_document = runif(20, 0, 30) %>% as.integer() %>% unique()
  number_permutations = 1e5
  # Generate permutations
  permutations <- lime:::get_index_permutations(original_document, number_permutations)

  # Expected number of permutations
  expect_length(permutations, number_permutations)

  # First permutation is the original document
  expect_equal(permutations[[1]], original_document)

  # No permutation is larger than the original document
  expect_true(all(lengths(permutations) <= length(original_document)))

  # Permutations doesn't contain duplicates
  expect_true(permutations %>% map(~ all(!duplicated(.))) %>% flatten_lgl() %>% all)
})
