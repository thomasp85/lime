library(magrittr)
library(purrr)

context("Text permutations")

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

  # There is no empty permutation
  expect_true(sum(lengths(permutations) == 0) == 0)
})

test_that("Cosine computation is correct", {
  v <- 5:13
  m <- Matrix::Matrix(1:81, ncol = 9, sparse = TRUE)
  cos <- function(x) crossprod(v, x)/sqrt(crossprod(v) * crossprod(x))
  testthat::expect_equal(lime:::cosine_distance_vector_to_matrix_rows(v, m), apply(m, MARGIN = 1, cos))
})

test_that("there is no empty generated text", {
  generated_documents <- lime:::permute_cases.character(cases = "this is a test ", n_permutations = 5e3, tokenization = default_tokenize, keep_word_position = FALSE)

  # There is no empty permutation
  expect_true(sum(generated_documents$permutations %>% map(~ nchar(.x)) %>% flatten_int() == 0) == 0)

})

test_that("Default tokenizer works for multiple documents", {
  r <- lime:::default_tokenize(c("    this is a test.", "this is another       test."))
  expect_equal(r , c("this", "is",   "a", "test", "this", "is", "another","test"))
})

