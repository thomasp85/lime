library(lime)
library(stringi)
library(text2vec)
library(data.table)
library(magrittr)
library(purrr)
library(xgboost)

set.seed(2000)

# Data loading
data("train_sentences")
data("test_sentences")
data("stop_words_sentences")

setDT(train_sentences)
setDT(test_sentences)

label_to_explain <- "OWNX"

# label train set and test set
train_sentences[, label := class.text == label_to_explain]
test_sentences[, label := class.text == label_to_explain]

get.iterator <- function(data) itoken(data, preprocess_function = tolower, tokenizer = word_tokenizer, progressbar = F)

# Extract vocabulary
v <- create_vocabulary(get.iterator(train_sentences$text), stopwords = stop_words_sentences)

# Function to transform text in matrix
get.matrix <- function(data) {
  i <- get.iterator(data)
  create_dtm(i, vocab_vectorizer(v))
}

lsa.full.text <- LSA$new(n_topics = 100)
tfidf <- TfIdf$new()
invisible(get.matrix(train_sentences$text) %>% tfidf$fit_transform())
invisible(get.matrix(train_sentences$text) %>% transform(tfidf) %>% lsa.full.text$fit_transform())

add.lsa <- function(m, lsa) {
  l <- transform(m, lsa)
  colnames(l) <- ncol(l) %>% seq() %>% paste0("lsa.", .)
  cbind2(m, l)
}

dtrain <- get.matrix(train_sentences$text) %>% transform(tfidf) %>% add.lsa(lsa.full.text) %>% xgb.DMatrix(label = train_sentences$label)
dtest <-  get.matrix(test_sentences$text) %>% transform(tfidf) %>% add.lsa(lsa.full.text) %>% xgb.DMatrix(label = test_sentences$label)

watchlist <- list(train = dtrain, eval = dtest)
param <- list(max_depth = 7, eta = 0.1, objective = "binary:logistic", eval_metric = "error", nthread = 1)
bst <- xgb.train(param, dtrain, nrounds = 500, watchlist, early_stopping_rounds = 100)

test_sentences[,prediction := predict(bst, dtest, type = "prob") > 0.5]
test_sentences[label == T, sum(label != prediction)]
test_sentences[label == T, sum(label == prediction)]
test_sentences[, sum(label == prediction)/length(label)]
test_sentences[, mean(label)]

get.features.matrix <- . %>%
  get.matrix() %>%
  transform(tfidf) %>%
  add.lsa(lsa.full.text) %>%
  xgb.DMatrix()

sentences_to_explain <- test_sentences[label == T][1:10, text]

system.time(results <- lime(sentences_to_explain, bst, get.features.matrix, keep_word_position = FALSE)(cases = sentences_to_explain, n_labels = 1, n_features = 5) %T>%
  print)

system.time(lime(sentences_to_explain, bst, get.features.matrix, keep_word_position = FALSE)(cases = sentences_to_explain, n_labels = 1, n_features = 4, feature_select = "tree"))

plot_text_explanations(results) %>% print()

long_document <- test_sentences[label == T][5, text] %>% rep(50) %>% paste(collapse = " ")
system.time(lime(long_document, bst, get.features.matrix, keep_word_position = FALSE, feature_select = "highest_weights")(cases = long_document, n_labels = 1, n_features = 5) %T>%
              print)
system.time(lime(long_document, bst, get.features.matrix, keep_word_position = FALSE, feature_select = "tree")(cases = long_document, n_labels = 1, n_features = 5) %T>%
              print)
system.time(lime(long_document, bst, get.features.matrix, keep_word_position = TRUE, feature_select = "tree")(cases = long_document, n_labels = 1, n_features = 5) %T>%
              print)
system.time(lime(long_document, bst, get.features.matrix, keep_word_position = TRUE, feature_select = "highest_weights")(cases = long_document, n_labels = 1, n_features = 5) %T>%
              print)
