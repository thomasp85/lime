library(stringi)
library(lime)
library(text2vec)
library(data.table)
library(magrittr)
library(purrr)
library(xgboost)

set.seed(2000)

data("train.sentences")
data("test.sentences")

train.sentences[, label := class.text == "OWNX"]
test.sentences[, label := class.text == "OWNX"]

get.iterator <- function(data) itoken(data, preprocess_function = tolower, tokenizer = word_tokenizer, progressbar = F)
stop.words <- readLines("./data-raw/SentenceCorpus/word_lists/stopwords.txt")

v <-  create_vocabulary(get.iterator(train.sentences$text), stopwords = stop.words)

get.matrix <- function(data) {
  i <- get.iterator(data)
  create_dtm(i, vocab_vectorizer(v))
}

lsa.full.text <- LSA$new(n_topics = 100)
tfidf <- TfIdf$new()
get.matrix(train.sentences$text) %>% fit(tfidf)
get.matrix(train.sentences$text) %>% transform(tfidf) %>% fit(lsa.full.text)

add.lsa <- function(m, lsa) {
  l <- transform(m, lsa)
  colnames(l) <- ncol(l) %>% seq() %>% paste0("lsa.", .)
  cbind2(m, l)
}

dtrain <- get.matrix(train.sentences$text) %>% transform(tfidf) %>% add.lsa(lsa.full.text) %>% xgb.DMatrix(label = train.sentences$label)
dtest <-  get.matrix(test.sentences$text) %>% transform(tfidf) %>% add.lsa(lsa.full.text) %>% xgb.DMatrix(label = test.sentences$label)


watchlist <- list(eval = dtest)
param <- list(max_depth = 7, eta = 0.1, objective = "binary:logistic", eval_metric = "error")
bst <- xgb.train(param, dtrain, nrounds = 500, watchlist, early_stopping_rounds = 50)

test.sentences[,prediction := predict(bst, dtest, type = "prob") > 0.5]
test.sentences[label == T, sum(label != prediction)]
test.sentences[label == T, sum(label == prediction)]
test.sentences[, sum(label == prediction)/length(label)]
test.sentences[, mean(label)]


require(lime)

get.features.matrix <- . %>%
  get.matrix() %>%
  transform(tfidf) %>%
  add.lsa(lsa.full.text) %>%
  xgb.DMatrix()




lime(test.sentences[label == T][4:6, text], bst, get.features.matrix, n_labels = 1, number_features_explain = 2) %>%
  print

# m <- xgb.DMatrix(r$tabular, label = predicted.labels, weight = 1 - r$permutation.distances)
# bst.bow <- xgb.train(list(max_depth = 3, eta = 1, silent = 1, objective = "binary:logistic"), m, nrounds = 1)
#
# bst.bow %>%
#   xgb.importance(model = .) %>%
#   .[,word := r$tokens[as.numeric(Feature) + 1]] %>%
#   print()
#
# predicted.labels <- predicted.labels.raw %>%
#   is_greater_than(0.5)
#which(predicted.labels == T) %>% permutation.candidates[.]
