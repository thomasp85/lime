#' Sentence corpus - train part
#'
#' This corpus contains sentences from
#' the abstract and introduction of 30 scientific articles that have been
#' annotated (i.e. labeled or tagged) according to a modified version of the
#' Argumentative Zones annotation scheme.
#'
#' These 30 scientific articles come
#' from three different domains:
#' 1. PLoS Computational Biology (PLOS)
#' 2. The machine learning repository on arXiv (ARXIV)
#' 3. The psychology journal Judgment and Decision Making (JDM)
#'
#' There are 10 articles from each domain. In addition to the labeled data, this
#' corpus also contains a corresponding set of unlabeled articles. These unlabeled
#' articles also come from PLOS, ARXIV, and JDM. There are 300 unlabeled articles
#' from each domain (again, only the sentences from the abstract and
#' introduction). These unlabeled articles can be used for unsupervised or
#' semi-supervised approaches to sentence classification which rely on a small set
#' of labeled data and a larger set of unlabeled data.
#'
#' ===== References =====
#'
#' S. Teufel and M. Moens. Summarizing scientific articles: experiments with
#' relevance and rhetorical status. Computational Linguistics, 28(4):409-445,
#' 2002.
#'
#' S. Teufel. Argumentative zoning: information extraction from scientific
#' text. PhD thesis, School of Informatics, University of Edinburgh, 1999.
#'
#' @format 2 data frame with 3117 rows and 2 variables:
#' \describe{
#'   \item{text}{the sentences as a character vector}
#'   \item{class.text}{the category of the sentence}
#' }
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Sentence+Classification}
"train_sentences"

#' Sentence corpus - test part
#'
#' This corpus contains sentences from
#' the abstract and introduction of 30 scientific articles that have been
#' annotated (i.e. labeled or tagged) according to a modified version of the
#' Argumentative Zones annotation scheme.
#'
#' These 30 scientific articles come
#' from three different domains:
#' 1. PLoS Computational Biology (PLOS)
#' 2. The machine learning repository on arXiv (ARXIV)
#' 3. The psychology journal Judgment and Decision Making (JDM)
#'
#' There are 10 articles from each domain. In addition to the labeled data, this
#' corpus also contains a corresponding set of unlabeled articles. These unlabeled
#' articles also come from PLOS, ARXIV, and JDM. There are 300 unlabeled articles
#' from each domain (again, only the sentences from the abstract and
#' introduction). These unlabeled articles can be used for unsupervised or
#' semi-supervised approaches to sentence classification which rely on a small set
#' of labeled data and a larger set of unlabeled data.
#'
#' ===== References =====
#'
#' S. Teufel and M. Moens. Summarizing scientific articles: experiments with
#' relevance and rhetorical status. Computational Linguistics, 28(4):409-445,
#' 2002.
#'
#' S. Teufel. Argumentative zoning: information extraction from scientific
#' text. PhD thesis, School of Informatics, University of Edinburgh, 1999.
#'
#' @format 2 data frame with 3117 rows and 2 variables:
#' \describe{
#'   \item{text}{the sentences as a character vector}
#'   \item{class.text}{the category of the sentence}
#' }
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Sentence+Classification}
"test_sentences"

#' Stop words list
#'
#' List of words that can be safely removed from sentences.
#'
#' @format Character vector of stop words
#' @source \url{https://archive.ics.uci.edu/ml/datasets/}
"stop_words_sentences"
