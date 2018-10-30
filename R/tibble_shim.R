# Avoid depending on tibble but provide nice printing for people who have it
as_tibble <- function(x) {
  class(x) <- c('tbl_df', class(x))
  x
}
