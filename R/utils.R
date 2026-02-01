encode_sentiment <- function(x) {

  dplyr::case_match(x,
    "v" ~ 3L,
    "r" ~ 2L,
    "x" ~ 1L,
    "o" ~ NA_integer_
  )
}
