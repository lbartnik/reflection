as_tokens <- function (x) structure(x, class = 'tokens')

is_tokens <- function (x) inherits(x, 'tokens')


tokenize <- function(x) {
  stop_if_no_sourcetools()

  if (is.expression(x)) x <- deparse(x)

  tokens <- sourcetools::tokenize_string(x)
  nws <- (tokens$type != "whitespace")

  as_tokens(tokens$value[nws])
}


edit_dist <- function (a, b) {
  stopifnot(is_tokens(a), is_tokens(b))
  edit_dist_impl(a, b)
}

