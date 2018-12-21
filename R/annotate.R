#' Calculate edit distance between R expressions.
#'
#' @description Calculates edit distance at the level of R tokens. See
#' [sourcetools::tokenize_string] for details on tokenizer.
#'
#' @name edit-dist
#' @rdname edit_dist
NULL

#' @param x R [base::expression] or R expression string (see [base::deparse]).
#'
#' @name edit-dist
#' @rdname edit_dist
#'
#' @importFrom rlang is_symbolic
tokenize <- function(x) {
  stop_if_no_sourcetools()

  if (is.expression(x)) {
    x <- lapply(x, I)
    if (length(x) > 1) abort("Cannot handle more than one expression at a time.")
    x <- first(x)
  }

  if (is_symbolic(x)) x <- paste(deparse(x), collapse = '\n')

  if (!is.character(x)) {
    abort(glue("Cannot tokenize object of class {first(class(x))}. Pass R expression or a character string."))
  }

  tokens <- sourcetools::tokenize_string(x)

  i <- tokens$type == 'invalid'
  if (any(i)) {
    abort(glue("Found invalid tokens: ", paste(tokens$value[i], collapse = ' ')))
  }

  nws <- (tokens$type != "whitespace")
  as_tokens(tokens$value[nws])
}

as_tokens <- function (x) structure(x, class = 'tokens')

is_tokens <- function (x) inherits(x, 'tokens')

#' @param a tokenized R expression; output of `tokenize()`.
#' @param b tokenized R expression; output of `tokenize()`.
#'
#' @name edit-dist
#' @rdname edit_dist
#'
#' @examples
#' \dontrun{
#' edit_dist(tokenize(bquote(x <- 1)), tokenize(bquote(y <- 1)))
#' }
edit_dist <- function (a, b) {
  stopifnot(is_tokens(a), is_tokens(b))
  edit_dist_impl(a, b)
}



annotate_file <- function (path, repo, n = 3) {
  exprs <- parse(file = path, keep.source = TRUE)
  artfs <- annotate_expressions(exprs, repo, n = n)

  structure(exprs, class = c("annotated", class(exprs)), artref = artfs)
}

#' @export
print.annotated <- function(x, ...) {
  Map(function (expr, artref) {
    # print matches
    a_ids <- lapply(artref, function(a) toString(a$id))
    if (length(a_ids)) {
      ccat0(grey = "# Matching artifacts: ", green = paste(a_ids, collapse = " "), '\n')
    } else {
      ccat(grey = "# No matching artifacts\n")
    }

    # print expression
    print(expr)
  }, expr = x, artref = attr(x, 'artref'))

  invisible(x)
}

annotate_expressions <- function (exprs, repo, n) {
  # each expression will be assigned n top matching artifacts
  matches <- lapply(exprs, function (expr) {
    identify_expression(expr, repo, n = n)
  })

  # clean up the results: make sure each artifact is assigned
  # only once, to the best-matching expression
  artfs <- new_map()
  summary <- do.call(rbind, unlist(imap(matches, function (artifacts, index) {
    lapply(artifacts, function (a) {
      artfs$assign(a$id, a)
      data.frame(e_no = index, a_id = unclass(a$id), dist = a$dist, stringsAsFactors = FALSE)
    })
  }), recursive = FALSE))

  # keep those artifacts that have exact matches (dist == 0)
  exact <- summary[summary$dist == 0, ]
  # remove exact matches: by artifacts and by expression number; what is
  # left are expressions without an exact match
  rest  <- summary[summary$a_id %nin% exact$a_id,]
  rest  <- rest[rest$e_no %nin% exact$e_no,]

  # TODO this shouldn't happen in real life, but some expressions might
  #      need to be re-matched after artifacts are assigned to their closest
  #      expressions; maybe there are other, not yet assigned, artifacts that
  #      still make sense to assign to "rest" but didn't make the cut in the
  #      first pass?`

  summary <- rbind(exact, rest)

  # finally, match artifacts to expressions; re-assign dist because the
  # "artfs" map holds a single artifact for each a_id and it might not
  # be the result of that particular expression match
  lapply(seq_along(exprs), function (index) {
    smr <- summary[summary$e_no == index, ]
    Map(function (a_id, dist) {
      art <- artfs$at(a_id)
      art$dist <- dist
      art
    }, a_id = smr$a_id, dist = smr$dist)
  })
}
