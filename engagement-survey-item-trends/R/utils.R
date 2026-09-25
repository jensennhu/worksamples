suppressPackageStartupMessages(library(dplyr))

# Asserts a data frame is uniquely identified by the given columns, the way
# Stata's `isid` does. Returns the data invisibly (unchanged) so it can sit
# inline in a pipe; stops if any combination of key columns repeats.
isid <- function(data, ...) {
  keys <- rlang::ensyms(...)

  dup_count <- data %>%
    dplyr::count(!!!keys) %>%
    dplyr::filter(n > 1) %>%
    nrow()

  if (dup_count > 0) {
    stop("isid: data is not uniquely identified by ", paste(sapply(keys, rlang::as_name), collapse = ", "))
  }

  invisible(data)
}
