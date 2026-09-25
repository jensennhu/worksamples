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

# Stata-style merge: tags every row 1 (master/x only), 2 (using/y only), or
# 3 (matched in both), so a mismatched join key is visible in a `table()`
# instead of silently dropping or duplicating rows.
stata_merge <- function(x, y, by, type = c("full", "left"), tab = FALSE) {
  type <- match.arg(type)

  x_flagged <- x %>% dplyr::mutate(.in_x = TRUE)
  y_flagged <- y %>% dplyr::mutate(.in_y = TRUE)

  joined <- if (type == "full") {
    dplyr::full_join(x_flagged, y_flagged, by = by)
  } else {
    dplyr::left_join(x_flagged, y_flagged, by = by)
  }

  joined <- joined %>%
    dplyr::mutate(
      merge = dplyr::case_when(
        .in_x & .in_y                 ~ 3L,
        .in_x & is.na(.in_y)          ~ 1L,
        is.na(.in_x) & .in_y          ~ 2L
      )
    ) %>%
    dplyr::select(-.in_x, -.in_y)

  if (isTRUE(tab)) {
    print(table(joined$merge))
  }

  joined
}

full_join_check <- function(x, y, by, tab = FALSE) stata_merge(x, y, by, type = "full", tab = tab)
left_join_check <- function(x, y, by, tab = FALSE) stata_merge(x, y, by, type = "left", tab = tab)
