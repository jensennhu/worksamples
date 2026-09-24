suppressPackageStartupMessages(library(dplyr))

# Left join that tags each row "matched" or "left_only", so mismatched keys
# are visible instead of silently disappearing. Optionally prints a quick
# tally, which is handy right after a join to sanity-check merge rates.
checked_join <- function(x, y, by, tab = FALSE) {
  y_flagged <- y %>% mutate(.matched_flag = TRUE)

  result <- x %>%
    left_join(y_flagged, by = by) %>%
    mutate(merge = if_else(is.na(.matched_flag), "left_only", "matched")) %>%
    select(-.matched_flag)

  if (isTRUE(tab)) {
    print(table(result$merge))
  }

  result
}
