suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# Simulates a pre-aggregated "percent positive by item, by year" table, the
# kind of analytic file a survey team would hand off after scoring raw
# responses. Every number here is randomly generated.
generate_engagement_items <- function(seed = 20240601) {

  set.seed(seed)

  categories <- c(
    "Leadership & Communication",
    "My Work Unit",
    "Supervisor Support",
    "Work-Life Balance",
    "Career Development",
    "Recognition",
    "Diversity & Inclusion",
    "Global Satisfaction"
  )

  items_per_category <- 4

  item_grid <- tibble(
    Category = rep(categories, each = items_per_category)
  ) %>%
    group_by(Category) %>%
    mutate(item_num = row_number()) %>%
    ungroup() %>%
    mutate(
      item_text = paste0(Category, " - item ", item_num),
      Secondary = paste0(Category, " (detail)")
    ) %>%
    select(Category, Secondary, item_text)

  # flag a couple of categories as needing a split when rendering (too many items)
  split_categories <- categories[c(2, 6)]
  item_grid <- item_grid %>%
    mutate(Split = if_else(Category %in% split_categories,
                            if_else(row_number() %% 2 == 0, " (A)", " (B)"),
                            ""))

  facilities <- c("Example Medical Center", "Peer Facility Average", "National Benchmark")

  items <- item_grid %>%
    tidyr::crossing(facility = facilities)

  n <- nrow(items)

  base <- runif(n, 0.45, 0.85)
  drift <- function(prev) pmin(pmax(prev + rnorm(n, 0, 0.03), 0), 1)

  items <- items %>%
    mutate(
      pos_2019 = round(base, 3),
      pos_2020 = round(drift(pos_2019), 3),
      pos_2021 = round(drift(pos_2020), 3),
      pos_2022 = round(drift(pos_2021), 3),
      # nudge some items with a deliberate jump so "notable change" has signal
      shock    = ifelse(row_number() %% 9 == 0, runif(n, 0.06, 0.12) * sample(c(-1, 1), n, TRUE), 0),
      pos_2023 = round(pmin(pmax(pos_2022 + rnorm(n, 0, 0.03) + shock, 0), 1), 3),
      diff_pos_2022_2023 = pos_2023 - pos_2022,
      subst_diff_pos_2022_2023 = as.integer(abs(diff_pos_2022_2023) >= 0.05),
      consistent_low_pos = as.integer(pos_2019 < 0.5 & pos_2020 < 0.5 & pos_2021 < 0.5 &
                                         pos_2022 < 0.5 & pos_2023 < 0.5)
    ) %>%
    select(-shock)

  items
}
