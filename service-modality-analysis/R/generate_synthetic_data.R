suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
})

# Simulates a store-level panel plus a region-level survey completion feed,
# standing in for the two source tables the real analysis would read from a
# data warehouse. Every number here is randomly generated.
generate_store_panel <- function(n_regions = 30, stores_per_region = 6, seed = 20240115) {

  set.seed(seed)

  region_id   <- sprintf("RG%02d", seq_len(n_regions))
  region_name <- paste("Region", region_id)

  stores <- tibble(region_id = rep(region_id, each = stores_per_region)) %>%
    group_by(region_id) %>%
    mutate(store_seq = row_number()) %>%
    ungroup() %>%
    mutate(
      store_id   = paste0(region_id, sprintf("%03d", store_seq)),
      store_name = paste("Store", store_id)
    ) %>%
    left_join(tibble(region_id, region_name), by = "region_id") %>%
    select(store_id, store_name, region_id, region_name)

  n_stores <- nrow(stores)

  # store-level characteristics that stay fixed across fiscal years
  fixed <- stores %>%
    mutate(
      resp_cust_survey = rbinom(n_stores, 1, 0.55),
      resp_emp_survey  = rbinom(n_stores, 1, 0.45),
      market_type      = sample(1:4, n_stores, replace = TRUE, prob = c(0.30, 0.35, 0.20, 0.15)),
      store_size_tier  = sample(c("small", "medium", "large"), n_stores, replace = TRUE, prob = c(0.35, 0.45, 0.20))
    )

  # service-channel mix: one measurement per store, in-store/curbside/delivery sum to 100
  raw_mix <- matrix(runif(n_stores * 3), ncol = 3)
  mix <- raw_mix / rowSums(raw_mix) * 100
  fixed <- fixed %>%
    mutate(
      svc_instore_prop  = round(mix[, 1], 2),
      svc_curbside_prop = round(mix[, 2], 2),
      svc_delivery_prop = round(100 - svc_instore_prop - svc_curbside_prop, 2)
    )

  fiscal_years <- 1:6

  panel <- fixed %>%
    tidyr::crossing(fiscal_year = fiscal_years)

  n <- nrow(panel)

  # customer segment mix, sums to 100 per store-year
  seg_raw <- matrix(runif(n * 6), ncol = 6)
  seg <- seg_raw / rowSums(seg_raw) * 100
  colnames(seg) <- c(
    "pct_seg_young_adult", "pct_seg_family", "pct_seg_senior",
    "pct_seg_business", "pct_seg_student", "pct_seg_other"
  )

  panel <- panel %>%
    bind_cols(as.data.frame(round(seg, 2))) %>%
    mutate(
      total_customers          = round(rnorm(n, mean = 8000, sd = 1500)),
      discount_zone_flag       = rbinom(n, 1, 0.25),
      emp_fte                  = round(rnorm(n, mean = 22, sd = 5), 1),
      emp_customer_ratio       = round(total_customers / pmax(emp_fte, 1), 1),
      total_customers_loyalty  = round(total_customers * runif(n, 0.15, 0.45)),
      numvalid_ops             = round(rnorm(n, mean = 400, sd = 60)),
      numvalid_svc             = round(rnorm(n, mean = 380, sd = 60)),
      pct_target_ops           = round(pmin(pmax(rnorm(n, 62, 12), 0), 100), 1),
      pct_target_svc           = round(pmin(pmax(rnorm(n, 58, 12), 0), 100), 1),
      include_analysis         = rbinom(n, 1, 0.92)
    )

  # baseline (period 3, "fy19") experience-index draws, then a period-6 ("fy22")
  # value that reflects a channel-mix effect on top of the baseline plus noise
  baseline_noise <- function() rnorm(n_stores, mean = 65, sd = 8)

  idx_base <- tibble(
    store_id          = fixed$store_id,
    idx_overall_cust_b = baseline_noise(),
    idx_loyalty_cust_b = baseline_noise(),
    idx_service_cust_b = baseline_noise(),
    idx_culture_cust_b = baseline_noise(),
    idx_overall_emp_b  = baseline_noise(),
    idx_loyalty_emp_b  = baseline_noise(),
    idx_service_emp_b  = baseline_noise(),
    idx_culture_emp_b  = baseline_noise()
  )

  effect <- fixed %>%
    transmute(
      store_id,
      shift_effect = 0.10 * svc_delivery_prop + 0.06 * svc_curbside_prop
    )

  idx_wide <- idx_base %>%
    left_join(effect, by = "store_id") %>%
    mutate(
      idx_overall_cust_f = idx_overall_cust_b + shift_effect + rnorm(n_stores, 0, 4),
      idx_loyalty_cust_f = idx_loyalty_cust_b + shift_effect + rnorm(n_stores, 0, 4),
      idx_service_cust_f = idx_service_cust_b + shift_effect + rnorm(n_stores, 0, 4),
      idx_culture_cust_f = idx_culture_cust_b + shift_effect + rnorm(n_stores, 0, 4),
      idx_overall_emp_f  = idx_overall_emp_b  + 0.5 * shift_effect + rnorm(n_stores, 0, 4),
      idx_loyalty_emp_f  = idx_loyalty_emp_b  + 0.5 * shift_effect + rnorm(n_stores, 0, 4),
      idx_service_emp_f  = idx_service_emp_b  + 0.5 * shift_effect + rnorm(n_stores, 0, 4),
      idx_culture_emp_f  = idx_culture_emp_b  + 0.5 * shift_effect + rnorm(n_stores, 0, 4)
    )

  # manager-reported score: correlated with the employee score, but missing
  # for a chunk of stores (mirrors a lower-response-rate rater group)
  mgr_from_emp <- function(emp_b, emp_f) {
    list(
      b = emp_b + rnorm(n_stores, 0, 3),
      f = emp_f + rnorm(n_stores, 0, 3)
    )
  }
  mgr_overall <- mgr_from_emp(idx_wide$idx_overall_emp_b, idx_wide$idx_overall_emp_f)
  mgr_loyalty <- mgr_from_emp(idx_wide$idx_loyalty_emp_b, idx_wide$idx_loyalty_emp_f)
  mgr_service <- mgr_from_emp(idx_wide$idx_service_emp_b, idx_wide$idx_service_emp_f)
  mgr_culture <- mgr_from_emp(idx_wide$idx_culture_emp_b, idx_wide$idx_culture_emp_f)

  missing_mask <- rbinom(n_stores, 1, 0.3) == 1

  idx_wide <- idx_wide %>%
    mutate(
      idx_overall_mgr_b = ifelse(missing_mask, NA, mgr_overall$b),
      idx_overall_mgr_f = ifelse(missing_mask, NA, mgr_overall$f),
      idx_loyalty_mgr_b = ifelse(missing_mask, NA, mgr_loyalty$b),
      idx_loyalty_mgr_f = ifelse(missing_mask, NA, mgr_loyalty$f),
      idx_service_mgr_b = ifelse(missing_mask, NA, mgr_service$b),
      idx_service_mgr_f = ifelse(missing_mask, NA, mgr_service$f),
      idx_culture_mgr_b = ifelse(missing_mask, NA, mgr_culture$b),
      idx_culture_mgr_f = ifelse(missing_mask, NA, mgr_culture$f)
    )

  # spread the baseline/follow-up index draws across the six fiscal periods:
  # periods 1-2 trend toward baseline, period 3 IS baseline, periods 4-5 trend
  # toward follow-up, period 6 IS follow-up
  idx_long <- idx_wide %>%
    select(store_id, ends_with("_b"), ends_with("_f")) %>%
    pivot_longer(-store_id, names_to = "measure", values_to = "value") %>%
    mutate(
      stage  = ifelse(grepl("_b$", measure), "b", "f"),
      measure = sub("_(b|f)$", "", measure)
    ) %>%
    pivot_wider(names_from = stage, values_from = value) %>%
    crossing(fiscal_year = fiscal_years) %>%
    mutate(
      value = case_when(
        fiscal_year <= 3 ~ b + (fiscal_year - 1) / 2 * (f - b) * 0.15,
        TRUE              ~ b + (fiscal_year - 3) / 3 * (f - b)
      )
    ) %>%
    select(store_id, fiscal_year, measure, value) %>%
    pivot_wider(names_from = measure, values_from = value)

  panel <- panel %>%
    left_join(idx_long, by = c("store_id", "fiscal_year"))

  # region-level survey completion feed, joined on a differently-named key,
  # deliberately missing a few regions so the join demo shows unmatched rows
  completion_regions <- sample(region_id, size = round(n_regions * 0.85))
  survey_completion <- tibble(
    csat_region_key   = completion_regions,
    completion_status = sample(c("complete", "partial"), length(completion_regions),
                                replace = TRUE, prob = c(0.7, 0.3))
  )

  list(
    store_panel       = panel,
    survey_completion = survey_completion
  )
}
