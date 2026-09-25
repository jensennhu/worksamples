suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(openxlsx)
})

# Writes a set of synthetic raw source files to disk, mimicking two data
# feeds a health-services research project would pull from: a facility
# directory/roster extract, and a quality-measure reporting extract, plus a
# late-arriving year that ships as two separimplementary workbooks instead
# of the regular CSV drop. Every number is randomly generated.
generate_synthetic_source_files <- function(raw_dir, seed = 20240201, n_facilities = 60) {

  set.seed(seed)

  facility_dir <- file.path(raw_dir, "facility_directory")
  quality_dir  <- file.path(raw_dir, "quality_reporting")
  registry_dir <- file.path(quality_dir, "StateQualityRegistry_RY2021-2022")
  dir.create(facility_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(registry_dir, recursive = TRUE, showWarnings = FALSE)

  race_cats <- c("aian", "asian", "black", "hisp", "multi", "nhpi", "white")
  bands     <- 3:12

  age_band_order <- c(
    "Neonatal", "Infant", "Toddler", "Preschool Age", "5-9 yrs", "10-14 yrs",
    "15-19 yrs", "20-34 yrs", "35-49 yrs", "50-64 yrs", "65-74 yrs",
    "75-84 yrs", "85-94 yrs", "95+ yrs"
  )

  # ---- master facility roster (fixed across years) ------------------------
  network_id      <- sprintf("NW%02d", sample(1:12, n_facilities, replace = TRUE))
  facility_number <- sprintf("F%03d", seq_len(n_facilities))
  roster <- tibble(
    facility_id        = sprintf("FAC%05d", seq_len(n_facilities)),
    state_facility_id  = paste0("ST-", network_id, "-", facility_number),
    network_id         = network_id,
    facility_number    = facility_number,
    health_system_name = paste("Health System", network_id),
    health_system_id   = network_id,
    facility_name       = paste("Facility", facility_number),
    city                = paste0("City", sample(1:20, n_facilities, replace = TRUE)),
    zip                 = sprintf("%05d", sample(10000:99999, n_facilities)),
    urbanicity          = sample(1:4, n_facilities, replace = TRUE, prob = c(.3, .35, .2, .15)),
    facility_type       = sample(c("Hospital", "Clinic", "Health Center"), n_facilities,
                                  replace = TRUE, prob = c(.25, .5, .25)),
    oldest_idx  = sample(6:14, n_facilities, replace = TRUE),
    youngest_idx = sample(1:5, n_facilities, replace = TRUE)
  ) %>%
    mutate(
      oldest_age_band_served   = age_band_order[oldest_idx],
      youngest_age_band_served = age_band_order[youngest_idx]
    ) %>%
    select(-oldest_idx, -youngest_idx)

  # ---- demographic helper: split totals across race x sex, exactly -------
  split_demographics <- function(total_patients) {
    n <- length(total_patients)
    k <- length(race_cats)

    raw_shares <- matrix(runif(n * k), nrow = n)
    shares <- raw_shares / rowSums(raw_shares)
    race_totals <- round(shares * total_patients)

    resid <- total_patients - rowSums(race_totals)
    max_idx <- apply(shares, 1, which.max)
    for (i in seq_len(n)) race_totals[i, max_idx[i]] <- race_totals[i, max_idx[i]] + resid[i]
    race_totals[race_totals < 0] <- 0

    male_share <- runif(n, 0.45, 0.55)
    race_male <- round(sweep(race_totals, 1, male_share, `*`))
    race_male <- pmin(pmax(race_male, 0), race_totals)
    race_female <- race_totals - race_male

    colnames(race_totals) <- paste0("total_patients_", race_cats)
    colnames(race_male)   <- paste0("total_patients_", race_cats, "_male")
    colnames(race_female) <- paste0("total_patients_", race_cats, "_female")

    out <- as_tibble(cbind(race_totals, race_male, race_female))
    out$total_patients        <- rowSums(race_totals)
    out$total_patients_male   <- rowSums(race_male)
    out$total_patients_female <- rowSums(race_female)
    out
  }

  # ---- age-band columns: only the aian/male/female fields are load-bearing
  build_band_columns <- function(n, total_patients) {
    out <- list()
    for (x in bands) {
      band_total  <- pmax(round(total_patients * runif(n, 0.05, 0.15)), 1)
      male_share  <- runif(n, 0.45, 0.55)
      band_male   <- round(band_total * male_share)
      band_female <- band_total - band_male

      aian_total  <- pmin(round(band_total * runif(n, 0, 0.03)), band_total)
      aian_male   <- pmin(round(aian_total * male_share), band_male)
      aian_female <- aian_total - aian_male

      out[[paste0("total_patients_grade_", x)]]        <- band_total
      out[[paste0("total_patients_ab", x, "_male")]]   <- band_male
      out[[paste0("total_patients_ab", x, "_female")]] <- band_female
      out[[paste0("total_patients_ab", x, "_aian")]]        <- aian_total
      out[[paste0("total_patients_ab", x, "_aian_male")]]   <- aian_male
      out[[paste0("total_patients_ab", x, "_aian_female")]] <- aian_female

      # remaining race categories per band: unused downstream, filled for
      # column-count fidelity only
      for (r in setdiff(race_cats, "aian")) {
        r_total <- pmax(round(band_total * runif(n, 0, 0.2)), 0)
        r_male  <- round(r_total * male_share)
        out[[paste0("total_patients_ab", x, "_", r)]]        <- r_total
        out[[paste0("total_patients_ab", x, "_", r, "_male")]]   <- r_male
        out[[paste0("total_patients_ab", x, "_", r, "_female")]] <- r_total - r_male
      }
    }
    as_tibble(out)
  }

  write_facility_year <- function(reporting_year_label) {
    n <- n_facilities
    total_patients <- round(rnorm(n, mean = 9000, sd = 2500))
    total_patients <- pmax(total_patients, 500)

    demo <- split_demographics(total_patients)
    bandcols <- build_band_columns(n, total_patients)

    has_ab <- as_tibble(setNames(
      replicate(14, rbinom(n, 1, 0.8), simplify = FALSE),
      c("has_ab0", paste0("has_ab", 1:13))
    ))

    df <- roster %>%
      mutate(
        school_year             = reporting_year_label,
        state_name               = "MERIDIAN",
        total_stud_frpl          = round(total_patients * runif(n, 0.1, 0.4)),
        sch_wide_title1          = rbinom(n, 1, 0.3),
        tch_fte                  = round(rnorm(n, 40, 10), 1),
        tchr_pupil_ratio         = round(total_patients / pmax(round(rnorm(n, 40, 10), 1), 1), 1)
      ) %>%
      bind_cols(demo, bandcols, has_ab) %>%
      mutate(
        pct_stud_asian = round(total_patients_asian / total_patients * 100, 2),
        pct_stud_nhpi  = round(total_patients_nhpi  / total_patients * 100, 2),
        pct_stud_aian  = round(total_patients_aian  / total_patients * 100, 2),
        pct_stud_black = round(total_patients_black / total_patients * 100, 2),
        pct_stud_hisp  = round(total_patients_hisp  / total_patients * 100, 2),
        pct_stud_multi = round(total_patients_multi / total_patients * 100, 2),
        pct_stud_white = round(total_patients_white / total_patients * 100, 2),
        pct_stud_male  = round(total_patients_male  / total_patients * 100, 2),
        pct_stud_female= round(total_patients_female/ total_patients * 100, 2)
      ) %>%
      rename(
        pct_patients_asian = pct_stud_asian, pct_patients_nhpi = pct_stud_nhpi,
        pct_patients_aian = pct_stud_aian, pct_patients_black = pct_stud_black,
        pct_patients_hisp = pct_stud_hisp, pct_patients_multi = pct_stud_multi,
        pct_patients_white = pct_stud_white, pct_patients_male = pct_stud_male,
        pct_patients_female = pct_stud_female,
        total_patients_uninsured = total_stud_frpl,
        safety_net_flag = sch_wide_title1,
        clinical_staff_fte = tch_fte,
        staff_patient_ratio = tchr_pupil_ratio
      ) %>%
      select(-network_id, -facility_number)

    df
  }

  facility_years <- c("2016-2017", "2017-2018", "2018-2019", "2020-2021", "2021-2022")
  facility_file_labels <- c("RY2016-2017", "RY2017-2018", "RY2018-2019", "RY2020-2021", "RY2021-2022")

  for (i in seq_along(facility_years)) {
    df <- write_facility_year(facility_years[i])
    write_csv(df, file.path(facility_dir, paste0("FacilityDirectory_Data_", facility_file_labels[i], "_v01.csv")))
  }

  # ---- quality reporting: long by measure domain, 4 years (no 2021-2022) --
  write_quality_year <- function() {
    base <- tibble(
      stnam            = "MERIDIAN",
      systemnm         = roster$health_system_name,
      system_id        = roster$health_system_id,
      facnm            = roster$facility_name,
      facility_ref_id  = roster$facility_id
    )

    make_domain <- function(domain) {
      base %>%
        mutate(
          measure_domain = domain,
          numvalid       = round(rnorm(n_facilities, 300, 60)),
          pctprof_numeric = round(pmin(pmax(rnorm(n_facilities, 60, 12), 0), 100), 1),
          category = "ALL"
        )
    }

    kept <- bind_rows(make_domain("chronic"), make_domain("preventive"))

    # decoy rows that should get filtered out by the category == "ALL" check
    decoy <- kept %>%
      slice(1:5) %>%
      mutate(category = "MEDICAID", numvalid = round(numvalid * 0.2))

    bind_rows(kept, decoy)
  }

  quality_file_labels <- c("RY2016-2017", "RY2017-2018", "RY2018-2019", "RY2020-2021")
  for (lab in quality_file_labels) {
    df <- write_quality_year() %>% mutate(school_year = sub("RY", "", lab))
    write_csv(df, file.path(quality_dir, paste0("QualityReporting_Data_", lab, "_v01.csv")))
  }

  # ---- special late-arriving 2021-2022 workbooks --------------------------
  hospital_rows <- tibble(
    `Network ID`     = roster$network_id,
    `Facility Number`= roster$facility_number,
    `Network Name`   = roster$health_system_name,
    `Facility Name`  = roster$facility_name,
    Cohort           = "Adult"
  ) %>%
    tidyr::crossing(`Measure Domain` = c("Diabetes Control", "Wellness Visit")) %>%
    mutate(
      `Patient Group` = "All Patients",
      `Number Assessed` = round(rnorm(n(), 300, 60)),
      `Percent Meeting Target`   = round(pmin(pmax(rnorm(n(), 45, 10), 0), 90), 1),
      `Percent Exceeding Target` = round(pmin(pmax(rnorm(n(), 15, 6), 0), 100 - `Percent Meeting Target`), 1)
    )

  hospital_decoy <- hospital_rows %>%
    slice(1:4) %>%
    mutate(`Patient Group` = "Medicaid Patients")

  hospital_wb_data <- bind_rows(hospital_rows, hospital_decoy)

  clinic_rows <- tibble(
    `Network ID`     = roster$network_id,
    `Facility Number`= roster$facility_number,
    `Network Name`   = roster$health_system_name,
    `Facility Name`  = roster$facility_name,
    Cohort           = "All Ages",
    `Reporting Year` = "2021-2022"
  ) %>%
    tidyr::crossing(`Measure Domain` = c("HbA1c Screening", "Immunization Rate")) %>%
    mutate(
      `Patient Group` = "All Patients",
      `Number Assessed` = round(rnorm(n(), 300, 60)),
      `Percent Meeting Target`   = round(pmin(pmax(rnorm(n(), 50, 10), 0), 90), 1),
      `Percent Exceeding Target` = round(pmin(pmax(rnorm(n(), 15, 6), 0), 100 - `Percent Meeting Target`), 1)
    )

  clinic_decoy <- clinic_rows %>%
    slice(1:4) %>%
    mutate(Cohort = "Pediatric")

  clinic_wb_data <- bind_rows(clinic_rows, clinic_decoy)

  write_report_workbook <- function(df, title, path) {
    wb <- createWorkbook()
    addWorksheet(wb, "Sheet1")
    writeData(wb, "Sheet1", title, startRow = 1, startCol = 1)
    writeData(wb, "Sheet1", df, startRow = 5, startCol = 1)
    saveWorkbook(wb, path, overwrite = TRUE)
  }

  write_report_workbook(
    hospital_wb_data,
    "State Quality Registry - 2022 Hospital-level Data",
    file.path(registry_dir, "2022 Hospital-level quality data.xlsx")
  )
  write_report_workbook(
    clinic_wb_data,
    "State Quality Registry - 2022 Clinic-level Data",
    file.path(registry_dir, "2022 Clinic-level quality data.xlsx")
  )

  invisible(NULL)
}
