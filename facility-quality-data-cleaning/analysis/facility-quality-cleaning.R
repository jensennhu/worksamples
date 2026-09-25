# Synthetic, portfolio demonstration. All facility/system names, states,
# and data are fictional / randomly generated. See ../README.md for details.
#
# PURPOSE: merge a multi-year facility directory extract with a multi-year
#          quality-measure reporting extract, patch in a late-arriving
#          reporting year that ships as two separate summary workbooks
#          instead of the regular file drop, and apply small-cell
#          suppression to a low-count demographic subgroup.

### Setup ---------------------------------------------------------------------#
rm(list = ls())

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(readxl)
library(assertr)
library(glue)
library(here)

options(dplyr.print_max = 100)

raw_dir      <- here("data", "raw")
facility_dir <- file.path(raw_dir, "facility_directory")
quality_dir  <- file.path(raw_dir, "quality_reporting")
inter        <- here("data", "intermediate")
dir.create(inter, recursive = TRUE, showWarnings = FALSE)

source(here("R", "generate_synthetic_source_files.R"))
source(here("R", "utils.R"))

generate_synthetic_source_files(raw_dir)

facility_files <- c(
  "FacilityDirectory_Data_RY2016-2017_v01.csv",
  "FacilityDirectory_Data_RY2017-2018_v01.csv",
  "FacilityDirectory_Data_RY2018-2019_v01.csv",
  "FacilityDirectory_Data_RY2020-2021_v01.csv",
  "FacilityDirectory_Data_RY2021-2022_v01.csv")

quality_files <- c(
  "QualityReporting_Data_RY2016-2017_v01.csv",
  "QualityReporting_Data_RY2017-2018_v01.csv",
  "QualityReporting_Data_RY2018-2019_v01.csv",
  "QualityReporting_Data_RY2020-2021_v01.csv")

facility_vars <- c(
  "school_year",
  "state_name",
  "health_system_name",
  "health_system_id",
  "state_facility_id",
  "city",
  "zip",
  "facility_name",
  "facility_id",
  "urbanicity",
  "facility_type",
  "total_patients_uninsured",
  "safety_net_flag",
  "clinical_staff_fte",
  "staff_patient_ratio",
  "oldest_age_band_served",
  "youngest_age_band_served",
  "pct_patients_asian",
  "pct_patients_nhpi",
  "pct_patients_aian",
  "pct_patients_black",
  "pct_patients_hisp",
  "pct_patients_multi",
  "pct_patients_white",
  "pct_patients_male",
  "pct_patients_female"
  )

facility_totals <-c(
  "total_patients",
  "total_patients_male",
  "total_patients_female",
  "total_patients_aian",
  "total_patients_aian_male",
  "total_patients_aian_female",
  "total_patients_asian",
  "total_patients_asian_male",
  "total_patients_asian_female",
  "total_patients_black",
  "total_patients_black_male",
  "total_patients_black_female",
  "total_patients_hisp",
  "total_patients_hisp_male",
  "total_patients_hisp_female",
  "total_patients_multi",
  "total_patients_multi_male",
  "total_patients_multi_female",
  "total_patients_nhpi",
  "total_patients_nhpi_male",
  "total_patients_nhpi_female",
  "total_patients_white",
  "total_patients_white_male",
  "total_patients_white_female"
)

gen_age_bands <- function(band_list){
list(
  c(paste0("total_patients_grade_", c(parse_number(band_list)))),
  c(paste0("total_patients_", c(band_list), "_male")),
  c(paste0("total_patients_", c(band_list), "_female")),
  c(paste0("total_patients_", c(band_list), "_aian")),
  c(paste0("total_patients_", c(band_list), "_aian_male")),
  c(paste0("total_patients_", c(band_list), "_aian_female")),
  c(paste0("total_patients_", c(band_list), "_asian")),
  c(paste0("total_patients_", c(band_list), "_asian_male")),
  c(paste0("total_patients_", c(band_list), "_asian_female")),
  c(paste0("total_patients_", c(band_list), "_black")),
  c(paste0("total_patients_", c(band_list), "_black_male")),
  c(paste0("total_patients_", c(band_list), "_black_female")),
  c(paste0("total_patients_", c(band_list), "_hisp")),
  c(paste0("total_patients_", c(band_list), "_hisp_male")),
  c(paste0("total_patients_", c(band_list), "_hisp_female")),
  c(paste0("total_patients_", c(band_list), "_multi")),
  c(paste0("total_patients_", c(band_list), "_multi_male")),
  c(paste0("total_patients_", c(band_list), "_multi_female")),
  c(paste0("total_patients_", c(band_list), "_nhpi")),
  c(paste0("total_patients_", c(band_list), "_nhpi_male")),
  c(paste0("total_patients_", c(band_list), "_nhpi_female")),
  c(paste0("total_patients_", c(band_list), "_white")),
  c(paste0("total_patients_", c(band_list), "_white_male")),
  c(paste0("total_patients_", c(band_list), "_white_female"))
  )
}

all_bands <- unlist(gen_age_bands(paste0("ab", seq(3,12))))
has_ab <- c("has_ab0", paste0("has_ab", seq(1,13)))

quality_vars <- c(
  "stnam",
  "systemnm",
  "system_id",
  "facnm",
  "facility_ref_id",
  "measure_domain",
  "numvalid",
  "pctprof_numeric",
  "category",
  "school_year"
)

age_band_order <-
  c("Neonatal",
    "Infant",
    "Toddler",
    "Preschool Age",
    "5-9 yrs",
    "10-14 yrs",
    "15-19 yrs",
    "20-34 yrs",
    "35-49 yrs",
    "50-64 yrs",
    "65-74 yrs",
    "75-84 yrs",
    "85-94 yrs",
    "95+ yrs")


read_facility_data <- function(path, file, var, y, pivot){

  # extract data source from file name
  source <- strsplit(file[y], "_")[[1]][1] # e.g. FacilityDirectory

  # extract reporting-year from file name
  x <- strsplit(file[y], "_")[[1]][3]      # e.g. RY2016-2017
  a <- substring(x, 5, 6)                  # e.g. 16
  b <- substring(x, 10, 11)                # e.g. 17
  sch_year_str <- paste0(a,b)              # e.g. 1617
  message("starting:", source, " ", x)

  # read data, subset to relevant cols, filter to focal state
  if(source == "QualityReporting"){
    # quality reporting
    df <-
      readr::read_csv(file.path(path, file[y]),
                      col_select = any_of(c(var, "cohort"))) %>%
      filter(
        toupper(stnam) == "MERIDIAN",
        category        == "ALL"
        )

    # store number of columns
    num_col <- ncol(df)

    # pivot wider by measure domain
    df <- df %>%
      pivot_wider(
        names_from = "measure_domain",
        values_from = c("numvalid", "pctprof_numeric")
      )
  } else {
    # facility directory
    df <-
      readr::read_csv(file.path(path, file[y]),
                      col_select = any_of(c(var, all_bands, has_ab))) %>%
      filter(
        toupper(state_name) == "MERIDIAN"
        )

    # order oldest/youngest age band served
    df$oldest_age_band_served <- ordered(as.factor(df$oldest_age_band_served), levels = age_band_order)
    df$youngest_age_band_served <- ordered(as.factor(df$youngest_age_band_served), levels = age_band_order)

    # store number of columns
    num_col <- ncol(df)

    # pct patients aapi
    df$pct_patients_aapi <- df$pct_patients_asian + df$pct_patients_nhpi

  }

  # check columns
  if((source == "QualityReporting" & num_col == length(var)) |
     (source != "QualityReporting" & num_col == length(c(var, all_bands, has_ab)))) {
    message("all required variables in dataset")
  } else {
    message("missing var(s):", setdiff(var, colnames(df)))
  }

  if(pivot == 1){
    # add reporting-year suffix to column names
    colnames(df) <- paste(tolower(colnames(df)), sch_year_str, sep = "_")

    # id var for merge
    if(source == "QualityReporting"){

      df$facility_ref_id <- as.character(df[[paste0("facility_ref_id_", sch_year_str)]])
      isid(df, facility_ref_id)

    } else {

      df$facility_id <- as.character(df[[paste0("facility_id_", sch_year_str)]])
      isid(df, facility_id)

    }
  }

  message("end")

  return(df)
}

#----------------------#
#### Read in files  ####
#----------------------#

##### merge all facility directory data ####
facility_data <-
  seq(1, length(facility_files)) %>%
  map(
    read_facility_data,
    path = facility_dir,
    file = facility_files,
    var = c(facility_vars, facility_totals),
    pivot = 1
  ) %>%
  reduce(
    full_join_check,
    tab = T,
    by = "facility_id"
  ) %>%
  select(-merge) %>%
  select(facility_id, everything())


##### merge all quality reporting data ####
quality_data <-
  seq(1, length(quality_files)) %>%
  map(
    read_facility_data,
    path = quality_dir,
    file = quality_files,
    var = quality_vars,
    pivot = 1
  ) %>%
  reduce(
    full_join_check,
    tab = T,
    by = "facility_ref_id"
  ) %>%
  select(-merge) %>%
  select(facility_ref_id, everything())

##### merge facility directory and quality reporting ####
facility_quality_allyears <-
  facility_data %>%
  full_join_check(
    quality_data %>% select(-starts_with("school_year")),
    tab = T,
    by = c("facility_id" = "facility_ref_id")) %>%
  # assess match rate after filtering to facilities serving the relevant age range
  filter(if_all(starts_with("oldest_age_band_served"), ~.x >= "5-9 yrs")) %>%
  filter(if_all(starts_with("youngest_age_band_served"), ~.x <= "95+ yrs")) %>%
  # assert >99% of facility-directory records have matching quality data
  verify(nrow(.[.$merge == 3,])/nrow(.)>.99) %>%
  filter(merge == 3) %>%
  select(-merge)

# pivot long
v <- c("_1617", "_1718", "_1819", "_2021", "_2122")
long <- function(x){
  y <- facility_quality_allyears %>% select(ends_with(x))
  colnames(y)<-sub(x,"",colnames(y))
  return(y)
}

quality_2122_vars <- c(
  "stnam",
  "systemnm",
  "system_id",
  "facnm",
  "facility_ref_id",
  "numvalid_chronic",
  "numvalid_preventive",
  "pctprof_numeric_chronic",
  "pctprof_numeric_preventive",
  "category"
)
# create empty columns for 2122 quality reporting and cbind
quality_2122 <- data.frame(matrix(ncol = length(quality_2122_vars), nrow = nrow(facility_quality_allyears)))
x <- paste0(quality_2122_vars, "_2122")
colnames(quality_2122) <- x
facility_quality_allyears <- facility_quality_allyears %>% cbind(quality_2122)

# create long dataset
facility_quality_allyears_long <-
  map(v, long) %>%
  reduce(rbind)

# compile data for the 2022 quality registry workbooks
quality_2022_hospital <- readxl::read_excel(paste0(quality_dir, "/StateQualityRegistry_RY2021-2022/2022 Hospital-level quality data.xlsx"), skip = 4)
quality_2022_clinic   <- readxl::read_excel(paste0(quality_dir, "/StateQualityRegistry_RY2021-2022/2022 Clinic-level quality data.xlsx"), skip = 4)

# Prep 2022 quality registry data
quality_2022 <- quality_2022_hospital %>%
  mutate(category = "Hospital") %>%
  verify(Cohort == "Adult") %>%
  select(-Cohort) %>%
  bind_rows(
    quality_2022_clinic %>%
      select(-`Reporting Year`) %>%
      filter(Cohort == "All Ages") %>%
      select(-Cohort) %>%
      mutate(category = 'Clinic')
  ) %>%
  mutate(
    # normalize measure domain labels across the two workbooks
    `Measure Domain` = case_when(
      `Measure Domain` %in% c("Diabetes Control", "HbA1c Screening") ~ "chronic",
      `Measure Domain` %in% c("Wellness Visit", "Immunization Rate") ~ "preventive")
  ) %>%
  # restrict datafile
  filter(
    `Measure Domain` %in% c("chronic", "preventive"),
    `Patient Group` == "All Patients"
  ) %>%
  # construct quality-target var
  mutate(
    `Network ID` = as.character(`Network ID`),
    pctprof_numeric = `Percent Meeting Target` + `Percent Exceeding Target`
    ) %>%
  select(-starts_with("Percent"), -`Network Name`, -`Facility Name`) %>%
  rename(numvalid = `Number Assessed`) %>%
  # prep to summarize
  pivot_wider(
    names_from = category,
    values_from = c(pctprof_numeric, numvalid)
  ) %>%
  isid(`Network ID`, `Facility Number`, `Measure Domain`) %>%
  # calculate weighted average of 2022 hospital- and clinic-level quality scores
  mutate(
    pctprof_numeric = case_when(
      !is.na(pctprof_numeric_Hospital) &  is.na(pctprof_numeric_Clinic)  ~  pctprof_numeric_Hospital,
       is.na(pctprof_numeric_Hospital) & !is.na(pctprof_numeric_Clinic)  ~  pctprof_numeric_Clinic,
      !is.na(pctprof_numeric_Hospital) & !is.na(pctprof_numeric_Clinic)  ~ ((pctprof_numeric_Hospital * numvalid_Hospital)/(numvalid_Hospital + numvalid_Clinic)) + ((pctprof_numeric_Clinic * numvalid_Clinic)/(numvalid_Hospital+numvalid_Clinic))
      ),
    numvalid = case_when(
      !is.na(pctprof_numeric_Hospital) &  is.na(pctprof_numeric_Clinic)  ~  numvalid_Hospital,
       is.na(pctprof_numeric_Hospital) & !is.na(pctprof_numeric_Clinic)  ~  numvalid_Clinic,
      !is.na(pctprof_numeric_Hospital) & !is.na(pctprof_numeric_Clinic)  ~  numvalid_Hospital + numvalid_Clinic
    )
  ) %>%
  select(-pctprof_numeric_Hospital, -pctprof_numeric_Clinic,
         -numvalid_Hospital, -numvalid_Clinic) %>%
  pivot_wider(
    names_from = `Measure Domain`,
    names_glue = "{.value}_{`Measure Domain`}",
    values_from = c(pctprof_numeric, numvalid)
  ) %>%
  mutate(
    category = "ALL",
    stnam    = "MERIDIAN") %>%
  isid(`Network ID`, `Facility Number`) %>%
  select(`Network ID`, `Facility Number`, any_of(quality_2122_vars))

# merge 2022 quality registry data with 2022 facility directory
facility_quality_2022_mapped <- facility_quality_allyears_long %>%
  mutate(
    `Network ID`      = gsub("^[^-]*-([^-]+).*", "\\1", state_facility_id),
    `Facility Number`  = gsub(".*-([^-.[:space:]]+).*", "\\1", state_facility_id)
    ) %>%
  relocate(`Network ID`, .after = state_facility_id) %>%
  relocate(`Facility Number`, .after = `Network ID`) %>%
  filter(school_year == "2021-2022") %>%
  select(-all_of(quality_2122_vars)) %>%
  isid(`Network ID`, `Facility Number`) %>%
  left_join_check(
    quality_2022 %>% isid(`Network ID`, `Facility Number`),
    by = c("Network ID", "Facility Number"),
    tab = T
  ) %>%
  # assert >99% of facility records have matching 2022 quality data
  verify(nrow(.[.$merge == 3,])/nrow(.)>.99) %>%
  select(-merge, -`Network ID`, -`Facility Number`)

# append and overwrite main df
facility_quality_allyears_long <- facility_quality_allyears_long %>%
  mutate(
    system_id = as.character(system_id),
    facility_ref_id = as.character(facility_ref_id)) %>%
  filter(school_year != "2021-2022") %>%
  bind_rows(facility_quality_2022_mapped) %>%
  verify(nrow(.) == nrow(facility_quality_allyears_long))


# merged output containing aian counts
saveRDS(facility_quality_allyears_long,   file = file.path(inter, "facility_quality_long_aian.RDS"))


# function to remove aian patient age-band counts from facility directory data
remove_aian <- function(data, x){
        data %>%
          tidylog::mutate(
            "total_patients_ab{x}_male"  := !!rlang::sym(glue("total_patients_ab{x}_male"))   - !!rlang::sym(glue("total_patients_ab{x}_aian_male")),
            "total_patients_ab{x}_female":= !!rlang::sym(glue("total_patients_ab{x}_female")) - !!rlang::sym(glue("total_patients_ab{x}_aian_female")),
            "total_patients_grade_{x}"   := !!rlang::sym(glue("total_patients_grade_{x}"))   - !!rlang::sym(glue("total_patients_ab{x}_aian"))
          ) %>% select(
            facility_id, school_year, glue("total_patients_ab{x}_male"), glue("total_patients_ab{x}_female"), glue("total_patients_grade_{x}")
          )

}


# Impute all missing AIAN values to 0
facility_quality_allyears_long <- facility_quality_allyears_long %>%
  tidylog::mutate(
    across(matches("total_patients_aian.*") | matches("total_patients_ab.*_aian"),
           ~if_else(is.na(.x), 0, .x))
    )

# remove total_patients_ab#_aian from overall age-band patient counts - ab3-ab12
revised_counts <- map(
  seq(3,12),
  ~remove_aian(data = facility_quality_allyears_long, x = .x)
  ) %>%
  reduce(left_join_check, tab = T, by = c("facility_id", "school_year")) %>%
  verify(merge == 3) %>%
  select(-merge)

# store recalculated variable names
changed_vars <- revised_counts %>% select(starts_with("total_patients")) %>% colnames()

# overwrite main df
facility_quality_allyears_long <- facility_quality_allyears_long %>%
  mutate(
    # remove total_patients_aian counts from total_patients
    total_patients = total_patients - total_patients_aian,

    # remove total_patients_aian_female/male counts from total_patients_male/female
    total_patients_male = total_patients_male - total_patients_aian_male,
    total_patients_female = total_patients_female - total_patients_aian_female,

    # Set all aian patient counts to zero
    across(matches("total_patients_aian.*") | matches("total_patients_ab.*_aian"),
           ~if_else(.x != 0, 0, .x)),

    # recalculate pct variables
    pct_patients_asian  = total_patients_asian/total_patients  * 100,
    pct_patients_nhpi   = total_patients_nhpi/total_patients   * 100,
    pct_patients_aian   = total_patients_aian/total_patients   * 100,
    pct_patients_black  = total_patients_black/total_patients  * 100,
    pct_patients_hisp   = total_patients_hisp/total_patients   * 100,
    pct_patients_multi  = total_patients_multi/total_patients  * 100,
    pct_patients_white  = total_patients_white/total_patients  * 100,
    pct_patients_male   = total_patients_male/total_patients   * 100,
    pct_patients_female = total_patients_female/total_patients * 100,

    # recalc aapi
    pct_patients_aapi   = pct_patients_asian + pct_patients_nhpi

  ) %>%

  # QA pct sum to 100%
  mutate(
        check_race  =   select(.,
                               pct_patients_asian,
                               pct_patients_nhpi,
                               pct_patients_aian,
                               pct_patients_black,
                               pct_patients_white,
                               pct_patients_hisp,
                               pct_patients_multi) %>% rowSums(na.rm = T),
        check_gender =  select(.,
                               pct_patients_male,
                               pct_patients_female) %>% rowSums(na.rm = T)) %>%
  verify(check_race   == "100" | check_race   == "0") %>%
  verify(check_gender == "100" | check_gender == "0") %>%
  select(-check_race, -check_gender) %>%

  # drop old subgroup variables (e.g. total_patients_ab#_female)
  select(-all_of(changed_vars)) %>%

  # add new recalculated subgroup variables (e.g. total_patients_ab#_female)
  left_join_check(
    revised_counts,
    by = c("facility_id", "school_year"),
    tab = T
  ) %>%
  verify(merge == 3) %>%
  select(-merge)


#----------------------#
#### Save RDS files ####
#----------------------#

# merged output without AIAN counts
saveRDS(facility_quality_allyears_long,   file = file.path(inter, "facility_quality_long.RDS"))
