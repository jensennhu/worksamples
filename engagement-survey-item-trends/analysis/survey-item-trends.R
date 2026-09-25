# Synthetic, portfolio demonstration. All facility names and data are
# fictional / randomly generated. See ../README.md for details.
#
# PURPOSE: build item-level "percent positive" trend tables and sparkline
#          visualizations from a staff engagement survey, plus an Excel
#          workbook with native sparklines.
#--------------------------------------------------------------------------------------------------------------------------#
#### Setup ####
library(dplyr)
library(openxlsx)
library(openxlsx2)
library(purrr)
library(janitor)
library(tidyr)
library(assertr)
library(haven)
library(formattable)
library(sparkline)
library(glue)
library(webshot)
library(here)

# options
options(dplyr.print_max = 100)

data_dir   <- here("data")
inter      <- here("data")
output_dir <- here("output")

source(here("R", "generate_synthetic_data.R"))
source(here("R", "utils.R"))

df <- generate_engagement_items()


# Prep data for plotting Overall
# positive percent trends for all survey items across years
positive_df <- df %>%
  select(
    facility,
    item_text,
    Category,
    Secondary,
    Split,
    starts_with("pos_"),
    subst_diff_pos_2022_2023,
    consistent_low_pos,
    diff_pos_2022_2023
  ) %>%
  mutate(
    diff_pos_2022_2023 = round(diff_pos_2022_2023 * 100, 0)
  ) %>%
  mutate(parent_cat =  trimws(gsub('[[:punct:] ]+',' ', Category))) %>%
  mutate(question_cat =  substr(gsub(' ', '',  gsub('[[:punct:] ]+',' ', item_text)), 1, 50)) %>%
  isid(question_cat, facility) %>%
  arrange(item_text) %>%
  rename(
    "Facility Name" = facility,
    "Survey Item" = item_text,
    "2019" = pos_2019,
    "2020" = pos_2020,
    "2021" = pos_2021,
    "2022" = pos_2022,
    "2023" = pos_2023,
    "Notable Change"        = subst_diff_pos_2022_2023,
    "Consistently Low"      = consistent_low_pos,
    "Pct Pt Diff, 2023-2022"  = diff_pos_2022_2023
  )

excel_input <-  positive_df %>%
  select(-Split, -question_cat, -parent_cat)

#--------------------------------------------------------------------------------------------------------------------------#
####  Plotting/Table Generating Functions ####
# -------------------------------------------------------------------------------------------------------------------------#

generate_sparklines <- function(df, category){

  customGreen = "#71CA97"
  customRed = "#ff7f7f"

  positive_spark <- df

  positive_spark$Trend <- apply(
    positive_spark %>% select(c("2019", "2020", "2021", "2022", "2023")), 1,
    FUN = function(x) as.character(
      htmltools::as.tags(sparkline(as.numeric(x), type = "line"))
    )
  )

  positive_spark <- positive_spark %>%
    mutate(
      across(c("2019", "2020", "2021", "2022", "2023"), ~percent(.x, 0)),
      across(c("2019", "2020", "2021", "2022", "2023"), ~ifelse(!is.na(.x), as.character(.x), "-"))
    )


  positive_spark <- as.htmlwidget(
    formattable(
      positive_spark,
      align = c("l", rep("c", ncol(positive_spark)-1)),
      list(
        "Survey Item" =  formatter("span",
                                   style = ~style(
                                     style(color = "grey"))),

        "Notable Change" = formatter("span",
                                     x ~ icontext(ifelse(x == 1, "ok", "")),
                                     style = ~ style(color = "green"),
                                     width = 150),

        "Consistently Low" = formatter("span",
                                       x ~ icontext(ifelse(x == 1, "flag", "")),
                                       style = ~ style(color = "orange")),

        "Pct Pt Diff, 2023-2022" = formatter("span",
                                           style = x ~ style(font.weight = "bold",
                                                             color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))),
                                           x ~ icontext(ifelse(x < 0, "arrow-down", ifelse(x > 0, "arrow-up", "-")), ifelse(is.na(x), "-", as.character(x))))
      )))

  positive_spark$dependencies <- c(positive_spark$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))

  return(positive_spark)

}

save_as_image <- function(obj, category){

  htmlwidgets::saveWidget(obj, file.path(output_dir, paste0(category, ".html")), selfcontained = TRUE)

  webshot::webshot(url = file.path(output_dir,  paste0(category, ".html")), file = file.path(output_dir, paste0(category, "_table.png")), vwidth = 1024, vheight = 768)
  message("image generated!")
}

# check output
generate_sparklines(positive_df, "all")

#--------------------------------------------------------------------------------------------------------------------------#
####  Generate HTML files and Images (via webshot) by question ####
# -------------------------------------------------------------------------------------------------------------------------#

row.names(positive_df) <- NULL

saveRDS(positive_df, file.path(inter, "positive_pcnt_survey_items.RDS"))

# Overall output to images
# split and nest dataframes by question
nested_df <- positive_df %>%
  select(-Category, -Secondary, -Split) %>%
  rename(Category = parent_cat) %>%
  mutate(
    across(c("2019", "2020", "2021", "2022", "2023"), ~percent(.x, 0))
  ) %>%
  select("Facility Name", "Survey Item", Category, everything()) %>%
  nest(.by = c("Survey Item", Category, question_cat))

# create sparklines and save to image for each question
nested_df %>%
  mutate(
    sp = map2(data, question_cat,
              ~generate_sparklines(df = .x, category = .y))) %>%
  mutate(
    saved = map2(sp, question_cat,
                 ~save_as_image(obj = .x, category = .y)))


saveRDS(paste0(nested_df$question_cat, "_table.png"), file.path(inter, "png_questions.RDS"))
saveRDS(nested_df$`Survey Item`, file.path(inter, "all_questions.RDS"))


#--------------------------------------------------------------------------------------------------------------------------#
####  Generate HTML files and Images (via webshot) by category (focal facility only) ####
# -------------------------------------------------------------------------------------------------------------------------#

# Overall output to images
# split and nest dataframes by category
nested_df <- positive_df %>%
  filter(`Facility Name` == "Example Medical Center") %>%
  # Splitting up categories with many questions
  mutate(parent_cat = paste0(Category, Split)) %>%
  mutate(
    across(c("2019", "2020", "2021", "2022", "2023"), ~percent(.x, 0))
  ) %>%
  select(-Category, -Secondary, -`Facility Name`, -question_cat, -Split) %>%
  select("Survey Item", everything()) %>%
  nest(.by = parent_cat)

# create sparklines and save to image for each parent category
nested_df %>%
  mutate(
    sp = map2(data, parent_cat,
              ~generate_sparklines(df = .x, category = .y))) %>%
  mutate(
    saved = map2(sp, parent_cat,
                 ~save_as_image(obj = .x, category = .y)))


saveRDS(paste0(nested_df$parent_cat, "_table.png"), file.path(inter, "categories.RDS"))




#--------------------------------------------------------------------------------------------------------------------------#
####  Excel Output ####
# -------------------------------------------------------------------------------------------------------------------------#


# Sparkline excel output
wb <- wb_workbook() %>%
  wb_add_worksheet() %>%
  wb_add_data(x = excel_input)

add_sparklines <- function(x){
  sl <- create_sparklines("Sheet 1", dims = glue("E{x}:I{x}"), sqref = glue("M{x}"))
  wb <- wb %>% wb_add_sparklines(sparklines = sl)
  return(wb)
}

for (i in 2:nrow(positive_df)){
  wb <- add_sparklines(i)
}

openxlsx2::wb_save(wb, file.path(output_dir, "item-level-summary-output.xlsx"), TRUE)
