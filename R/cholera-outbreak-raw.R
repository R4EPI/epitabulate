
# R4pis person analysis re-write --------------
# https://r4epis.netlify.app/training/walk-through/person_analyses/
# Based off template 2019-06-19-cholera-outbreak.Rmd

# Setup -----------------------------------------

## Installing required packages for this template
required_packages <- c("knitr",       # create output docs
                       "here",        # find your files
                       "dplyr",       # clean/shape data
                       "forcats",     # clean/shape data
                       "stringr",     # clean text
                       "rio",         # read in data
                       "ggplot2",     # create plots and charts
                       "patchwork",   # combine plots in one
                       "sitrep",      # MSF field epi functions
                       "linelist",    # Functions for cleaning/standardising data/dates
                       "matchmaker",  # dictionary-based standardization of variables
                       "incidence",   # create epicurves
                       "aweek",       # define epi weeks
                       "epitrix",     # epi helpers and tricks
                       "sf",          # encode spatial vector data
                       "ggspatial")   # plot maps

for (pkg in required_packages) {
  # install packages if not already present
  if (!pkg %in% rownames(installed.packages())) {
    install.packages(pkg)
  }

  # load packages to this current session
  library(pkg, character.only = TRUE)
}



## Set default options for plots and charts

## set default text size to 16 for plots
## give classic black/white axes for plots
ggplot2::theme_set(theme_classic(base_size = 18))

## sets the theme in ggplot for epicurves
epicurve_theme <- theme(
  axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
  legend.title = element_blank(),
  panel.grid.major.x = element_line(color = "grey60", linetype = 3),
  panel.grid.major.y = element_line(color = "grey60", linetype = 3))
aweek::date2week("2017-04-27")  # Ask for the epi week of this date
## sets the labels in ggplot for the epicurves
epicurve_labels <- labs(x = "Calendar week",
                        y = "Cases (n)",
                        title = "Cases by week of onset",
                        subtitle = str_glue("Source: MSF data from {reporting_week}")
)









# Load data & setup data

# Linelist data
# linelist_raw <- rio::import(here::here("AJS_AmTiman.xlsx"), which = "linelist")
linelist_raw <- gen_data("AJS")

# Load dictionary
linelist_dict <- msf_dict("AJS", compact = FALSE)

linelist_cleaned <- linelist_raw
## Clean column names
## make a copy of your orginal dataset and name it linelist_cleaned
linelist_cleaned <- linelist_raw

## define clean variable names using clean_labels from the epitrix package
cleaned_colnames <- epitrix::clean_labels(colnames(linelist_raw))

## overwrite variable names with defined clean names
colnames(linelist_cleaned) <- cleaned_colnames



linelist_cleaned <- matchmaker::match_df(linelist_cleaned,
                                         dict  = linelist_dict,
                                         from  = "option_code",
                                         to    = "option_name",
                                         by    = "data_element_shortname",
                                         order = "option_order_in_set"
)

linelist_cleaned <- linelist_cleaned %>%
  filter(!is.na(case_number) & !is.na(date_of_consultation_admission))


## make sure all date variables are formatted as dates
DATEVARS <- filter(linelist_dict, data_element_valuetype == "DATE") %>%
  filter(data_element_shortname %in% names(linelist_cleaned)) %>%
  ## filter to match the column names of your data
  pull(data_element_shortname)

## change to dates
linelist_cleaned <- linelist_cleaned %>%
  mutate_at(DATEVARS, linelist::guess_dates,
            error_tolerance = 0.5)

## Create epiweek variable
## This step creates an epiweek variable from the date of onset.
## You can use date_of_consultation_admission if you are missing many date_of_onset.

linelist_cleaned$epiweek <- aweek::date2week(linelist_cleaned$date_of_onset,
                                             floor_day = TRUE,
                                             factor = TRUE)


## OPTIONAL: add under 2 years to the age_years variable
## data dictionary defines that under 2s dont have year filled in (but months/days instead)
linelist_cleaned <- linelist_cleaned %>%
  mutate(age_months = case_when(
    is.na(age_years) & is.na(age_months) ~ as.numeric(age_days / 30),
    TRUE                                 ~ as.numeric(age_months)
  ),
  age_years = case_when(
    is.na(age_years) & is.na(age_months) ~ as.numeric(age_days / 365.25),
    is.na(age_years)                     ~ as.numeric(age_months / 12),
    TRUE                                 ~ as.numeric(age_years)
  ))

## create age group variable for under 5 years based on months
linelist_cleaned$age_group_mon <- age_categories(linelist_cleaned$age_months,
                                                 breakers = c(0, 6, 9, 12, 24),
                                                 ceiling = TRUE)

## create an age group variable by specifying categorical breaks
linelist_cleaned$age_group <- age_categories(linelist_cleaned$age_years,
                                             breakers = c(0, 3, 15, 30, 45))





linelist_cleaned <- mutate(linelist_cleaned,
                           obs_days = as.numeric(date_of_exit -
                                                   date_of_consultation_admission))

linelist_cleaned$DIED <- str_detect(linelist_cleaned$exit_status, "Dead")
linelist_cleaned <- linelist_cleaned %>%
  mutate(case_def = case_when(
    is.na(hep_e_rdt) & is.na(other_cases_in_hh)           ~ NA_character_,
    hep_e_rdt == "Positive"                               ~ "Confirmed",
    hep_e_rdt != "Positive" & other_cases_in_hh == "Yes"  ~ "Probable",
    TRUE                                                  ~ "Suspected"
  ))

linelist_cleaned <- linelist_cleaned %>%
  mutate(exit_status2 = case_when(
    exit_status %in% c("Dead on arrival", "Dead in facility") ~ "Died",
    exit_status %in% c("Transferred (to an MSF facility)",
                       "Transferred (to External Facility)")  ~ "Transferred",
    exit_status == "Discharged home"                          ~ "Discharged",
    exit_status == "Left against medical advice"              ~ "Left"
  ))


linelist_cleaned <- linelist_cleaned %>%
  mutate(case_def = case_when(
    is.na(hep_e_rdt) & is.na(other_cases_in_hh)           ~ NA_character_,
    hep_e_rdt == "Positive"                               ~ "Confirmed",
    hep_e_rdt != "Positive" & other_cases_in_hh == "Yes"  ~ "Probable",
    TRUE                                                  ~ "Suspected"
  ))


# Force missing values to NA
# important for sex to generate age pyramids
linelist_cleaned$sex <- fct_recode(linelist_cleaned$sex,
                                   NULL = "Unknown/unspecified")


# change the order of levels in a single categorical variable
# This orders the levels -- since in all figures / tables, 0-4 should come
# before 4-24, etc
linelist_cleaned <- linelist_cleaned %>%
  mutate(time_to_death = factor(time_to_death,
                                levels = c("0-4 hours",
                                           ">4-24 hours",
                                           ">24-48 hours",
                                           ">48 hours")))



# Change the order of levels of multiple categorical variables at the same time
linelist_cleaned <- linelist_cleaned %>%
  mutate_at(vars(
    # Looks for variables beginning with "test"
    starts_with("test")),
    fct_relevel,
    # Sets order of levels
    "Positive", "Negative", "Not done")


# create a grouping of all symptoms
SYMPTOMS <- c("history_of_fever",
              "fever",
              "nausea_anorexia",
              "vomiting",
              "epigastric_pain_heartburn",
              "generalized_itch",
              "headache",
              "joint_pains",
              "diarrhoea",
              "bleeding",
              "convulsions",
              "mental_state",
              "other_symptoms"
)



# create a grouping of all lab tests
LABS <- c("hep_b_rdt",
          "hep_c_rdt",
          "hep_e_rdt",
          "test_hepatitis_a",
          "test_hepatitis_b",
          "test_hepatitis_c",
          "test_hepatitis_e_igg",
          "test_hepatitis_e_igm" ,
          "test_hepatitis_e_genotype",
          "test_hepatitis_e_virus",
          "malaria_rdt_at_admission",
          "malaria_blood_film",
          "dengue",
          "dengue_rdt",
          "yellow_fever",
          "typhoid",
          "chikungunya_onyongnyong",
          "ebola_marburg",
          "lassa_fever",
          "other_arthropod_transmitted_virus",
          "other_pathogen"
)

obs_end   <- week2date(str_glue("{reporting_week}-7"))

# filter out cases after end of reporting week
linelist_cleaned <- linelist_cleaned %>%
  filter(date_of_consultation_admission <= obs_end)

# define the first week of outbreak (date of first case)
first_week <- as.aweek(min(as.character(linelist_cleaned$epiweek)))

# outbreak start
# return the first day in the week of first case
obs_start <- as.Date(first_week)


# Population data
# estimate population size by age group in years
population_data_age <- gen_population(total_pop = 5000,
                                      groups = c("0-2", "3-14", "15-29", "30-44", "45+"),
                                      proportions = c(0.068, 0.3622, 0.276, 0.1616, 0.1321),
                                      strata = NULL) %>%
  rename(age_group = groups,
         population = n)


# estimate population size by age group in months (under 2 years)
population_data_age_months <- gen_population(total_pop = 5000,
                                             groups = c("0-5", "6-8", "9-11","12-24"),
                                             proportions = c(0.0164, 0.088, 0.088, 0.034),
                                             strata = NULL) %>%
  rename(age_group_mon = groups,
         population = n)

## estimate population size by region proportion
population_data_region <- gen_population(total_pop = 5000,         # set the total population
                                         groups = c("Village A", "Village B", "Village C", "Village D"),  # set the groups
                                         proportions = c(0.221, 0.174, 0.355, 0.245),                     # set the proportions for each group
                                         strata = NULL) %>%               # do not stratify by gender
  rename(patient_origin = groups,  # rename columns (syntax is NEW NAME = OLD NAME)
         population = n)




# Person analysis -----------------------------------------------------------


# Tab linelist age -----------
# Case def by age with col and row totals
tab_linelist(linelist_cleaned,
             age_group, strata = case_def,
             col_total = TRUE, row_total = TRUE) %>%
  select(-variable) %>%
  rename("Age group" = value) %>%
  rename_redundant("%" = proportion) %>%
  augment_redundant(" cases (n)" = " n$") %>%
  kable(digits = 1)

table(linelist_cleaned$age_group, linelist_cleaned$case_def, useNA = "always")

# gtsummary age
gt_age <- linelist_cleaned %>% dplyr::select(age_group, case_def) %>%
  # The total is also in the column title - we can either remove the "N = x" or
  # remove the total line at the bottom, depending on preference.
  dplyr::mutate("Total" = 1) %>%
  gtsummary::tbl_summary(
    by = case_def,
    missing = "ifany",
    statistic = list(digits = gtsummary::all_categorical() ~ c(0, 1)),
    label = age_group ~ "Age group") %>%
  gtsummary::add_overall()
gt_age




# Tab linelist gender -----------

tab_linelist(linelist_cleaned,
             age_group, strata = sex,
             col_total = TRUE, row_total = TRUE, prop_total = TRUE) %>%
  select(-variable) %>%
  rename("Age group" = value) %>%
  rename_redundant("%" = proportion) %>%
  augment_redundant(" cases (n)" = " n$") %>%
  kable(digits = 1)

table(linelist_cleaned$age_group, linelist_cleaned$sex, useNA = "always")


# gtsummary gender

gt_gender <- linelist_cleaned %>% dplyr::select(age_group, sex) %>%
  # The total is also in the column title - we can either remove the "N = x" or
  # remove the total line at the bottom, depending on preference.
  dplyr::mutate("Total" = 1) %>%
  # Add Missing level to strata column (sex)
  dplyr::mutate(sex = forcats::fct_explicit_na(sex, na_level = "Missing")) %>%
  gtsummary::tbl_summary(
    by = sex,
    missing = "ifany",
    statistic = list(digits = gtsummary::all_categorical() ~ c(0, 1)),
    label = age_group ~ "Age group") %>%
  gtsummary::add_overall()
gt_gender


# Tab linelist symptoms -----------
tab_linelist(linelist_cleaned, SYMPTOMS, keep = "Yes") %>%
  select(-value) %>%
  # fix the way symptoms are displayed
  # str_replace_all switches underscore for space in the variable column
  # str_to_sentence makes the first letter capital, and all others lowercase
  mutate(variable = str_to_sentence(str_replace_all(variable, "_", " "))) %>%
  # rename accordingly
  rename_redundant("%" = proportion) %>%
  augment_redundant(" (n)" = " n$") %>%
  kable(digits = 1)

# gtsummary symptoms

gt_symptoms <- linelist_cleaned %>% dplyr::select(SYMPTOMS) %>%
  dplyr::rename_with(~ gsub("_", " ", .x)) %>%
  dplyr::rename_with(~ str_to_sentence(.x)) %>%
  gtsummary::tbl_summary(
    missing = "ifany",
    statistic = list(digits = gtsummary::all_categorical() ~ c(0, 1)))
gt_symptoms


# Tab linelist labs -----------
tab_linelist(linelist_cleaned, LABS,
             transpose = "value") %>%
  # fix the way lab test names are displayed
  # str_replace_all switches underscore for space in the variable column
  # str_to_sentence makes the first letter capital, and all others lowercase
  mutate(variable = str_to_sentence(str_replace_all(variable, "_", " "))) %>%
  # rename accordingly
  rename("Lab test" = variable) %>%
  rename_redundant("%" = proportion) %>%
  augment_redundant(" (n)" = " n$") %>%
  kable(digits = 1)

# gtsummary labs

# Option 1 (simplest)
gt_labs_long <- linelist_cleaned %>% dplyr::select(LABS) %>%
  dplyr::rename_with(~ gsub("_", " ", .x)) %>%
  dplyr::rename_with(~ str_to_sentence(.x)) %>%
  gtsummary::tbl_summary(
    missing = "ifany",
    statistic = list(digits = gtsummary::all_categorical() ~ c(0, 1)))
gt_labs_long

# Option 1 (transpose then summarize)

gt_labs_wide <- linelist_cleaned %>% dplyr::select(LABS) %>%
  # dplyr::rename_with(~ gsub("_", " ", .x)) %>%
  tidyr::pivot_longer(LABS, names_to = "lab", values_to = "result") %>%
  mutate(lab = str_to_sentence(str_replace_all(lab, "_", " "))) %>%
  gtsummary::tbl_summary(
    by = result,
    missing = "ifany",
    statistic = list(digits = gtsummary::all_categorical() ~ c(0, 1)))

gt_labs_wide

# tab linelist time to death
filter(linelist_cleaned, DIED, patient_facility_type == "Inpatient") %>%
  tab_linelist(time_to_death, col_total = TRUE) %>%
  select(-variable) %>%
  rename("Time (hours)" = value,
         "Deaths (n)" = n,
         "%" = proportion) %>%
  kable(digits = 1)

gt_labs_time_death <- linelist_cleaned %>%
  dplyr::filter(patient_facility_type == "Inpatient") %>%
  dplyr::select(time_to_death) %>%
  gtsummary::tbl_summary(
    missing = "ifany",
    statistic = list(digits = gtsummary::everything() ~ c(0, 1)))
gt_labs_time_death

# Case Fatality Ratio (CFR) ---------------
# use arguments from above to produce overall CFR
overall_cfr <- linelist_cleaned %>%
  filter(patient_facility_type == "Inpatient") %>%
  case_fatality_rate_df(deaths = DIED, mergeCI = TRUE) %>%
  rename("Deaths" = deaths,
         "Cases" = population,
         "CFR (%)" = cfr,
         "95%CI" = ci)
knitr::kable(overall_cfr, digits = 1)

# use arguments from above to produce inpatient CFR
linelist_cleaned %>%
  filter(patient_facility_type == "Inpatient") %>%
  case_fatality_rate_df(deaths = DIED, mergeCI = TRUE) %>%
  rename("Deaths" = deaths,
         "Cases" = population,
         "CFR (%)" = cfr,
         "95%CI" = ci) %>%
  kable(digits = 1)


# use gtsummary add_stat function-------------------------------
# Create wrapper function for
add_cfr_stat <- function(data, variable, by, ...) {
  browser()
  if(!is.null(by)) {
    warning("cfr by strata is not currently available, ignoring `by` argument")
  }

  stat_new <- data %>%
    epikit::case_fatality_rate_df(
      deaths = data[[variable]],
      mergeCI = TRUE) %>%
    dplyr::mutate(deaths = as.integer(deaths))
}

gt_cfr <- linelist_cleaned %>%
  dplyr::filter(patient_facility_type == "Inpatient") %>%
  dplyr::select(DIED) %>%
  # case_fatality_rate_df(deaths = DIED, mergeCI = TRUE) %>%
  gtsummary::tbl_summary(
    statistic = everything() ~ "",
    label = DIED ~ "All participants") %>%
  gtsummary::add_stat(
   fns = list(gtsummary::everything() ~ add_cfr_stat)
  ) %>%
  gtsummary::modify_table_body(
    ~ .x %>%
      dplyr::mutate(stat_0 = NULL)
  )

gt_cfr #,
  # location = gtsummary::all_categorical(FALSE) ~ "level"

linelist_deaths_gender  <- linelist_cleaned %>%
  dplyr::filter(patient_facility_type == "Inpatient") %>%
  dplyr::select(case_number, DIED, sex) %>%
  dplyr::mutate(
                sex = forcats::fct_explicit_na(sex, "Missing")) %>%
  tidyr::pivot_wider(names_from = "sex", values_from = "DIED")

gender_cfr <- linelist_deaths_gender %>%
  select(Male, Female, Missing) %>%
  gtsummary::tbl_summary(
    missing = "no"
  ) %>%
  gtsummary::add_stat(
    fns = list(gtsummary::everything() ~ add_cfr_stat)
  )  %>%
  gtsummary::modify_table_body(
    ~ .x %>%
      dplyr::mutate(stat_0 = NULL)
  )

# FAIL
# gt_cfrall <- gtsummary::tbl_merge(list(gt_cfr, gt_cfr_gender))

# Combine cfr gender and total tables
gtcfrallgender <- gt_cfr %>%
  gtsummary::modify_table_body(
    ~ .x %>% add_row(gender_cfr$table_body)
  )
gtcfrallgender


# repeat for age
linelist_deaths_age  <- linelist_cleaned %>%
  dplyr::filter(patient_facility_type == "Inpatient") %>%
  dplyr::select(case_number, DIED, age_group) %>%
  dplyr::mutate(
    sex = forcats::fct_explicit_na(age_group, "Missing")) %>%
  tidyr::pivot_wider(names_from = "age_group", values_from = "DIED")

levels(linelist_cleaned$age_group)

age_cfr <- linelist_deaths_age %>%
  select(levels(linelist_cleaned$age_group)) %>%
  gtsummary::tbl_summary(
    missing = "no"
  ) %>%
  gtsummary::add_stat(
    fns = list(gtsummary::everything() ~ add_cfr_stat)
  )  %>%
  gtsummary::modify_table_body(
    ~ .x %>%
      dplyr::mutate(stat_0 = NULL)
  )
# Combine cfr age and total tables
gtcfrageall <- gt_cfr %>%
  gtsummary::modify_table_body(
    ~ .x %>% add_row(age_cfr$table_body)
  )

gtcfrageall



# by level age_cfr  ---------------------

add_cfr_stat_level <- function(data, variable, by, ...) {
  if(!is.null(by)) {
    warning("cfr by strata is not currently available, ignoring `by` argument")
  }

  variable_ <- variable

  tb <- list(...)$tbl
  gt_dt <- tb$table_body
  var_dt <- gt_dt %>%
    dplyr::filter(variable %in% c("sex") & !is.na(stat_0))
  var_levels <- unique(var_dt$label)

  by_sym <- as.symbol("sex")
  deaths_var <- data$deaths_var[1]
  deaths <- data[[deaths_var]]
  var <- rlang::enquo(variable)
  var_sym <- as.symbol("sex")
  qvariable <- rlang::enquo(var_sym)


  stat_new <- data %>%
    case_fatality_rate_df(
      deaths = deaths,
      group = var_sym,
      mergeCI = TRUE) %>%
    dplyr::filter(!!qvariable %in% var_levels) %>%
    select(deaths, population, cfr, ci)

#
#
#   data <- data %>%
#     mutate(age_group = factor(age_group))
#
#   stat_new <- data %>%
#     epikit::case_fatality_rate_df(
#       deaths = expression("DIED",
#       # group = age_group,
#       mergeCI = TRUE) %>%
#     dplyr::mutate(deaths = as.integer(deaths))
}

age_cfr <- linelist_cleaned %>%
  dplyr::filter(patient_facility_type == "Inpatient") %>%
  dplyr::select(case_number, DIED, sex) %>%
  dplyr::mutate(deaths_var = "DIED") %>%
  # dplyr::mutate() %>%
  gtsummary::tbl_summary(
    include = sex,
    missing = "no") %>%
  gtsummary::add_stat(
    fns = list(gtsummary::everything() ~ add_cfr_stat_level),
    location = gtsummary::everything() ~ "level"
  )  %>%
  gtsummary::modify_table_body(
    ~ .x %>%
      dplyr::mutate(stat_0 = NULL)
  )

# Combine cfr age and total tables

gtcfrageall <- gt_cfr %>%
  gtsummary::modify_table_body(
    ~ .x %>% add_row(age_cfr$table_body)
  )

gtcfrageall

# Attack rates --------------------------------------------------
ar <- attack_rate(nrow(linelist_cleaned), population, multiplier = 10000)

ar %>%
  merge_ci_df(e = 3) %>% # merge the lower and upper CI into one column
  rename("Cases (n)" = cases,
         "Population" = population,
         "AR (per 10,000)" = ar,
         "95%CI" = ci) %>%
  select(-Population) %>% # drop the population column as it is not changing
  knitr::kable(digits = 1, align = "r")

# gtsummary attack rate
add_attack_rate_stat <- function(data, variable, by=NULL, ...) {
  browser()
  population <- data$population[1]
  multiplier <- data$multiplier[1]
  cases <- nrow(data)
  # browser()
  if(!is.null(by)) {
    warning("cfr by strata is not currently available, ignoring `by` argument")
  }

  ar <- epikit::attack_rate(cases = cases,
                            population = population,
                            multiplier = multiplier)

  # browser()
  ar %>%
    merge_ci_df(e = 3) %>% # merge the lower and upper CI into one column
    rename("Cases (n)" = cases,
           "Population" = population,
           "AR (per 10,000)" = ar,
           "95%CI" = ci) %>%
    select(-Population) %>% # drop the population column as it is not changing
    tibble::tibble()
  #
  # # dput(stat_new)
  # structure(list(`Cases (n)` = 76L, `AR (per 10,000)` = 15.2015201520152,
  #                `95%CI` = "(12.16--18.98)"), class = c("tbl_df", "tbl", "data.frame"
  #                ), row.names = c(NA, -1L))

}


gt_ar <- linelist_cleaned %>%
  # Add population and multiplier to data frame (can't pass args to add_stat)
  dplyr::mutate(cases = 1, population = population, multiplier = 10000) %>%
  dplyr::select(cases, population, multiplier) %>%
  # case_fatality_rate_df(deaths = DIED, mergeCI = TRUE) %>%
  gtsummary::tbl_summary(
    include = cases,
    statistic = everything() ~ "",
    label = cases ~ "All participants") %>%
  gtsummary::add_stat(
    fns = list(gtsummary::everything() ~ add_attack_rate_stat)
  ) %>%
  gtsummary::modify_table_body(
    ~ .x %>%
      dplyr::mutate(stat_0 = NULL)

  )

gt_ar

age_levs <- levels(linelist_cleaned$age_group)

gt_ar_age <- linelist_cleaned %>%
  dplyr::select(age_group, case_number) %>%
  # Add population and multiplier to data frame (can't pass args to add_stat)
  dplyr::mutate(cases = 1) %>%
  tidyr::pivot_wider(names_from = age_group, values_from = cases) %>%
  dplyr::mutate(population = population, multiplier = 10000) %>%
  gtsummary::tbl_summary(
    missing = "no",
    include = age_levs,
    statistic = everything() ~ "") %>%
  gtsummary::add_stat(
    fns = list(gtsummary::everything() ~ add_attack_rate_stat)
  ) %>%
  gtsummary::modify_table_body(
    ~ .x %>%
      dplyr::mutate(stat_0 = NULL)
  )

gt_ar_age




