
# Setup -----------------------------------------
#
# ## Installing required packages for this template
required_packages <- c("knitr",       # create output docs
                       "here",        # find your files
                       "dplyr",       # clean/shape data
                       "epikit",      # clean/shape data
                       "forcats",     # clean/shape data
                       "stringr",     # clean text
                       "rio",         # read in data
                       "ggplot2",     # create plots and charts
                       "patchwork",   # combine plots in one
                       "sitrep",      # MSF field epi functions
                       "linelist",    # Functions for cleaning/standardising data/dates
                       #"matchmaker",  # dictionary-based standardization of variables
                       #"incidence",   # create epicurves
                       "aweek",       # define epi weeks
                       #"sf",          # encode spatial vector data
                       #"ggspatial",   # plot maps
                       "testthat"     # testing functions
)

for (pkg in required_packages) {
  # install packages if not already present
  if (!pkg %in% rownames(installed.packages())) {
    install.packages(pkg)
  }

  # load packages to this current session
  library(pkg, character.only = TRUE)
}



# linelist_raw <- rio::import(here::here("AJS_AmTiman.xlsx"), which = "linelist")
linelist_raw <- epidict::gen_data("AJS")

# Load dictionary
linelist_dict <- epidict::msf_dict("AJS", compact = FALSE)

linelist_cleaned <- linelist_raw
## Clean column names

## define clean variable names using clean_labels from the epitrix package
# cleaned_colnames <- epitrix::clean_labels(colnames(linelist_raw))
#
# ## overwrite variable names with defined clean names
# colnames(linelist_cleaned) <- cleaned_colnames

linelist_cleaned <- matchmaker::match_df(linelist_cleaned,
                                         dict  = linelist_dict,
                                         from  = "option_code",
                                         to    = "option_name",
                                         by    = "data_element_shortname",
                                         order = "option_order_in_set"
)

linelist_cleaned <- linelist_cleaned %>%
  filter(!is.na(case_number) & !is.na(date_of_consultation_admission))


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

# Add under 2 years to the age_years variable
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
linelist_cleaned$age_group_mon <- epikit::age_categories(linelist_cleaned$age_months,
                                                         breakers = c(0, 6, 9, 12, 24),
                                                         ceiling = TRUE)

## create an age group variable by specifying categorical breaks
linelist_cleaned$age_group <- epikit::age_categories(linelist_cleaned$age_years,
                                                     breakers = c(0, 3, 15, 30, 45))

#
linelist_cleaned$gender <- factor(linelist_cleaned$sex, levels = c("Male", "Female"))


# Population data
# estimate population size by age group in years
population_data_age <- gen_population(total_pop = 5000,
                                      groups = c("0-2", "3-14", "15-29", "30-44", "45+"),
                                      proportions = c(0.068, 0.3622, 0.276, 0.1616, 0.1321),
                                      strata = NULL) %>%
  rename(age_group = groups,
         population = n)


## estimate population size by region proportion
population_data_region <- gen_population(total_pop = 5000,         # set the total population
                                         groups = c("Village A", "Village B", "Village C", "Village D"),  # set the groups
                                         proportions = c(0.221, 0.174, 0.355, 0.245),                     # set the proportions for each group
                                         strata = NULL) %>%               # do not stratify by gender
  rename(patient_origin = groups,  # rename columns (syntax is NEW NAME = OLD NAME)
         population = n)


population <- sum(population_data_age$population)


# Tests start ------------------------------------------------------------------


test_that("cfr calculation returns gtsummary object and correct results with dichotomous variables", {
  # Uses gtsummary::add_stat but simiplifies it - trade off some customisability/transparency for convenience
  # should be able to handle dichot, logical, multi-level variables - both label and level gtsummary::add_stat
  # function: add_gt_case_fatality_rate
  expected_cfr <-  linelist_cleaned %>%
    filter(patient_facility_type == "Inpatient") %>%
    case_fatality_rate_df(deaths = DIED, mergeCI = TRUE) %>%
    mutate(cfr = formatC(cfr, digits = 2, format = "f"))

  gt_cfr <- linelist_cleaned %>%
    dplyr::filter(patient_facility_type == "Inpatient") %>%
    dplyr::select(DIED) %>%
    # case_fatality_rate_df(deaths = DIED, mergeCI = TRUE) %>%
    gtsummary::tbl_summary(
      statistic = everything() ~ "{N}",
      label = DIED ~ "All participants") %>%
    # Use wrapper function to calculate cfr
    add_cfr(deaths_var = "DIED")
  cfr_df <- gt_cfr$table_body

  expect_s3_class(gt_cfr, "gtsummary")
  expect_equal(as.numeric(cfr_df$Deaths), expected_cfr$deaths)
  expect_equal(as.numeric(cfr_df$stat_0), expected_cfr$population)
  expect_equal(cfr_df$`CFR (%)`, expected_cfr$cfr)
  expect_equal(cfr_df$`95%CI`, expected_cfr$ci)
})

test_that("cfr calculation returns gtsummary object and correct results with categorical variables", {
  # Uses gtsummary::add_stat but simiplifies it - trade off some customisability/transparency for convenience
  # should be able to handle dichot, logical, multi-level variables - both label and level gtsummary::add_stat
  # function: add_gt_case_fatality_rate
  expected_cfr <-  linelist_cleaned %>%
    filter(patient_facility_type == "Inpatient") %>%
    epitabulate::case_fatality_rate_df(deaths = DIED, group = gender, mergeCI = TRUE) %>%
    mutate(cfr = formatC(cfr, 2, format = "f"))

  gt_cfr <- linelist_cleaned %>%
    dplyr::filter(patient_facility_type == "Inpatient") %>%
    dplyr::select(DIED, gender) %>%
    dplyr::mutate(deaths_var = "DIED") %>%
    gtsummary::tbl_summary(
      include = gender,
      statistic = gender ~ "{n}",
      missing = "no",
      label = gender ~ "Gender"
    ) %>%
    # Use wrapper function to calculate cfr
    add_cfr(deaths_var = "DIED")

  cfr_df <- gt_cfr$table_body

  male_exp_cfr <- expected_cfr %>% dplyr::filter(gender == "Male")
  female_exp_cfr <- expected_cfr %>% dplyr::filter(gender == "Female")

  male_cfr <- cfr_df %>% dplyr::filter(label == "Male")
  female_cfr <- cfr_df %>% dplyr::filter(label == "Female")


  expect_s3_class(gt_cfr, "gtsummary")
  expect_equal(as.numeric(male_cfr$Deaths), male_exp_cfr$deaths)
  expect_equal(as.numeric(female_cfr$stat_0), female_exp_cfr$population)
  expect_equal(male_cfr$`CFR (%)`, male_exp_cfr$cfr)
  expect_equal(female_cfr$`95%CI`, female_exp_cfr$ci)

})

test_that("cfr calculation returns gtsummary object and correct results with categorical and dichotomous variables", {
  # Uses gtsummary::add_stat but simiplifies it - trade off some customisability/transparency for convenience
  # should be able to handle dichot, logical, multi-level variables - both label and level gtsummary::add_stat
  # function: add_gt_case_fatality_rate
  expected_cfr_all <- linelist_cleaned %>%
    filter(patient_facility_type == "Inpatient") %>%
    case_fatality_rate_df(deaths = DIED, mergeCI = TRUE) %>%
    mutate(cfr = formatC(cfr, digits = 2, format = "f"))
  expected_cfr <-  linelist_cleaned %>%
    filter(patient_facility_type == "Inpatient") %>%
    epitabulate::case_fatality_rate_df(deaths = DIED, group = gender, mergeCI = TRUE) %>%
    mutate(cfr = formatC(cfr, 2, format = "f"))

  gt_cfr <- linelist_cleaned %>%
    dplyr::filter(patient_facility_type == "Inpatient") %>%
    dplyr::select(DIED, gender) %>%
    gtsummary::tbl_summary(
      statistic = list(DIED ~ "{N}", gender ~ "{n}"),
      label = list(gender ~ "Gender", DIED ~ "All participants")
    ) %>%
    # Use wrapper function to calculate cfr
    add_cfr(deaths_var = "DIED")

  cfr_df <- gt_cfr$table_body

  all_participants <- cfr_df %>% dplyr::filter(variable == "DIED")
  male_exp_cfr <- expected_cfr %>% dplyr::filter(gender == "Male")
  female_exp_cfr <- expected_cfr %>% dplyr::filter(gender == "Female")

  male_cfr <- cfr_df %>% dplyr::filter(label == "Male")
  female_cfr <- cfr_df %>% dplyr::filter(label == "Female")

  expect_s3_class(gt_cfr, "gtsummary")
  expect_equal(as.numeric(male_cfr$Deaths), male_exp_cfr$deaths)
  expect_equal(as.numeric(female_cfr$stat_0), female_exp_cfr$population)
  expect_equal(male_cfr$`CFR (%)`, male_exp_cfr$cfr)
  expect_equal(female_cfr$`95%CI`, female_exp_cfr$ci)
  expect_equal(all_participants$`CFR (%)`, expected_cfr_all$cfr)

})

test_that("attack rate calculation returns gtsummary object and correct results with dichotomous variables", {
  # Case variable but be a logical (T/F or 1,0) with no missing data (NAs)
  # Since there is no missing data forthese  symptoms in linelist, we can code to
  # logical using ifelse - can also be coded as 1,0
  linelist_cleaned <- linelist_cleaned %>%
    mutate(case = ifelse(diarrhoea == "Yes" & bleeding == "Yes" & fever == "Yes", T, F))

  case_count <- sum(linelist_cleaned$case)
  total_pop <- nrow(linelist_cleaned)
  expected_ar <- attack_rate( case_count,  total_pop, multiplier = 10000) %>%
    epikit::merge_ci_df(e = 3)

  gt_ar <- linelist_cleaned %>%
    gtsummary::tbl_summary(
      include = case,
      statistic = case ~ "{n}",
      label = case ~ "Case")  %>%
    add_ar(case_var = "case", multiplier = 10000)

  gt_ar
  ar_df <- gt_ar$table_body

  expect_s3_class(gt_ar, "gtsummary")
  expect_equal(as.numeric(ar_df$stat_0), expected_ar$cases)
  expect_equal(ar_df$`AR (per 10,000)`, formatC(expected_ar$ar, digits = 2, format = "f"))
  expect_equal(ar_df$`95%CI`, expected_ar$ci)
})

test_that("attack rate calculation returns gtsummary object and correct results with categorical variables", {

  # Case variable but be a logical (T/F or 1,0) with no missing data (NAs)
  # Since there is no missing data forthese  symptoms in linelist, we can code to
  # logical using ifelse - can also be coded as 1,0
  linelist_cleaned <- linelist_cleaned %>%
    mutate(case = ifelse(diarrhoea == "Yes" & bleeding == "Yes" & fever == "Yes", T, F))

  counts <- linelist_cleaned %>%
    dplyr::mutate(case = factor(case)) %>%
    dplyr::group_by(age_group, case, .drop = FALSE) %>%
    dplyr::count(name = "case_n") %>%
    dplyr::group_by(age_group) %>%
    dplyr::mutate(total = sum(case_n), .drop = FALSE) %>%
    dplyr::filter(case == T)

  # calculate population total from population table
  expected_ar_lev <- attack_rate(counts$case_n, counts$total, multiplier = 10000) %>%
    epikit::merge_ci_df(e = 3)


  gt_ar_lev <- linelist_cleaned %>%
    # Add population and multiplier to data frame (can't pass args to add_stat)
    dplyr::select(age_group, case) %>%
    gtsummary::tbl_summary(
      include = age_group,
      statistic = age_group ~ "{n}",
      label = age_group ~ "Age group") %>%
    add_ar(case_var = "case", multiplier = 10000)

  ar_df_lev <- gt_ar_lev$table_body
  ar_df_lev <- ar_df_lev %>% filter(label != "Age Group")

  expect_s3_class(gt_ar_lev, "gtsummary")
  expect_equal(as.numeric(ar_df_lev$Cases[-1]), expected_ar_lev$cases)
  expect_equal(ar_df_lev$`AR (per 10,000)`[-1], formatC(expected_ar_lev$ar, digits = 2, format = "f"))
  expect_equal(ar_df_lev$`95%CI`[-1], expected_ar_lev$ci)
})

test_that("attack rate calculation returns gtsummary object and correct results with dichotomous variable forced to categorical", {
  # Case variable but be a logical (T/F or 1,0) with no missing data (NAs)
  # Since there is no missing data forthese  symptoms in linelist, we can code to
  # logical using ifelse - can also be coded as 1,0
  linelist_cleaned <- linelist_cleaned %>%
    mutate(case = ifelse(diarrhoea == "Yes" & bleeding == "Yes" & fever == "Yes", T, F))

  case_count <- sum(linelist_cleaned$case)
  total_pop <- nrow(linelist_cleaned)
  expected_ar <- attack_rate( case_count,  total_pop, multiplier = 10000) %>%
    epikit::merge_ci_df(e = 3)
   # Create dichotomous variable to force to categorical
  linelist_cleaned <- linelist_cleaned %>%
    mutate(setting = factor(
           sample(c("rural", "urban"), nrow(linelist_cleaned), prob = c(0.25, 0.75), replace = TRUE),
           levels = c("rural", "urban")))
  # create fake population numbers for urban/rural
  setting_population <- c(4000, 10000)

  gt_ar_lev <- linelist_cleaned %>%
    # Add population and multiplier to data frame (can't pass args to add_stat)
    dplyr::select(setting, case) %>%
    gtsummary::tbl_summary(
      include = setting,
      statistic = setting ~ "{n}",
      label = setting ~ "Setting",
      type = setting ~ "categorical") %>%
    add_ar(case_var = "case")

  ar_df_lev <- gt_ar_lev$table_body
  ar_df_lev <- ar_df_lev %>% filter(label != "Setting")
  totals <- linelist_cleaned %>% group_by(setting) %>% count()

  expect_s3_class(gt_ar_lev, "gtsummary")
  expect_equal(as.numeric(ar_df_lev$stat_0), totals$n)
})

test_that("attack rate calculation returns gtsummary object and correct results with categorical and dichotomous variables", {
  linelist_cleaned <- linelist_cleaned %>%
    mutate(case = ifelse(diarrhoea == "Yes" & bleeding == "Yes" & fever == "Yes", T, F))


  cases <- linelist_cleaned %>%
    dplyr::mutate(case = factor(case)) %>%
    dplyr::group_by(age_group, case, .drop = FALSE) %>%
    dplyr::count(name = "cases") %>%
    group_by(age_group, .drop = FALSE) %>%
    dplyr::mutate(total = sum(cases)) %>%
    dplyr::filter(case == TRUE)

  # attack rate for all
  case_count <- sum(linelist_cleaned$case)
  total_pop <- nrow(linelist_cleaned)
  expected_ar <- attack_rate( case_count,  total_pop, multiplier = 10000) %>%
    epikit::merge_ci_df(e = 3) %>%
    mutate(ar = formatC(ar, 2, format = "f"))

  # attack rate for each group
  expected_ar_lev <- epitabulate::attack_rate(cases$cases, cases$total, multiplier = 10000) %>%
    epikit::merge_ci_df(e = 3) %>%
    dplyr::mutate(cases = as.character(cases)) %>%
    mutate(ar = formatC(ar, 2, format = "f"))

  gt_ar <- linelist_cleaned %>%
    # Add population and multiplier to data frame (can't pass args to add_stat)
    dplyr::select(case, age_group) %>%
    gtsummary::tbl_summary(
      statistic = list(case ~ "{N}",
                       age_group ~ "{n}"),
      label = list(case ~ "All participants", age_group ~ "Age Group")
    ) %>%
    add_ar(case_var = "case")

  ar_df <- gt_ar$table_body

  expect_s3_class(gt_ar, "gtsummary")

  # Tests for dichotomous
  expect_equal(as.numeric(ar_df$Cases[1]), expected_ar$cases)
  expect_equal(ar_df$`AR (per 10,000)`[1], expected_ar$ar)
  expect_equal(ar_df$`95%CI`[1], expected_ar$ci)

  # Tests for categorical
  expect_equal(ar_df$Cases[-c(1,2)], expected_ar_lev$cases)
  expect_equal(ar_df$`AR (per 10,000)`[-c(1,2)], expected_ar_lev$ar)
  expect_equal(ar_df$`95%CI`[-c(1,2)], expected_ar_lev$ci)
})

test_that("gt_remove_stat removes stat by column name", {
  gt <- linelist_cleaned %>%
    select(DIED, gender, age_group) %>%
    gtsummary::tbl_summary()

  expect(!is.null(gt$table_body$stat_0), "column does not exist")

  gt_removed <- gt %>% gt_remove_stat(col_name = "stat_0")
  suppressWarnings(
    expect(is.null(gt_removed$table_body$stat_0), "column exists")
  )
})

test_that("mortality rate calculation returns gtsummary object and correct results with dichotomous variables", {
  # Deaths variable but be a logical (T/F or 1,0) with no missing data (NAs)

  deaths <- linelist_cleaned %>%
    dplyr::group_by(age_group, DIED) %>%
    dplyr::count(name = "deaths") %>%
    group_by(age_group) %>%
    dplyr::mutate(total = sum(deaths)) %>%
    dplyr::filter(DIED == TRUE)

  # attack rate for each group
  expected_mr_lev <- epitabulate::mortality_rate(deaths$deaths, deaths$total, multiplier = 10000) %>%
    epikit::merge_ci_df(e = 3) %>%
    dplyr::mutate(deaths = as.character(deaths)) %>%
    dplyr::mutate(mr = `mortality per 10 000`) %>%
    mutate(mr = formatC(mr, 2, format = "f"))

  deaths_count <- sum(linelist_cleaned$DIED)
  total_pop <- nrow(linelist_cleaned)

  expected_mr <- epitabulate::mortality_rate( deaths_count,  total_pop, multiplier = 10000) %>%
    epikit::merge_ci_df(e = 3)


  gt_mr <- linelist_cleaned %>%
    dplyr::select(DIED, age_group) %>%
    gtsummary::tbl_summary(
      statistic = list(DIED ~ "{N}",
                       age_group ~ "{n}"),
      label = list(DIED ~ "All participants", age_group ~ "Age Group"))  %>%
    add_mr(deaths_var = "DIED", multiplier = 10000)

  gt_mr
  # Remove 0 from comparison df, because they won't be in expected df
  mr_df <- gt_mr$table_body %>% filter(Deaths != 0)

  expect_s3_class(gt_mr, "gtsummary")
  # Check all participants deaths
  expect_equal(as.numeric(mr_df$Deaths[1]), expected_mr$deaths)
  expect_equal(
    mr_df$`MR (per 10,000)`[1],
    formatC(expected_mr$`mortality per 10 000`, digits = 2, format = "f"))
  expect_equal(mr_df$`95%CI`[1], expected_mr$ci)

  # Tests for categorical
  expect_equal(mr_df$Deaths[-c(1)], expected_mr_lev$deaths)
  expect_equal(mr_df$`MR (per 10,000)`[-c(1)], expected_mr_lev$mr)
  expect_equal(mr_df$`95%CI`[-c(1)], expected_mr_lev$ci)

})


test_that("mortality rate calculation returns gtsummary object and correct results with dichotomous variables with population provided", {
  # Deaths variable but be a logical (T/F or 1,0) with no missing data (NAs)

  age_groups <- levels(linelist_cleaned$age_group)
  deaths <- linelist_cleaned %>%
    dplyr::group_by(age_group, DIED, .drop = FALSE) %>%
    dplyr::count(name = "deaths") %>%
    dplyr::group_by(age_group, .drop = FALSE) %>%
    dplyr::mutate(total = sum(deaths)) %>%
    # dplyr::group_by(age_group, DIED)  %>%
    ungroup() %>%
    tidyr::complete(age_group, DIED, fill = list(deaths = 0, total = 0)) %>%
    dplyr::filter(DIED == TRUE)

  population_arg <- deaths$total * 15

  # attack rate for each group
  expected_mr_lev <- epitabulate::mortality_rate(deaths$deaths, population_arg, multiplier = 10000) %>%
    epikit::merge_ci_df(e = 3) %>%
    dplyr::mutate(deaths = as.character(deaths)) %>%
    dplyr::mutate(mr = `mortality per 10 000`) %>%
    mutate(mr = formatC(mr, 2, format = "f"))

  deaths_count <- sum(linelist_cleaned$DIED)
  total_pop <- sum(population_arg)

  expected_mr <- epitabulate::mortality_rate( deaths_count,  total_pop, multiplier = 10000) %>%
    epikit::merge_ci_df(e = 3)


  gt_mr <- linelist_cleaned %>%
    dplyr::select(DIED, age_group) %>%
    gtsummary::tbl_summary(
      statistic = list(DIED ~ "{N}",
                       age_group ~ "{n}"),
      label = list(DIED ~ "All participants", age_group ~ "Age Group"))  %>%
    add_mr(deaths_var = "DIED",
           population = population_arg,
           multiplier = 10000,
           drop_tblsummary_stat = TRUE)

  gt_mr
  mr_df <- gt_mr$table_body

  expect_s3_class(gt_mr, "gtsummary")
  expect_equal(as.numeric(mr_df$Deaths[1]), expected_mr$deaths)
  # the following should be the expected if population argument not given divided by 15
  # (set by the population_arg variable above)
  expect_equal(
    mr_df$`MR (per 10,000)`[1],
    formatC(expected_mr$`mortality per 10 000`, digits = 2, format = "f"))
  expect_equal(mr_df$`95%CI`[1], expected_mr$ci)

  # Tests for categorical
  expect_equal(mr_df$Deaths[-c(1,2)], expected_mr_lev$deaths)
  expect_equal(mr_df$`MR (per 10,000)`[-c(1,2)], expected_mr_lev$mr)
  expect_equal(mr_df$`95%CI`[-c(1,2)], expected_mr_lev$ci)

})

test_that("attack rate calculation returns gtsummary object and correct results with categorical and dichotomous variables with population provided", {
  linelist_cleaned <- linelist_cleaned %>%
    mutate(case = ifelse(diarrhoea == "Yes" & bleeding == "Yes" & fever == "Yes", T, F))


  cases <- linelist_cleaned %>%
    dplyr::mutate(case = factor(case)) %>%
    dplyr::group_by(age_group, case, .drop = FALSE) %>%
    dplyr::count(name = "cases") %>%
    group_by(age_group, .drop = FALSE) %>%
    dplyr::mutate(total = sum(cases)) %>%
    dplyr::filter(case == TRUE)

  population_arg <- cases$total * 15
  total_pop <- sum(population_arg)

  # attack rate for all
  case_count <- sum(linelist_cleaned$case)
  expected_ar <- attack_rate( case_count,  total_pop, multiplier = 10000) %>%
    epikit::merge_ci_df(e = 3) %>%
    mutate(ar = formatC(ar, 2, format = "f"))

  # attack rate for each group
  expected_ar_lev <- epitabulate::attack_rate(
    cases$cases,
    population_arg,
    multiplier = 10000) %>%
    epikit::merge_ci_df(e = 3) %>%
    dplyr::mutate(cases = as.character(cases)) %>%
    mutate(ar = formatC(ar, 2, format = "f"))

  gt_ar <- linelist_cleaned %>%
    # Add population and multiplier to data frame (can't pass args to add_stat)
    dplyr::select(case, age_group) %>%
    gtsummary::tbl_summary(
      statistic = list(case ~ "{N}",
                       age_group ~ "{n}"),
      label = list(case ~ "All participants", age_group ~ "Age Group")
    ) %>%
    add_ar(case_var = "case",
           population = population_arg,
           drop_tblsummary_stat = TRUE)

  gt_ar
  ar_df <- gt_ar$table_body

  expect_s3_class(gt_ar, "gtsummary")

  # Tests for dichotomous
  expect_equal(as.numeric(ar_df$Cases[1]), expected_ar$cases)
  expect_equal(ar_df$`AR (per 10,000)`[1], expected_ar$ar)
  expect_equal(ar_df$`95%CI`[1], expected_ar$ci)

  # Tests for categorical
  expect_equal(ar_df$Cases[-c(1,2)], expected_ar_lev$cases)
  expect_equal(ar_df$`AR (per 10,000)`[-c(1,2)], expected_ar_lev$ar)
  expect_equal(ar_df$`95%CI`[-c(1,2)], expected_ar_lev$ci)
})



