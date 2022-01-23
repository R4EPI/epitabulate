
# Setup -----------------------------------------
#
# ## Installing required packages for this template
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
                       "sf",          # encode spatial vector data
                       "ggspatial",   # plot maps
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
linelist_cleaned$age_group_mon <- age_categories(linelist_cleaned$age_months,
                                                 breakers = c(0, 6, 9, 12, 24),
                                                 ceiling = TRUE)

## create an age group variable by specifying categorical breaks
linelist_cleaned$age_group <- age_categories(linelist_cleaned$age_years,
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


test_that("cfr calculation returns gtsummary object and correct results for a single result", {
  # This should be used with dichotomus data types without showing missing value
  # and for gtsummary::add_stat by location = "label"
  # Works like gtsummary::add_stat - for those who want to have more control and fully customize
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
    # Use add stat to add attack rate by label
    gtsummary::add_stat(
      fns = list(gtsummary::everything() ~ add_gt_cfr_stat_label)
    )
  cfr_df <- gt_cfr$table_body

  expect_s3_class(gt_cfr, "gtsummary")
  expect_equal(as.numeric(cfr_df$Deaths), expected_cfr$deaths)
  expect_equal(as.numeric(cfr_df$stat_0), expected_cfr$population)
  expect_equal(cfr_df$`CFR (%)`, expected_cfr$cfr)
  expect_equal(cfr_df$`95%CI`, expected_cfr$ci)
})

test_that("cfr calculation returns gtsummary object and correct results for a result by variable level", {
  # Works like gtsummary::add_stat - for those who want to have more control and fully customize
  expected_cfr <-  linelist_cleaned %>%
    filter(patient_facility_type == "Inpatient") %>%
    epikit::case_fatality_rate_df(deaths = DIED, group = gender, mergeCI = TRUE) %>%
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
    gtsummary::add_stat(
      # add purrr::partial with function name and required argument `deaths_var`
      fns = gtsummary::everything() ~ purrr::partial(
        add_gt_cfr_stat_level, deaths_var = "DIED"),
      location = gtsummary::everything() ~ "level"
    )

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


test_that("attack rate calculation returns gtsummary object and correct results for a single result", {
  # Works like add_stat - for those who want to have more control and fully customize

  # calculate population total from population table
  population_total <- sum(population_data_age$population)

  # linelist_cleaned <- linelist_cleaned %>%    # cases for each age_group
  #   mutate(population = population) # population data totdal

  expected_ar <- attack_rate(nrow(linelist_cleaned), population, multiplier = 10000) %>%
    epikit::merge_ci_df(e = 3)

  gt_ar <- linelist_cleaned %>%
    dplyr::mutate(case = 1) %>%
    dplyr::select(case) %>%
    # case_fatality_rate_df(deaths = DIED, mergeCI = TRUE) %>%
    gtsummary::tbl_summary(
      include = case,
      statistic = case ~ "{n}",
      label = case ~ "Case") %>%
    # Use add stat to add attack rate by label
    gtsummary::add_stat(
      # Add population and multiplier in purrr::partial
      fns = gtsummary::everything() ~ purrr::partial(
        add_gt_attack_rate_label, population = population_total, multiplier = 10000
    ))

  gt_ar
  ar_df <- gt_ar$table_body

  expect_s3_class(gt_ar, "gtsummary")
  expect_equal(as.numeric(ar_df$stat_0), expected_ar$cases)
  expect_equal(ar_df$`AR (per 10,000)`, expected_ar$ar)
  expect_equal(ar_df$`95%CI`, expected_ar$ci)
})


test_that("attack rate calculation returns gtsummary object and correct results for a result by variable level", {
  # Works like gtsummary::add_stat - for those who want to have more control and fully customize

  pop_table <- count(linelist_cleaned, age_group) %>%    # cases for each age_group
    left_join(population_data_age, by = "age_group") # merge population data (required for attack rate function)

  # attack rate for each group
  expected_ar_lev <- attack_rate(pop_table$n, pop_table$population, multiplier = 10000, mergeCI = TRUE)

  gt_ar_lev <- linelist_cleaned %>%
    # Add population and multiplier to data frame (can't pass args to add_stat)
    dplyr::mutate(cases = 1, population = population, multiplier = 10000) %>%
    dplyr::select(cases, age_group, population, multiplier) %>%
    # case_fatality_rate_df(deaths = DIED, mergeCI = TRUE) %>%
    gtsummary::tbl_summary(
      include = age_group,
      statistic = age_group ~ "{n}",
      label = age_group ~ "Age group") %>%
    # Use add stat to add attack rate by level
    gtsummary::add_stat(
      fns = gtsummary::everything() ~ purrr::partial(
        add_gt_attack_rate_level, population = pop_table$population, multiplier = 10000),
      location = everything() ~ "level")
  # function: add_gt_attack_rate_level

  ar_df_lev <- gt_ar_lev$table_body
  ar_df_lev <- ar_df_lev %>% filter(label != "Age Group")
  # ar_df_lev <- ar_df_lev[, -1]

  expect_s3_class(gt_ar_lev, "gtsummary")
  expect_equal(as.numeric(ar_df_lev$stat_0[-1]), expected_ar_lev$cases)
  expect_equal(ar_df_lev$`AR (per 10,000)`[-1], expected_ar_lev$ar)
  expect_equal(ar_df_lev$`95%CI`[-1], expected_ar_lev$ci)
})


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
    gtsummary_case_fatality_rate(deaths_var = "DIED")
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
    epikit::case_fatality_rate_df(deaths = DIED, group = gender, mergeCI = TRUE) %>%
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
    gtsummary_case_fatality_rate(deaths_var = "DIED")

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
    epikit::case_fatality_rate_df(deaths = DIED, group = gender, mergeCI = TRUE) %>%
    mutate(cfr = formatC(cfr, 2, format = "f"))

  gt_cfr <- linelist_cleaned %>%
    dplyr::filter(patient_facility_type == "Inpatient") %>%
    dplyr::select(DIED, gender) %>%
    gtsummary::tbl_summary(
      statistic = list(DIED ~ "{N}", gender ~ "{n}"),
      label = list(gender ~ "Gender", DIED ~ "All participants")
    ) %>%
    # Use wrapper function to calculate cfr
    gtsummary_case_fatality_rate(deaths_var = "DIED")

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

  # calculate population total from population table
  population_total <- sum(population_data_age$population)

  # linelist_cleaned <- linelist_cleaned %>%    # cases for each age_group
  #   mutate(population = population) # population data totdal

  expected_ar <- attack_rate(nrow(linelist_cleaned), population, multiplier = 10000) %>%
    epikit::merge_ci_df(e = 3)

  gt_ar <- linelist_cleaned %>%
    dplyr::mutate(case = 1) %>%
    dplyr::select(case) %>%
    # case_fatality_rate_df(deaths = DIED, mergeCI = TRUE) %>%
    gtsummary::tbl_summary(
      include = case,
      statistic = case ~ "{n}",
      label = case ~ "Case")  %>%
    gtsummary_attack_rate(population = population_total, multiplier = 10000)



  gt_ar
  ar_df <- gt_ar$table_body

  expect_s3_class(gt_ar, "gtsummary")
  expect_equal(as.numeric(ar_df$stat_0), expected_ar$cases)
  expect_equal(ar_df$`AR (per 10,000)`, expected_ar$ar)
  expect_equal(ar_df$`95%CI`, expected_ar$ci)
})

test_that("attack rate calculation returns gtsummary object and correct results with categorical variables", {
  # calculate population total from population table
  population_total <- sum(population_data_age$population)

  expected_ar <- attack_rate(nrow(linelist_cleaned), population, multiplier = 10000) %>%
    epikit::merge_ci_df(e = 3)
  pop_table <- count(linelist_cleaned, age_group) %>%    # cases for each age_group
    left_join(population_data_age, by = "age_group") # merge population data (required for attack rate function)

  # linelist_cleaned$population <- NULL
  # linelist_cleaned <- merge(linelist_cleaned, population_data_age, by = "age_group")

  # attack rate for each group
  expected_ar_lev <- attack_rate(pop_table$n, pop_table$population, multiplier = 10000, mergeCI = TRUE)


  gt_ar_lev <- linelist_cleaned %>%
    # Add population and multiplier to data frame (can't pass args to add_stat)
    dplyr::select(age_group) %>%
    # case_fatality_rate_df(deaths = DIED, mergeCI = TRUE) %>%
    gtsummary::tbl_summary(
      include = age_group,
      statistic = age_group ~ "{n}",
      label = age_group ~ "Age group") %>%
    gtsummary_attack_rate(population = pop_table$population, multiplier = 10000)

  # function: add_gt_attack_rate_level

  ar_df_lev <- gt_ar_lev$table_body
  ar_df_lev <- ar_df_lev %>% filter(label != "Age Group")
  # ar_df_lev <- ar_df_lev[, -1]

  expect_s3_class(gt_ar_lev, "gtsummary")
  expect_equal(as.numeric(ar_df_lev$stat_0[-1]), expected_ar_lev$cases)
  expect_equal(ar_df_lev$`AR (per 10,000)`[-1], expected_ar_lev$ar)
  expect_equal(ar_df_lev$`95%CI`[-1], expected_ar_lev$ci)
})

test_that("attack rate calculation returns gtsummary object and correct results with categorical and dichotomous variables", {
  # calculate population total and population table by age from population data age table
  population_total <- sum(population_data_age$population)
  pop_table <- count(linelist_cleaned, age_group) %>%    # cases for each age_group
    left_join(population_data_age, by = "age_group") # merge population data (required

  expected_ar <- attack_rate(nrow(linelist_cleaned), population, multiplier = 10000) %>%
    epikit::merge_ci_df(e = 3) %>%
    dplyr::mutate(cases = as.character(cases))

  # attack rate for each group
  expected_ar_lev <- attack_rate(pop_table$n, pop_table$population, multiplier = 10000, mergeCI = TRUE)

  gt_ar <- linelist_cleaned %>%
    # Add population and multiplier to data frame (can't pass args to add_stat)
    dplyr::mutate(cases = 1) %>%
    dplyr::select(cases, age_group) %>%
    gtsummary::tbl_summary(
      statistic = list(cases ~ "{N}", age_group ~ "{n}"),
      label = list(cases ~ "All participants", age_group ~ "Age Group")
    ) %>%
    gtsummary_attack_rate(population = pop_table$population, multiplier = 10000)

  ar_df <- gt_ar$table_body

  expect_s3_class(gt_ar, "gtsummary")

  # Tests for dichotomous
  expect_equal(ar_df$stat_0[1], expected_ar$cases)
  expect_equal(ar_df$`AR (per 10,000)`[1], expected_ar$ar)
  expect_equal(ar_df$`95%CI`[1], expected_ar$ci)

  # Tests for categorical
  expect_equal(as.numeric(ar_df$stat_0[-c(1,2)]), expected_ar_lev$cases)
  expect_equal(ar_df$`AR (per 10,000)`[-c(1,2)], expected_ar_lev$ar)
  expect_equal(ar_df$`95%CI`[-c(1,2)], expected_ar_lev$ci)
})

test_that("univariate regression with gtsummary returns counts with OR and RR", {
  linelist_3_14 <- count(linelist_cleaned, age_group, DIED) %>%
    filter(age_group == "3-14" & DIED == FALSE)
  linelist_total <- count(linelist_cleaned, DIED) %>%
    filter(DIED == FALSE)
  expected_percent <- formatC((linelist_3_14$n / linelist_total$n) * 100,
                              format = "f", digits = 1)
  expected_count_val <- paste0(linelist_3_14$n, " (", expected_percent, "%)")

  gt_uni <- linelist_cleaned %>%
    select(DIED, gender, age_group) %>%     ## keep variables of interest
    gtsummary::tbl_uvregression(            ## produce univariate table
      method = glm,                         ## define regression want to run (generalised linear model)
      y = DIED,                             ## define outcome variable
      method.args = list(family = binomial),  ## define what type of glm want to run (logistic)
      exponentiate = TRUE,                    ## exponentiate to produce odds ratios (rather than log odds)
      hide_n = TRUE                       ## dont include overall counts in regression table
    ) %>%
    merge_gt_univar_counts()

  uni_df <- gt_uni$table_body
  age_3_14 <- uni_df %>% dplyr::filter(label == "3-14")

  expect_s3_class(gt_uni, "gtsummary")
  # check death counts and percentage are in stat_1 cols
  expect_equal(age_3_14$stat_1_1, expected_count_val)
  # check spanner heading label matches regression y input
  expect("DIED" %in% gt_uni$table_styling$header$spanning_header,
         "missing spanner heading")
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






