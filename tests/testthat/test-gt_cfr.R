# Test suite for gtsummary wrapper functions
# Tests for add_cfr(), add_ar(), add_mr(), and gt_remove_stat()

# Generate synthetic test data ------------------------------------------------
set.seed(123)
n <- 500

test_data <- data.frame(
  id = 1:n,
  age_years = sample(1:80, n, replace = TRUE),
  gender = factor(sample(c("Male", "Female"), n, replace = TRUE)),
  died = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.15, 0.85)),
  case = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.25, 0.75)),
  setting = factor(sample(c("rural", "urban"), n, replace = TRUE, prob = c(0.3, 0.7))),
  stringsAsFactors = FALSE
)

# Create age groups
test_data$age_group <- cut(
  test_data$age_years,
  breaks = c(0, 3, 15, 30, 45, Inf),
  labels = c("0-2", "3-14", "15-29", "30-44", "45+"),
  right = FALSE
)


# Tests for CFR ----------------------------------------------------------------

test_that("CFR calculation works with dichotomous variables", {
  # Calculate expected CFR using epitabulate
  expected_cfr <- test_data %>%
    case_fatality_rate_df(deaths = died, mergeCI = TRUE) %>%
    mutate(
      deaths = formatC(deaths, digits = 0, format = "f"),
      cfr = formatC(cfr, digits = 2, format = "f")
    )

  # Calculate using wrapper function
  gt_cfr <- test_data %>%
    select(died) %>%
    gtsummary::tbl_summary(
      statistic = everything() ~ "{N}",
      label = died ~ "All participants"
    ) %>%
    add_cfr(deaths_var = "died")

  cfr_df <- gt_cfr$table_body

  expect_s3_class(gt_cfr, "gtsummary")
  expect_equal(cfr_df$Deaths, expected_cfr$deaths)
  expect_equal(as.numeric(cfr_df$stat_0), expected_cfr$population)
  expect_equal(cfr_df$`CFR (%)`, expected_cfr$cfr)
  expect_equal(cfr_df$`95%CI`, expected_cfr$ci)
})


test_that("CFR calculation works with categorical variables", {
  # Calculate expected CFR using epitabulate
  expected_cfr <- test_data %>%
    case_fatality_rate_df(deaths = died, group = gender, mergeCI = TRUE) %>%
    mutate(
      deaths = formatC(deaths, digits = 0, format = "f"),
      cfr = formatC(cfr, digits = 2, format = "f")
    )

  # Calculate using wrapper function
  gt_cfr <- test_data %>%
    select(died, gender) %>%
    gtsummary::tbl_summary(
      include = gender,
      statistic = gender ~ "{n}",
      missing = "no",
      label = gender ~ "Gender"
    ) %>%
    add_cfr(deaths_var = "died")

  cfr_df <- gt_cfr$table_body %>%
    filter(!is.na(label), label != "Gender")

  male_expected <- expected_cfr %>% filter(gender == "Male")
  female_expected <- expected_cfr %>% filter(gender == "Female")

  male_cfr <- cfr_df %>% filter(label == "Male")
  female_cfr <- cfr_df %>% filter(label == "Female")

  expect_s3_class(gt_cfr, "gtsummary")
  expect_equal(male_cfr$Deaths, male_expected$deaths)
  expect_equal(female_cfr$Deaths, female_expected$deaths)
  expect_equal(male_cfr$`CFR (%)`, male_expected$cfr)
  expect_equal(female_cfr$`CFR (%)`, female_expected$cfr)
  expect_equal(male_cfr$`95%CI`, male_expected$ci)
  expect_equal(female_cfr$`95%CI`, female_expected$ci)
})


test_that("CFR calculation works with mixed categorical and dichotomous variables", {
  # Calculate expected CFR for overall
  expected_cfr_all <- test_data %>%
    case_fatality_rate_df(deaths = died, mergeCI = TRUE) %>%
    mutate(
      deaths = formatC(deaths, digits = 0, format = "f"),
      cfr = formatC(cfr, digits = 2, format = "f")
    )

  # Calculate expected CFR by gender
  expected_cfr_gender <- test_data %>%
    case_fatality_rate_df(deaths = died, group = gender, mergeCI = TRUE) %>%
    mutate(
      deaths = formatC(deaths, digits = 0, format = "f"),
      cfr = formatC(cfr, digits = 2, format = "f")
    )

  # Calculate using wrapper function
  gt_cfr <- test_data %>%
    select(died, gender) %>%
    gtsummary::tbl_summary(
      statistic = list(died ~ "{N}", gender ~ "{n}"),
      label = list(gender ~ "Gender", died ~ "All participants")
    ) %>%
    add_cfr(deaths_var = "died")

  cfr_df <- gt_cfr$table_body

  all_participants <- cfr_df %>% filter(variable == "died")

  expect_s3_class(gt_cfr, "gtsummary")
  expect_equal(all_participants$Deaths, expected_cfr_all$deaths)
  expect_equal(all_participants$`CFR (%)`, expected_cfr_all$cfr)
  expect_equal(all_participants$`95%CI`, expected_cfr_all$ci)
})


# Tests for Attack Rate --------------------------------------------------------

test_that("Attack rate calculation works with dichotomous variables", {
  # Calculate expected AR using epitabulate
  cases <- sum(test_data$case)
  population <- nrow(test_data)

  expected_ar <- attack_rate(cases, population, multiplier = 10000) %>%
    merge_ci_df(e = 3) %>%
    mutate(
      cases = formatC(cases, digits = 0, format = "f"),
      ar = formatC(ar, digits = 2, format = "f")
    )

  # Calculate using wrapper function
  gt_ar <- test_data %>%
    select(case) %>%
    gtsummary::tbl_summary(
      statistic = case ~ "{n}",
      label = case ~ "Case"
    ) %>%
    add_ar(case_var = "case", multiplier = 10000)

  ar_df <- gt_ar$table_body

  expect_s3_class(gt_ar, "gtsummary")
  expect_equal(ar_df$`AR (per 10,000)`, expected_ar$ar)
  expect_equal(ar_df$`95%CI`, expected_ar$ci)
})


test_that("Attack rate calculation works with categorical variables", {
  # Calculate expected AR by age group
  counts <- test_data %>%
    mutate(case = factor(case)) %>%
    group_by(age_group, case, .drop = FALSE) %>%
    count(name = "case_n") %>%
    group_by(age_group, .drop = FALSE) %>%
    mutate(total = sum(case_n)) %>%
    filter(case == "TRUE")

  expected_ar <- attack_rate(counts$case_n, counts$total, multiplier = 10000) %>%
    merge_ci_df(e = 3) %>%
    mutate(
      cases = formatC(cases, digits = 0, format = "f"),
      ar = formatC(ar, digits = 2, format = "f")
    )

  # Calculate using wrapper function
  gt_ar <- test_data %>%
    select(age_group, case) %>%
    gtsummary::tbl_summary(
      include = age_group,
      statistic = age_group ~ "{n}",
      label = age_group ~ "Age group"
    ) %>%
    add_ar(case_var = "case", multiplier = 10000)

  ar_df <- gt_ar$table_body %>%
    filter(!is.na(label), label != "Age group")

  expect_s3_class(gt_ar, "gtsummary")
  expect_equal(ar_df$Cases, expected_ar$cases)
  expect_equal(ar_df$`AR (per 10,000)`, expected_ar$ar)
  expect_equal(ar_df$`95%CI`, expected_ar$ci)
})


test_that("Attack rate calculation works with provided population", {
  # Define population vector
  age_levels <- levels(test_data$age_group)
  population_vector <- c(1000, 5000, 4000, 3000, 2000)

  # Calculate expected AR with custom population
  counts <- test_data %>%
    mutate(case = factor(case)) %>%
    group_by(age_group, case, .drop = FALSE) %>%
    count(name = "case_n") %>%
    group_by(age_group, .drop = FALSE) %>%
    mutate(total = sum(case_n)) %>%
    filter(case == "TRUE")

  expected_ar <- attack_rate(counts$case_n, population_vector, multiplier = 10000) %>%
    merge_ci_df(e = 3) %>%
    mutate(
      cases = formatC(cases, digits = 0, format = "f"),
      ar = formatC(ar, digits = 2, format = "f")
    )

  # Calculate using wrapper function
  gt_ar <- test_data %>%
    select(age_group, case) %>%
    gtsummary::tbl_summary(
      include = age_group,
      statistic = age_group ~ "{n}",
      label = age_group ~ "Age group"
    ) %>%
    add_ar(
      case_var = "case",
      population = population_vector,
      multiplier = 10000
    )

  ar_df <- gt_ar$table_body %>%
    filter(!is.na(label), label != "Age group")

  expect_s3_class(gt_ar, "gtsummary")
  expect_true("Population" %in% names(ar_df))
  expect_equal(ar_df$Cases, expected_ar$cases)
  expect_equal(ar_df$`AR (per 10,000)`, expected_ar$ar)
})


test_that("Attack rate calculation works with mixed variables", {
  # Calculate expected AR for all
  cases_all <- sum(test_data$case)
  population_all <- nrow(test_data)

  expected_ar_all <- attack_rate(cases_all, population_all, multiplier = 10000) %>%
    merge_ci_df(e = 3) %>%
    mutate(
      cases = formatC(cases, digits = 0, format = "f"),
      ar = formatC(ar, digits = 2, format = "f")
    )

  # Calculate expected AR by age group
  counts <- test_data %>%
    mutate(case = factor(case)) %>%
    group_by(age_group, case, .drop = FALSE) %>%
    count(name = "case_n") %>%
    group_by(age_group, .drop = FALSE) %>%
    mutate(total = sum(case_n)) %>%
    filter(case == "TRUE")

  expected_ar_group <- attack_rate(counts$case_n, counts$total, multiplier = 10000) %>%
    merge_ci_df(e = 3) %>%
    mutate(
      cases = formatC(cases, digits = 0, format = "f"),
      ar = formatC(ar, digits = 2, format = "f")
    )

  # Calculate using wrapper function
  gt_ar <- test_data %>%
    select(case, age_group) %>%
    gtsummary::tbl_summary(
      statistic = list(case ~ "{N}", age_group ~ "{n}"),
      label = list(case ~ "All participants", age_group ~ "Age Group")
    ) %>%
    add_ar(case_var = "case", multiplier = 10000)

  ar_df <- gt_ar$table_body

  all_row <- ar_df %>% filter(variable == "case")
  group_rows <- ar_df %>% filter(variable == "age_group", !is.na(label), label != "Age Group")

  expect_s3_class(gt_ar, "gtsummary")
  expect_equal(all_row$`AR (per 10,000)`, expected_ar_all$ar)
  expect_equal(group_rows$Cases, expected_ar_group$cases)
  expect_equal(group_rows$`AR (per 10,000)`, expected_ar_group$ar)
})


# Tests for Mortality Rate -----------------------------------------------------

test_that("Mortality rate calculation works with dichotomous variables", {
  # Calculate expected MR using epitabulate
  deaths <- sum(test_data$died)
  population <- nrow(test_data)

  expected_mr <- mortality_rate(deaths, population, multiplier = 10000) %>%
    merge_ci_df(e = 3) %>%
    mutate(
      deaths = formatC(deaths, digits = 0, format = "f"),
      `mortality per 10 000` = formatC(`mortality per 10 000`, digits = 2, format = "f")
    )

  # Calculate using wrapper function
  gt_mr <- test_data %>%
    select(died) %>%
    gtsummary::tbl_summary(
      statistic = died ~ "{N}",
      label = died ~ "All participants"
    ) %>%
    add_mr(deaths_var = "died", multiplier = 10000)

  mr_df <- gt_mr$table_body

  expect_s3_class(gt_mr, "gtsummary")
  expect_equal(mr_df$`MR (per 10,000)`, expected_mr$`mortality per 10 000`)
  expect_equal(mr_df$`95%CI`, expected_mr$ci)
})


test_that("Mortality rate calculation works with categorical variables", {
  # Calculate expected MR for all
  deaths_all <- sum(test_data$died)
  population_all <- nrow(test_data)

  expected_mr_all <- mortality_rate(deaths_all, population_all, multiplier = 10000) %>%
    merge_ci_df(e = 3) %>%
    mutate(
      deaths = formatC(deaths, digits = 0, format = "f"),
      `mortality per 10 000` = formatC(`mortality per 10 000`, digits = 2, format = "f")
    )

  # Calculate expected MR by age group
  counts <- test_data %>%
    mutate(died = factor(died)) %>%
    group_by(age_group, died, .drop = FALSE) %>%
    count(name = "deaths_n") %>%
    group_by(age_group, .drop = FALSE) %>%
    mutate(total = sum(deaths_n)) %>%
    filter(died == "TRUE")

  expected_mr_group <- mortality_rate(counts$deaths_n, counts$total, multiplier = 10000) %>%
    merge_ci_df(e = 3) %>%
    mutate(
      deaths = formatC(deaths, digits = 0, format = "f"),
      `mortality per 10 000` = formatC(`mortality per 10 000`, digits = 2, format = "f")
    )

  # Calculate using wrapper function
  gt_mr <- test_data %>%
    select(died, age_group) %>%
    gtsummary::tbl_summary(
      statistic = list(died ~ "{N}", age_group ~ "{n}"),
      label = list(died ~ "All participants", age_group ~ "Age Group")
    ) %>%
    add_mr(deaths_var = "died", multiplier = 10000)

  mr_df <- gt_mr$table_body

  all_row <- mr_df %>% filter(variable == "died")
  group_rows <- mr_df %>% filter(variable == "age_group", !is.na(label), label != "Age Group")

  expect_s3_class(gt_mr, "gtsummary")
  expect_equal(all_row$`MR (per 10,000)`, expected_mr_all$`mortality per 10 000`)
  expect_equal(group_rows$Deaths, expected_mr_group$deaths)
  expect_equal(group_rows$`MR (per 10,000)`, expected_mr_group$`mortality per 10 000`)
})


test_that("Mortality rate calculation works with provided population", {
  # Define population vector
  age_levels <- levels(test_data$age_group)
  population_vector <- c(1000, 5000, 4000, 3000, 2000)

  # Calculate expected MR with custom population for all
  deaths_all <- sum(test_data$died)
  population_all <- sum(population_vector)

  expected_mr_all <- mortality_rate(deaths_all, population_all, multiplier = 10000) %>%
    merge_ci_df(e = 3) %>%
    mutate(
      deaths = formatC(deaths, digits = 0, format = "f"),
      `mortality per 10 000` = formatC(`mortality per 10 000`, digits = 2, format = "f")
    )

  # Calculate expected MR by age group
  counts <- test_data %>%
    mutate(died = factor(died)) %>%
    group_by(age_group, died, .drop = FALSE) %>%
    count(name = "deaths_n") %>%
    group_by(age_group, .drop = FALSE) %>%
    mutate(total = sum(deaths_n)) %>%
    filter(died == "TRUE")

  expected_mr_group <- mortality_rate(counts$deaths_n, population_vector, multiplier = 10000) %>%
    merge_ci_df(e = 3) %>%
    mutate(
      deaths = formatC(deaths, digits = 0, format = "f"),
      `mortality per 10 000` = formatC(`mortality per 10 000`, digits = 2, format = "f")
    )

  # Calculate using wrapper function
  gt_mr <- test_data %>%
    select(died, age_group) %>%
    gtsummary::tbl_summary(
      statistic = list(died ~ "{N}", age_group ~ "{n}"),
      label = list(died ~ "All participants", age_group ~ "Age Group")
    ) %>%
    add_mr(
      deaths_var = "died",
      population = population_vector,
      multiplier = 10000
    )

  mr_df <- gt_mr$table_body

  all_row <- mr_df %>% filter(variable == "died")
  group_rows <- mr_df %>% filter(variable == "age_group", !is.na(label), label != "Age Group")

  expect_s3_class(gt_mr, "gtsummary")
  expect_true("Population" %in% names(mr_df))
  expect_equal(all_row$`MR (per 10,000)`, expected_mr_all$`mortality per 10 000`)
  expect_equal(group_rows$Deaths, expected_mr_group$deaths)
})


# Tests for gt_remove_stat -----------------------------------------------------

test_that("gt_remove_stat removes specified column", {
  gt <- test_data %>%
    select(died, gender) %>%
    gtsummary::tbl_summary()

  expect_true("stat_0" %in% names(gt$table_body))

  gt_removed <- gt %>% gt_remove_stat(col_name = "stat_0")

  expect_false("stat_0" %in% names(gt_removed$table_body))
})


test_that("drop_tblsummary_stat parameter works for attack rate", {
  gt_ar <- test_data %>%
    select(case) %>%
    gtsummary::tbl_summary(
      statistic = case ~ "{n}",
      label = case ~ "Case"
    ) %>%
    add_ar(
      case_var = "case",
      multiplier = 10000,
      drop_tblsummary_stat = TRUE
    )

  expect_false("stat_0" %in% names(gt_ar$table_body))
})


test_that("drop_tblsummary_stat parameter works for mortality rate", {
  gt_mr <- test_data %>%
    select(died) %>%
    gtsummary::tbl_summary(
      statistic = died ~ "{N}",
      label = died ~ "All participants"
    ) %>%
    add_mr(
      deaths_var = "died",
      multiplier = 10000,
      drop_tblsummary_stat = TRUE
    )

  expect_false("stat_0" %in% names(gt_mr$table_body))
})
