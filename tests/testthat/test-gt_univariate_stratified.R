### Tests and data from Rothman et al (Modern Epidemiology 4th edition chapter 18)

# ODDS ratios

# data from Rothman spermicide study (Table 18-5)
# crude, strata 1, strata 2, MH
or_expect <- data.frame(
  ratio = c(3.50, 3.39, 5.73, 3.78),
  lower = c(0.97, 0.74, 0.26, 1.19),
  upper = c(10.2, 11.6, 55.6, 12.0)
)

arr <- array(
  data = c(3, 104, 9, 1059, 1, 5, 3, 86),
  dim  = c(2 , 2 , 2),
  dimnames = list(
    outcome    = c(TRUE , FALSE),
    exposure = c(TRUE , FALSE),
    age     = c(FALSE, TRUE)
  )
)

# Convert to data frame and expand counts into individual rows
arrt <- as.data.frame.table(arr) |>
  dplyr::mutate(
    outcome  = as.logical(outcome),
    exposure = as.logical(exposure),
    age      = as.logical(age)
  ) |>
  tidyr::uncount(weights = Freq)

# crude - confidence intervals here are based on profile likelihood from GLMs
or_outcome <- gtsummary::tbl_uvregression(arrt,
                                          method = glm,
                                          y = outcome,
                                          include = exposure,
                                          method.args = list(family = binomial),
                                          exponentiate = TRUE,
                                          hide_n = TRUE)

or_outcome_wide <- or_outcome |>
  add_crosstabs(wide = TRUE)

or_outcome <- or_outcome |>
  add_crosstabs()


# stratified
# strata confidence intervals based on profile likelihood from GLMs
# mantel haenszel CIs based on robins-breslow-greenland variance from rothman textbook
# (both more modern and precise than Wald)
# mantel haenszel p-value is a woolf test for heterogeneity of the strata
or_cmh_outcome <- tbl_cmh(arrt, outcome, exposure, age, measure = "OR") |>
  add_crosstabs()





# RISK ratios

# data from Rothman tolbutamide study
# (Table 18-2 and section "Mantel-Haenszel Estimation: Pure Count Data")
# crude, strata 1, strata 2, MH
rr_expect <- data.frame(
  ratio = c(1.44, 1.81, 1.19, 1.33),
  lower = c(0.83, 0.60, 0.63, 0.80),
  upper = c(2.54, 5.99, 2.31, 2.21)
)

arr_rr <- array(
  data = c(8, 98, 5, 115, 22, 76, 16, 69),
  dim  = c(2, 2, 2),
  dimnames = list(
    outcome  = c(TRUE, FALSE),
    exposure = c(TRUE, FALSE),
    age      = c(FALSE, TRUE)
  )
)

arrt_rr <- as.data.frame.table(arr_rr) |>
  dplyr::mutate(
    outcome  = as.logical(outcome),
    exposure = as.logical(exposure),
    age      = as.logical(age)
  ) |>
  tidyr::uncount(weights = Freq)

# crude - confidence intervals here are based on profile likelihood
rr_outcome <- gtsummary::tbl_uvregression(arrt_rr,
                                          method = MASS::glm.nb,
                                          y = outcome,
                                          include = exposure,
                                          exponentiate = TRUE,
                                          hide_n = TRUE)

rr_outcome_wide <- rr_outcome |>
  add_crosstabs(wide = TRUE)

rr_outcome <- rr_outcome |>
  add_crosstabs()

# stratified
# strata confidence intervals based on profile likelihood from GLM.NB
# mantel haenszel CIs based on robins-breslow-greenland variance from rothman textbook
# (both more modern and precise than Wald)
# mantel haenszel p-value is a woolf test for heterogeneity of the strata
rr_cmh_outcome <- tbl_cmh(arrt_rr, outcome, exposure, age, measure = "RR") |>
  add_crosstabs()



# INCIDENCE RATE ratios

# data from Rothman british doctors smoking study
# (Table 18-1 and section "Mantel-Haenszel Estimation: Person-Time Data")

irr_expect <- data.frame(
  ratio = c(1.72, 5.74, 2.14, 1.47, 1.36, 0.90, 1.42),
  lower = c(1.40, 1.74, 1.23, 1.01, 0.93, 0.61, 1.15),
  upper = c(2.13, 35.4, 4.10, 2.23, 2.06, 1.37, 1.76)
)


arr_irr <- data.frame(
  age = c("35-44", "35-44", "45-54", "45-54", "55-64", "55-64", "65-74", "65-74", "75-84", "75-84"),
  exposure = c("smoker", "nonsmoker", "smoker", "nonsmoker", "smoker", "nonsmoker", "smoker", "nonsmoker", "smoker", "nonsmoker"),
  deaths = c(32, 2, 104, 12, 206, 28, 186, 28, 102, 31),
  years  = c(52407, 18790, 43248, 10673, 28612, 5710, 12663, 2585, 5317, 1462)
)


arrt_irr <- do.call(
  rbind,
  apply(arr_irr, 1, function(x) {
    age <- x[["age"]]
    exposure <- x[["exposure"]]
    deaths <- as.integer(x[["deaths"]])
    years <- as.integer(x[["years"]])

    # build event vector: 1 for deaths, 0 for survivors
    event <- c(rep(1L, deaths), rep(0L, years - deaths))

    # return dataframe
    data.frame(
      age = rep(age, years),
      exposure = rep(exposure, years),
      event = event
    )
  })
)

arrt_irr <- arrt_irr |>
  dplyr::mutate(years = 1)

# crude
irr_outcome <- gtsummary::tbl_uvregression(arrt_irr,
                                      method = glm,
                                      y = event,
                                      include = exposure,
                                      method.args = list(family = poisson,
                                                         offset = log(years)),
                                      exponentiate = TRUE,
                                      hide_n = TRUE)

irr_outcome_wide <- irr_outcome |>
  add_crosstabs(wide = TRUE)

irr_outcome <- irr_outcome |>
  add_crosstabs()

# stratified
# strata confidence intervals based on profile likelihood from GLM
# mantel haenszel CIs based on wald from rothman textbook
# ("this yields virtually the same point estimates and confidence limits for IR")
# mantel haenszel p-value is a woolf test for heterogeneity of the strata
irr_cmh_outcome <- tbl_cmh(arrt_irr, event, exposure, age,
                           obstime = years, measure = "IRR") |>
  add_crosstabs()



### Tests for tbl_cmh()
test_that("OR estimates correct", {

  ## estimates
  ## crude
  expect_equal(or_expect$ratio[1], or_cmh_outcome$table_body$estimate[3], tolerance = 0.01)
  ## strata
  expect_equal(or_expect$ratio[2], or_cmh_outcome$table_body$estimate[6], tolerance = 0.01)
  expect_equal(or_expect$ratio[3], or_cmh_outcome$table_body$estimate[9], tolerance = 0.01)
  ## mh
  expect_equal(or_expect$ratio[4], as.numeric(or_cmh_outcome$table_body$mh_estimate[3]), tolerance = 0.01)
})

test_that("OR CIs are correct", {

  ## CI
  ## crude
  expect_equal(or_expect$lower[1], or_cmh_outcome$table_body$conf.low[3], tolerance = 0.01)
  expect_equal(or_expect$upper[1], or_cmh_outcome$table_body$conf.high[3], tolerance = 0.01)
  ## strata
  expect_equal(or_expect$lower[2], or_cmh_outcome$table_body$conf.low[6], tolerance = 0.01)
  expect_equal(or_expect$upper[2], or_cmh_outcome$table_body$conf.high[6], tolerance = 0.01)
  expect_equal(or_expect$lower[3], or_cmh_outcome$table_body$conf.low[9], tolerance = 0.01)
  expect_equal(or_expect$upper[3], or_cmh_outcome$table_body$conf.high[9], tolerance = 0.01)
  ## mh
  expect_equal(or_expect$lower[4], as.numeric(or_cmh_outcome$table_body$mh_conf.low[3]), tolerance = 0.01)
  expect_equal(or_expect$upper[4], as.numeric(or_cmh_outcome$table_body$mh_conf.high[3]), tolerance = 0.01)
})


test_that("RR estimates correct", {
  expect_equal(rr_expect$ratio[1], unname(rr_cmh_outcome$table_body$estimate[3]), tolerance = 0.01)
  expect_equal(rr_expect$ratio[2], unname(rr_cmh_outcome$table_body$estimate[6]), tolerance = 0.01)
  expect_equal(rr_expect$ratio[3], unname(rr_cmh_outcome$table_body$estimate[9]), tolerance = 0.01)
  expect_equal(rr_expect$ratio[4], unname(as.numeric(rr_cmh_outcome$table_body$mh_estimate[3])), tolerance = 0.01)
})

test_that("RR CIs are correct", {
  expect_equal(rr_expect$lower[1], unname(rr_cmh_outcome$table_body$conf.low[3]), tolerance = 0.01)
  expect_equal(rr_expect$upper[1], unname(rr_cmh_outcome$table_body$conf.high[3]), tolerance = 0.01)
  expect_equal(rr_expect$lower[2], unname(rr_cmh_outcome$table_body$conf.low[6]), tolerance = 0.01)
  expect_equal(rr_expect$upper[2], unname(rr_cmh_outcome$table_body$conf.high[6]), tolerance = 0.01)
  expect_equal(rr_expect$lower[3], unname(rr_cmh_outcome$table_body$conf.low[9]), tolerance = 0.01)
  expect_equal(rr_expect$upper[3], unname(rr_cmh_outcome$table_body$conf.high[9]), tolerance = 0.01)
  expect_equal(rr_expect$lower[4], unname(as.numeric(rr_cmh_outcome$table_body$mh_conf.low[3])), tolerance = 0.01)
  expect_equal(rr_expect$upper[4], unname(as.numeric(rr_cmh_outcome$table_body$mh_conf.high[3])), tolerance = 0.01)
})

test_that("IRR estimates correct", {
  expect_equal(irr_expect$ratio[1], unname(irr_cmh_outcome$table_body$estimate[3]), tolerance = 0.01)
  expect_equal(irr_expect$ratio[2], unname(irr_cmh_outcome$table_body$estimate[6]), tolerance = 0.01)
  expect_equal(irr_expect$ratio[3], unname(irr_cmh_outcome$table_body$estimate[9]), tolerance = 0.01)
  expect_equal(irr_expect$ratio[4], unname(irr_cmh_outcome$table_body$estimate[12]), tolerance = 0.01)
  expect_equal(irr_expect$ratio[5], unname(irr_cmh_outcome$table_body$estimate[15]), tolerance = 0.01)
  expect_equal(irr_expect$ratio[6], unname(irr_cmh_outcome$table_body$estimate[18]), tolerance = 0.01)
  expect_equal(irr_expect$ratio[7], unname(as.numeric(irr_cmh_outcome$table_body$mh_estimate[3])), tolerance = 0.01)
})

test_that("IRR CIs are correct", {
  for (i in 1:6) {
    expect_equal(irr_expect$lower[i], unname(irr_cmh_outcome$table_body$conf.low[i*3]), tolerance = 0.01)
    expect_equal(irr_expect$upper[i], unname(irr_cmh_outcome$table_body$conf.high[i*3]), tolerance = 0.01)
  }
  expect_equal(irr_expect$lower[7], unname(as.numeric(irr_cmh_outcome$table_body$mh_conf.low[3])), tolerance = 0.01)
  expect_equal(irr_expect$upper[7], unname(as.numeric(irr_cmh_outcome$table_body$mh_conf.high[3])), tolerance = 0.01)
})



## pvals
# test_that("woolf p-values work", {
#
#   expect_equivalent(get_woolf_pval(arr, "RR"), RR_woolf)
#   expect_equivalent(get_woolf_pval(arr, "OR"), OR_woolf)
#
# })


## errors
# test_that("tab_univariate requires a data frame", {
#   expect_error(tab_univariate(arr, some, vars), "x must be a data frame")
# })
#
# test_that("Exposure variables must be logical", {
#   itest <- iris %>%
#     dplyr::mutate(
#       sl = Sepal.Length > 6,
#       pl = Petal.Length > 4,
#       ve = Species == "versicolor"
#     )
#
#   expect_error(tab_univariate(itest, Species, sl, strata = pl),
#                "outcome must be a TRUE/FALSE variable")
#
#   expect_error(tab_univariate(itest, ve, sl, strata = Petal.Length),
#                "strata variable must be a TRUE/FALSE variable")
#
#   expect_error(tab_univariate(itest, ve, sl, Petal.Length),
#                "exposure variables must be TRUE/FALSE variables, but Petal.Length is a numeric.")
#
#   expect_error(tab_univariate(itest, ve, sl, strata = pl, measure = "IRR"),
#                "You have selected IRR as a measure but not specified a perstime variable."
#   )
#
# })




#### Tests for add_crosstabs()

test_that("add_crosstabs throws error for wrong input class", {
  expect_error(
    add_crosstabs(data.frame(x = 1:5)),
    "`x` must be class 'tbl_uvregression' or 'tbl_cmh'"
  )
})

test_that("add_crosstabs works with tbl_uvregression for logistic regression", {

  expect_s3_class(or_outcome, "gtsummary")
  expect_s3_class(or_outcome, "tbl_uvregression")

  tb <- or_outcome$table_body

  # ---- Expected counts directly from arr ----
  # collapse over age strata
  expected_cases_exposed <- sum(arr["TRUE", "TRUE", ])
  expected_controls_exposed <- sum(arr["FALSE", "TRUE", ])
  expected_cases_unexposed <- sum(arr["TRUE", "FALSE", ])
  expected_controls_unexposed <- sum(arr["FALSE", "FALSE", ])

  result_row <- tb[!tb$reference_row & !is.na(tb$reference_row), ]
  reference_row <- tb[tb$reference_row & !is.na(tb$reference_row), ]

  expect_equal(unname(result_row$n_event), expected_cases_exposed)
  expect_equal(unname(result_row$n_nonevent), expected_controls_exposed)
  expect_equal(unname(reference_row$n_event), expected_cases_unexposed)
  expect_equal(unname(reference_row$n_nonevent), expected_controls_unexposed)
})

test_that("add_crosstabs calculates correct counts for each exposure level", {

  tb <- or_outcome$table_body

  expected_cases_exposed <- sum(arr["TRUE", "TRUE", ])
  expected_controls_exposed <- sum(arr["FALSE", "TRUE", ])
  expected_cases_unexposed <- sum(arr["TRUE", "FALSE", ])
  expected_controls_unexposed <- sum(arr["FALSE", "FALSE", ])

  exposed_row <- tb[!tb$reference_row & !is.na(tb$reference_row), ]
  unexposed_row <- tb[tb$reference_row & !is.na(tb$reference_row), ]

  expect_equal(unname(exposed_row$n_event), expected_cases_exposed)
  expect_equal(unname(exposed_row$n_nonevent), expected_controls_exposed)
  expect_equal(unname(unexposed_row$n_event), expected_cases_unexposed)
  expect_equal(unname(unexposed_row$n_nonevent), expected_controls_unexposed)
})

test_that("add_crosstabs works with tbl_cmh for stratified analysis", {

  expect_s3_class(or_cmh_outcome, "gtsummary")
  expect_s3_class(or_cmh_outcome, "tbl_cmh")

  tb <- or_cmh_outcome$table_body |>
    tidyr::fill(stratifier)

  # ---- Stratum-specific counts from arr ----
  # age = FALSE stratum
  stratum1_cases_exposed <- arr["TRUE", "TRUE", "FALSE"]
  stratum1_controls_exposed <- arr["FALSE", "TRUE", "FALSE"]
  # age = TRUE stratum
  stratum2_cases_exposed <- arr["TRUE", "TRUE", "TRUE"]
  stratum2_controls_exposed <- arr["FALSE", "TRUE", "TRUE"]

  stratum1 <- tb[tb$stratifier == FALSE &
                   !tb$reference_row & !is.na(tb$reference_row), ]
  stratum2 <- tb[tb$stratifier == TRUE &
                   !tb$reference_row & !is.na(tb$reference_row), ]

  expect_equal(unname(stratum1$n_event), stratum1_cases_exposed)
  expect_equal(unname(stratum1$n_nonevent), stratum1_controls_exposed)
  expect_equal(unname(stratum2$n_event), stratum2_cases_exposed)
  expect_equal(unname(stratum2$n_nonevent), stratum2_controls_exposed)
})

test_that("add_crosstabs header labels are correct for logistic regression", {

  headers <- or_outcome$table_styling$header
  n_event_header <- headers[headers$column == "n_event", "label"]
  n_nonevent_header <- headers[headers$column == "n_nonevent", "label"]

  expect_equal(dplyr::pull(n_event_header), "**Case (n)**")
  expect_equal(dplyr::pull(n_nonevent_header), "**Control (n)**")
})

test_that("add_crosstabs works in wide format for dichotomous variables", {

  tb <- or_outcome_wide$table_body

  expect_true(all(c("n_event_FALSE", "n_event_TRUE",
                    "n_nonevent_FALSE", "n_nonevent_TRUE") %in% names(tb)))

  expect_equal(nrow(tb), 1)

  # ---- Expected counts directly from arr ----
  expected_cases_exposed <- sum(arr["TRUE", "TRUE", ])
  expected_controls_exposed <- sum(arr["FALSE", "TRUE", ])
  expected_cases_unexposed <- sum(arr["TRUE", "FALSE", ])
  expected_controls_unexposed <- sum(arr["FALSE", "FALSE", ])

  expect_equal(unname(tb$n_event_FALSE), expected_cases_exposed)
  expect_equal(unname(tb$n_nonevent_FALSE), expected_controls_exposed)
  expect_equal(unname(tb$n_event_TRUE), expected_cases_unexposed)
  expect_equal(unname(tb$n_nonevent_TRUE), expected_controls_unexposed)
})


test_that("add_crosstabs works with tbl_uvregression for risk ratios", {

  expect_s3_class(rr_outcome, "gtsummary")
  expect_s3_class(rr_outcome, "tbl_uvregression")

  tb <- rr_outcome$table_body

  # ---- Expected counts directly from arr_rr ----
  expected_cases_exposed <- sum(arr_rr["TRUE", "TRUE", ])
  expected_total_exposed <- sum(arr_rr[, "TRUE", ])
  expected_cases_unexposed <- sum(arr_rr["TRUE", "FALSE", ])
  expected_total_unexposed <- sum(arr_rr[, "FALSE", ])

  result_row <- tb[!tb$reference_row & !is.na(tb$reference_row), ]
  reference_row <- tb[tb$reference_row & !is.na(tb$reference_row), ]

  expect_equal(unname(result_row$n_event), expected_cases_exposed)
  expect_equal(unname(result_row$n_obs), expected_total_exposed)
  expect_equal(unname(reference_row$n_event), expected_cases_unexposed)
  expect_equal(unname(reference_row$n_obs), expected_total_unexposed)
})

test_that("add_crosstabs calculates correct counts for each exposure level", {

  tb <- rr_outcome$table_body

  expected_cases_exposed <- sum(arr_rr["TRUE", "TRUE", ])
  expected_total_exposed <- sum(arr_rr[, "TRUE", ])
  expected_cases_unexposed <- sum(arr_rr["TRUE", "FALSE", ])
  expected_total_unexposed <- sum(arr_rr[, "FALSE", ])

  exposed_row <- tb[!tb$reference_row & !is.na(tb$reference_row), ]
  unexposed_row <- tb[tb$reference_row & !is.na(tb$reference_row), ]

  expect_equal(unname(exposed_row$n_event), expected_cases_exposed)
  expect_equal(unname(exposed_row$n_obs), expected_total_exposed)
  expect_equal(unname(unexposed_row$n_event), expected_cases_unexposed)
  expect_equal(unname(unexposed_row$n_obs), expected_total_unexposed)
})

test_that("add_crosstabs works with tbl_cmh for stratified analysis", {

  expect_s3_class(rr_cmh_outcome, "gtsummary")
  expect_s3_class(rr_cmh_outcome, "tbl_cmh")

  tb <- rr_cmh_outcome$table_body |>
    tidyr::fill(stratifier)

  # ---- Stratum-specific counts from arr_rr ----
  # age = FALSE stratum
  stratum1_cases_exposed <- arr_rr["TRUE", "TRUE", "FALSE"]
  stratum1_total_exposed <- sum(arr_rr[, "TRUE", "FALSE"])
  # age = TRUE stratum
  stratum2_cases_exposed <- arr_rr["TRUE", "TRUE", "TRUE"]
  stratum2_total_exposed <- sum(arr_rr[, "TRUE", "TRUE"])

  stratum1 <- tb[tb$stratifier == FALSE &
                   !tb$reference_row & !is.na(tb$reference_row), ]
  stratum2 <- tb[tb$stratifier == TRUE &
                   !tb$reference_row & !is.na(tb$reference_row), ]

  expect_equal(unname(stratum1$n_event), stratum1_cases_exposed)
  expect_equal(unname(stratum1$n_obs), stratum1_total_exposed)
  expect_equal(unname(stratum2$n_event), stratum2_cases_exposed)
  expect_equal(unname(stratum2$n_obs), stratum2_total_exposed)
})

test_that("add_crosstabs header labels are correct for risk ratios", {

  headers <- rr_outcome$table_styling$header
  n_event_header <- headers[headers$column == "n_event", "label"]
  n_obs_header <- headers[headers$column == "n_obs", "label"]

  expect_equal(dplyr::pull(n_event_header), "**Cases exposed (n)**")
  expect_equal(dplyr::pull(n_obs_header), "**Total exposed (N)**")
})

test_that("add_crosstabs works in wide format for dichotomous variables", {

  tb <- rr_outcome_wide$table_body

  expect_true(all(c("n_event_FALSE", "n_event_TRUE",
                    "n_obs_FALSE", "n_obs_TRUE") %in% names(tb)))

  expect_equal(nrow(tb), 1)

  # ---- Expected counts directly from arr_rr ----
  expected_cases_exposed <- sum(arr_rr["TRUE", "TRUE", ])
  expected_total_exposed <- sum(arr_rr[, "TRUE", ])
  expected_cases_unexposed <- sum(arr_rr["TRUE", "FALSE", ])
  expected_total_unexposed <- sum(arr_rr[, "FALSE", ])

  expect_equal(unname(tb$n_event_FALSE), expected_cases_exposed)
  expect_equal(unname(tb$n_obs_FALSE), expected_total_exposed)
  expect_equal(unname(tb$n_event_TRUE), expected_cases_unexposed)
  expect_equal(unname(tb$n_obs_TRUE), expected_total_unexposed)
})


test_that("add_crosstabs works with tbl_uvregression for IRR", {

  expect_s3_class(irr_outcome, "gtsummary")
  expect_s3_class(irr_outcome, "tbl_uvregression")

  tb <- irr_outcome$table_body

  expected_cases_exposed <- sum(arr_irr$deaths[arr_irr$exposure == "smoker"])
  expected_total_exposed <- sum(arr_irr$years[arr_irr$exposure == "smoker"])
  expected_cases_unexposed <- sum(arr_irr$deaths[arr_irr$exposure == "nonsmoker"])
  expected_total_unexposed <- sum(arr_irr$years[arr_irr$exposure == "nonsmoker"])

  result_row <- tb[!tb$reference_row & !is.na(tb$reference_row), ]
  reference_row <- tb[tb$reference_row & !is.na(tb$reference_row), ]

  expect_equal(unname(result_row$n_event), expected_cases_exposed)
  expect_equal(unname(result_row$exposure), expected_total_exposed)
  expect_equal(unname(reference_row$n_event), expected_cases_unexposed)
  expect_equal(unname(reference_row$exposure), expected_total_unexposed)
})

test_that("add_crosstabs calculates correct counts for each exposure level", {

  tb <- irr_outcome$table_body

  expected_cases_exposed <- sum(arr_irr$deaths[arr_irr$exposure == "smoker"])
  expected_total_exposed <- sum(arr_irr$years[arr_irr$exposure == "smoker"])
  expected_cases_unexposed <- sum(arr_irr$deaths[arr_irr$exposure == "nonsmoker"])
  expected_total_unexposed <- sum(arr_irr$years[arr_irr$exposure == "nonsmoker"])

  exposed_row <- tb[!tb$reference_row & !is.na(tb$reference_row), ]
  unexposed_row <- tb[tb$reference_row & !is.na(tb$reference_row), ]

  expect_equal(unname(exposed_row$n_event), expected_cases_exposed)
  expect_equal(unname(exposed_row$exposure), expected_total_exposed)
  expect_equal(unname(unexposed_row$n_event), expected_cases_unexposed)
  expect_equal(unname(unexposed_row$exposure), expected_total_unexposed)
})

test_that("add_crosstabs works with tbl_cmh for stratified analysis", {

  expect_s3_class(irr_cmh_outcome, "gtsummary")
  expect_s3_class(irr_cmh_outcome, "tbl_cmh")

  tb <- irr_cmh_outcome$table_body |>
    tidyr::fill(stratifier)

  # ---- Stratum-specific counts from arr_irr ----
  ages <- unique(arr_irr$age)
  for (age_group in ages) {
    stratum_cases_exposed <- sum(arr_irr$deaths[arr_irr$age == age_group & arr_irr$exposure == "smoker"])
    stratum_total_exposed <- sum(arr_irr$years[arr_irr$age == age_group & arr_irr$exposure == "smoker"])

    stratum_row <- tb[tb$stratifier == age_group & !tb$reference_row & !is.na(tb$reference_row), ]

    expect_equal(unname(stratum_row$n_event), stratum_cases_exposed)
    expect_equal(unname(stratum_row$exposure), stratum_total_exposed)
  }
})

test_that("add_crosstabs header labels are correct for IRR", {

  headers <- irr_outcome$table_styling$header
  n_event_header <- headers[headers$column == "n_event", "label"]
  exposure_header <- headers[headers$column == "exposure", "label"]

  expect_equal(dplyr::pull(n_event_header), "**Cases exposed (n)**")
  expect_equal(dplyr::pull(exposure_header), "**Total exposed (person-time)**")
})

test_that("add_crosstabs works in wide format for dichotomous variables", {

  tb <- irr_outcome_wide$table_body

  expect_true(all(c("n_event_FALSE", "n_event_TRUE",
                    "exposure_FALSE", "exposure_TRUE") %in% names(tb)))

  expect_equal(nrow(tb), 1)

  expected_cases_exposed <- sum(arr_irr$deaths[arr_irr$exposure == "smoker"])
  expected_total_exposed <- sum(arr_irr$years[arr_irr$exposure == "smoker"])
  expected_cases_unexposed <- sum(arr_irr$deaths[arr_irr$exposure == "nonsmoker"])
  expected_total_unexposed <- sum(arr_irr$years[arr_irr$exposure == "nonsmoker"])

  expect_equal(unname(tb$n_event_FALSE), expected_cases_exposed)
  expect_equal(unname(tb$exposure_FALSE), expected_total_exposed)
  expect_equal(unname(tb$n_event_TRUE), expected_cases_unexposed)
  expect_equal(unname(tb$exposure_TRUE), expected_total_unexposed)
})
