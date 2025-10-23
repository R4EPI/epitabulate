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
                                          hide_n = TRUE) |>
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
                                          hide_n = TRUE) |>
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
                                      hide_n = TRUE) |>
  add_crosstabs()

# stratified
# strata confidence intervals based on profile likelihood from GLM
# mantel haenszel CIs based on wald from rothman textbook
# ("this yields virtually the same point estimates and confidence limits for IR")
# mantel haenszel p-value is a woolf test for heterogeneity of the strata
irr_cmh_outcome <- tbl_cmh(arrt_irr, event, exposure, age,
                           obstime = years, measure = "IRR") |>
  add_crosstabs()




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


## add counts check for add_crosstabs()



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

