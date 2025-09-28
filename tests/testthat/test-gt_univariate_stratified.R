# generate a real data set from
# http://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704-ep713_confounding-em/BS704-EP713_Confounding-EM7.html
# https://statsvenu.com/cochran-mantel-haenszel-statistics/

## TODO consider swapping to this
# https://online.stat.psu.edu/stat504/lesson/5/5.3/5.3.5


arr <- array(
  data = c(10, 35, 90, 465, 36, 25, 164, 175),
  dim  = c(2 , 2 , 2),
  dimnames = list(
    risk    = c(TRUE , FALSE),
    outcome = c(TRUE , FALSE),
    old     = c(FALSE, TRUE)
  )
)

iarr <- arr
# testing incidence rate
iarr[, 2, ] <- c(2, 10, 4, 4) * 100 # equivalent of a person time column of 2

# crude, strata 1, strata 2
or_expect <- data.frame(
  ratio = c(1.93175853018373, 1.47619047619048, 1.53658536585366),
  lower = c(1.28113688091955, 0.705624518294847, 0.883931958442549),
  upper = c(2.91279649701376, 3.08824065136142, 2.67112707488908)
)

rr_expect <- data.frame(
  ratio = c(1.78888888888889, 1.42857142857143, 1.44),
  lower = c(1.24868722508264, 0.731610579575668, 0.898998784981615),
  upper = c(2.56279025884831, 2.78948443817513, 2.30656596498337)
)

irr_expect <- data.frame(
  ratio = c(1.78888888888889, 1.42857142857143, 1.44, 1.43636363636364),
  lower = c(1.19120007668515, 0.630921934944737, 0.840862493415672,
            0.949805681292558),
  upper = c(2.67098559419565, 2.94770477147601, 2.50257320594582,
            2.17217114669193)
)


MH_RR  <- data.frame(est = 1.43636363636364, lower = 0.97698703277564,  upper = 2.11173784979146)
MH_OR  <- data.frame(est = 1.51612903225806, lower = 0.973921554763186, upper = 2.3601975243424)
MH_IRR <- irr_expect[4, , drop = FALSE]

RR_woolf <- data.frame(
  test.statistic = 0.000364164132934994,
  df = 1,
  p.value = 0.984774825192526
)
OR_woolf <- data.frame(
  test.statistic = c(6.56694874232559e-05),
  df = c(1),
  p.value = c(0.993534276605677)
)

the_pval <- c(0.00145682488649103, 0.298455836637157, 0.126045957638772)

arrt <- as.data.frame.table(arr) %>%
  dplyr::summarise(res = list(data.frame(
    risk    = rep(risk, Freq),
    outcome = rep(outcome, Freq),
    old = rep(old, Freq)
  ))) %>%
  tidyr::unnest(cols = c(res)) %>%
  lapply(as.logical) %>%
  tibble::as_tibble()

arrt$pt <- 2 # person time



# ODDS ratios
# crude
or_outcome <- gtsummary::tbl_uvregression(arrt,
                                           method = glm,
                                           y = outcome,
                                           include = risk,
                                           method.args = list(family = binomial),
                                           exponentiate = TRUE,
                                           hide_n = TRUE) |>
  add_crosstabs()

# stratified
or_cmh_outcome <- tbl_cmh(arrt, outcome, risk, old, measure = "OR") |>
  add_crosstabs()


# RISK ratios
# crude
rr_outcome <- gtsummary::tbl_uvregression(arrt,
                                          method = MASS::glm.nb,
                                          y = outcome,
                                          include = risk,
                                          exponentiate = TRUE,
                                          hide_n = TRUE) |>
  add_crosstabs()


# stratified
rr_cmh_outcome <- tbl_cmh(arrt, outcome, risk, old, measure = "RR") |>
  add_crosstabs()


# INCIDENCE RATE ratios
# crude
irr_outcome <- gtsummary::tbl_uvregression(arrt,
                                      method = glm,
                                      y = outcome,
                                      include = risk,
                                      method.args = list(family = poisson,
                                                         offset = log(pt)),
                                      exponentiate = TRUE,
                                      hide_n = TRUE) |>
  add_crosstabs()

# stratified
irr_cmh_outcome <- tbl_cmh(arrt, outcome, risk, old, obstime = pt, measure = "IRR")



test_that("OR estimates correct", {
  ## estimates
  ## crude
  expect_equal(or_expect$ratio[1], or_cmh_outcome$table_body$estimate[3])
  ## strata
  expect_equal(or_expect$ratio[2], or_cmh_outcome$table_body$estimate[6])
  expect_equal(or_expect$ratio[3], or_cmh_outcome$table_body$estimate[9])
  ## mh
  expect_equal(MH_OR$est, as.numeric(or_cmh_outcome$table_body$mh_estimate[3]))
})

test_that("OR CIs are correct", {
  ## CI
  ## crude
  expect_equal(or_expect$lower[1], or_cmh_outcome$table_body$conf.low[3])
  expect_equal(or_expect$upper[1], or_cmh_outcome$table_body$conf.high[3])
  ## strata
  expect_equal(or_expect$lower[2], or_cmh_outcome$table_body$conf.low[6])
  expect_equal(or_expect$upper[2], or_cmh_outcome$table_body$conf.high[6])
  expect_equal(or_expect$lower[3], or_cmh_outcome$table_body$conf.low[9])
  expect_equal(or_expect$upper[3], or_cmh_outcome$table_body$conf.high[9])
  ## mh
  expect_equal(MH_OR$lower, as.numeric(or_cmh_outcome$table_body$mh_conf.low[3]))
  expect_equal(MH_OR$upper, as.numeric(or_cmh_outcome$table_body$mh_conf.high[3]))
})
