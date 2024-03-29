# generate a real data set from
# http://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704-ep713_confounding-em/BS704-EP713_Confounding-EM7.html
context("univariate tests")

# {{{
`%>%` <- dplyr::`%>%`
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

get_utab <- function(dat, what, ...) {
  tab_univariate(dat,
    "outcome",
    "risk",
    strata = "old",
    measure = what,
    ...
  )
}

get_missing_list <- function(dat, index = 1, what) {
  miss <- dat[-index, ]
  risk <- out <- old <- dat
  risk$risk[index]   <- NA
  out$outcome[index] <- NA
  old$old[index]     <- NA
  pt_exists          <- !is.null(dat$pt)
  if (pt_exists) {
    pt             <- "pt"
    ptim           <- dat
    ptim$pt[index] <- NA
    persna         <- get_utab(ptim, what = what, woolf_test = TRUE, perstime = pt)
  } else {
    pt     <- NULL
    persna <- NULL
  }
  res <- list(
    missna = get_utab(miss, what = what, woolf_test = TRUE, perstime = pt),
    riskna = get_utab(risk, what = what, woolf_test = TRUE, perstime = pt),
    outna  = get_utab(out,  what = what, woolf_test = TRUE, perstime = pt),
    oldna  = get_utab(old,  what = what, woolf_test = TRUE, perstime = pt),
    persna = persna
  )
  return(res)
}

expect_missing_equal <- function(misslist, IRR = FALSE) {
  expect_identical(misslist$missna, misslist$riskna)
  expect_identical(misslist$riskna, misslist$oldna)
  expect_identical(misslist$riskna, misslist$outna)
  if (IRR) expect_identical(misslist$riskna, misslist$persna)
  expect_identical(misslist$riskna$est_type,
    c("crude", "old: TRUE", "old: FALSE", "MH", if (IRR) NULL else "woolf")
  )
}

# }}}

# Errors -----------------------------------------------------------------------
test_that("tab_univariate requires a data frame", {
  expect_error(tab_univariate(arr, some, vars), "x must be a data frame")
})

test_that("Exposure variables must be logical", {
  itest <- iris %>%
    dplyr::mutate(
      sl = Sepal.Length > 6,
      pl = Petal.Length > 4,
      ve = Species == "versicolor"
   )

  expect_error(tab_univariate(itest, Species, sl, strata = pl),
               "outcome must be a TRUE/FALSE variable")

  expect_error(tab_univariate(itest, ve, sl, strata = Petal.Length),
               "strata variable must be a TRUE/FALSE variable")

  expect_error(tab_univariate(itest, ve, sl, Petal.Length),
               "exposure variables must be TRUE/FALSE variables, but Petal.Length is a numeric.")

  expect_error(tab_univariate(itest, ve, sl, strata = pl, measure = "IRR"),
    "You have selected IRR as a measure but not specified a perstime variable."
  )

})

# Internal Estimators ----------------------------------------------------------
test_that("internal estimate functions works", {

  expect_equivalent(get_ratio_est(arr, "OR")[1:3, 1:3],  (or_expect))
  expect_equivalent(get_ratio_est(arr, "RR")[1:3, 1:3],  (rr_expect))
  expect_equivalent(get_ratio_est(iarr, "IRR")[1:4, 1:3], (irr_expect))

})

test_that("MH estimate works" , {

  expect_equivalent(mh_rr(arr),  MH_RR)
  expect_equivalent(mh_or(arr),  MH_OR)
  expect_equivalent(mh_irr(iarr), MH_IRR)

})

test_that("woolf p-values work", {

  expect_equivalent(get_woolf_pval(arr, "RR"), RR_woolf)
  expect_equivalent(get_woolf_pval(arr, "OR"), OR_woolf)

})


# Full tables ------------------------------------------------------------------


OR_strat_woolf <- get_utab(arrt, what = "OR", woolf_test = TRUE)
OR_strata <- get_utab(arrt, what = "OR", extend_output = FALSE, mergeCI = TRUE)
OR_simple <- tab_univariate(arrt, outcome, risk, measure = "OR", extend_output = FALSE, mergeCI = TRUE)
OR_names <- c("variable", "est_type", "exp_cases", "unexp_cases", "exp_controls", "unexp_controls", "est_ci", "p.value")

RR_strat_woolf <- get_utab(arrt, what = "RR", woolf_test = TRUE)
RR_strata <- get_utab(arrt, what = "RR", extend_output = FALSE, mergeCI = TRUE)
RR_simple <- tab_univariate(arrt, outcome, risk, measure = "RR", extend_output = FALSE, mergeCI = TRUE)
RR_names <- c("variable", "est_type", "exp_cases", "exp_total", "unexp_cases", "unexp_total", "est_ci", "p.value")

IRR_strat_woolf <- get_utab(arrt, what = "IRR", perstime = "pt", woolf_test = TRUE)
IRR_strata <- get_utab(arrt, what = "IRR", perstime = "pt", extend_output = FALSE, mergeCI = TRUE)
IRR_simple <- tab_univariate(arrt, outcome, risk, perstime = pt, measure = "IRR", extend_output = FALSE, mergeCI = TRUE)
IRR_names <- c("variable", "est_type", "exp_cases", "exp_perstime", "unexp_cases", "unexp_perstime", "est_ci", "p.value")


test_that("tab_univariate OR works with strata", {

  expect_identical(OR_strat_woolf$est_type, c("crude", "old: TRUE", "old: FALSE", "MH", "woolf"))

  # Testing cases and basic ratios
  expect_equal(OR_strat_woolf$exp_cases  , c(36 + 10, 36     , 10     , NA, NA))
  expect_equal(OR_strat_woolf$unexp_cases, c(25 + 35, 25     , 35     , NA, NA))
  expect_equal(OR_strat_woolf$cases_odds , c(46 / 60, 36 / 25, 10 / 35, NA, NA))

  # Testing controls and basic ratios
  expect_equal(OR_strat_woolf$exp_controls  , c(164 +  90, 164      , 90      , NA, NA))
  expect_equal(OR_strat_woolf$unexp_controls, c(175 + 465, 175      , 465     , NA, NA))
  expect_equal(OR_strat_woolf$controls_odds , c(254 / 640, 164 / 175, 90 / 465, NA, NA))

  # Testing odds ratios
  expected <- or_expect[c(1, 3, 2), ] # correct for alphabetical sorting
  this_pval <- the_pval[c(1, 3, 2)]

  expect_equal(OR_strat_woolf$ratio  , c(expected$ratio, MH_OR$est  , NA))
  expect_equal(OR_strat_woolf$lower  , c(expected$lower, MH_OR$lower, NA))
  expect_equal(OR_strat_woolf$upper  , c(expected$upper, MH_OR$upper, NA))
  expect_equal(OR_strat_woolf$p.value, c(this_pval     , NA         , OR_woolf$p.value))


})


test_that("tab_univariate RR works with strata", {

  expect_identical(RR_strat_woolf$est_type, c("crude", "old: TRUE", "old: FALSE", "MH", "woolf"))

  # Testing cases and basic ratios
  expect_equal(RR_strat_woolf$exp_cases, c(36 + 10           , 36      , 10      , NA, NA))
  expect_equal(RR_strat_woolf$exp_total, c(36 + 10 + 90 + 164, 36 + 164, 10 + 90 , NA, NA))
  expect_equal(RR_strat_woolf$exp_risk , c(46 / 300          , 36 / 200, 10 / 100, NA, NA) * 100)

  # Testing controls and basic ratios
  expect_equal(RR_strat_woolf$unexp_cases, c(25 + 35            , 25      , 35      , NA, NA))
  expect_equal(RR_strat_woolf$unexp_total, c(25 + 35 + 175 + 465, 25 + 175, 35 + 465, NA, NA))
  expect_equal(RR_strat_woolf$unexp_risk , c(60 / 700           , 25 / 200, 35 / 500, NA, NA) * 100)

  # Testing odds ratios
  expected <- rr_expect[c(1, 3, 2), ] # correct for alphabetical sorting
  this_pval <- the_pval[c(1, 3, 2)]

  expect_equal(RR_strat_woolf$ratio  , c(expected$ratio, MH_RR$est  , NA))
  expect_equal(RR_strat_woolf$lower  , c(expected$lower, MH_RR$lower, NA))
  expect_equal(RR_strat_woolf$upper  , c(expected$upper, MH_RR$upper, NA))
  expect_equal(RR_strat_woolf$p.value, c(this_pval     , NA         , RR_woolf$p.value))


})


test_that("tab_univariate works with IRR strata", {

  expect_equal(IRR_strat_woolf$exp_cases      , c(36 + 10  , 36      , 10       , NA))
  expect_equal(IRR_strat_woolf$exp_perstime   , c(600      , 400     , 200      , NA))
  expect_equal(IRR_strat_woolf$exp_incidence  , c(46 / 600 , 36 / 400, 10 / 200 , NA) * 100)

  expect_equal(IRR_strat_woolf$unexp_cases    , c(25 + 35  , 25      , 35       , NA))
  expect_equal(IRR_strat_woolf$unexp_perstime , c(1400     , 400     , 1000     , NA))
  expect_equal(IRR_strat_woolf$unexp_incidence, c(60 / 1400, 25 / 400, 35 / 1000, NA) * 100)

  expected <- irr_expect[c(1, 3, 2, 4), ]
  expect_equal(IRR_strat_woolf$ratio, expected$ratio)
  expect_equal(IRR_strat_woolf$lower, expected$lower)
  expect_equal(IRR_strat_woolf$upper, expected$upper)

})

test_that("setting extend_output to FALSE will remove only the estimates", {

  expect_named(RR_strata, RR_names)
  expect_identical(RR_strata[1:6], RR_strat_woolf[-5, RR_names[1:6]])
  expect_named(OR_strata, OR_names)
  expect_identical(OR_strata[1:6], OR_strat_woolf[-5, OR_names[1:6]])
  expect_named(IRR_strata, IRR_names)
  expect_identical(IRR_strata[1:6], IRR_strat_woolf[-5, IRR_names[1:6]])

  expect_named(RR_simple, RR_names)
  expect_identical(RR_simple, RR_strata[1, ])
  expect_named(OR_simple, OR_names)
  expect_identical(OR_simple, OR_strata[1, ])
  expect_named(IRR_simple, IRR_names)
  expect_identical(IRR_simple, IRR_strata[1, ])

})

context("Missing data in ratios")


test_that("tab_univariate OR works with strata with missing data", {

  # adding missing data into the first column effectively removes the first row
  # Number of exposed cases and old = FALSE decreases
  na_exp_case <- get_missing_list(arrt, index = 1, what = "OR")
  # Number of unexposed controls and old = TRUE decreases
  na_unexp_control <- get_missing_list(arrt, index = nrow(arrt), what = "OR")

  expect_missing_equal(na_exp_case)
  expect_missing_equal(na_unexp_control)

  riskna <- na_exp_case$riskna
  # Testing cases and basic ratios
  expect_equal(riskna$exp_cases  , c(36 +  9, 36     ,  9     , NA, NA))
  expect_equal(riskna$unexp_cases, c(25 + 35, 25     , 35     , NA, NA))
  expect_equal(riskna$cases_odds , c(45 / 60, 36 / 25,  9 / 35, NA, NA))

  # Testing controls and basic ratios
  expect_equal(riskna$exp_controls  , c(164 +  90, 164      , 90      , NA, NA))
  expect_equal(riskna$unexp_controls, c(175 + 465, 175      , 465     , NA, NA))
  expect_equal(riskna$controls_odds , c(254 / 640, 164 / 175, 90 / 465, NA, NA))

  # Testing odds ratios
  expected <- or_expect[c(1, 3, 2), ] # correct for alphabetical sorting
  this_pval <- the_pval[c(1, 3, 2)]

  expect_true(all(riskna$ratio  <= c(expected$ratio, MH_OR$est  , NA), na.rm = TRUE))

  riskna <- na_unexp_control$riskna
  # Testing cases and basic ratios
  expect_equal(riskna$exp_cases  , c(36 + 10, 36     , 10     , NA, NA))
  expect_equal(riskna$unexp_cases, c(25 + 35, 25     , 35     , NA, NA))
  expect_equal(riskna$cases_odds , c(46 / 60, 36 / 25, 10 / 35, NA, NA))

  # Testing controls and basic ratios
  expect_equal(riskna$exp_controls  , c(164 +  90, 164      , 90      , NA, NA))
  expect_equal(riskna$unexp_controls, c(174 + 465, 174      , 465     , NA, NA))
  expect_equal(riskna$controls_odds , c(254 / 639, 164 / 174, 90 / 465, NA, NA))

  # Testing odds ratios
  expected <- or_expect[c(1, 3, 2), ] # correct for alphabetical sorting
  this_pval <- the_pval[c(1, 3, 2)]

  expect_true(all(riskna$ratio  <= c(expected$ratio, MH_OR$est  , NA), na.rm = TRUE))

})

test_that("tab_univariate RR works with strata with missing data", {

  # adding missing data into the first column effectively removes the first row
  # Number of exposed cases and old = FALSE decreases
  na_exp_case      <- get_missing_list(arrt, index = 1, what = "RR")
  # Number of unexposed controls and old = TRUE decreases
  na_unexp_control <- get_missing_list(arrt, index = nrow(arrt), what = "RR")

  expect_missing_equal(na_exp_case)
  expect_missing_equal(na_unexp_control)

  riskna <- na_exp_case$riskna

  # Testing cases and basic ratios
  expect_equal(riskna$exp_cases, c(36 +  9           , 36      ,  9      , NA, NA))
  expect_equal(riskna$exp_total, c(36 +  9 + 90 + 164, 36 + 164,  9 + 90 , NA, NA))
  expect_equal(riskna$exp_risk , c(45 / 299          , 36 / 200,  9 /  99, NA, NA) * 100)

  # Testing controls and basic ratios
  expect_equal(riskna$unexp_cases, c(25 + 35            , 25      , 35      , NA, NA))
  expect_equal(riskna$unexp_total, c(25 + 35 + 175 + 465, 25 + 175, 35 + 465, NA, NA))
  expect_equal(riskna$unexp_risk , c(60 / 700           , 25 / 200, 35 / 500, NA, NA) * 100)

  # Testing odds ratios
  expected <- rr_expect[c(1, 3, 2), ] # correct for alphabetical sorting
  this_pval <- the_pval[c(1, 3, 2)]

  expect_true(all(riskna$ratio <= c(expected$ratio, MH_RR$est  , NA), na.rm = TRUE))

  riskna <- na_unexp_control$riskna

  # Testing cases and basic ratios
  expect_equal(riskna$exp_cases, c(36 + 10           , 36      , 10      , NA, NA))
  expect_equal(riskna$exp_total, c(36 + 10 + 90 + 164, 36 + 164, 10 + 90 , NA, NA))
  expect_equal(riskna$exp_risk , c(46 / 300          , 36 / 200, 10 / 100, NA, NA) * 100)

  # Testing controls and basic ratios
  expect_equal(riskna$unexp_cases, c(25 + 35            , 25      , 35      , NA, NA))
  expect_equal(riskna$unexp_total, c(25 + 35 + 174 + 465, 25 + 174, 35 + 465, NA, NA))
  expect_equal(riskna$unexp_risk , c(60 / 699           , 25 / 199, 35 / 500, NA, NA) * 100)

  expect_true(all(riskna$ratio <= c(expected$ratio, MH_RR$est  , NA), na.rm = TRUE))
})

test_that("tab_univariate works with IRR strata with missing data", {

  # adding missing data into the first column effectively removes the first row
  # Number of exposed cases and old = FALSE decreases
  na_exp_case      <- get_missing_list(arrt, index = 1, what = "IRR")
  # Number of unexposed controls and old = TRUE decreases
  na_unexp_control <- get_missing_list(arrt, index = nrow(arrt), what = "IRR")

  expect_missing_equal(na_exp_case, IRR = TRUE)
  expect_missing_equal(na_unexp_control, IRR = TRUE)

  missna <- na_exp_case$missna

  expect_equal(missna$exp_cases      , c(36 +  9  , 36      ,  9       , NA))
  expect_equal(missna$exp_perstime   , c(598      , 400     , 198      , NA))
  expect_equal(missna$exp_incidence  , c(45 / 598 , 36 / 400,  9 / 198 , NA) * 100)

  expect_equal(missna$unexp_cases    , c(25 + 35  , 25      , 35       , NA))
  expect_equal(missna$unexp_perstime , c(1400     , 400     , 1000     , NA))
  expect_equal(missna$unexp_incidence, c(60 / 1400, 25 / 400, 35 / 1000, NA) * 100)

  expected <- irr_expect[c(1, 3, 2, 4), ]
  expect_true(all(missna$ratio <= expected$ratio))

  missna <- na_unexp_control$missna

  expect_equal(missna$exp_cases      , c(36 + 10  , 36      , 10       , NA))
  expect_equal(missna$exp_perstime   , c(600      , 400     , 200      , NA))
  expect_equal(missna$exp_incidence  , c(46 / 600 , 36 / 400, 10 / 200 , NA) * 100)

  expect_equal(missna$unexp_cases    , c(25 + 35  , 25      , 35       , NA))
  expect_equal(missna$unexp_perstime , c(1398     , 398     , 1000     , NA))
  expect_equal(missna$unexp_incidence, c(60 / 1398, 25 / 398, 35 / 1000, NA) * 100)

  expected <- irr_expect[c(1, 3, 2, 4), ]
  expect_true(all(missna$ratio <= expected$ratio))

})
