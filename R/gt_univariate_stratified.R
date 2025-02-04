#' A {gtsummary} wrapper function that takes a dataframe and produces crude,
#' stratified and Cochran-Mantel-Haenszel estimates.
#'
#' @param data A data frame
#'
#' @param case Name of a variable as your outcome of interest (e.g. illness)
#'
#' @param exposure Names of variable as exposures of interest (e.g. risk
#'   factors)
#'
#' @param strata Name of a variable to be used for stratifying
#'   results. This gives you a table of crude measure, measures for each
#'   strata and the mantel-haeszel adjusted measure for each exposure variable
#'
#' @param measure Specify what you would like to calculated, options are "OR",
#'   "RR" or "IRR". Default is "OR". If "OR or "RR" are specified then a
#'   woolf test for homogeneity p-value is produced. This tests whether there
#'   is a significant difference in the estimates between strata.
#'
#' @param obstime A numeric variable containing the observation time for each
#'   individual
#'
#' @importFrom tidyselect vars_select
#' @importFrom dplyr mutate relocate filter
#' @importFrom rlang enquo as_label
#' @importFrom tidyr drop_na
#' @importFrom gtsummary tbl_uvregression modify_table_body tbl_stack modify_header style_ratio style_pvalue
#' @importFrom stats glm
#' @importFrom MASS glm.nb
#'
#' @references Inspired by Daniel Sjoberg,
#' see [gtsummary github repo](https://github.com/ddsjoberg/gtsummary)
#'
#' @export


tbl_cmh <- function(data, case, exposure, strata, measure, obstime = NULL, conf.level = 0.95) {

  ## make variables available for use
  case_var     <- tidyselect::vars_select(colnames(data), {{ case }})
  exposure_var <- tidyselect::vars_select(colnames(data), {{ exposure }})
  strata_var   <- tidyselect::vars_select(colnames(data), {{ strata }})
  obstime_var  <- tidyselect::vars_select(colnames(data), {{ obstime }})


  ## if stratifying variable is not already a factor then make it one
  if(!is.factor(data[[strata_var]])) {
    data <- dplyr::mutate(data, {{strata}} := as.factor({{strata}}))
  }

  ## add a temporary variable with the log of obstime
  if (length(obstime_var) != 0) {
    data[["log_obs"]] <- log(data[[obstime_var]])
  }


  ## drop missings from all relevant variables
  data <- tidyr::drop_na(data,
                         {{case}},
                         {{exposure}},
                         {{strata}},
                         {{obstime}})

  ##### CRUDE ESTIMATES --------------------------------------------------------



  ## TODO: simplify these by defining the args with switch() to pass to tbl_uvreg
  if (measure == "OR") {
    crude <- gtsummary::tbl_uvregression(data = data,
                                  method = glm,
                                  y = {{case}},
                                  include = {{exposure}},
                                  method.args = list(family = binomial),
                                  exponentiate = TRUE,
                                  hide_n = TRUE)
  }

  if (measure == "RR") {
    crude <- gtsummary::tbl_uvregression(data = data,
                                  method = MASS::glm.nb,
                                  y = {{case}},
                                  include = {{exposure}},
                                  exponentiate = TRUE,
                                  hide_n = TRUE)
  }

  if (measure == "IRR") {
    crude <- gtsummary::tbl_uvregression(data = data,
                                  method = glm,
                                  y = {{case}},
                                  include = {{exposure}},
                                  method.args = list(family = poisson,
                                                     offset = log_obs),
                                  exponentiate = TRUE,
                                  hide_n = TRUE)
  }

  # edit the table body (contents of table)
  crude <- gtsummary::modify_table_body(
    crude,
    # define a function to make two steps and avoid piping
    fun = function(.x){
      # remove cases from total obs to get control counts
      .x <- dplyr::mutate(.x, stratifier = "Crude")
      # move case and control counts before chara
      .x <- dplyr::relocate(.x, stratifier, .before = variable)
    }
  )

  ## store input.args for later (this is removed when stacking)
  og_method_args <- crude$inputs$method.args


  ######## STRATIFIED ESTIMATES ------------------------------------------------

  strata_levels <- levels(data[[strata_var]])

  stratz <- purrr::map(strata_levels,
                       .f = function(i){

                         ## filter dataset to only keep strata of interest
                         strat_data <- dplyr::filter(data, {{strata}} == i)

                         ### TODO: simplify these by defining the args with switch() to pass to tbl_uvreg
                         if (measure == "OR") {
                           the_table <- gtsummary::tbl_uvregression(data = strat_data,
                                                                method = glm,
                                                                y = {{case}},
                                                                include = {{exposure}},
                                                                method.args = list(family = binomial),
                                                                exponentiate = TRUE,
                                                                hide_n = TRUE)
                         }

                         if (measure == "RR") {
                           the_table <- gtsummary::tbl_uvregression(data = strat_data,
                                                                method = MASS::glm.nb,
                                                                y = {{case}},
                                                                include = {{exposure}},
                                                                method.args = list(family = poisson),
                                                                exponentiate = TRUE,
                                                                hide_n = TRUE)
                         }

                         if (measure == "IRR") {
                           the_table <- gtsummary::tbl_uvregression(data = strat_data,
                                                                method = glm,
                                                                y = {{case}},
                                                                include = {{exposure}},
                                                                method.args = list(family = poisson,
                                                                                   offset = log_obs),
                                                                exponentiate = TRUE,
                                                                hide_n = TRUE)
                         }


                         # edit the table body (contents of table)
                         the_table <- gtsummary::modify_table_body(
                           the_table,
                           # define a function to make two steps and avoid piping
                           fun = function(.x){
                             # remove cases from total obs to get control counts
                             .x <- dplyr::mutate(.x, stratifier = i)
                             # move strata label before exposure
                             .x <- dplyr::relocate(.x, stratifier, .before = variable)
                           }
                         )

                         the_table

                       }
  )


  ######### MERGE TABLES -------------------------------------------------------

  collaps_stratz <- gtsummary::tbl_stack(stratz)

  combine_tab <- gtsummary::tbl_stack(list(crude, collaps_stratz))

  # change column names
  combine_tab <- gtsummary::modify_header(combine_tab, stratifier = "**Strata**")



  ######## MANTEL HAENSZEL ESTIMATES -------------------------------------------

  ##### prep data for mh estimates
  mh_data <- combine_tab$table_body

  mh_data <- filter(mh_data, stratifier != "Crude", !header_row)

  stratalength <- length(strata_levels)

  exposurelength <- mh_data$var_nlevels[1L]

  ##### p-value for the mental haenszel estimate
  woolf <- get_woolf_pval(mh_data, measure = measure,
                          stratalength = stratalength)


  ##### mantel haenszel estimates

  mh <- get_mh(mh_data, measure, conf.level, exposurelength, stratalength)

  ####### CLEAN UP OUTPUT TABLE ------------------------------------------------

  ### put results in the table
  combine_tab <- gtsummary::modify_table_body(
    combine_tab,
    ## define a function to make two steps and avoid piping
    fun = function(.x){
      ## remove cases from total obs to get control counts
      ## TODO: pull out the ifelse filter in to one obj
      .x <- dplyr::mutate(.x,
                          mh_estimate = ifelse(stratifier == "Crude" & !reference_row,
                                               mh$ratio, NA),
                          mh_conf.low = ifelse(stratifier == "Crude" & !reference_row,
                                               mh$lower, NA),
                          mh_conf.high = ifelse(stratifier == "Crude" & !reference_row,
                                                mh$upper, NA),
                          woolf_p.value = ifelse(stratifier == "Crude" & !reference_row,
                                                 woolf$p.value, NA),

                          ## TODO: simplify with across()

                          mh_estimate = gtsummary::style_ratio(mh_estimate),
                          mh_conf.low = gtsummary::style_ratio(mh_conf.low),
                          mh_conf.high = gtsummary::style_ratio(mh_conf.high),
                          woolf_p.value = gtsummary::style_pvalue(woolf_p.value),

                          mh_ci = ifelse(!is.na(mh_conf.low),
                                         paste0(mh_conf.low, ", ", mh_conf.high),
                                         NA),

                          stratifier = ifelse(row_type != "label", NA,
                                              stratifier)
                          )
      # move strata label before exposure
      .x <- dplyr::relocate(.x, mh_ci, .before = woolf_p.value)
    }
  )

  ## rename the columnns for mh estimates
  combine_tab <- gtsummary::modify_header(
    combine_tab,
    mh_estimate = "**CMH estimate**",
    mh_ci = "**95% CI**",
    woolf_p.value = "**p-value**"
    )

  ## add tbl_cmh to the class (so can use as catch in other functions)
  class(combine_tab) <- c("tbl_stack", "gtsummary", "tbl_cmh")

  ## reinstate method args
  combine_tab$inputs$method.args <- og_method_args

  ## return table
  combine_tab

}






#' These are internal functions written by Zhian Kamvar.
#' This was adapted from the code in the epiR::epi.2by2 version 1.0-2
#         between 20 August 2019 and 22 August 2019.
#' Many of the changes involve abstracting repetative routines into functions,
#' splitting nested calculations into separate variables, and modifying
#' variable names.

# Mantel-Haenszel tests (no p-values)

get_mh <- function(arr, measure = "OR", conf = 0.95,
                   exposurelength = NULL, stratalength = NULL) {
  switch(measure,
         OR  = mh_or(arr, conf, exposurelength, stratalength),
         RR  = mh_rr(arr, conf),
         IRR = mh_irr(arr, conf)
  )
}


# The M-H statistic for odds ratios already exist in R, so it's just a matter of
# formatting data input and then pulling out the correct values
mh_or <- function(arr, conf = 0.95,
                  exposurelength = exposurelength, stratalength = stratalength) {

  ## need to flip because need non-reference row on top
  arr <- arrange(arr, tbl_id2, reference_row)
  arr <- mutate(arr, n_nonevent = n_obs - n_event)
  arr <- select(arr, n_event, n_nonevent)

  wtf <- aperm(
    array(
      t(as.matrix(arr)),
      c(2,exposurelength, stratalength)),
    c(2,1,3))

  MH <- stats::mantelhaen.test(wtf,  conf.level = conf, exact = TRUE)

  data.frame(ratio = MH$estimate, lower = MH$conf.int[1], upper = MH$conf.int[2])

}


# These functions are adapted from epiR::epi.2by2 lines 1185--1204
#
# Many of the changes involve abstracting repetative routines into functions,
# splitting nested calculations into separate variables, and modifying
# variable names.
mh_rr <- function(arr, conf.level = 0.95) {
  z <- get_z(conf.level)

  ## Summary incidence risk ratio (Rothman 2002 p 148 and 152, equation 8-2):
  A <- arr$n_event[!arr$reference_row]
  C <- arr$n_event[arr$reference_row]
  total_cases <- A + C
  total_exposed <- arr$n_obs[!arr$reference_row]
  total_unexposed <- arr$n_obs[arr$reference_row]
  N <- total_exposed + total_unexposed
  A_TU <- A * total_unexposed
  C_TE <- C * total_exposed

  MH_risk_ratio <- sum(A_TU / N) / sum(C_TE / N)

  total_prod <- total_cases * total_exposed * total_unexposed
  var_numerator <- sum((total_prod / N^2) - ((A * C) / N))
  var_denominator <- sum(A_TU / N) * sum(C_TE / N)

  MH_risk_ratio_var <- var_numerator / var_denominator

  se_limits <- get_ci_from_var(log(MH_risk_ratio), MH_risk_ratio_var, z)

  data.frame(ratio = MH_risk_ratio, lower = se_limits[["ll"]], upper = se_limits[["ul"]])
}


mh_irr <- function(arr, conf.level = 0.95) {

  ## Summary incidence rate ratio (Rothman 2002 p 153, equation 8-5):
  A <- arr$n_event[!arr$reference_row]
  B <- arr$exposure[!arr$reference_row]
  C <- arr$n_event[arr$reference_row]
  D <- arr$exposure[arr$reference_row]
  cases <- A + C # M1

  person_time <- B + D # M0

  numerator <- sum((A * D) / person_time)
  denominator <- sum((C * B) / person_time)
  MH_IRR <- numerator / denominator

  MH_IRR_var <- sum((cases * B * D) / (person_time^2)) / (numerator * denominator)

  se_limits <- get_ci_from_var(log(MH_IRR), MH_IRR_var, get_z(conf.level))

  data.frame(ratio = MH_IRR, lower = se_limits[["ll"]], upper = se_limits[["ul"]])
}


get_z <- function(conf.level) {
  alpha <- 1 - ((1 - conf.level) / 2)
  z <- stats::qnorm(alpha, mean = 0, sd = 1)
  z
}

get_ci_from_var <- function(log_ratio, log_ratio_var, z) {
  log_ratio_se <- sqrt(log_ratio_var)
  lower_limit <- exp(log_ratio - (z * log_ratio_se))
  upper_limit <- exp(log_ratio + (z * log_ratio_se))

  data.frame(se = log_ratio_se, ll = lower_limit, ul = upper_limit)
}





#### wolf pval

## this needs to be fed littler the dataframe
get_woolf_pval <- function(arr, measure = "OR", stratalength = stratalength) {

  nstrata <- stratalength

  if (measure == "OR") {

    ## this line is duplicated in mh_or()
    arr <- mutate(arr, n_nonevent = n_obs - n_event)

    ## define cases and controls for OR
    A <- arr$n_event[!arr$reference_row]
    B <- arr$n_nonevent[!arr$reference_row]
    C <- arr$n_event[arr$reference_row]
    D <- arr$n_nonevent[arr$reference_row]

    exposed <- A + B
    unexposed <- C + D
  }
  else {
    ## define cases and controls for RR
    A <- arr$n_event[!arr$reference_row]
    B <- arr$n_obs[!arr$reference_row] - arr$n_event[!arr$reference_row]
    C <- arr$n_event[arr$reference_row]
    D <- arr$n_obs[arr$reference_row] - arr$n_event[arr$reference_row]

    exposed <- arr$exposure[!arr$reference_row]
    unexposed <- arr$exposure[arr$reference_row]
  }


  # This is taken from lines 1320--1340 of epiR::epi.2by2
  #
  # Many of the changes involve abstracting repetative routines into functions,
  # splitting nested calculations into separate variables, and modifying
  # variable names.
  if (measure == "RR") {
    ## Test of homogeneity of risk ratios (Jewell 2004, page 154). First work
    ## out the Woolf estimate of the adjusted risk ratio (labelled adj_log_est
    ## here) based on Jewell (2004, page 134):

    log_est         <- log((A / exposed) / (C / unexposed))
    log_est_var     <- (B / (A * exposed)) + (D / (C * unexposed))

  } else {

    ## Test of homogeneity of odds ratios (Jewell 2004, page 154). First work
    ## out the Woolf estimate of the adjusted odds ratio (labelled adj_log_est
    ## here) based on Jewell (2004, page 129):
    log_est     <- log(((A + 0.5) * (D + 0.5)) / ((B + 0.5) * (C + 0.5)))
    log_est_var <- (1 / (A + 0.5)) + (1 / (B + 0.5)) + (1 / (C + 0.5)) + (1 / (D + 0.5))
  }

  inv_log_est_var <- 1 / log_est_var
  adj_log_est <- sum(inv_log_est_var * log_est) / sum(inv_log_est_var)

  ## Equation 10.3 from Jewell (2004):
  res <- sum(inv_log_est_var * (log_est - adj_log_est)^2)
  p_value <- 1 - stats::pchisq(res, df = nstrata - 1)

  data.frame(statistic = res, df = nstrata - 1, p.value = p_value)
}
