# gt_wrapper internal functions (not exported)

#' A case fatality rate wrapper function to be passed to the gtsummary::add_stat
#' function, which returns a data frame with a single row to be used with
#' dichotomous data or overall data.Calls epitabulate::case_fatality_rate_df.
#'
#' @param data A data frame, passed by the gtsummary::add_stat function.
#'
#' @param variable Name of a variable as the outcome of interest, passed by the
#' gtsummary::add_stat function (e.g. illness).
#'
#' @param by Name of a variable for stratifying, passed by the gtsummary::add_stat function
#'   (eov.g. illness).
#'
#' @param deaths_var the name of a logical column in the data that indicates that the case died,
#' is passed as the first argument to `epitabulate::case_fatality_rate_df`
#'
#' @param ... additional params that may be passed from gtsummary functions.
#'
#' @return a single row gtsummary object with case fatality rate results for
#' deaths, cases, cfr, and 95% confidence interval.
#'
#' @rdname gtsummary_wrappers
#' @import dplyr
#' @importFrom epikit merge_ci_df
#'
#'
#'
add_gt_cfr_stat_label  <- function(data, variable, by, deaths_var, ...) {

  # Declare local variables for CMD check
  deaths <- population <- cfr <- ci <- NULL
  if(!is.null(by)) {
    warning("cfr by strata is not currently available, ignoring `by` argument")
  }

  stat_new <- data %>%
    epitabulate::case_fatality_rate_df(
      deaths = data[[deaths_var]],
      mergeCI = TRUE) %>%
    dplyr::mutate(deaths = formatC(deaths, digits = 0, format = "f")) %>%
    dplyr::mutate(cfr = formatC(cfr, digits = 2, format = "f")) %>%
    dplyr::rename("Deaths" = deaths,
           # "Cases" = population,
           "CFR (%)" = cfr,
           "95%CI" = ci) %>%
    dplyr::select(-population)
}


#' A case fatality rate wrapper function to be passed to the gtsummary::add_stat function,
#' which returns a data frame with multiple rows to be used when location is set
#' to "level" for multi-level categorical data. Calls epitabulate::case_fatality_rate_df.
#'
#' @param data A data frame, passed by the gtsummary::add_stat function.
#'
#' @param variable Name of a variable as the outcome of interest, passed by the
#' gtsummary::add_stat function (e.g. illness).
#'
#' @param by Name of a variable for stratifying, passed by the gtsummary::add_stat
#' function (e.g. illness).
#'
#' @param ... additional params that may be passed from gtsummary functions.
#'
#' @return a single row or multiple row gtsummary object with with case fatality
#' rate results for deaths, cases, cfr, and 95% confidence interval.
#'
#' @rdname gtsummary_wrappers
#'
add_gt_cfr_stat_level <- function(data, variable, by, deaths_var, ...) {
  # Declare local variables for CMD check

  if(variable != deaths_var) {
    stat_0 <- deaths <- population <- cfr <- ci <- NULL
    if(!is.null(by)) {
      warning("cfr by strata is not currently available, ignoring `by` argument")
    }

    variable_ <- variable

    tb <- list(...)$tbl
    gt_dt <- tb$table_body
    var_dt <- gt_dt %>%
      dplyr::filter(variable %in% variable & !is.na(stat_0))
    var_levels <- unique(var_dt$label)

    # create rlang::enquo objects to use in epitabulate::case_fatality_rate_df
    deaths_sym <- as.symbol(deaths_var)
    qdeaths <- rlang::enquo(deaths_sym)
    var_sym <- as.symbol(variable)
    qvariable <- rlang::enquo(var_sym)

    stat_new <- data %>%
      epitabulate::case_fatality_rate_df(deaths = !!qdeaths, group =  !!qvariable , mergeCI = TRUE) %>%
      dplyr::filter(!!qvariable %in% var_levels) %>%
      dplyr::mutate(deaths = formatC(deaths, digits = 0, format = "f")) %>%
      dplyr::mutate(cfr =  formatC(cfr, digits = 2, format = "f")) %>%
      dplyr::select(deaths, cfr, ci) %>%
      dplyr::rename("Deaths" = deaths,
                    # "Cases" = population,
                    "CFR (%)" = cfr,
                    "95%CI" = ci)
  }
}

#' An attack rate wrapper function to be passed to the gtsummary::add_stat
#' function, which returns a data frame with a single row to be used with
#' dichotomous data or overall data. Calls epitabulate::attack_rate.
#'
#' @param data A data frame, passed by the gtsummary::add_stat function.
#'
#' @param variable Name of a variable as the outcome of interest, passed by the
#' gtsummary::add_stat function (e.g. illness).
#'
#' @param by Name of a variable for stratifying, passed by the gtsummary::add_stat function
#'   (e.g. illness).
#'
#'@param multiplier The base by which to multiply the output:
# '1: multiplier = 1: ratio between 0 and 1;
# '2: multiplier = 100:proportion;
# '3: multiplier = 10^4: x per 10,000 people; passed to `epitabulate::attack_rate`
#'
#'@param drop_total whether or not to include the population column; default TRUE
#'
#' @param ... additional params that may be passed from gtsummary functions.
#'
#' @return a single row gtsummary object with with attack rate results with
#' cases, attack rate, and 95% confidence interval.
#'
#' @rdname gtsummary_wrappers

add_gt_attack_rate_stat_label <-
  function(data, variable, by=NULL, case_var, population = population,
           multiplier = 10^4, drop_total = TRUE, drop_cases = TRUE, ...) {
  # Declare local variables for CMD check

  cases <- ci <- Population <- NULL

  if(is.null(multiplier)) {
    stop("`multiplier` argument required")
  }

  if(!is.null(by)) {
    warning("attack rate by strata is not currently available, ignoring `by` argument")
  }

  if(is.null(population)) {
    population <- nrow(data)
  } else {
    population <- sum(population)
    drop_total <- FALSE
  }

  cases <- sum(data[[case_var]])

  ar_label <- paste0("AR (per ", format(multiplier, big.mark=","), ")")
  cols_rename <- setNames("ar", ar_label)
  ar <- epitabulate::attack_rate(cases = cases,
                            population = population,
                            multiplier = multiplier) %>%
    epikit::merge_ci_df(e = 3) %>%
    dplyr::mutate(cases = formatC(cases, digits = 0, format = "f")) %>%
    dplyr::mutate(ar = formatC(ar, digits = 2, format = "f")) %>%
     # merge the lower and upper CI into one column
    dplyr::rename(
      "Cases" = cases,
      "Population" = population,
      "95%CI" = ci) %>%
    dplyr::rename(dplyr::all_of(cols_rename))

  if(drop_cases){
    # can drop the population if specified (default)
    ar <- ar %>% dplyr::select(-Cases)
  }

  if(drop_total){
    # drop the population if specified (default)
    ar <- ar %>% dplyr::select(-Population)
  }

  ar
}

#' An attack rate wrapper function to be passed to the gtsummary::add_stat function,
#' which returns a data frame with multiple rows to be used when location is set
#' to "level" for multi-level categorical data. Calls epitabulate::attack_rate.
#'
#' @param data A data frame, passed by the gtsummary::add_stat function.
#'
#' @param variable Name of a variable as the outcome of interest, passed by the
#' gtsummary::add_stat function (e.g. illness)
#'
#' @param by Name of a variable for stratifying, passed by the gtsummary::add_stat
#' function (e.g. illness).
#'
#'@param multiplier The base by which to multiply the output:
# '1: multiplier = 1: ratio between 0 and 1;
# '2: multiplier = 100:proportion;
# '3: multiplier = 10^4: x per 10,000 people; passed to `epitabulate::attack_rate`
#'
#' @param ... additional params that may be passed from gtsummary functions.
#'
#' @return a single-row gtsummary object with attack rate results cases,
#' population, attack rate, and 95% confidence interval.
#'
#' @rdname gtsummary_wrappers
#'
add_gt_attack_rate_level <-
  function(data, variable, by=NULL, case_var, population = NULL,
           multiplier = 10^4, drop_total = TRUE, drop_cases = TRUE, ...) {

  # Declare local variables for CMD check
  cases <- ci <- Population <- NULL

  if (is.null(case_var) | is.null(data[[case_var]])) {
    stop("`case_var` argument is required and must be a column in the data")
  }

  data[[case_var]] <- as.logical(data[[case_var]])
  if (!is.logical(data[[case_var]]) & sum(is.na(data[[case_var]])) != nrow(data)){
    stop("`case_var` column must be logical or convertable to logical using `as.logical`")
  }

  if (is.null(multiplier)) {
    stop("`multiplier` argument required")
  }

  if (!is.null(by)) {
    warning("attack rate by strata is not currently available, ignoring `by` argument")
  }

  sym_var <- as.symbol(variable)
  sym_case <- as.symbol(case_var)
  counts <- data %>%
    # change to factor and keep all counts (including 0 counts)
    dplyr::mutate(!!case_var := factor(!!sym_case)) %>%
    dplyr::group_by(!!sym_var, !!sym_case, .drop = FALSE) %>%
    dplyr::count(name = "case_n") %>%
    dplyr::group_by(!!sym_var, .drop = FALSE) %>%
    dplyr::mutate(total = sum(case_n)) %>%
    dplyr::filter(!!sym_case == 'TRUE')

  if(is.null(population)) {
    population <- counts$total
  } else {
    drop_total <- FALSE
  }

  ar_label <- paste0("AR (per ", format(multiplier, big.mark=","), ")")
  cols_rename <- setNames("ar", ar_label)
  ar <- epitabulate::attack_rate(cases = counts$case_n,
                            population = population,
                            multiplier = multiplier) %>%
    epikit::merge_ci_df(e = 3) %>% # merge the lower and upper CI into one column
    dplyr::mutate(cases = formatC(cases, digits = 0, format = "f")) %>%
    dplyr::mutate(ar = formatC(ar, digits = 2, format = "f")) %>%
    dplyr::rename(
      "Cases" = cases,
      "Population" = population,
      "95%CI" = ci) %>%
    dplyr::rename(dplyr::all_of(cols_rename))

  if(drop_total){
    # drop the population if specified (default)
    ar <- ar %>% dplyr::select(-Population)
  }


  return(ar)
}

add_gt_mortality_rate_stat_label <-
  function(data, variable, by=NULL, deaths_var, population = NULL,
           multiplier = 10^4, drop_total = TRUE, drop_deaths = TRUE, ...) {
    # Declare local variables for CMD check

    deaths <- ci <- Population <- NULL

    if(is.null(multiplier)) {
      stop("`multiplier` argument required")
    }

    if(!is.null(by)) {
      warning("death rate by strata is not currently available, ignoring `by` argument")
    }


    if(is.null(population)) {
      # if no population argument is passed, population will be the number of rows in the data
      population <- nrow(data)
    } else {
      population <- sum(population)
      drop_total <- FALSE
      # msg <- "population value is the sum of the population provided"
      # warning(paste(msg, "\n", "Population: ", population))
    }

    deaths <- sum(data[[deaths_var]])
    mr_label <- paste0("MR (per ", format(multiplier, big.mark=","), ")")
    cols_rename <- setNames("mortality per 10 000", mr_label)
    mr <- epitabulate::mortality_rate(deaths = deaths,
                                 population = population,
                                 multiplier = multiplier) %>%
      epikit::merge_ci_df(e = 3) %>%
      dplyr::mutate(deaths = formatC(deaths, digits = 0, format = "f")) %>%
      dplyr::mutate(`mortality per 10 000` =
                      formatC(`mortality per 10 000`, digits = 2, format = "f")) %>%
      # merge the lower and upper CI into one column
      dplyr::rename(
        "Deaths" = deaths,
        "Population" = population,
        "95%CI" = ci) %>%
      dplyr::rename(dplyr::all_of(cols_rename))

    if(drop_deaths){
      # can drop the population if specified (default)
      mr <- mr %>% dplyr::select(-Deaths)
    }

    if(drop_total){
      # drop the population if specified (default)
      mr <- mr %>% dplyr::select(-Population)
    }

    mr
  }


#' A mortality rate wrapper function to be passed to the gtsummary::add_stat function,
#' which returns a data frame with multiple rows to be used when location is set
#' to "level" for multi-level categorical data. Calls epitabulate::attack_rate.
#'
#' @param data A data frame, passed by the gtsummary::add_stat function.
#'
#' @param variable Name of a variable as the outcome of interest, passed by the
#' gtsummary::add_stat function (e.g. illness)
#'
#' @param by Name of a variable for stratifying, passed by the gtsummary::add_stat
#' function (e.g. illness).
#'
#'@param multiplier The base by which to multiply the output:
# '1: multiplier = 1: ratio between 0 and 1;
# '2: multiplier = 100:proportion;
# '3: multiplier = 10^4: x per 10,000 people; passed to `epitabulate::attack_rate`
#'
#' @param ... additional params that may be passed from gtsummary functions.
#'
#' @return a single-row gtsummary object with attack rate results cases,
#' population, attack rate, and 95% confidence interval.
#'
#' @rdname gtsummary_wrappers
#'
add_gt_mortality_rate_level <- function(data,
                                        variable,
                                        by=NULL,
                                        deaths_var,
                                        population = NULL,
                                        multiplier = 10^4,
                                        drop_total = TRUE,
                                        drop_deaths = TRUE, ...) {
  # Declare local variables for CMD check
  deaths <- ci <- Population <- NULL

  if (is.null(deaths_var) | is.null(data[[deaths_var]])) {
    stop("`deaths_var` argument is required and must be a column in the data")
  }

  data[[deaths_var]] <- as.logical(data[[deaths_var]])
  if (!is.logical(data[[deaths_var]]) & sum(is.na(data[[deaths_var]])) != nrow(data)){
    stop("`deaths_var` column must be logical or convertable to logical using `as.logical`")
  }

  if (is.null(multiplier)) {
    stop("`multiplier` argument required")
  }

  if (!is.null(by)) {
    warning("mortality rate by strata is not currently available, ignoring `by` argument")
  }


  sym_var <- as.symbol(variable)
  sym_deaths <- as.symbol(deaths_var)

  counts <- data %>%
    # change to factor and keep all counts (including 0 counts)
    dplyr::mutate(!!deaths_var := factor(!!sym_deaths)) %>%
    dplyr::group_by(!!sym_var, !!sym_deaths, .drop = FALSE) %>%
    dplyr::count(name = "deaths_n") %>%
    dplyr::group_by(!!sym_var, .drop = FALSE) %>%
    dplyr::mutate(total = sum(deaths_n)) %>%
    dplyr::filter(!!sym_deaths == TRUE)

  if(is.null(population)) {
    # if no population argument is passed, population will be the number of rows in each level
    population <- counts$total
  } else {
    drop_total = FALSE
  }

  mr_label <- paste0("MR (per ", format(multiplier, big.mark=","), ")")
  cols_rename <- setNames("mortality per 10 000", mr_label)
  mr <- epitabulate::mortality_rate(deaths = counts$deaths_n,
                               population = population,
                               multiplier = multiplier) %>%
    epikit::merge_ci_df(e = 3) %>% # merge the lower and upper CI into one column
    dplyr::mutate(deaths = formatC(deaths, digits = 0, format = "f")) %>%
    dplyr::mutate(
      `mortality per 10 000` =
        formatC(`mortality per 10 000`, digits = 2, format = "f")) %>%
    dplyr::rename(
      "Deaths" = deaths,
      "Population" = population,
      "95%CI" = ci) %>%
    dplyr::rename(dplyr::all_of(cols_rename))


  if(drop_deaths){
    # can drop the population if specified (default)
    mr <- mr %>% dplyr::select(-Deaths)
  }

  if(drop_total){
    # drop the population if specified (default)
    mr <- mr %>% dplyr::select(-Population)
  }

  mr
}






gt_mh_odds_single_var <- function(data, exposure, outcome, strata,  exposure_label = NULL,
    outcome_label = NULL, how_overall = FALSE) {

      gt_obj <- data %>%
        add_crosstabs(
          exposure = exposure,
          outcome = outcome,
          exposure_label = exposure_label,
          outcome_label = outcome_label,
          var_name = strata,
          show_overall = FALSE) %>%
        add_risk(strata = strata)

      gt_obj <- gt_obj %>%
        add_mh_odds(exposure = exposure, outcome = outcome, strata = strata)


    return(gt_obj)
  }



gt_stat_risk <-
  function(data, variable, by=NULL, exposure, outcome, cs, measure = "OR", ...) {

    df <- data %>% dplyr::select(
      dplyr::all_of(variable),
      dplyr::all_of(exposure),
      dplyr::all_of(outcome))

    # if(variable is dichotomous) {
    # var_levels <- c(TRUE, FALSE)

      # or_results <- tab_univariate(df,
      #                              outcome = outcome,
      #                              risk = exposure,
      #                              measure = measure
      # )
      # ratio <- formatC(or_results$ratio, digits = 2, format = "f")

    var_levels <- levels(df[[variable]])

    result_dfs <- list()
    for (i in 1:length(var_levels)) {
      sub_df <- df %>% filter(.data[[variable]] == var_levels[i])
      if (nrow(sub_df) > 0) {
        or_results <- tab_univariate(sub_df, outcome = outcome, risk = exposure)

        ratio <- gtsummary::style_number(or_results$ratio, digits = 2)
        ci <- paste(gtsummary::style_number(or_results$lower, digits = 2),
                    "--",
                    gtsummary::style_number(or_results$upper, digits = 2))

        pvalue <- gtsummary::style_pvalue(or_results$p.value, digits = 2)

        result_dfs[[i]] <-
          data.frame(risk_estimate = ratio, risk_CI = ci, risk_pvalue = pvalue)
      } else {
        no_data <- "--"
        result_dfs[[i]] <-
          data.frame(risk_estimate = no_data, risk_CI = no_data, risk_pvalue = no_data)
      }
    }

    do.call("rbind", result_dfs)
  }



add_risk <- function(gt_object, strata) {
  exposure <- gt_object$meta_data$exposure
  outcome <- gt_object$meta_data$outcome

  exposure_sym <- as.symbol(exposure)
  qexposure <- rlang::enquo(exposure_sym)

  outcome_sym <- as.symbol(outcome)
  qoutcome <- rlang::enquo(outcome_sym)

  df <- gt_object$data

  if(!is.logical(df[[exposure]])) {
    df <- df %>% mutate(!!qexposure := as.logical(!!qexposure))
  }

  gto <-
    gtsummary::tbl_summary(
      df,
      include = c(gtsummary::all_of(strata)),
      statistic = gtsummary::all_categorical() ~ "") %>%
    gt_remove_stat()

  gt_risk <- gto %>%
    gtsummary::add_stat(
      # Add population and multiplier in purrr::partial
      fns = gtsummary::all_categorical(dichotomous = TRUE) ~ purrr::partial(
        gt_stat_risk,
        exposure = exposure,
        outcome = outcome,
        strata = strata),
      location = everything() ~ "level")

  gt_combined <-
    gtsummary::tbl_merge(list(gt_object, gt_risk), tab_spanner = FALSE)

  gt_combined[["data"]] <- gt_object$data

  return(gt_combined)
}


add_mh_odds <- function(gt_object, exposure, outcome, strata) {
  exposure_sym <- as.symbol(exposure)
  qexposure <- rlang::enquo(exposure_sym)

  outcome_sym <- as.symbol(outcome)
  qoutcome <- rlang::enquo(outcome_sym)

  df <- gt_object$data

  df <- df %>% select(outcome, exposure, strata)

  if(!is.logical(df[[exposure]])) {
    df <- df %>% mutate(!!qexposure := as.logical(!!qexposure))
  }

  gto <-
    gtsummary::tbl_summary(
      df,
      include = c(gtsummary::all_of(strata)),
      statistic = gtsummary::all_categorical() ~ "") %>%
    gt_remove_stat()



  gt_mh <- gto %>%
    gtsummary::add_stat(
      # Add population and multiplier in purrr::partial
      fns = gtsummary::all_categorical(dichotomous = TRUE) ~ purrr::partial(
        add_stat_mh_label, exposure = exposure, outcome = outcome, strata = strata),
      location = gtsummary::everything() ~ "label")

  gt_combined <-
    gtsummary::tbl_merge(list(gt_object, gt_mh), tab_spanner = FALSE)

  gt_combined[["data"]] <- gt_object$data

  return(gt_combined)
}


add_stat_mh_label <- function(data, variable, strata, by=NULL, exposure, outcome, ...) {

  df <- data %>% dplyr::select(variable, exposure, outcome)

  mh_results <- mh_odds(df, exposure = exposure, outcome = outcome, variable = variable)
  if(!is.null(mh_results)) {
    ratio <- gtsummary::style_number(mh_results$estimate, digits = 2)
    ci <- paste(gtsummary::style_number(mh_results$conf.int[[1]], digits = 2),
                "--",
                gtsummary::style_number(mh_results$conf.int[[2]], digits = 2))
    p_value <- gtsummary::style_pvalue(mh_results$p.value, digits = 2)

    data.frame(MHOR = ratio, MHORCI = ci, MHORpvalue = p_value)
  } else {
    no_data <- "--"
    data.frame(MHOR = no_data, MHORCI = no_data, MHORpvalue = no_data)
  }

}

mh_odds <- function(df, exposure, outcome, variable) {
  # example https://stackoverflow.com/questions/44953507/arranging-a-3-dimensional-contingency-table-in-r-in-order-to-run-a-cochran-mante
  # df must be in order of stratifying variable, exposure, outome, to prepare 3D matrix
  df <- df %>% dplyr::select(variable, exposure, outcome)

  if (!is.factor(df[[variable]])) {
    stop("for mh_odds, stratifying variables must be factors")
  }
  # need number of strata levs to create matrix
  strata_levs <- levels(df[[variable]])

  # stats::ftable creates a "flat", contigency table with exposure, outcome, variable
  df_ftable <- stats::ftable(df)

  # check for 0 values, do not attempt mantelhaen.test if any
  if (length(df_ftable[df_ftable == 0]) == 0) {
    # aperm is a base function that permutates the dimensions of an array,
    # using the length of the variable by using the levels function, and
    # resizing to create a 3d array (requred for stats::mantelhaen.test)
    matrix3d <- aperm(
      array(t(as.matrix(df_ftable)), c(2,2,length(strata_levs))),
      c(2,1,3))
    result <- stats::mantelhaen.test(matrix3d, exact=FALSE)
    return(result)
  } else {

    warning("mh_odds  internal function: cannot calculate MH odds with 0 values in any cell")
    return(NULL)
  }

}
