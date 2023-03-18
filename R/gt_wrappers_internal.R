# gt_wrapper internal functions (not exported)


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
