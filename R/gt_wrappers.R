# gtsummary and epikit wrapper functions


#' An attack rate wrapper function (using gtsummary and epikit packages)that takes
#' a gtsummary object and returns a gtsummary object with attack rate (per given
#'  multiple) with 95% confidence interval
#'
#' @param gts_object A data frame, passed by the gtsummary::add_stat function.
#'
#' @param deaths_var logical variable name that indicates deaths
#'
#' @param variable Name of a variable as the outcome of interest, passed by the
#' gtsummary::add_stat function (e.g. illness)
#'
#' @param by Name of a variable for stratifying, passed by the gtsummary::add_stat
#' function (e.g. illness).
#'
#'#'@param population the number of individuals in the population, passed to
#'`epikit::mortality_rate`
#'
#' @param ... additional params that may be passed from gtsummary functions.
#'
#' @return a single-row gtsummary object with attack rate results cases,
#' population, attack rate, and 95% confidence interval.
#'
#' @rdname gtsummary_wrappers
#'
#' @export
add_mr <- function(gts_object,
                   deaths_var,
                   population = NULL,
                   multiplier = 10^4,
                   drop_tblsummary_stat = FALSE) {
  summary_types <- unique(gts_object$meta_data$summary_type)

  if(!"categorical" %in% summary_types & "dichotomous" %in% summary_types) {
    gts_object <- gts_object %>%
      # Use add stat to add attack rate by label
      gtsummary::add_stat(
        # Add population and multiplier in purrr::partial
        fns = gtsummary::everything() ~ purrr::partial(
          add_gt_mortality_rate_stat_label,
          deaths = deaths_var,
          population = population,
          multiplier = multiplier))
  } else if("categorical" %in% summary_types & !"dichotomous" %in% summary_types) {

    gts_object <- gts_object %>%
      # Use add stat to add attack rate by level
      gtsummary::add_stat(
        fns = gtsummary::everything() ~ purrr::partial(
          add_gt_mortality_rate_level,
          deaths = deaths_var,
          population = population,
          multiplier = multiplier),
        location = everything() ~ "level")
  } else if ("categorical" %in% summary_types & "dichotomous" %in% summary_types) {
    gts_object <- gts_object %>%
      gtsummary::add_stat(
        # Add population and multiplier in purrr::partial
        fns = list(
          gtsummary::all_categorical() ~ purrr::partial(
            add_gt_mortality_rate_level,
            deaths = deaths_var,
            population = population,
            multiplier = multiplier,
            drop_deaths = FALSE),
          gtsummary::all_dichotomous() ~ purrr::partial(
            add_gt_mortality_rate_stat_label,
            deaths = deaths_var,
            population = population,
            multiplier = multiplier,
            drop_total = TRUE,
            drop_deaths = FALSE)),
        location = list(
          gtsummary::all_categorical() ~ "level",
          gtsummary::all_dichotomous() ~ "label"
        )
      )
  }

  if("Deaths" %in% names(gts_object$table_body)) {
    gts_object <- gts_object %>%
      gtsummary::modify_table_body(~.x %>% dplyr::relocate(Deaths, .after = label))
  }

  if(drop_tblsummary_stat) {
    gts_object <- gts_object %>% gt_remove_stat(col_name = "stat_0")
  }

  return(gts_object)
}

#' An attack rate wrapper function (using gtsummary and epikit packages)that takes
#' a gtsummary object and returns a gtsummary object withattack rate (per given
#'  multiple) with 95% confidence interval
#'
#' @param gts_object A data frame, passed by the gtsummary::add_stat function.
#'
#' @param variable Name of a variable as the outcome of interest, passed by the
#' gtsummary::add_stat function (e.g. illness)
#'
#' @param by Name of a variable for stratifying, passed by the gtsummary::add_stat
#' function (e.g. illness).
#'
#'#'@param population the number of individuals in the population, passed to
#'`epikit::attack_rate`
#'
#'@param multiplier The base by which to multiply the output:
# '1: multiplier = 1: ratio between 0 and 1;
# '2: multiplier = 100:proportion;
# '3: multiplier = 10^4: x per 10,000 people; passed to `epikit::attack_rate`
#'
#' @param ... additional params that may be passed from gtsummary functions.
#'
#' @return a single-row gtsummary object with attack rate results cases,
#' population, attack rate, and 95% confidence interval.
#'
#' @rdname gtsummary_wrappers
#'
#' @export
#'
add_ar <- function(gts_object,
                   case_var,
                   population = NULL,
                   multiplier = 10^4,
                   drop_tblsummary_stat = FALSE) {
  summary_types <- unique(gts_object$meta_data$summary_type)

  if(!"categorical" %in% summary_types & "dichotomous" %in% summary_types) {

    gts_object <- gts_object %>%
      # Use add stat to add attack rate by label
      gtsummary::add_stat(
        # Add population and multiplier in purrr::partial
        fns = gtsummary::everything() ~ purrr::partial(
          add_gt_attack_rate_stat_label,
          case_var = case_var,
          population = population,
          multiplier = multiplier))
  } else if("categorical" %in% summary_types & !"dichotomous" %in% summary_types) {
    gts_object <- gts_object %>%
      # Use add stat to add attack rate by level
      gtsummary::add_stat(
        fns = gtsummary::everything() ~ purrr::partial(
          add_gt_attack_rate_level,
          case_var = case_var,
          population = population,
          multiplier = multiplier),
        location = everything() ~ "level")
  } else if ("categorical" %in% summary_types & "dichotomous" %in% summary_types) {
    gts_object <-  gts_object %>%
      gtsummary::add_stat(
        # Add AR, population, and multiplier in purrr::partial
        fns = list(
          gtsummary::all_categorical() ~ purrr::partial(
            add_gt_attack_rate_level,
            case_var = case_var,
            population = population,
            multiplier = multiplier),
          gtsummary::all_dichotomous() ~ purrr::partial(
            add_gt_attack_rate_stat_label,
            case_var = case_var,
            population = population,
            multiplier = multiplier,
            drop_cases = FALSE)),
        location = list(
          gtsummary::all_categorical() ~ "level",
          gtsummary::all_dichotomous() ~ "label"
        )
      )
  }

  if("Cases" %in% names(gts_object$table_body)) {
    gts_object <- gts_object %>%
      gtsummary::modify_table_body(~.x %>% dplyr::relocate(Cases, .after = label))
  }

  if(drop_tblsummary_stat) {
    gts_object <- gts_object %>% gt_remove_stat(col_name = "stat_0")
  }

  return(gts_object)
}



#' An case fatality rate wrapper function (using gtsummary and epikit packages)
#' that takes a gtsummary object and returns a gtsummary object with number
#' of deaths, case fatality rate, and 95% confidence interval.
#'
#' @param data A data frame, passed by the gtsummary::add_stat function.
#'
#' @param variable Name of a variable as the outcome of interest, passed by the
#' gtsummary::add_stat function (e.g. illness).
#'
#' @param by Name of a variable for stratifying, passed by the gtsummary::add_stat function
#'   (e.g. illness).
#'
#' @param deaths_var the name of a logical column in the data that indicates that the case died,
#' is passed as the first argument to `epikit::case_fatality_rate_df`
#'
#' @param ... additional params that may be passed from gtsummary functions.
#'
#' @return a single row gtsummary object with case fatality rate results for
#' deaths, cases, cfr, and 95% confidence interval.
#'
#' @rdname gtsummary_wrappers
#' @import dplyr
#' @export
#'
add_cfr <- function(gts_object, deaths_var) {

  summary_types <- unique(gts_object$meta_data$summary_type)

  if(!"categorical" %in% summary_types & "dichotomous" %in% summary_types) {
    gts_object %>%  gtsummary::add_stat(
      fns = gtsummary::all_dichotomous() ~ purrr::partial(
        add_gt_cfr_stat_label, deaths_var = deaths_var))
  } else if("categorical" %in% summary_types & !"dichotomous" %in% summary_types) {
    gts_object %>% gtsummary::add_stat(
      # add purrr::partial with function name and required argument `deaths_var`
      fns = gtsummary::everything() ~ purrr::partial(
        add_gt_cfr_stat_level, deaths_var = deaths_var),
      location = gtsummary::everything() ~ "level"
    )
  } else if ("categorical" %in% summary_types & "dichotomous" %in% summary_types) {
    gts_object %>% gtsummary::add_stat(
      # add purrr::partial with function name and required argument `deaths_var`
      fns = list(
        gtsummary::all_categorical() ~ purrr::partial(
          add_gt_cfr_stat_level, deaths_var = deaths_var),
        gtsummary::all_dichotomous() ~ purrr::partial(
          add_gt_cfr_stat_label, deaths_var = deaths_var)),
      location = list(gtsummary::all_categorical() ~ "level",
                      gtsummary::all_dichotomous() ~ "label")
    )
  }

}




#' A gtsummary wrapper function that takes a gtsummary object and removes a
#' column from the table body by column name
#'
#' @param gts_object A data frame, passed by the gtsummary::add_stat function
#'
#' @param col_name the column name from the gtsummary object's table_body to remove

#' @return a gtsummary object without the named column
#'
#' @rdname gtsummary_wrappers
#' @export
#'
gt_remove_stat <- function(gts_object, col_name = "stat_0") {
  gts_object %>% gtsummary::modify_table_body(
    ~ .x %>%
      dplyr::select(-dplyr::all_of(col_name)))
}

#' A gtsummary wrapper function that takes a data frame and adds cross tabs
#' by exposure and outcome
#'
#' @param data A data frame with an exposure and outcome variable
#'
#' @param exposure Name of the exposure variable, which should be a factor ordered
#' by case and control - in that order (eg if case = 1, control = 0, factor levels
#' should be ordered as 1,0. The code labels the Cases as the first pair of
#' gstummary stat columns and the second pair as Controls.
#'
#' @param outcome Name of the outcome variable
#'
#' @param show_overall Logical argument to include overall column in gtsummary output;
#' defaults to TRUE
#'
#' @param exposure_label exposure label for the gtsummary output, if none passed,
#' exposure variable name is used instead
#'
#' @param outcome_label outcome label for the gtsummary output, if none passed,
#' outcome variable name is used instead
#'
#' @rdname gtsummary_wrappers
#'
#' @export

add_crosstabs <- function(
    data, exposure, outcome, case_reference = "outcome", var_name = NULL, show_overall = TRUE,
    exposure_label = NULL, outcome_label = NULL, var_label = NULL,
    two_by_two = FALSE, gt_statistic = "{n}", show_N_header = FALSE) {

  # Create exposure and outcome variables and labels ----
  exposure_sym <- as.symbol(exposure)
  qexposure <- rlang::enquo(exposure_sym)

  outcome_sym <- as.symbol(outcome)
  qoutcome <- rlang::enquo(outcome_sym)

  if (is.null(exposure_label)) exposure_label <- exposure
  if (is.null(outcome_label)) outcome_label <- outcome

  if (is.logical(data[[exposure]])) {
    # message("add_crosstabs: Ordering exposure to logical factor TRUE FALSE")
    data <- data %>%
      mutate(!!qexposure := factor(!!qexposure, levels = c(TRUE, FALSE)))
  }

  if (is.logical(data[[outcome]])) {
    # message("add_crosstabs: Ordering outcome to logical factor TRUE FALSE")
    data <- data %>%
      mutate(!!qoutcome := factor(!!qoutcome, levels = c(TRUE, FALSE)))
  }


  if (two_by_two) {
    gts <- data %>%
      dplyr::select(!!qexposure, !!qoutcome) %>%
      gtsummary::tbl_summary(
        include = !!qexposure,
        statistic = everything() ~ gt_statistic,
        by = !!qoutcome,
        type = exposure ~ "categorical",
        label = exposure ~ exposure_label
      ) %>%
      gtsummary::modify_header(label ~ "") %>%
      gtsummary::modify_spanning_header(c("stat_1", "stat_2") ~ outcome_label)

    if (show_overall) {
      gts <- gts %>% gtsummary::add_overall(last = TRUE)

      if(!show_N_header) {
        gts <- gts %>%
          gtsummary::modify_header(c("stat_1", "stat_2") ~ "**{level}**")
      }
    }
  } else {
    if(is.null(var_name)) {
      var_name <- "All"
      data <- data %>% mutate(All = TRUE)
      summary_type <- "dichotomous"
    } else {
      summary_type <- "categorical"
    }

    var_sym <- as.symbol(var_name)
    qvar <- rlang::enquo(var_sym)
    exposure_levels <- levels(data[[exposure]])
    outcome_levels <- levels(data[[outcome]])


    footnote <- paste0(
      paste0("Case defined as ", paste(outcome_label, "value of", outcome_levels[1])),
      "; ",
      paste0("Control defined as ", paste(outcome_label, "value of", outcome_levels[2])))
    if (is.null(var_label)) var_label <- var_name

    df_strata <-
      data %>%
      dplyr::select(dplyr::all_of(c(var_name, outcome, exposure))) %>%
      tidyr::nest(data = -dplyr::all_of(outcome)) %>%
      dplyr::mutate(
        tbl = purrr::map(
          data, ~ gtsummary::tbl_summary(
            .x,
            include = !!qvar,
            statistic = everything() ~ gt_statistic,
            by = !!qexposure,
            type = var_name ~ summary_type,
            label = var_name ~ var_label,
            missing = "ifany"
          ))
      ) %>%
      mutate_at(vars(outcome), as.factor) %>%
      mutate(!!qoutcome := fct_relevel(!! rlang::sym(outcome), outcome_levels)) %>%
      arrange(!!qoutcome)

    # gts <- gtsummary::tbl_merge(df_strata$tbl)
    if (show_overall) {
      gt_overall <- data %>%
        dplyr::select(dplyr::all_of(c(var_name, exposure))) %>%
        gtsummary::tbl_summary(
          include = var_name,
          statistic = everything() ~ gt_statistic,
          label = var_name ~ var_label)

      if(!show_N_header) {
        gt_overall <- gt_overall %>%
          gtsummary::modify_header(gtsummary::all_stat_cols() ~ "**N**", )
      }

      tbls <- df_strata$tbl
      tbls[[3]] <- gt_overall
      gts <- gtsummary::tbl_merge(tbls) %>%
        gtsummary::modify_header(label ~ exposure_label) %>%
        gtsummary::modify_spanning_header(list(
          c("stat_1_1", "stat_2_1") ~ "**Cases**"),
          c("stat_1_2", "stat_2_2") ~ "**Controls**",
          "stat_0_3" ~ "**Overall**") %>%
        gtsummary::modify_footnote(gtsummary::all_stat_cols() ~ footnote)

      if (!show_N_header) {
        gts <- gts %>%
          gtsummary::modify_header(
            c("stat_1_1", "stat_2_1", "stat_1_2", "stat_2_2") ~ "**{level}**")
      }
    } else {
      gts <- gtsummary::tbl_merge(df_strata$tbl) %>%
        gtsummary::modify_header(label ~ exposure_label) %>%
        gtsummary::modify_spanning_header(list(
          c("stat_1_1", "stat_2_1") ~ "**Cases**"),
          c("stat_1_2", "stat_2_2") ~ "**Controls**") %>%
        gtsummary::modify_footnote(gtsummary::all_stat_cols() ~ footnote)
      if (!show_N_header) {
        gts <- gts %>%
          gtsummary::modify_header(
            c("stat_1_1", "stat_2_1", "stat_1_2", "stat_2_2") ~ "**{level}**")
      }
    }
  }


  data <- data %>%
    mutate(!!qoutcome := as.logical(!!qoutcome))
  gts[["data"]] <- data
  gts[["meta_data"]] <- list(
    exposure = exposure,
    outcome = outcome,
    var_name = var_name,
    show_overall = show_overall,
    exposure_label = exposure_label,
    outcome_label = outcome_label,
    var_name = ifelse(!is.null(var_name), var_name, NA),
    var_label = ifelse(!is.null(var_label), var_label, NA),
    two_by_two = two_by_two,
    gt_statistic = gt_statistic
  )

  return(gts)
}





#' A function that adds mh odds ratio to an existing gtsummary object with same
#' dimensions (will add to this later.)
#'
#' @param data A data frame with linelist-style individual-level case data
#'
#' @param exposure column name to use as the exposure variable, must be logical class
#'
#' @param outcome column name to use as the outcome variable, must be logical class
#
#' @param exposure_label label for exposure variable
#'
#' @param outcome_label label for outcome variable
#
#' @param variable Name of a variable as the outcome of interest, passed by the
#' gtsummary::add_stat function (e.g. illness)
#'
#' @param by Name of a variable for stratifying, passed by the gtsummary::add_stat
#' function (e.g. illness).
#'
#'#'@param population the number of individuals in the population, passed to
#'`epikit::mortality_rate`
#'
#' @param ... additional params that may be passed from gtsummary functions.
#'
#' @return gtsummary object with case and control counts tabulated by exposure,
#' along with a crude overall odds ratio and odds using the
#' Cochran-Mantel-Haenszel test with 95% confidence interval
#' (https://cran.r-project.org/web/packages/samplesizeCMH/vignettes/samplesizeCMH-introduction.html)
#'
#' @rdname gtsummary_wrappers
#'
#' @export
gt_mh_odds <- function(
    data, exposure, outcome, strata,  exposure_label = NULL, outcome_label = NULL, strata_label = NULL) {

  data <- data %>% mutate(Overall = factor("All"))
  gt_obj_overall <- data %>%
    add_crosstabs(
      exposure = exposure,
      outcome = outcome,
      exposure_label = exposure_label,
      outcome_label = outcome_label,
      var_name = "Overall",
      show_overall = FALSE) %>%
    add_risk(strata = "Overall")

  gt_obj_var <- gt_mh_odds_single_var(
    data = data,
    exposure = exposure,
    outcome = outcome,
    exposure_label = exposure_label,
    outcome_label = outcome_label,
    strata = strata
  )

  gtstack <- gtsummary::tbl_stack(list(gt_obj_overall, gt_obj_var))
  # Align stacked columns from gtsummary-generated column names
  gtstack <- gtstack %>%
    gtsummary::modify_table_body(
      ~.x %>% dplyr::mutate(
        stat_1_1_1 = ifelse(is.na(stat_1_1_1), stat_1_1_1_1, stat_1_1_1),
        stat_2_1_1 = ifelse(is.na(stat_2_1_1), stat_2_1_1_1, stat_2_1_1),
        stat_1_2_1 = ifelse(is.na(stat_1_2_1), stat_1_2_1_1, stat_1_2_1),
        stat_2_2_1 = ifelse(is.na(stat_2_2_1), stat_2_2_1_1, stat_2_2_1),
        risk_estimate_2 = ifelse(is.na(risk_estimate_2), risk_estimate_2_1, risk_estimate_2),
        risk_CI_2 = ifelse(is.na(risk_CI_2), risk_CI_2_1, risk_CI_2),
        risk_pvalue_2 = ifelse(is.na(risk_pvalue_2), risk_pvalue_2_1, risk_pvalue_2)


      ) %>%
        dplyr::select(
          -c("stat_1_1_1_1", "stat_2_1_1_1", "stat_1_2_1_1", "stat_2_2_1_1",
             "risk_estimate_2_1", "risk_CI_2_1", "risk_pvalue_2_1")
        ))


  return(gtstack)
}

