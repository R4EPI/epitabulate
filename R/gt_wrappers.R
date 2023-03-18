# gtsummary wrapper functions






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
#'`epitabulate::mortality_rate`
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

  # Align stacked columns by standardizing gtsummary-generated column names
  gtstack <- gtstack %>%
    gtsummary::modify_table_body(
      ~.x %>%
        dplyr::mutate(
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

  if (is.null(exposure_label)) exposure_label <- exposure
  if (is.null(outcome_label)) outcome_label <- outcome
  if (is.null(strata_label)) strata_label <- strata

  gtstack <- gtstack %>%
    gtsummary::modify_table_body(
      ~.x %>%
        dplyr::mutate(label = ifelse(label == strata, strata_label, label)
        )
    ) %>% gtsummary::modify_spanning_header(list(
      c("risk_estimate_2", "risk_CI_2", "risk_pvalue_2") ~ "**Crude Odds Ratio**",
      c("MHOR_2", "MHORCI_2", "MHORpvalue_2") ~ "**MH Odds Ratio**"
    )) %>%
    gtsummary::modify_header(
      "risk_estimate_2" ~ "**OR**",
      "risk_CI_2" ~ "**95% CI**",
      "risk_pvalue_2" ~ "**p-value**",
      "MHOR_2" ~ "**MH OR**",
      "MHORCI_2" ~ "**95% CI**",
      "MHORpvalue_2" ~ "**p-value**"

    ) %>%
      gtsummary::modify_footnote(
        "risk_estimate_2" ~ "Odds ratio",
        c("risk_CI_2", "MHORCI_2") ~ "95% confidence interval",
        "MHOR_2" ~ "Cochran-Mantel-Haenszel odds ratio"
      )

  return(gtstack)
}

