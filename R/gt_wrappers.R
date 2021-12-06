#' Produce
#'
#' @param x A data frame
#'
#' @param outcome Name of A TRUE/FALSE variable as your outcome of interest
#'   (e.g. illness)
#'
#' @param ... Names of TRUE/FALSE variables as exposures of interest (e.g. risk
#'   factors)
#'   #' @return a long or wide tibble with tabulations n, ci, and deff
#'
#'
#'
add_gt_cfr_stat_label  <- function(data, variable, by, ...) {

  if(!is.null(by)) {
    warning("cfr by strata is not currently available, ignoring `by` argument")
  }

  stat_new <- data %>%
    epikit::case_fatality_rate_df(
      deaths = data[[variable]],
      mergeCI = TRUE) %>%
    dplyr::mutate(deaths = as.integer(deaths)) %>%
    rename("Deaths" = deaths,
           "Cases" = population,
           "CFR (%)" = cfr,
           "95%CI" = ci)
}


add_gt_cfr_stat_level <- function(data, variable, by, ...) {
  if(!is.null(by)) {
    warning("cfr by strata is not currently available, ignoring `by` argument")
  }

  variable_ <- variable

  tb <- list(...)$tbl
  gt_dt <- tb$table_body
  var_dt <- gt_dt %>%
    dplyr::filter(variable %in% variable & !is.na(stat_0))
  var_levels <- unique(var_dt$label)

  deaths_var <- data$deaths_var[1]
  deaths <- data[[deaths_var]]
  var <- rlang::enquo(variable)
  var_sym <- as.symbol(variable)
  qvariable <- rlang::enquo(var_sym)


  stat_new <- data %>%
    epikit::case_fatality_rate_df(deaths = DIED, group = !!qvariable, mergeCI = TRUE) %>%
    dplyr::filter(!!qvariable %in% var_levels) %>%
    select(deaths, population, cfr, ci) %>%
    rename("Deaths" = deaths,
           "Cases" = population,
           "CFR (%)" = cfr,
           "95%CI" = ci)

}


add_gt_attack_rate_label <- function(data, variable, by=NULL, ...) {
  if(is.null(data$population)) {
    stop("`population` column (equal to total population) required")
  }

  if(is.null(data$multiplier)) {
    stop("`multiplier` column required")
  }

  if(!is.null(by)) {
    warning("attack rate by strata is not currently available, ignoring `by` argument")
  }

  population <- data$population[1]
  multiplier <- data$multiplier[1]
  cases <- nrow(data)


  ar <- epikit::attack_rate(cases = cases,
                            population = population,
                            multiplier = multiplier)

  ar %>%
    merge_ci_df(e = 3) %>% # merge the lower and upper CI into one column
    rename("Cases (n)" = cases,
           "AR (per 10,000)" = ar,
           "95%CI" = ci) %>%
    select(-population) %>% # drop the population column as it is not changing
    tibble::tibble()
}


add_gt_attack_rate_level <- function(data, variable, by=NULL, ...) {
  if(is.null(data$population)) {
    stop("`population` column, stratified by variable required")
  }

  if(is.null(data$multiplier)) {
    stop("`multiplier` column required")
  }

  if(!is.null(by)) {
    warning("attack rate by strata is not currently available, ignoring `by` argument")
  }

  multiplier <- data$multiplier[1]
  sym_var <- as.symbol(variable)
  cases <- count(data, !!rlang::enquo(sym_var), population)

  epikit::attack_rate(cases = cases$n,
                      population = cases$population,
                      multiplier = multiplier) %>%
    merge_ci_df(e = 3) %>% # merge the lower and upper CI into one column
    # dplyr::add_row(cases = NA, .before = 1) %>%
    rename("Cases (n)" = cases,
           "Population" = population,
           "AR (per 10,000)" = ar,
           "95%CI" = ci) %>%
    tibble::tibble()
}


merge_gt_univar_counts <- function(gt_object) {
  gt_data <- gt_object$inputs$data
  by <- gt_object$inputs$y
  ## produce counts for each of the variables of interest
  cross_tab <- gt_data %>%
    gtsummary::tbl_summary(by = by)
  ## combine for a full table
  gtsummary::tbl_merge(list(cross_tab, gt_object)) %>%
    gtsummary::modify_spanning_header(gtsummary::everything() ~ NA_character_) %>%
    gtsummary::modify_spanning_header(gtsummary::all_stat_cols() ~ by)
}


gt_remove_stat <- function(gt_object, col_name = "stat_0") {
  gt_object %>% gtsummary::modify_table_body(
    ~ .x %>%
      dplyr::select(-all_of(col_name)))
}


