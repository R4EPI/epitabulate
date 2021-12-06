# gtsummary and epikit wrapper functions

#' A case fatality rate wrapper function to be passed to the gtsummary::add_stat
#' function, which returns a data frame with a single row to be used with
#' dichotomous data or overall data.Calls epikit::case_fatality_rate_df.
#'
#' @param data A data frame, passed by the gtsummary::add_stat function.
#'
#' @param variable Name of a variable as the outcome of interest, passed by the
#' gtsummary::add_stat function (e.g. illness).
#'
#' @param by Name of a variable for stratifying, passed by the gtsummary::add_stat function
#'   (e.g. illness).
#'
#' @param ... additional params that may be passed from gtsummary functions.
#'
#' @return a single row gtsummary object with case fatality rate results for
#' deaths, cases, cfr, and 95% confidence interval.
#'
#' @rdname gtsummary_wrappers
#' @export
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


#' A case fatality rate wrapper function to be passed to the gtsummary::add_stat function,
#' which returns a data frame with multiple rows to be used when location is set
#' to "level" for multi-level categorical data. Calls epikit::case_fatality_rate_df.
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
#' @export
#'
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

#' An attack rate wrapper function to be passed to the gtsummary::add_stat
#' function, which returns a data frame with a single row to be used with
#' dichotomous data or overall data. Calls epikit::attack_rate.
#'
#' @param data A data frame, passed by the gtsummary::add_stat function.
#'
#' @param variable Name of a variable as the outcome of interest, passed by the
#' gtsummary::add_stat function (e.g. illness).
#'
#' @param by Name of a variable for stratifying, passed by the gtsummary::add_stat function
#'   (e.g. illness).
#'
#' @param ... additional params that may be passed from gtsummary functions.
#'
#' @return a single row gtsummary object with with attack rate results with
#' cases, attack rate, and 95% confidence interval.
#'
#' @rdname gtsummary_wrappers
#' @export

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

  ar_label <- paste0("AR (per ", multiplier, ")")
  ar %>%
    merge_ci_df(e = 3) %>% # merge the lower and upper CI into one column
    rename("Cases (n)" = cases,
           ar_label = ar,
           "95%CI" = ci) %>%
    select(-population) %>% # drop the population column as it is not changing
    tibble::tibble()
}

#' An attack rate wrapper function to be passed to the gtsummary::add_stat function,
#' which returns a data frame with multiple rows to be used when location is set
#' to "level" for multi-level categorical data. Calls epikit::attack_rate.
#'
#' @param data A data frame, passed by the gtsummary::add_stat function.
#'
#' @param variable Name of a variable as the outcome of interest, passed by the
#' gtsummary::add_stat function (e.g. illness)
#'
#' @param by Name of a variable for stratifying, passed by the gtsummary::add_stat
#' function (e.g. illness).
#'
#' @param ... additional params that may be passed from gtsummary functions.
#'
#' @return a single-row gtsummary object with attack rate results cases,
#' population, attack rate, and 95% confidence interval.
#'
#' @rdname gtsummary_wrappers
#' @export
#'
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

    ar_label <- paste0("AR (per ", multiplier, ")")
    rename("Cases (n)" = cases,
           "Population" = population,
           ar_label = ar,
           "95%CI" = ci) %>%
    tibble::tibble()
}

#' A gtsummary wrapper function that takes a tbl_uvregression gtsummary object
#' and adds count and percentage columns.
#'
#' @param gt_object A data frame, passed by the gtsummary::add_stat function

#' @return a tbl_merge gtsummary object with counts, percentage columns, and results
#' of univariate regression.
#'
#' @rdname gtsummary_wrappers
#' @export
#'
merge_gt_univar_counts <- function(gt_object) {
  gt_data <- gt_object$inputs$data
  by <- gt_object$inputs$y
  ## produce counts for each of the variables of interest
  cross_tab <- gt_data %>%
    gtsummary::tbl_summary(
      by = by,
      digits = list(gtsummary::all_categorical() ~ c(0, 1)))
  ## combine for a full table
  gtsummary::tbl_merge(list(cross_tab, gt_object)) %>%
    gtsummary::modify_spanning_header(gtsummary::everything() ~ NA_character_) %>%
    gtsummary::modify_spanning_header(gtsummary::all_stat_cols() ~ all_of(by))
}

#' A gtsummary wrapper function that takes a gtsummary object and removes a
#' column from the table body by column name
#'
#' @param gt_object A data frame, passed by the gtsummary::add_stat function
#'
#' @param col_name the column name from the gtsummary object's table_body to remove

#' @return a gtsummary object without the named column
#'
#' @rdname gtsummary_wrappers
#' @export
#'
gt_remove_stat <- function(gt_object, col_name = "stat_0") {
  gt_object %>% gtsummary::modify_table_body(
    ~ .x %>%
      dplyr::select(-all_of(col_name)))
}

