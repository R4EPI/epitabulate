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
#' @import dplyr
#' @export
#'
add_gt_cfr_stat_label  <- function(data, variable, by, ...) {
  # Declare local variables for CMD check
  deaths <- population <- cfr <- ci <- NULL
  if(!is.null(by)) {
    warning("cfr by strata is not currently available, ignoring `by` argument")
  }

  stat_new <- data %>%
    epikit::case_fatality_rate_df(
      deaths = data[[variable]],
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

    # create rlang::enquo objects to use in epikit::case_fatality_rate_df
    deaths_sym <- as.symbol(deaths_var)
    qdeaths <- rlang::enquo(deaths_sym)
    var_sym <- as.symbol(variable)
    qvariable <- rlang::enquo(var_sym)

    stat_new <- data %>%
      epikit::case_fatality_rate_df(deaths = !!qdeaths, group =  !!qvariable , mergeCI = TRUE) %>%
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

add_gt_attack_rate_label <- function(data, variable, by=NULL, population, multiplier, ...) {
  # Declare local variables for CMD check
  cases <- ci <- NULL
  if(is.null(population)) {
    stop("`population` argument (equal to total population) required")
  }

  if(is.null(multiplier)) {
    stop("`multiplier` argument required")
  }

  if(!is.null(by)) {
    warning("attack rate by strata is not currently available, ignoring `by` argument")
  }

  # population <- data$population[1]
  # multiplier <- data$multiplier[1]
  cases <- nrow(data)


  ar_label <- paste0("AR (per ", format(multiplier, big.mark=","), ")")
  cols_rename <- setNames("ar", ar_label)

  ar <- epikit::attack_rate(cases = cases,
                            population = population,
                            multiplier = multiplier) %>%
    epikit::merge_ci_df(e = 3) %>% # merge the lower and upper CI into one column
    dplyr::rename(
      # "Cases (n)" = cases,
      "95%CI" = ci) %>%
    dplyr::rename(dplyr::all_of(cols_rename)) %>%
    # drop the population column as it is not changing,
      # and drop cases as it's in the statistic of gtsummary
    dplyr::select(-c(population, cases))
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
add_gt_attack_rate_level <- function(data, variable, population, multiplier, by=NULL, ...) {
  # Declare local variables for CMD check
  cases <- ci <- NULL

  if(is.null(population)) {
    stop("`population` argument, stratified by variable required")
  }

  if(is.null(multiplier)) {
    stop("`multiplier` argument required")
  }

  if(!is.null(by)) {
    warning("attack rate by strata is not currently available, ignoring `by` argument")
  }

  sym_var <- as.symbol(variable)
  cases <- count(data, !!rlang::enquo(sym_var))
  ar_label <- paste0("AR (per ", format(multiplier, big.mark=","), ")")
  cols_rename <- setNames("ar", ar_label)

  if(length(population) != nrow(cases)) {
    stop("`population` argument, must have a value for each category in variable")
  }

  epikit::attack_rate(cases = cases$n,
                      population = population,
                      multiplier = multiplier) %>%
    epikit::merge_ci_df(e = 3) %>% # merge the lower and upper CI into one column
    dplyr::rename(
           # "Cases (n)" = cases,
           "Population" = population,
           "95%CI" = ci) %>%
    # Addresses a cmd check
    dplyr::rename(dplyr::all_of(cols_rename)) %>%
    # drop cases as it's in the statistic of gtsummary
    dplyr::select(-c(cases))
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
    gtsummary::modify_spanning_header(gtsummary::all_stat_cols() ~ gtsummary::all_of(by))
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
      dplyr::select(-dplyr::all_of(col_name)))
}


gtsummary_case_fatality_rate <- function(gts_object, deaths_var) {

  summary_types <- unique(gts_object$meta_data$summary_type)

  if(!"categorical" %in% summary_types & "dichotomous" %in% summary_types) {
    gts_object %>%  gtsummary::add_stat(
        fns = gtsummary::all_dichotomous() ~ add_gt_cfr_stat_label)
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
      fns = list( gtsummary::all_categorical() ~ purrr::partial(
        add_gt_cfr_stat_level, deaths_var = deaths_var),
        gtsummary::all_dichotomous() ~ add_gt_cfr_stat_label),
      location = list(gtsummary::all_categorical() ~ "level",
                      gtsummary::all_dichotomous() ~ "label")
    )
  }

}


