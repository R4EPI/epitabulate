
#' A mortality rate wrapper function (using the gtsummary package)that takes
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
#'`epitabulate::mortality_rate`
#'
#' @param ... additional params that may be passed from gtsummary functions.
#'
#' @return a single-row gtsummary object with attack rate results cases,
#' population, attack rate, and 95% confidence interval.
#'
#' @rdname gt_attackrate
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

#' An attack rate wrapper function (using the gtsummary package)that takes
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
#'`epitabulate::attack_rate`
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
#' @rdname gt_attackrate
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



#' An case fatality rate wrapper function (using the gtsummary package)
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
#' is passed as the first argument to `epitabulate::case_fatality_rate_df`
#'
#' @param ... additional params that may be passed from gtsummary functions.
#'
#' @return a single row gtsummary object with case fatality rate results for
#' deaths, cases, cfr, and 95% confidence interval.
#'
#' @rdname gt_attackrate
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


#' Internal functions (not exported)
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
#' @noRd
#' @import dplyr
#' @importFrom epikit merge_ci_df

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


#' Internal functions (not exported)
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
#' @noRd

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

#' Internal functions (not exported)
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
#' @noRd

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

#' Internal functions (not exported)
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
#' @noRd

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


#' Internal functions (not exported)
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
#' @noRd

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
