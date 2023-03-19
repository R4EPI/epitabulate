
#' A {gtsummary} wrapper function that takes a gtsummary univariate regression
#' table and adds appropriate cross tabs by exposure and outcome
#'
#'@param x Object with class `tbl_uvregression` from the gtsummary
#'tbl_uvregression function.
#'
#'@importFrom gtsummary modify_table_styling modify_table_body modify_header modify_fmt_fun style_number modify_footnote
#'@importFrom dplyr mutate relocate
#'
#'@references Inspired by Daniel Sjoberg,
#' see [gtsummary github repo](https://github.com/ddsjoberg/gtsummary)

add_crosstabs <- function(x) {

  # checking that input is class tbl_summary
  if (!(inherits(x, "tbl_uvregression"))) {
    stop("`x` must be class 'tbl_uvregression'")
  }

  # grab the table
  the_table <- x

  # grab the type of regression
  regression_type <- x$table_body$coefficients_type[1L]

  if (!regression_type %in% c("logistic", "poisson")) {
    stop("The regression must type 'logistic' or 'poisson' (negative binomials appear as 'poisson')")
  }

  # hide the original N variable
  # (shows up if the original tbl_uvregression didn't specify hide_n = TRUE)
  if (!the_table$inputs$hide_n) {
    the_table <-
      gtsummary::modify_table_styling(
        the_table,
        columns = "stat_n",
        hide = TRUE
      )
  }

  # ODDS Ratios ----------------------------------------------------------------
  if (regression_type == "logistic") {

    # edit the table body (contents of table)
    the_table <- gtsummary::modify_table_body(
      the_table,
      # define a function to make two steps and avoid piping
      fun = function(.x){
        # remove cases from total obs to get control counts
        .x <- dplyr::mutate(.x, n_nonevent = n_obs - n_event)
        # move case and control counts before estimate
        .x <- dplyr::relocate(.x, c(n_event, n_nonevent), .before = estimate)
      }
    )

    # rename columns appropriately
    the_table <- gtsummary::modify_header(
      the_table,
      n_nonevent = "**Control (n)**",
      n_event = "**Case (n)**")

    # set the columns to numeric
    the_table <- gtsummary::modify_fmt_fun(
      the_table,
      c(n_event, n_nonevent) ~ gtsummary::style_number)
  }

  # RISK Ratios ----------------------------------------------------------------
  # note both poisson (glm) and negbin (MASS) are "poisson" in gtsummary
  if (regression_type == "poisson" &
      is.null(the_table$inputs$method.args$offset)){

    # edit the table body
    the_table <- gtsummary::modify_table_body(
      the_table,
      # define a function to make two steps and avoid piping
      fun = function(.x){
        # move case and control counts before estimate
        .x <- dplyr::relocate(.x, c(n_event, n_obs), .before = estimate)
        # change the estimate label from IRR to RR (doesn't change col header, could remove)
        .x <- dplyr::mutate(.x, coefficients_label = "RR")
      }
    )

    # rename columns appropriately
    the_table <- gtsummary::modify_header(
      the_table,
      n_obs = "**Total exposed (N)**",
      n_event = "**Cases exposed (n)**",
      estimate = "**RR**"
      )

    # set the columns to numeric
    the_table <- gtsummary::modify_fmt_fun(
      the_table,
      c(n_event, n_obs) ~ gtsummary::style_number)


    # update the footnote to say risk ratio
    the_table <- gtsummary::modify_footnote(
      the_table,
      estimate = "RR = Risk Ratio",
      abbreviation = TRUE
    )
  }

  # INCIDENCE RATE Ratios ------------------------------------------------------
  # note both poisson (glm) and negbin (MASS) are "poisson" in gtsummary
  if (regression_type == "poisson" &
      !is.null(the_table$inputs$method.args$offset)){

    # rename columns appropriately
    the_table <- gtsummary::modify_header(
      the_table,
      exposure = "**Total exposed (person-time)**",
      n_event = "**Cases exposed (n)**")

    # set the columns to numeric
    the_table <- gtsummary::modify_fmt_fun(
      the_table,
      c(n_event, exposure) ~ gtsummary::style_number)

  }

  # return table
  the_table

}




#### NOTES



## ODDS Ratios
blabla <- linelist_cleaned %>%
  select(DIED, gender_bin, age_group) %>%
  gtsummary::tbl_uvregression(method = glm,
                              y = DIED,
                              method.args = list(family = binomial),
                              exponentiate = TRUE,
                              hide_n = TRUE)

## adding number of non-events to table
## from https://www.danieldsjoberg.com/gtsummary/reference/modify_table_body.html
blabla %>%  gtsummary::modify_table_body(
  ~ .x %>%
    dplyr::mutate(n_nonevent = n_obs - n_event) %>%
    dplyr::relocate(c(n_event, n_nonevent), .before = estimate)) %>%
  ## assigning header labels
  gtsummary::modify_header(n_nonevent = "**Control n**", n_event = "**Case n**") %>%
  gtsummary::modify_fmt_fun(c(n_event, n_nonevent) ~ gtsummary::style_number)


## RISK ratios (poisson)
blabla <- linelist_cleaned %>%
  select(DIED, gender_bin, age_group) %>%
  gtsummary::tbl_uvregression(method = glm,
                              y = DIED,
                              method.args = list(family = poisson),
                              exponentiate = TRUE,
                              hide_n = TRUE)

blabla %>%
  ## assigning header labels
  gtsummary::modify_header(n_obs = "**Total exposed N**", n_event = "**Cases exposed n**") %>%
  gtsummary::modify_fmt_fun(c(n_event, n_obs) ~ gtsummary::style_number)


## RISK ratios (negbin)
blabla <- linelist_cleaned %>%
  select(DIED, gender_bin, age_group) %>%
  gtsummary::tbl_uvregression(method = MASS::glm.nb,
                              y = DIED,
                              exponentiate = TRUE,
                              hide_n = TRUE)

blabla %>%
  ## assigning header labels
  gtsummary::modify_header(n_obs = "**Total exposed N**", n_event = "**Cases exposed n**") %>%
  gtsummary::modify_fmt_fun(c(n_event, n_obs) ~ gtsummary::style_number)



## TODO: add a catch that if blabla$inputs$method.args$offset is Null then change colname to RR


## INCIDENCE RATE ratios
blabla <- linelist_cleaned %>%
  mutate(obstime = sample.int(30, nrow(linelist_cleaned), replace = TRUE)) %>%
  filter(!is.na(obstime)) %>%
  select(DIED, gender_bin, age_group, obstime) %>%
  gtsummary::tbl_uvregression(method = glm,
                              y = DIED,
                              method.args = list(family = poisson,
                                                 offset = log(obstime)),
                              exponentiate = TRUE,
                              hide_n = TRUE)

blabla %>%
  ## assigning header labels
  gtsummary::modify_header(exposure = "**Total exposed (person-time)**", n_event = "**Cases exposed n**") %>%
  gtsummary::modify_fmt_fun(c(n_event, exposure) ~ gtsummary::style_number)




## extract the obstime variable
if (!is.null(blabla$inputs$method.args$offset)) {
  obstimevar <- as.character(blabla$inputs$method.args$offset)[2]
}

## select exposure vars
exposurevars <- c(blabla$inputs$include[!blabla$inputs$include %in% c(blabla$inputs$y, obstimevar)])

cross_tab <- purrr::map(
  exposurevars,
  function(i) {
    blabla$inputs$data %>%
      group_by(.data[[i]]) %>%
      summarise(outcome = sum(.data[[blabla$inputs$y]]),
                perstime = sum(.data[[obstimevar]])) %>%
      rename(group = all_of(i)) %>%
      mutate(group = as.character(group),
             var = i)
    }) %>%
  bind_rows()

heh <- blabla$inputs$data %>%
  select(DIED, age_group) %>%
  gtsummary::tbl_summary(by = DIED)


blabla$inputs$data %>%
  select(obstime, age_group) %>%
  gtsummary::tbl_summary(by = age_group,
                        statistic = list(everything() ~ "{sum}"))



## This is complete horseshit
add_crosstabs(
  data = blabla$inputs$data,
  outcome = blabla$inputs$y,
  exposure = blabla$inputs$include[2],
)




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
      paste0("Control defined as ", paste(outcome_label, "value of", outcome_levels[2])),
      "; ",
      paste0("Exposure variable is ", exposure_label))
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
