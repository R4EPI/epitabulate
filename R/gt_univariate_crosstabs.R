
#' A {gtsummary} wrapper function that takes a gtsummary univariate regression
#' table and adds appropriate cross tabs by exposure and outcome
#'
#'@param x Object with class `tbl_uvregression` from the gtsummary
#'tbl_uvregression function or `tbl_cmh` from the epitabulate tbl_cmh function.
#'
#'@param wide TRUE/FALSE to specify whether would like to have the output in
#'wide format. Results in four columns rather than two, but in a single row.
#'This is only works for dichotomous variables (yes/no, TRUE/FALSE,
#'male/female), others will be dropped with a warning message. (Default is FALSE)
#'
#'@importFrom gtsummary modify_table_styling modify_table_body modify_header modify_fmt_fun style_number modify_footnote
#'@importFrom dplyr mutate relocate
#'
#'@references Inspired by Daniel Sjoberg,
#' see [gtsummary github repo](https://github.com/ddsjoberg/gtsummary)

add_crosstabs <- function(x, wide = FALSE) {

  # checking that input is class tbl_summary
  if (!(inherits(x, c("tbl_uvregression", "tbl_cmh")))) {
    stop("`x` must be class 'tbl_uvregression' or 'tbl_cmh'")
  }

  # grab the table
  the_table <- x

  # grab the type of regression
  regression_type <- x$table_body$coefficients_type[1L]

  if (!regression_type %in% c("logistic", "poisson")) {
    stop("The regression must be type 'logistic' or 'poisson' (negative binomials appear as 'poisson')")
  }

  # hide the original N variable
  # (shows up if the original tbl_uvregression didn't specify hide_n = TRUE)
  ## TODO: this is an ugly quick fix to allow this to work for tbl_cmh()
  if(inherits(x, c("tbl_uvregression"))){
    if (!the_table$inputs$hide_n) {
      the_table <-
        gtsummary::modify_table_styling(
          the_table,
          columns = "stat_n",
          hide = TRUE
        )
    }
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

  # WIDE FORMAT DICHOTOMOUS ----------------------------------------------------
  if (wide) {

    # drop non dichotomous variables and chuck a warning
    # find if there are any non dichotomous
    check_categoricals <- filter(the_table$table_body, var_type != "dichotomous")

    if (nrow(check_categoricals) >= 1 ) {
      # get the variable names which are not dichotomous
      check_categoricals <- distinct(check_categoricals, variable)
      check_categoricals <- pull(check_categoricals)

      # chuck a warning listing the names
      warning(paste0("The following non-dichotomous variables were dropped:",
                     check_categoricals), call. = FALSE)

      # edit the table body (contents of table)
      the_table <- gtsummary::modify_table_body(
        the_table,
        # define a function to avoid piping
        fun = function(.x){
          # only keep the row with the estimates
          .x <- filter(.x, var_type == "dichotomous")
        }
      )

    }

    # chuck an error if they have used show_single_row
    if (!is.null(the_table$inputs$show_single_row)) {
      stop("Wide format is not possible when specifying 'show_single_row' in your tbl_uvregression. Please change this")
    }

    ## TODO: do some fiddling to make tbl_cmh work in wide too
    if (inherits(x, c("tbl_cmh"))) {

      the_table <- gtsummary::modify_table_body(
        the_table,
        # define a function to avoid piping
        fun = function(.x){

          # pull values down to have all in the right row
          .x <- tidyr::fill(.x,
                            stratifier,
                            .direction = "down")
        }
      )
    }


    # define moving variables and labels based on regression type
    if (regression_type == "logistic") {
      # define vars of interest
      the_vars <- c("n_event", "n_nonevent")
      # define column headers for new vars
      new_names <- c("**Cases exposed (n)**",
                     "**Controls exposed (n)**",
                     "**Cases unexposed (n)**",
                     "**Controls unexposed (n)**")
    }
    if (regression_type == "poisson" &
        is.null(the_table$inputs$method.args$offset)) {
      # define vars of interest
      the_vars <- c("n_obs", "n_event")
      # define column headers for new vars
      new_names <- c("**Total exposed (N)**",
                     "**Cases exposed (n)**",
                     "**Total unexposed (N)**",
                     "**Cases unexposed (n)**")
    }
    if (regression_type == "poisson" &
        !is.null(the_table$inputs$method.args$offset)) {
      # define vars of interest
      the_vars <- c("exposure", "n_event")
      # define column headers for new vars
      new_names <- c("**Total exposed (person-time)**",
                     "**Cases exposed (n)**",
                     "**Total unexposed (person-time)**",
                     "**Cases unexposed (n)**")
    }

    # store the reference_row for later use in filtering
    # (gets dropped by pivot_wider)
    identifier <- dplyr::select(the_table$table_body, reference_row)

    # combine the vars of interest with TRUE/FALSE
    new_vars <- expand.grid(the_vars, c("_FALSE", "_TRUE"))
    new_vars <- paste0(new_vars$Var1, new_vars$Var2)

    # put vars and labels into a list for renaming
    relabel_vars <- as.list(new_names)
    names(relabel_vars) <- new_vars


    # edit the table body (contents of table)
    the_table <- gtsummary::modify_table_body(
      the_table,
      # define a function to avoid piping
      fun = function(.x){

        # spread to wide format
        .x <- tidyr::pivot_wider(.x,
                                 names_from = reference_row,
                                 values_from = all_of(the_vars))

        # pull values down to have all in the right row
        .x <- tidyr::fill(.x,
                          all_of(new_vars),
                          .direction = "down")

        # add the reference_row back in for filtering
        .x <- cbind(.x, identifier)

        # edit row names
        .x <- mutate(.x,
                     # make them show up as variables
                     row_type = "label",
                     # change to show variable label
                     label = var_label
                     )

        # only keep the row with the estimates
        .x <- filter(.x, !reference_row)

        # move case and control counts before estimate
        .x <- dplyr::relocate(.x, all_of(new_vars), .before = estimate)

      }
    )

    # change column names
    the_table <- gtsummary::modify_header(the_table, update = relabel_vars)

  }

  # return outputs
  the_table
}




# #### NOTES
#
#
#
# ## ODDS Ratios
# blabla <- linelist_cleaned %>%
#   select(DIED, gender_bin, age_group) %>%
#   gtsummary::tbl_uvregression(method = glm,
#                               y = DIED,
#                               method.args = list(family = binomial),
#                               exponentiate = TRUE,
#                               hide_n = TRUE)
#
# ## adding number of non-events to table
# ## from https://www.danieldsjoberg.com/gtsummary/reference/modify_table_body.html
# blabla %>%  gtsummary::modify_table_body(
#   ~ .x %>%
#     dplyr::mutate(n_nonevent = n_obs - n_event) %>%
#     dplyr::relocate(c(n_event, n_nonevent), .before = estimate)) %>%
#   ## assigning header labels
#   gtsummary::modify_header(n_nonevent = "**Control n**", n_event = "**Case n**") %>%
#   gtsummary::modify_fmt_fun(c(n_event, n_nonevent) ~ gtsummary::style_number)
#
#
# ## RISK ratios (poisson)
# blabla <- linelist_cleaned %>%
#   select(DIED, gender_bin, age_group) %>%
#   gtsummary::tbl_uvregression(method = glm,
#                               y = DIED,
#                               method.args = list(family = poisson),
#                               exponentiate = TRUE,
#                               hide_n = TRUE)
#
# blabla %>%
#   ## assigning header labels
#   gtsummary::modify_header(n_obs = "**Total exposed N**", n_event = "**Cases exposed n**") %>%
#   gtsummary::modify_fmt_fun(c(n_event, n_obs) ~ gtsummary::style_number)
#
#
# ## RISK ratios (negbin)
# blabla <- linelist_cleaned %>%
#   select(DIED, gender_bin, age_group) %>%
#   gtsummary::tbl_uvregression(method = MASS::glm.nb,
#                               y = DIED,
#                               exponentiate = TRUE,
#                               hide_n = TRUE)
#
# blabla %>%
#   ## assigning header labels
#   gtsummary::modify_header(n_obs = "**Total exposed N**", n_event = "**Cases exposed n**") %>%
#   gtsummary::modify_fmt_fun(c(n_event, n_obs) ~ gtsummary::style_number)
#
#
# ## INCIDENCE RATE ratios
# blabla <- linelist_cleaned %>%
#   mutate(obstime = sample.int(30, nrow(linelist_cleaned), replace = TRUE)) %>%
#   filter(!is.na(obstime)) %>%
#   select(DIED, gender_bin, age_group, obstime) %>%
#   gtsummary::tbl_uvregression(method = glm,
#                               y = DIED,
#                               method.args = list(family = poisson,
#                                                  offset = log(obstime)),
#                               exponentiate = TRUE,
#                               hide_n = TRUE)
#
# blabla %>%
#   ## assigning header labels
#   gtsummary::modify_header(exposure = "**Total exposed (person-time)**", n_event = "**Cases exposed n**") %>%
#   gtsummary::modify_fmt_fun(c(n_event, exposure) ~ gtsummary::style_number)
#
#
#
#
# ## extract the obstime variable
# if (!is.null(blabla$inputs$method.args$offset)) {
#   obstimevar <- as.character(blabla$inputs$method.args$offset)[2]
# }
#
# ## select exposure vars
# exposurevars <- c(blabla$inputs$include[!blabla$inputs$include %in% c(blabla$inputs$y, obstimevar)])
#
# cross_tab <- purrr::map(
#   exposurevars,
#   function(i) {
#     blabla$inputs$data %>%
#       group_by(.data[[i]]) %>%
#       summarise(outcome = sum(.data[[blabla$inputs$y]]),
#                 perstime = sum(.data[[obstimevar]])) %>%
#       rename(group = all_of(i)) %>%
#       mutate(group = as.character(group),
#              var = i)
#     }) %>%
#   bind_rows()
#
# heh <- blabla$inputs$data %>%
#   select(DIED, age_group) %>%
#   gtsummary::tbl_summary(by = DIED)
#
#
# blabla$inputs$data %>%
#   select(obstime, age_group) %>%
#   gtsummary::tbl_summary(by = age_group,
#                         statistic = list(everything() ~ "{sum}"))
#
#
#
# ## WIDE FORMAT
# ### use reference_row to filter for relevant row
# blabla <- linelist_cleaned %>%
#   select(DIED, gender_bin, gender, fever) %>%
#   gtsummary::tbl_uvregression(method = glm,
#                               y = DIED,
#                               method.args = list(family = binomial),
#                               exponentiate = TRUE,
#                               hide_n = TRUE)
#
# blabla2 <- linelist_cleaned %>%
#   select(DIED, gender_bin, gender, fever) %>%
#   gtsummary::tbl_uvregression(method = glm,
#                               y = DIED,
#                               method.args = list(family = binomial),
#                               exponentiate = TRUE,
#                               hide_n = TRUE,
#                               show_single_row = everything())
#
#
#
