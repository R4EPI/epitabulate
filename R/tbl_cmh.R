#' Stratified Odds Ratios
#'
#' @param data a data frame
#' @param case case variable
#' @param exposure exposure variable
#' @param strata stratifying variables
#' @param label list of formulas specifying labels for strata
#' @param estimate_fun function styling the odds ratios.
#' Default is `gtsummary::style_ratio`
#' @param overall_label string indicating the un-stratified ORs.
#' Default is `"Crude"`
#'
#' @return gtsummary table
#' @export
#' @author Daniel D. Sjoberg
#' @importFrom dplyr %>% vars starts_with ends_with all_of desc
#' @importFrom rlang .data .env expr
#'
#' @examples
#' tbl_cmh_ex1 <-
#'   gtsummary::trial %>%
#'   dplyr::select(grade, stage, death, response) %>%
#'   dplyr::rename(case = response, exposure = death) %>%
#'   tbl_cmh(case = case, exposure = exposure, strata = c(stage, grade))

tbl_cmh <- function(data, case, exposure, strata,
                    label = NULL,
                    estimate_fun = gtsummary::style_ratio,
                    overall_label = "Crude") {
  # checking inputs ------------------------------------------------------------
  if (!is.data.frame(data)) rlang::abort("`data=` must be a data frame")
  if (!rlang::is_string(overall_label)) rlang::abort("`overall_label=` must be a string")
  if (!rlang::is_function(estimate_fun)) rlang::abort("`estimate_fun=` must be a function")

  # converting selectors to character names ------------------------------------
  case <- dplyr::select(data, {{ case }}) %>% names()
  exposure <- dplyr::select(data, {{ exposure }}) %>% names()
  strata <- dplyr::select(data, {{ strata }}) %>% names()
  label <-
    broom.helpers::.formula_list_to_named_list(
      label,
      data = select(data, all_of(strata))
    )

  # removing missing case and exposure observations ----------------------------
  data <- data %>% tidyr::drop_na(all_of(c(case, exposure)))

  # @aspina7 add check that case and exposure are dichotomous
  # @aspina7 add check that strata are all categorical variables
  # @aspina7 add check that the overall_label value is not a level of any strata

  # construct cross tabs for each stratifying variable -------------------------
  tbl_crosstabs <-
    strata %>%
    purrr::map(~.stratified_crosstab(data = data, case = case, exposure = exposure,
                                     strata = .x, label = label,
                                     overall_label = overall_label)) %>%
    gtsummary::tbl_stack()

  # calculate ORs --------------------------------------------------------------
  df_unstratified_or <-
    .calculate_unstrat_or(data = data, strata = strata, case = case, exposure = exposure,
                          overall_label = overall_label, estimate_fun = estimate_fun)

  df_stratified_or <-
    .calculate_cmh_or(data = data, strata = strata, case = case, exposure = exposure,
                      overall_label = overall_label, estimate_fun = estimate_fun)

  # merge ORs with cross table -------------------------------------------------
  tbl_crosstabs %>%
    gtsummary::modify_table_body(
      ~.x %>%
        dplyr::left_join(df_unstratified_or, by = c("variable", "row_type", "overall_row", "label")) %>%
        dplyr::left_join(df_stratified_or, by = c("variable", "row_type", "overall_row", "label"))
    ) %>%
    gtsummary::modify_header(
      list(or = "**Odds Ratio**",
           cmh_or = "**CMH Odds Ratio**",
           p.value = "**p-value**")
    ) %>%
    gtsummary::modify_fmt_fun(p.value ~ gtsummary::style_pvalue)
}


# this function constructs a single stratified cross tab WITH overall row
.stratified_crosstab <- function(data, case, exposure, strata,
                                 label, overall_label) {
  data %>%
    # drop missing values before calculation
    tidyr::drop_na(all_of(c(strata, case, exposure))) %>%
    # build cross tab stratified by case status
    gtsummary::tbl_strata(
      strata = all_of(case),
      .tbl_fun =
        ~.x %>%
        gtsummary::tbl_cross(all_of(strata), all_of(exposure),
                             margin = "row", label = label[strata]) %>%
        gtsummary::modify_header(list(stat_1 ~ "**Not Exposed**",
                                      stat_2 ~ "**Exposed**"))
    ) %>%
    gtsummary::modify_spanning_header(
      list(vars(starts_with("stat_") & ends_with("_1")) ~ "**Control**",
           vars(starts_with("stat_") & ends_with("_2")) ~ "**Case**")
    ) %>%
    # updating internal `x$table_body` to make the overall row a level within the stratum
    gtsummary::modify_table_body(
      ~.x %>%
        mutate(
          overall_row = .data$variable %in% "..total..",
          row_type = ifelse(.data$variable %in% "..total..",
                            "level", .data$row_type),
          label = ifelse(.data$variable %in% "..total..",
                         overall_label, .data$label),
          variable = ifelse(.data$variable %in% "..total..",
                            strata, .data$variable),
        ) %>%
        dplyr::arrange(desc(.data$row_type == "label"),
                       desc(.data$label == overall_label))
    ) %>%
    # italicize the overall row label
    gtsummary::modify_table_styling(
      columns = "label",
      rows = !!expr(.data$label == !!overall_label),
      text_format = "italic"
    )
}


# this function calculates the ORs for the Crude and strtfied variables
# returns a tibble ready to be merged into the stratfied cross tab
.calculate_unstrat_or <- function(data, strata, case, exposure,
                                  overall_label, estimate_fun) {
  df_or_stratified <-
    strata %>%
    purrr::map_dfr(
      ~data %>%
        tidyr::drop_na(all_of(c(.x, case, exposure))) %>%
        dplyr::select(dplyr::all_of(c(.x, exposure, case))) %>%
        tidyr::nest(data = -dplyr::all_of(.x)) %>%
        dplyr::mutate(
          overall_row = FALSE,
          variable = .x,
          label = as.character(!!rlang::sym(.x)),
          row_type = "level",
          or = purrr::map_chr(
            data,
            ~with(.x, effectsize::oddsratio(!!rlang::sym(exposure), !!rlang::sym(case))) %>%
              as.data.frame() %>%
              dplyr::mutate_at(dplyr::vars(.data$Odds_ratio, .data$CI_low, .data$CI_high), estimate_fun) %>%
              dplyr::mutate(or = stringr::str_glue("{Odds_ratio} ({CI_low}, {CI_high})")) %>%
              dplyr::pull(or)
          )
        ) %>%
        dplyr::select(.data$variable, .data$row_type, .data$overall_row, .data$label, .data$or)
    )

  df_or_overall <-
    strata %>%
    purrr::map_dfr(
      ~data %>%
        dplyr::select(dplyr::all_of(c(.x, exposure, case))) %>%
        tidyr::drop_na(all_of(c(.x, case, exposure))) %>%
        {effectsize::oddsratio(.[[exposure]], .[[case]])} %>%
        as.data.frame() %>%
        dplyr::mutate_at(dplyr::vars(.data$Odds_ratio, .data$CI_low, .data$CI_high), estimate_fun) %>%
        dplyr::mutate(
          or = stringr::str_glue("{Odds_ratio} ({CI_low}, {CI_high})"),
          overall_row = TRUE,
          variable = .x,
          label = overall_label,
          row_type = "level"
        ) %>%
        dplyr::select(.data$variable, .data$row_type, .data$overall_row, .data$label, .data$or)
    )

  # return tibble of results (ready to be merged with primary tibble)
  dplyr::bind_rows(
    df_or_stratified, df_or_overall
  )
}

.calculate_cmh_or <- function(data, strata, case, exposure,
                              overall_label, estimate_fun) {
  strata %>%
    purrr::map_dfr(
      ~stats::mantelhaen.test(data[[case]], data[[exposure]], data[[.x]]) %>%
        broom::tidy() %>%
        dplyr::mutate_at(dplyr::vars(estimate, conf.low, conf.high), ~estimate_fun(.)) %>%
        dplyr::mutate(
          overall_row = TRUE,
          variable = .x,
          label = overall_label,
          row_type = "level",
          cmh_or = glue::glue("{estimate} ({conf.high}, {conf.low})")
        ) %>%
        dplyr::select(.data$variable, .data$row_type, .data$overall_row,
                      .data$label, .data$cmh_or, .data$p.value)
    )
}
