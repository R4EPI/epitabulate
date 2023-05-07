
#' A {gtsummary} wrapper function for producing stratified univariate regression
#' and mantel-haenszel estimates
#'
#'@param data a data frame
#'@param y case variable
#'@param strata variable to stratify by
#'@param measure "OR", "RR", "IRR"
#'@param obstime  time under observation for each individual to use as an offset


##### THIS WILL PURRR FOR EACH EXPOSURE OVER THE INTERNAL FUNC
  ## which does one exposure at a time as below

## overall table for each exposure (tab_univ) - modify table body identify as crude
## purrr over each strata for each exposure (tab_univ) - modify table body to identify strata
## merge tables (bind rows?)
## sort by? necessary at this stage or can do at end?
## calculate the MH estimate with generalisation of zhian fx
## can either add_crosstab here optionally
    ## or they just add after
        ## (req adding class of this output to incl tbl_mh  and outupdating add_crosstab to allow tbl_mh)








# ## add a catch to make everything factors
# linelist_cleaned$gender_bin <- as.factor(linelist_cleaned$gender_bin)
#
# ### ?Need to filter so that all variables included have no missings
#     ## as done in line 227 of tab_univariate.R (or with tidyr below)
# linelist_cleaned <- linelist_cleaned %>% tidyr::drop_na(DIED,
#                                                         fever_bin,
#                                                         gender_bin,
#                                                         obstime)
#
#
#
#
# potato <- linelist_cleaned %>%
#   select(DIED, fever_bin, obstime)
#
#
#
# ### TODO: simplify these by defining the args with switch() to pass to tbl_uvreg
# if (measure == "OR") {
#   crude <- potato %>%
#     gtsummary::tbl_uvregression(method = glm,
#                               y = DIED,
#                               method.args = list(family = binomial),
#                               exponentiate = TRUE,
#                               hide_n = TRUE) %>%
#     add_crosstabs(wide = FALSE)
# }
#
# if (measure == "RR") {
#   crude <- potato %>%
#     gtsummary::tbl_uvregression(method = glm,
#                               y = DIED,
#                               method.args = list(family = poisson),
#                               exponentiate = TRUE,
#                               hide_n = TRUE) %>%
#     add_crosstabs(wide = FALSE)
# }
#
# if (measure == "IRR") {
#   crude <- potato %>%
#     gtsummary::tbl_uvregression(method = glm,
#                               y = DIED,
#                               include = fever_bin,
#                               method.args = list(family = poisson,
#                                                  offset = log(obstime)),
#                               exponentiate = TRUE,
#                               hide_n = TRUE) %>%
#     add_crosstabs(wide = FALSE)
# }
#
#
#
# # edit the table body (contents of table)
# crude <- gtsummary::modify_table_body(
#   crude,
#   # define a function to make two steps and avoid piping
#   fun = function(.x){
#     # remove cases from total obs to get control counts
#     .x <- dplyr::mutate(.x, strata = "Crude")
#     # move case and control counts before chara
#     .x <- dplyr::relocate(.x, strata, .before = variable)
#   }
# )
#
#
#
# ## ISSUE: levels not always present unless a factor (but also - peoples own responsibility)
#
# stratz <- purrr::map(levels(linelist_cleaned$gender_bin),
#            .f = function(i){
#              the_table <- linelist_cleaned %>%
#                filter(gender_bin == i) %>%
#                select(DIED, fever_bin, obstime)
#
#              if (measure == "OR") {
#                the_table <- the_table %>%
#                  gtsummary::tbl_uvregression(method = glm,
#                                              y = DIED,
#                                              method.args = list(family = binomial),
#                                              exponentiate = TRUE,
#                                              hide_n = TRUE) %>%
#                  add_crosstabs(wide = FALSE)
#              }
#
#              if (measure == "RR") {
#
#                the_table <- the_table %>%
#                  gtsummary::tbl_uvregression(method = glm,
#                                              y = DIED,
#                                              method.args = list(family = poisson),
#                                              exponentiate = TRUE,
#                                              hide_n = TRUE) %>%
#                  add_crosstabs(wide = FALSE)
#
#              }
#
#              if (measure == "IRR") {
#
#                the_table <- the_table %>%
#                  gtsummary::tbl_uvregression(method = glm,
#                                              y = DIED,
#                                              include = fever_bin,
#                                              method.args = list(family = poisson,
#                                                                 offset = log(obstime)),
#                                              exponentiate = TRUE,
#                                              hide_n = TRUE) %>%
#                  add_crosstabs(wide = FALSE)
#              }
#
#
#              # edit the table body (contents of table)
#              the_table <- gtsummary::modify_table_body(
#                the_table,
#                # define a function to make two steps and avoid piping
#                fun = function(.x){
#                  # remove cases from total obs to get control counts
#                  .x <- dplyr::mutate(.x, strata = i)
#                  # move case and control counts before chara
#                  .x <- dplyr::relocate(.x, strata, .before = variable)
#                }
#              )
#
#              the_table
#
#            }
# )
#
# collaps_stratz <- gtsummary::tbl_stack(stratz)
#
# combine_tab <- gtsummary::tbl_stack(list(crude, collaps_stratz))
#
# # change column names
# combine_tab <- gtsummary::modify_header(combine_tab, strata = "**Strata**")
#
#
#
# ##### prep data for mh estimates
# littler <- combine_tab$table_body
#
# littler <- littler %>%
#   filter(strata != "Crude", !header_row)
#
# stratalength <- length(unique(littler$strata))
#
# exposurelength <- littler$var_nlevels[1L]
#
# ##### p-value for the mental haenszel estimate
# woolf <- get_woolf_pval(littler, measure = measure)
#
#
# ##### mantel haenszel estimates
#
# mh <- get_mh(littler, measure, conf.level)
#
# ### put results in the table
# combine_tab <- gtsummary::modify_table_body(
#     combine_tab,
#     # define a function to make two steps and avoid piping
#     fun = function(.x){
#       # remove cases from total obs to get control counts
#       ## TODO: pull out the ifelse filter in to one obj
#       ## TODO: wtf is this mutate in a mutate
#       mutate(
#         .x <- dplyr::mutate(.x,
#                             mh_estimate = ifelse(strata == "Crude" & reference_row,
#                                                  mh$ratio, NA),
#                             mh_conf.low = ifelse(strata == "Crude" & reference_row,
#                                                  mh$lower, NA),
#                             mh_conf.high = ifelse(strata == "Crude" & reference_row,
#                                                  mh$upper, NA),
#                             woolf_p.value = ifelse(strata == "Crude" & reference_row,
#                                                    woolf$p.value, NA)
#                             )
#       )
#       # move case and control counts before chara
#       # .x <- dplyr::relocate(.x, strata, .before = variable)
#     }
# )
#



