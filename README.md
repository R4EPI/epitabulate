Epitabulate
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/epitabulate)](https://CRAN.R-project.org/package=epitabulate)
[![Codecov test
coverage](https://codecov.io/gh/R4EPI/epitabulate/branch/master/graph/badge.svg)](https://codecov.io/gh/R4EPI/epitabulate?branch=master)
[![R build
status](https://github.com/R4EPI/epitabulate/workflows/R-CMD-check/badge.svg)](https://github.com/R4EPI/epitabulate/actions)
<!-- badges: end -->

The {epitabulate} package produces tables for descriptive
epidemiological analysis. It contains four functions:

-   `tab_linelist()` — Tabulate and describe counts of variables in a
    linelist
-   `tab_survey()` — Tabulate and describe counts of variables in a
    survey (with appropriate CIs)
-   `tab_univariate()` — Calculate Odds / Risk / Incidence Rate Ratios
    directly from a linelist
-   `data_frame_from_2x2()` — Creates a data frame from a 2x2 table for
    unambiguous interpretation

## Installation

{epitabulate} is currently under development, but you can keep
up-to-date by installing it from the R4EPIs drat repository:

``` r
# install.packages("drat")
drat::addRepo("R4EPI")
install.packages("epitabulate")
```

You can also install the in-development version from GitHub using the
{remotes} package (but there’s no guarantee that it will be stable):

``` r
# install.packages("remotes")
remotes::install_github("R4EPI/epitabulate") 
```

# Examples

Here is an example of the tables produced by {epitabulate} from a
randomly generated data set based on an MSF data dictionary for Measles
data.

``` r
library(epidict)
library(matchmaker)
library(epikit)
library(dplyr)
#> 
#> Attache Paket: 'dplyr'
#> Die folgenden Objekte sind maskiert von 'package:stats':
#> 
#>     filter, lag
#> Die folgenden Objekte sind maskiert von 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(epitabulate)

linelist <- epidict::gen_data("Measles", numcases = 1000, org = "MSF")
measles_dict <- epidict::msf_dict("Measles", compact = FALSE)

# Cleaning linelist data
linelist_clean <- matchmaker::match_df(
  x = linelist,
  dictionary = measles_dict,
  from = "option_code",
  to = "option_name",
  by = "data_element_shortname",
  order = "option_order_in_set"
)
linelist_clean
#> # A tibble: 1,000 × 52
#>    case_number date_of_c…¹ patie…² patie…³ age_y…⁴ age_m…⁵ age_d…⁶ sex   pregn…⁷
#>    <chr>       <date>      <fct>   <chr>     <int>   <int>   <int> <fct> <fct>  
#>  1 A1          2018-02-17  Outpat… Villag…      27      NA      NA Unkn… Not ap…
#>  2 A2          2018-02-24  Outpat… Villag…      11      NA      NA Unkn… Not ap…
#>  3 A3          2018-03-31  Inpati… Villag…      13      NA      NA Unkn… Not ap…
#>  4 A4          2018-02-15  Outpat… Villag…      NA       4      NA Fema… Not ap…
#>  5 A5          2018-04-13  Inpati… Villag…      23      NA      NA Unkn… Not ap…
#>  6 A6          2018-02-04  Inpati… Villag…      78      NA      NA Fema… Not ap…
#>  7 A7          2018-01-27  Inpati… Villag…      47      NA      NA Unkn… Not ap…
#>  8 A8          2018-01-26  Inpati… Villag…      32      NA      NA Unkn… Not ap…
#>  9 A9          2018-01-05  Inpati… Villag…      54      NA      NA Fema… Not cu…
#> 10 A10         2018-02-21  Inpati… Villag…       4      NA      NA Fema… Not cu…
#> # … with 990 more rows, 43 more variables: trimester <fct>,
#> #   foetus_alive_at_admission <fct>, exit_status <fct>, date_of_exit <date>,
#> #   time_to_death <fct>, pregnancy_outcome_at_exit <fct>,
#> #   baby_born_with_complications <fct>, previously_vaccinated <fct>,
#> #   previous_vaccine_doses_received <fct>, detected_by <fct>,
#> #   msf_involvement <fct>, residential_status <fct>,
#> #   residential_status_brief <fct>, date_of_last_vaccination <date>, …
#> # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names
```

## Quick tabulations

``` r
the_symptoms <- tab_linelist(linelist_clean,
  cough, nasal_discharge, severe_oral_lesions,
  transpose = "value"
) 
the_symptoms
#> # A tibble: 3 × 5
#>   variable            `Yes n` `Yes proportion` `No n` `No proportion`
#>   <fct>                 <dbl>            <dbl>  <dbl>           <dbl>
#> 1 cough                   501             50.1    499            49.9
#> 2 nasal_discharge         501             50.1    499            49.9
#> 3 severe_oral_lesions     487             48.7    513            51.3
```

``` r
the_symptoms %>%
  epikit::rename_redundant("%" = "proportion") %>%
  epikit::augment_redundant("(n)" = "n") %>%
  knitr::kable()
```

| variable            | Yes (n) |    % | No (n) |    % |
|:--------------------|--------:|-----:|-------:|-----:|
| cough               |     501 | 50.1 |    499 | 49.9 |
| nasal_discharge     |     501 | 50.1 |    499 | 49.9 |
| severe_oral_lesions |     487 | 48.7 |    513 | 51.3 |

# 2x2 tables

In R, creating 2x2 tables is as simple as using the function `table()`,
but unfortunately, it can be difficult to interpret the values of these
tables because the dimensions are often flipped around for different
analyses. The function `data_frame_from_2x2()` will present these values
labeled unambiguously.

``` r
symptoms_tf <- linelist_clean %>%
  dplyr::transmute(
    pneumonia = grepl("Yes", pneumonia),
    cough = grepl("Yes", cough),
    nasal_discharge = grepl("Yes", nasal_discharge),
    oral_lesions = grepl("Yes", severe_oral_lesions),
    contact = grepl("known case", contact_history)
  ) 

symptoms_tf
#> # A tibble: 1,000 × 5
#>    pneumonia cough nasal_discharge oral_lesions contact
#>    <lgl>     <lgl> <lgl>           <lgl>        <lgl>  
#>  1 FALSE     TRUE  FALSE           FALSE        TRUE   
#>  2 TRUE      FALSE FALSE           TRUE         FALSE  
#>  3 TRUE      TRUE  FALSE           FALSE        TRUE   
#>  4 TRUE      FALSE TRUE            FALSE        TRUE   
#>  5 FALSE     FALSE FALSE           FALSE        FALSE  
#>  6 TRUE      TRUE  FALSE           TRUE         FALSE  
#>  7 TRUE      FALSE TRUE            FALSE        FALSE  
#>  8 TRUE      TRUE  FALSE           TRUE         TRUE   
#>  9 TRUE      FALSE TRUE            FALSE        TRUE   
#> 10 TRUE      TRUE  TRUE            FALSE        FALSE  
#> # … with 990 more rows
#> # ℹ Use `print(n = ...)` to see more rows

print(pxc <- with(symptoms_tf, table(pneumonia, cough)))
#>          cough
#> pneumonia FALSE TRUE
#>     FALSE   152  171
#>     TRUE    347  330
print(pxcxc <- with(symptoms_tf, table(pneumonia, cough, contact)))
#> , , contact = FALSE
#> 
#>          cough
#> pneumonia FALSE TRUE
#>     FALSE    76   86
#>     TRUE    168  170
#> 
#> , , contact = TRUE
#> 
#>          cough
#> pneumonia FALSE TRUE
#>     FALSE    76   85
#>     TRUE    179  160

data_frame_from_2x2(pxc)
#>   A_exp_cases B_exp_controls C_unexp_cases D_unexp_controls total_cases
#> 1         152            171           347              330         499
#>   total_controls total_exposed total_unexposed total
#> 1            501           323             677  1000
data_frame_from_2x2(pxcxc)
#>       A_exp_cases B_exp_controls C_unexp_cases D_unexp_controls total_cases
#> crude         152            171           347              330         499
#> FALSE          76             86           168              170         244
#> TRUE           76             85           179              160         255
#>       total_controls total_exposed total_unexposed total
#> crude            501           323             677  1000
#> FALSE            256           162             338   500
#> TRUE             245           161             339   500
```

## Odds / Risk / Incidence Rate Ratios

``` r
tu <- tab_univariate(symptoms_tf, 
  outcome = pneumonia, 
  cough, nasal_discharge, oral_lesions, contact,
  mergeCI = TRUE,
  extend_output = FALSE,
  measure = "OR"
)
tu %>% 
  select(-est_type) %>% 
  epikit::augment_redundant("exposed " = "exp_") %>%
  rename(odds = est_ci) %>%
  knitr::kable(digits = 3)
```

|         | variable        | exposed cases | unexposed cases | exposed controls | unexposed controls | odds                | p.value |
|:--------|:----------------|--------------:|----------------:|-----------------:|-------------------:|:--------------------|--------:|
| crude…1 | cough           |           330 |             347 |              171 |                152 | 0.845 (0.648–1.102) |   0.215 |
| crude…2 | nasal_discharge |           348 |             329 |              153 |                170 | 1.175 (0.901–1.532) |   0.233 |
| crude…3 | oral_lesions    |           346 |             331 |              141 |                182 | 1.349 (1.034–1.761) |   0.027 |
| crude…4 | contact         |           339 |             338 |              161 |                162 | 1.009 (0.774–1.316) |   0.946 |
