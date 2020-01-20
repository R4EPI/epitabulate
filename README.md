Epibuffet
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/epibuffet)](https://CRAN.R-project.org/package=epibuffet)
[![Travis build
status](https://travis-ci.org/R4EPI/epibuffet.svg?branch=master)](https://travis-ci.org/R4EPI/epibuffet)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/zkamvar/epibuffet?branch=master&svg=true)](https://ci.appveyor.com/project/zkamvar/epibuffet)
[![Codecov test
coverage](https://codecov.io/gh/R4EPI/epibuffet/branch/master/graph/badge.svg)](https://codecov.io/gh/R4EPI/epibuffet?branch=master)
<!-- badges: end -->

The {epibuffet} package produces tables for descriptive epidemiological
analysis. It contains three functions:

  - `tab_linelist()` — Tabulate and describe counts of variables in a
    linelist
  - `tab_survey()` — Tabulate and describe counts of variables in a
    survey (with appropriate CIs)
  - `tab_univariate()` — Calculate Odds / Risk / Incidence Rate Ratios
    directly from a linelist

## Installation

{epibuffet} is currently under development, but you can keep up-to-date
by installing it from the R4EPIs drat repository:

``` r
# install.packages("drat")
drat::addRepo("R4EPI")
install.packages("epibuffet")
```

You can also install the in-development version from GitHub using the
{remotes} package (but there’s no guarantee that it will be stable):

``` r
# install.packages("remotes")
remotes::install_github("R4EPI/epibuffet") 
```

</details>

## Example

Here is an example of the tables produced by {epibuffet} from a randomly
generated data set based on an MSF data dictionary for Measles data.

``` r
library(msfdict)
library(matchmaker)
library(epikit)
library(dplyr)
library(epibuffet)

linelist <- msfdict::gen_data("Measles", numcases = 1000)
measles_dict <- msfdict::msf_dict("Measles", compact = FALSE)

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
#> # A tibble: 1,000 x 52
#>    seizure_episodes trimester croup dehydration_lev… residential_sta…
#>    <fct>            <fct>     <fct> <fct>            <fct>           
#>  1 No               <NA>      No    Severe           Migrant         
#>  2 Yes              <NA>      Yes   None             Migrant         
#>  3 Yes              <NA>      Yes   Severe           Resident        
#>  4 Yes              <NA>      No    Unknown          Refugee         
#>  5 Yes              <NA>      No    None             Resident        
#>  6 No               <NA>      Yes   Severe           Unspecified     
#>  7 Yes              3rd trim… No    Severe           Internally Disp…
#>  8 No               <NA>      No    None             Refugee         
#>  9 Yes              <NA>      Yes   Unknown          Internally Disp…
#> 10 No               <NA>      No    None             Refugee         
#> # … with 990 more rows, and 47 more variables:
#> #   previously_vaccinated <fct>, patient_origin_free_text <chr>,
#> #   age_days <int>, msf_involvement <fct>,
#> #   nutrition_status_at_admission <fct>, fever <fct>, sex <fct>,
#> #   patient_origin <chr>, pregnancy_outcome_at_exit <fct>,
#> #   prescribed_vitamin_a <fct>, date_of_exit <date>,
#> #   date_of_consultation_admission <date>, event_file_type <fct>,
#> #   residential_status_brief <fct>, other_eye_complications <fct>,
#> #   treatment_facility_site <chr>, prescribed_antibiotics <fct>,
#> #   treatment_location <chr>, date_of_onset <date>, exit_status <fct>,
#> #   treatment_facility_name <lgl>, severe_oral_lesions <fct>,
#> #   candidiasis <fct>, time_to_death <fct>,
#> #   malaria_rdt_at_admission <fct>, patient_facility_type <fct>,
#> #   previous_vaccine_doses_received <fct>, age_years <int>,
#> #   arrival_date_in_area_if_3m <date>, severity_of_illness <fct>,
#> #   age_months <int>, date_of_last_vaccination <date>,
#> #   baby_born_with_complications <fct>, case_number <chr>,
#> #   xerophthalmia <fct>, cough <fct>, contact_history <fct>,
#> #   detected_by <fct>, delivery_event <fct>, encephalitis <fct>,
#> #   nasal_discharge <fct>, acute_otitis_media <fct>, pregnant <fct>,
#> #   foetus_alive_at_admission <fct>, maculopapular_rash <fct>,
#> #   pneumonia <fct>, late_complications <fct>
```

## Quick tabulations

``` r
the_symptoms <- tab_linelist(linelist_clean,
  cough, nasal_discharge, severe_oral_lesions,
  transpose = "value"
) 
the_symptoms
#> # A tibble: 3 x 5
#>   variable            `Yes n` `Yes proportion` `No n` `No proportion`
#>   <fct>                 <dbl>            <dbl>  <dbl>           <dbl>
#> 1 cough                   519             51.9    481            48.1
#> 2 nasal_discharge         491             49.1    509            50.9
#> 3 severe_oral_lesions     494             49.4    506            50.6
```

``` r
the_symptoms %>%
  epikit::rename_redundant("%" = "proportion") %>%
  epikit::augment_redundant("(n)" = "n") %>%
  knitr::kable()
```

| variable              | Yes (n) |    % | No (n) |    % |
| :-------------------- | ------: | ---: | -----: | ---: |
| cough                 |     519 | 51.9 |    481 | 48.1 |
| nasal\_discharge      |     491 | 49.1 |    509 | 50.9 |
| severe\_oral\_lesions |     494 | 49.4 |    506 | 50.6 |

## Odds / Risk / Incidence Rate Ratios

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
#> # A tibble: 1,000 x 5
#>    pneumonia cough nasal_discharge oral_lesions contact
#>    <lgl>     <lgl> <lgl>           <lgl>        <lgl>  
#>  1 FALSE     FALSE FALSE           FALSE        TRUE   
#>  2 TRUE      TRUE  FALSE           FALSE        FALSE  
#>  3 TRUE      TRUE  FALSE           FALSE        FALSE  
#>  4 FALSE     FALSE TRUE            TRUE         FALSE  
#>  5 TRUE      TRUE  FALSE           FALSE        TRUE   
#>  6 TRUE      FALSE FALSE           TRUE         FALSE  
#>  7 TRUE      FALSE TRUE            TRUE         TRUE   
#>  8 TRUE      FALSE TRUE            TRUE         FALSE  
#>  9 FALSE     FALSE TRUE            FALSE        TRUE   
#> 10 TRUE      FALSE TRUE            TRUE         TRUE   
#> # … with 990 more rows

tu <- tab_univariate(symptoms_tf, 
  outcome = pneumonia, 
  cough, nasal_discharge, oral_lesions, contact,
  mergeCI = TRUE,
  extend_output = FALSE
)
tu %>% select(-est_type) %>% knitr::kable(digits = 3)
```

| variable         | exp\_cases | unexp\_cases | exp\_controls | unexp\_controls | est\_ci             | p.value |
| :--------------- | ---------: | -----------: | ------------: | --------------: | :------------------ | ------: |
| cough            |        340 |          343 |           179 |             138 | 0.764 (0.585–0.999) |   0.049 |
| nasal\_discharge |        338 |          345 |           153 |             164 | 1.050 (0.804–1.371) |   0.719 |
| oral\_lesions    |        325 |          358 |           169 |             148 | 0.795 (0.609–1.038) |   0.092 |
| contact          |        341 |          342 |           159 |             158 | 0.991 (0.759–1.293) |   0.946 |
