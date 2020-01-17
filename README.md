
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epibuffet

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/epibuffet)](https://CRAN.R-project.org/package=epibuffet)
<!-- badges: end -->

The goal of epibuffet is to …

## Installation

You can install the released version of epibuffet from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("epibuffet")
```

## Example

The {epibuffet} package produces tables for descriptive epidemiological
analysis. It contains four functions:

  - `tab_linelist()` — Tabulate and describe counts of variables in a
    linelist
  - `tab_survey()` — Tabulate and describe counts of variables in a
    survey (with appropriate CIs)
  - `tab_univariate()` — Calculate Odds / Risk / Incidence Rate Ratios
    directly from a linelist

<!-- end list -->

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
#>  1 Yes              <NA>      No    Some             Internally Disp…
#>  2 No               1st trim… Yes   None             Internally Disp…
#>  3 Yes              <NA>      No    None             Refugee         
#>  4 No               <NA>      No    None             Internally Disp…
#>  5 Yes              <NA>      No    Unknown          Migrant         
#>  6 No               <NA>      Yes   None             Resident        
#>  7 No               <NA>      Yes   Severe           Migrant         
#>  8 Yes              <NA>      No    None             Refugee         
#>  9 Yes              <NA>      Yes   None             Refugee         
#> 10 No               <NA>      No    Unknown          Internally Disp…
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
#> 1 cough                   497             49.7    503            50.3
#> 2 nasal_discharge         504             50.4    496            49.6
#> 3 severe_oral_lesions     527             52.7    473            47.3
```

``` r
the_symptoms %>%
  epikit::rename_redundant("%" = "proportion") %>%
  epikit::augment_redundant("(n)" = "n") %>%
  knitr::kable()
```

| variable              | Yes (n) |    % | No (n) |    % |
| :-------------------- | ------: | ---: | -----: | ---: |
| cough                 |     497 | 49.7 |    503 | 50.3 |
| nasal\_discharge      |     504 | 50.4 |    496 | 49.6 |
| severe\_oral\_lesions |     527 | 52.7 |    473 | 47.3 |

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
#>  1 TRUE      TRUE  FALSE           TRUE         FALSE  
#>  2 FALSE     FALSE FALSE           TRUE         TRUE   
#>  3 TRUE      FALSE TRUE            TRUE         TRUE   
#>  4 FALSE     FALSE TRUE            TRUE         TRUE   
#>  5 FALSE     TRUE  FALSE           TRUE         FALSE  
#>  6 TRUE      FALSE TRUE            FALSE        TRUE   
#>  7 FALSE     TRUE  FALSE           FALSE        FALSE  
#>  8 FALSE     TRUE  FALSE           FALSE        TRUE   
#>  9 FALSE     TRUE  FALSE           TRUE         FALSE  
#> 10 FALSE     TRUE  TRUE            FALSE        FALSE  
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
| cough            |        314 |          344 |           183 |             159 | 0.793 (0.610–1.030) |   0.082 |
| nasal\_discharge |        334 |          324 |           170 |             172 | 1.043 (0.803–1.354) |   0.752 |
| oral\_lesions    |        342 |          316 |           185 |             157 | 0.918 (0.707–1.194) |   0.525 |
| contact          |        338 |          320 |           170 |             172 | 1.069 (0.823–1.388) |   0.618 |
