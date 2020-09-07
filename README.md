Epibuffet
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/epibuffet)](https://CRAN.R-project.org/package=epibuffet)
status](https://ci.appveyor.com/api/project/status/github/zkamvar/epibuffet?branch=master&svg=true)](https://ci.appveyor.com/project/zkamvar/epibuffet)
[![Codecov test
coverage](https://codecov.io/gh/R4EPI/epibuffet/branch/master/graph/badge.svg)](https://codecov.io/gh/R4EPI/epibuffet?branch=master)
[![R build
status](https://github.com/R4EPI/epibuffet/workflows/R-CMD-check/badge.svg)](https://github.com/R4EPI/epibuffet/actions)
<!-- badges: end -->

The {epibuffet} package produces tables for descriptive epidemiological
analysis. It contains four functions:

  - `tab_linelist()` — Tabulate and describe counts of variables in a
    linelist
  - `tab_survey()` — Tabulate and describe counts of variables in a
    survey (with appropriate CIs)
  - `tab_univariate()` — Calculate Odds / Risk / Incidence Rate Ratios
    directly from a linelist
  - `data_frame_from_2x2()` — Creates a data frame from a 2x2 table for
    unambiguous interpretation

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

# Examples

Here is an example of the tables produced by {epibuffet} from a randomly
generated data set based on an MSF data dictionary for Measles data.

``` r
library(epidict)
library(matchmaker)
library(epikit)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(epibuffet)

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
#> # A tibble: 1,000 x 52
#>    seizure_episodes trimester croup dehydration_lev… residential_sta… previously_vacc…
#>    <fct>            <fct>     <fct> <fct>            <fct>            <fct>           
#>  1 Yes              <NA>      Yes   Unknown          Refugee          Yes - vaccinati…
#>  2 No               <NA>      No    Severe           Migrant          No              
#>  3 Yes              <NA>      Yes   Unknown          Refugee          Yes - verbal    
#>  4 No               <NA>      No    Severe           Migrant          No              
#>  5 Yes              <NA>      No    Some             Unspecified      Unsure          
#>  6 Yes              <NA>      Yes   None             Migrant          Yes - verbal    
#>  7 Yes              <NA>      No    Severe           Unspecified      Yes - vaccinati…
#>  8 No               3rd trim… No    Some             Resident         Yes - vaccinati…
#>  9 No               2nd trim… Yes   Some             Refugee          No              
#> 10 Yes              <NA>      No    Severe           Unspecified      Yes - vaccinati…
#> # … with 990 more rows, and 46 more variables: patient_origin_free_text <chr>, age_days <int>,
#> #   msf_involvement <fct>, nutrition_status_at_admission <fct>, fever <fct>, sex <fct>,
#> #   patient_origin <chr>, pregnancy_outcome_at_exit <fct>, prescribed_vitamin_a <fct>,
#> #   date_of_exit <date>, date_of_consultation_admission <date>, event_file_type <fct>,
#> #   residential_status_brief <fct>, other_eye_complications <fct>, treatment_facility_site <chr>,
#> #   prescribed_antibiotics <fct>, treatment_location <chr>, date_of_onset <date>,
#> #   exit_status <fct>, treatment_facility_name <lgl>, severe_oral_lesions <fct>, candidiasis <fct>,
#> #   time_to_death <fct>, malaria_rdt_at_admission <fct>, patient_facility_type <fct>,
#> #   previous_vaccine_doses_received <fct>, age_years <int>, arrival_date_in_area_if_3m <date>,
#> #   severity_of_illness <fct>, age_months <int>, date_of_last_vaccination <date>,
#> #   baby_born_with_complications <fct>, case_number <chr>, xerophthalmia <fct>, cough <fct>,
#> #   contact_history <fct>, detected_by <fct>, delivery_event <fct>, encephalitis <fct>,
#> #   nasal_discharge <fct>, acute_otitis_media <fct>, pregnant <fct>,
#> #   foetus_alive_at_admission <fct>, maculopapular_rash <fct>, pneumonia <fct>,
#> #   late_complications <fct>
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
#> 1 cough                   508             50.8    492            49.2
#> 2 nasal_discharge         497             49.7    503            50.3
#> 3 severe_oral_lesions     522             52.2    478            47.8
```

``` r
the_symptoms %>%
  epikit::rename_redundant("%" = "proportion") %>%
  epikit::augment_redundant("(n)" = "n") %>%
  knitr::kable()
```

| variable              | Yes (n) |    % | No (n) |    % |
| :-------------------- | ------: | ---: | -----: | ---: |
| cough                 |     508 | 50.8 |    492 | 49.2 |
| nasal\_discharge      |     497 | 49.7 |    503 | 50.3 |
| severe\_oral\_lesions |     522 | 52.2 |    478 | 47.8 |

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
#> # A tibble: 1,000 x 5
#>    pneumonia cough nasal_discharge oral_lesions contact
#>    <lgl>     <lgl> <lgl>           <lgl>        <lgl>  
#>  1 TRUE      TRUE  FALSE           FALSE        FALSE  
#>  2 TRUE      TRUE  TRUE            FALSE        FALSE  
#>  3 FALSE     TRUE  TRUE            TRUE         FALSE  
#>  4 TRUE      FALSE TRUE            FALSE        FALSE  
#>  5 FALSE     FALSE TRUE            FALSE        TRUE   
#>  6 TRUE      FALSE FALSE           FALSE        TRUE   
#>  7 FALSE     FALSE FALSE           TRUE         TRUE   
#>  8 FALSE     TRUE  TRUE            FALSE        TRUE   
#>  9 TRUE      FALSE TRUE            TRUE         TRUE   
#> 10 FALSE     TRUE  TRUE            TRUE         TRUE   
#> # … with 990 more rows

print(pxc <- with(symptoms_tf, table(pneumonia, cough)))
#>          cough
#> pneumonia FALSE TRUE
#>     FALSE   169  155
#>     TRUE    323  353
print(pxcxc <- with(symptoms_tf, table(pneumonia, cough, contact)))
#> , , contact = FALSE
#> 
#>          cough
#> pneumonia FALSE TRUE
#>     FALSE    79   70
#>     TRUE    165  179
#> 
#> , , contact = TRUE
#> 
#>          cough
#> pneumonia FALSE TRUE
#>     FALSE    90   85
#>     TRUE    158  174

data_frame_from_2x2(pxc)
#>   A_exp_cases B_exp_controls C_unexp_cases D_unexp_controls total_cases total_controls
#> 1         169            155           323              353         492            508
#>   total_exposed total_unexposed total
#> 1           324             676  1000
data_frame_from_2x2(pxcxc)
#>       A_exp_cases B_exp_controls C_unexp_cases D_unexp_controls total_cases total_controls
#> crude         169            155           323              353         492            508
#> FALSE          79             70           165              179         244            249
#> TRUE           90             85           158              174         248            259
#>       total_exposed total_unexposed total
#> crude           324             676  1000
#> FALSE           149             344   493
#> TRUE            175             332   507
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

|         | variable         | exposed cases | unexposed cases | exposed controls | unexposed controls | odds                | p.value |
| :------ | :--------------- | ------------: | --------------: | ---------------: | -----------------: | :------------------ | ------: |
| crude…1 | cough            |           353 |             323 |              155 |                169 | 1.192 (0.914–1.553) |   0.195 |
| crude…2 | nasal\_discharge |           350 |             326 |              147 |                177 | 1.293 (0.991–1.686) |   0.058 |
| crude…3 | oral\_lesions    |           346 |             330 |              176 |                148 | 0.882 (0.676–1.150) |   0.353 |
| crude…4 | contact          |           332 |             344 |              175 |                149 | 0.822 (0.630–1.072) |   0.147 |
