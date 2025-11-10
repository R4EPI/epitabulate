Epitabulate
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![CRAN
status](https://www.r-pkg.org/badges/version/epitabulate)](https://CRAN.R-project.org/package=epitabulate)
[![R-CMD-check](https://github.com/R4EPI/epitabulate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/R4EPI/epitabulate/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/R4EPI/epitabulate/graph/badge.svg)](https://app.codecov.io/gh/R4EPI/epitabulate)
<!-- badges: end -->

The goal of {epitabulate} is to facilitate producing tables for
descriptive epidemiological analysis. This is a product of the R4EPIs
project; learn more at <https://r4epi.github.io/sitrep/>

## Installation

You can install {epitabulate} from CRAN:

``` r
install.packages("epitabulate")
```

<details>

<!--
NOTE: everything inside the details tag will be collapsed and effectively
hidden from the user
-->

<summary style="text-decoration: underline">

Click here for alternative installation options
</summary>

You can also install the in-development version from GitHub using the
{remotes} package (but there’s no guarantee that it will be stable):

``` r
# install.packages("remotes")
remotes::install_github("R4EPI/epitabulate") 
```

</details>

# Examples

Here is an example of the tables produced by {epitabulate} from a
randomly generated dataset.

``` r
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

# define how many people to simulate
n <- 1000

# generate a fake dataset 
linelist <- data.frame(
  sex = sample(c("male", "female"), n, replace = TRUE),
  age_group = sample(c("<5", "5-14", "15-29", "30-44", "45-59", "60+"), n, replace = TRUE, 
                     prob = c(0.1, 0.15, 0.25, 0.2, 0.15, 0.15)),
  fever = sample(c("yes", "no"), n, replace = TRUE, prob = c(0.3, 0.7)),
  death = sample(c("yes", "no"), n, replace = TRUE, prob = c(0.05, 0.95)),
  observation_time = round(runif(n, min = 1, max = 365), 1)  # days under observation
)


# turn yes/no variables into logical (TRUE/FALSE) variables 
linelist <- linelist |> 
  mutate(
    fever = grepl("yes", fever), 
    death = grepl("yes", death)
  )
```

## Quick proportions with conficence intervals

There are three functions that will provide quick statistics for
different rates based on binomial estimates of proportions from
`binom::binom.wilson()`

- `attack_rate()`
- `case_fatality_rate()`
- `mortality_rate()`

``` r
attack_rate(10, 50)
#>   cases population ar    lower    upper
#> 1    10         50 20 11.24375 33.03711
case_fatality_rate(2, 50)
#>   deaths population cfr    lower    upper
#> 1      2         50   4 1.103888 13.46009
mortality_rate(40, 50000)
#>   deaths population mortality per 10 000   lower    upper
#> 1     40      50000                    8 5.87591 10.89109
```

In addition, it’s possible to rapidly calculate Case fatality rate from
a linelist, stratified by different groups (e.g. gender):

``` r

case_fatality_rate_df(linelist, 
                      deaths = death, 
                      group = sex, 
                      add_total = TRUE, 
                      mergeCI = TRUE)
#> # A tibble: 4 × 5
#>   sex       deaths population    cfr ci         
#>   <fct>      <int>      <int>  <dbl> <chr>      
#> 1 female        30        508   5.91 (4.17-8.31)
#> 2 male          28        492   5.69 (3.97-8.10)
#> 3 (Missing)      0          0 NaN    (NaN-NaN)  
#> 4 Total         58       1000   5.8  (4.51-7.42)
```

It is also possible to add these proportions on to {gtsummary}
tabulations

``` r
cfr <- linelist |> 
    select(death) |> 
    gtsummary::tbl_summary(
      statistic = everything() ~ "{N}",
      label = death ~ "All participants"
    ) %>%
    add_cfr(deaths_var = "death")
#> There was a warning for variable "death"
#> ! cfr by strata is not currently available, ignoring `by` argument
```

    #> # A tibble: 1 × 5
    #>   `**Characteristic**` `**N = 1,000**` Deaths `CFR (%)` `95%CI`    
    #>   <chr>                <chr>           <chr>  <chr>     <chr>      
    #> 1 All participants     1,000           58     5.80      (4.51-7.42)

## Cross-tabulations for Odds / Risk / Incidence Rate Ratios

It is possible to add counts to {gtsummary} `tbl_uvregression`

``` r

## Odds ratios
or <- gtsummary::tbl_uvregression(linelist, 
                            method = glm, 
                            y = death, 
                            include = fever, 
                            method.args = list(family = binomial), 
                            exponentiate = TRUE, 
                            hide_n = TRUE) |> 
  add_crosstabs(wide = TRUE)
```

    #>   tbl_id1 variable var_label    var_type row_type header_row N_obs N_event    N
    #> 1       1    fever     fever dichotomous    label      FALSE  1000      58 1000
    #>   coefficients_type coefficients_label label      term var_class var_nlevels
    #> 1          logistic                 OR fever feverTRUE   logical           2
    #>         contrasts contrasts_type n_obs n_event_FALSE n_nonevent_FALSE
    #> 1 contr.treatment      treatment   288            11              277
    #>   n_event_TRUE n_nonevent_TRUE  estimate std.error statistic nevent          ci
    #> 1           47             665 0.5618711 0.3424902  -1.68321     58 0.27,  1.06
    #>    conf.low conf.high    p.value n_event_NA n_nonevent_NA reference_row
    #> 1 0.2732414  1.059923 0.09233451         NA            NA         FALSE

``` r
## Risk ratios 
rr <- gtsummary::tbl_uvregression(linelist, 
                            method = MASS::glm.nb, 
                            y = death,
                            include = fever, 
                            exponentiate = TRUE,
                            hide_n = TRUE) |> 
  add_crosstabs(wide = TRUE)
#> There was a warning constructing the model for variable "fever". See message
#> below.
#> ! Iterationsgrenze erreicht and Iterationsgrenze erreicht
```

    #>   tbl_id1 variable var_label    var_type row_type header_row N_obs N_event    N
    #> 1       1    fever     fever dichotomous    label      FALSE  1000      58 1000
    #>   coefficients_type coefficients_label label      term var_class var_nlevels
    #> 1           poisson                 RR fever feverTRUE   logical           2
    #>         contrasts contrasts_type exposure n_obs_FALSE n_event_FALSE n_obs_TRUE
    #> 1 contr.treatment      treatment      288         288            11        712
    #>   n_event_TRUE  estimate std.error statistic nevent          ci conf.low
    #> 1           47 0.5786052 0.3349472 -1.633496     58 0.28,  1.07 0.284962
    #>   conf.high   p.value n_obs_NA n_event_NA reference_row
    #> 1  1.073527 0.1023647       NA         NA         FALSE

``` r

## Incidence rate ratios 
irr <- gtsummary::tbl_uvregression(linelist, 
                            method = glm, 
                            y = death,
                            include = fever, 
                            method.args = list(family = poisson,
                                               offset = log(observation_time)),
                            exponentiate = TRUE,
                            hide_n = TRUE) |> 
  add_crosstabs(wide = TRUE)
```

    #>   tbl_id1 variable var_label    var_type row_type header_row N_obs N_event    N
    #> 1       1    fever     fever dichotomous    label      FALSE  1000      58 1000
    #>   coefficients_type coefficients_label label      term var_class var_nlevels
    #> 1           poisson                IRR fever feverTRUE   logical           2
    #>         contrasts contrasts_type n_obs exposure_FALSE n_event_FALSE
    #> 1 contr.treatment      treatment   288        51881.9            11
    #>   exposure_TRUE n_event_TRUE  estimate std.error statistic nevent          ci
    #> 1      131182.9           47 0.5917744 0.3349395 -1.566342     58 0.29,  1.10
    #>    conf.low conf.high   p.value exposure_NA n_event_NA reference_row
    #> 1 0.2914501   1.09795 0.1172686          NA         NA         FALSE

## Stratification - Cochran Mantel-Haenszel estimates

It is also possible to run stratified analysis to produce Cochran
Mantel-haenszel estimates.

``` r

cmh <- tbl_cmh(data = linelist,
        case = death,
        exposure = fever,
        strata = age_group, 
        measure = "OR") |>
  add_crosstabs()
```

    #> # A tibble: 21 × 10
    #>    `**Strata**` `**Characteristic**` `**Case (n)**` `**Control (n)**` `**OR**`
    #>    <chr>        <chr>                <chr>          <chr>             <chr>   
    #>  1 Crude        fever                <NA>           <NA>              <NA>    
    #>  2 <NA>         FALSE                47             665               <NA>    
    #>  3 <NA>         TRUE                 11             277               0.56    
    #>  4 <5           fever                <NA>           <NA>              <NA>    
    #>  5 <NA>         FALSE                1              76                <NA>    
    #>  6 <NA>         TRUE                 1              19                4.00    
    #>  7 15-29        fever                <NA>           <NA>              <NA>    
    #>  8 <NA>         FALSE                14             152               <NA>    
    #>  9 <NA>         TRUE                 2              60                0.36    
    #> 10 30-44        fever                <NA>           <NA>              <NA>    
    #> # ℹ 11 more rows
    #> # ℹ 5 more variables: `**95% CI**` <chr>, `**p-value**` <chr>,
    #> #   `**CMH estimate**` <chr>, `**95% CI**` <chr>, `**p-value**` <chr>
