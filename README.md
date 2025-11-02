Epitabulate
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
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
#> 1 female        27        505   5.35 (3.70-7.67)
#> 2 male          17        495   3.43 (2.16-5.43)
#> 3 (Missing)      0          0 NaN    (NaN-NaN)  
#> 4 Total         44       1000   4.4  (3.29-5.86)
```

It is also possible to add these proportions on to {gtsummary}
tabulations

``` r
linelist |> 
    select(death) |> 
    gtsummary::tbl_summary(
      statistic = everything() ~ "{N}",
      label = death ~ "All participants"
    ) %>%
    add_cfr(deaths_var = "death")
#> There was a warning for variable "death"
#> ! cfr by strata is not currently available, ignoring `by` argument
```

<div id="xpyaljukks" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#xpyaljukks table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#xpyaljukks thead, #xpyaljukks tbody, #xpyaljukks tfoot, #xpyaljukks tr, #xpyaljukks td, #xpyaljukks th {
  border-style: none;
}
&#10;#xpyaljukks p {
  margin: 0;
  padding: 0;
}
&#10;#xpyaljukks .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#xpyaljukks .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#xpyaljukks .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#xpyaljukks .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#xpyaljukks .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#xpyaljukks .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xpyaljukks .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#xpyaljukks .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#xpyaljukks .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#xpyaljukks .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#xpyaljukks .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#xpyaljukks .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#xpyaljukks .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#xpyaljukks .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#xpyaljukks .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#xpyaljukks .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#xpyaljukks .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#xpyaljukks .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#xpyaljukks .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xpyaljukks .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#xpyaljukks .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#xpyaljukks .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#xpyaljukks .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xpyaljukks .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#xpyaljukks .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#xpyaljukks .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xpyaljukks .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xpyaljukks .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#xpyaljukks .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#xpyaljukks .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#xpyaljukks .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xpyaljukks .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#xpyaljukks .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xpyaljukks .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#xpyaljukks .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xpyaljukks .gt_left {
  text-align: left;
}
&#10;#xpyaljukks .gt_center {
  text-align: center;
}
&#10;#xpyaljukks .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#xpyaljukks .gt_font_normal {
  font-weight: normal;
}
&#10;#xpyaljukks .gt_font_bold {
  font-weight: bold;
}
&#10;#xpyaljukks .gt_font_italic {
  font-style: italic;
}
&#10;#xpyaljukks .gt_super {
  font-size: 65%;
}
&#10;#xpyaljukks .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#xpyaljukks .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#xpyaljukks .gt_indent_1 {
  text-indent: 5px;
}
&#10;#xpyaljukks .gt_indent_2 {
  text-indent: 10px;
}
&#10;#xpyaljukks .gt_indent_3 {
  text-indent: 15px;
}
&#10;#xpyaljukks .gt_indent_4 {
  text-indent: 20px;
}
&#10;#xpyaljukks .gt_indent_5 {
  text-indent: 25px;
}
&#10;#xpyaljukks .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#xpyaljukks div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_0"><span class='gt_from_md'><strong>N = 1,000</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Deaths"><span class='gt_from_md'>Deaths</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="CFR-(%)"><span class='gt_from_md'>CFR (%)</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="a95%CI"><span class='gt_from_md'>95%CI</span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">All participants</td>
<td headers="stat_0" class="gt_row gt_center">1,000</td>
<td headers="Deaths" class="gt_row gt_center">44</td>
<td headers="CFR (%)" class="gt_row gt_center">4.40</td>
<td headers="95%CI" class="gt_row gt_center">(3.29-5.86)</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="5"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>N</span></td>
    </tr>
  </tfoot>
</table>
</div>

## Cross-tabulations for Odds / Risk / Incidence Rate Ratios

It is possible to add counts to {gtsummary} `tbl_uvregression`

``` r


## Odds ratios
gtsummary::tbl_uvregression(linelist, 
                            method = glm, 
                            y = death, 
                            include = fever, 
                            method.args = list(family = binomial), 
                            exponentiate = TRUE, 
                            hide_n = TRUE) |> 
  add_crosstabs(wide = TRUE)
```

<div id="rzljhktzyx" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#rzljhktzyx table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#rzljhktzyx thead, #rzljhktzyx tbody, #rzljhktzyx tfoot, #rzljhktzyx tr, #rzljhktzyx td, #rzljhktzyx th {
  border-style: none;
}
&#10;#rzljhktzyx p {
  margin: 0;
  padding: 0;
}
&#10;#rzljhktzyx .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#rzljhktzyx .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#rzljhktzyx .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#rzljhktzyx .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#rzljhktzyx .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#rzljhktzyx .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rzljhktzyx .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#rzljhktzyx .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#rzljhktzyx .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#rzljhktzyx .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#rzljhktzyx .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#rzljhktzyx .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#rzljhktzyx .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#rzljhktzyx .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#rzljhktzyx .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#rzljhktzyx .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#rzljhktzyx .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#rzljhktzyx .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#rzljhktzyx .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rzljhktzyx .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#rzljhktzyx .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#rzljhktzyx .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#rzljhktzyx .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rzljhktzyx .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#rzljhktzyx .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#rzljhktzyx .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rzljhktzyx .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rzljhktzyx .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#rzljhktzyx .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#rzljhktzyx .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#rzljhktzyx .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rzljhktzyx .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#rzljhktzyx .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rzljhktzyx .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#rzljhktzyx .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rzljhktzyx .gt_left {
  text-align: left;
}
&#10;#rzljhktzyx .gt_center {
  text-align: center;
}
&#10;#rzljhktzyx .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#rzljhktzyx .gt_font_normal {
  font-weight: normal;
}
&#10;#rzljhktzyx .gt_font_bold {
  font-weight: bold;
}
&#10;#rzljhktzyx .gt_font_italic {
  font-style: italic;
}
&#10;#rzljhktzyx .gt_super {
  font-size: 65%;
}
&#10;#rzljhktzyx .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#rzljhktzyx .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#rzljhktzyx .gt_indent_1 {
  text-indent: 5px;
}
&#10;#rzljhktzyx .gt_indent_2 {
  text-indent: 10px;
}
&#10;#rzljhktzyx .gt_indent_3 {
  text-indent: 15px;
}
&#10;#rzljhktzyx .gt_indent_4 {
  text-indent: 20px;
}
&#10;#rzljhktzyx .gt_indent_5 {
  text-indent: 25px;
}
&#10;#rzljhktzyx .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#rzljhktzyx div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="n_event_FALSE"><span class='gt_from_md'><strong>Cases exposed (n)</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="n_nonevent_FALSE"><span class='gt_from_md'><strong>Controls exposed (n)</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="n_event_TRUE"><span class='gt_from_md'><strong>Cases unexposed (n)</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="n_nonevent_TRUE"><span class='gt_from_md'><strong>Controls unexposed (n)</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate"><span class='gt_from_md'><strong>OR</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="conf.low"><span class='gt_from_md'><strong>95% CI</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">fever</td>
<td headers="n_event_FALSE" class="gt_row gt_center">21</td>
<td headers="n_nonevent_FALSE" class="gt_row gt_center">269</td>
<td headers="n_event_TRUE" class="gt_row gt_center">23</td>
<td headers="n_nonevent_TRUE" class="gt_row gt_center">687</td>
<td headers="estimate" class="gt_row gt_center">2.33</td>
<td headers="conf.low" class="gt_row gt_center">1.26, 4.29</td>
<td headers="p.value" class="gt_row gt_center">0.006</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="8"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>

``` r


## Risk ratios 
gtsummary::tbl_uvregression(linelist, 
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

<div id="zeijinjdox" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#zeijinjdox table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#zeijinjdox thead, #zeijinjdox tbody, #zeijinjdox tfoot, #zeijinjdox tr, #zeijinjdox td, #zeijinjdox th {
  border-style: none;
}
&#10;#zeijinjdox p {
  margin: 0;
  padding: 0;
}
&#10;#zeijinjdox .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#zeijinjdox .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#zeijinjdox .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#zeijinjdox .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#zeijinjdox .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#zeijinjdox .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zeijinjdox .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#zeijinjdox .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#zeijinjdox .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#zeijinjdox .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#zeijinjdox .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#zeijinjdox .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#zeijinjdox .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#zeijinjdox .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#zeijinjdox .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#zeijinjdox .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#zeijinjdox .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#zeijinjdox .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#zeijinjdox .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zeijinjdox .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#zeijinjdox .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#zeijinjdox .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#zeijinjdox .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zeijinjdox .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#zeijinjdox .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#zeijinjdox .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zeijinjdox .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zeijinjdox .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#zeijinjdox .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#zeijinjdox .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#zeijinjdox .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zeijinjdox .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#zeijinjdox .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zeijinjdox .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#zeijinjdox .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zeijinjdox .gt_left {
  text-align: left;
}
&#10;#zeijinjdox .gt_center {
  text-align: center;
}
&#10;#zeijinjdox .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#zeijinjdox .gt_font_normal {
  font-weight: normal;
}
&#10;#zeijinjdox .gt_font_bold {
  font-weight: bold;
}
&#10;#zeijinjdox .gt_font_italic {
  font-style: italic;
}
&#10;#zeijinjdox .gt_super {
  font-size: 65%;
}
&#10;#zeijinjdox .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#zeijinjdox .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#zeijinjdox .gt_indent_1 {
  text-indent: 5px;
}
&#10;#zeijinjdox .gt_indent_2 {
  text-indent: 10px;
}
&#10;#zeijinjdox .gt_indent_3 {
  text-indent: 15px;
}
&#10;#zeijinjdox .gt_indent_4 {
  text-indent: 20px;
}
&#10;#zeijinjdox .gt_indent_5 {
  text-indent: 25px;
}
&#10;#zeijinjdox .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#zeijinjdox div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="n_obs_FALSE"><span class='gt_from_md'><strong>Total exposed (N)</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="n_event_FALSE"><span class='gt_from_md'><strong>Cases exposed (n)</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="n_obs_TRUE"><span class='gt_from_md'><strong>Total unexposed (N)</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="n_event_TRUE"><span class='gt_from_md'><strong>Cases unexposed (n)</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate"><span class='gt_from_md'><strong>RR</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="conf.low"><span class='gt_from_md'><strong>95% CI</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">fever</td>
<td headers="n_obs_FALSE" class="gt_row gt_center">290</td>
<td headers="n_event_FALSE" class="gt_row gt_center">21</td>
<td headers="n_obs_TRUE" class="gt_row gt_center">710</td>
<td headers="n_event_TRUE" class="gt_row gt_center">23</td>
<td headers="estimate" class="gt_row gt_center">2.24</td>
<td headers="conf.low" class="gt_row gt_center">1.23, 4.05</td>
<td headers="p.value" class="gt_row gt_center">0.008</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="8"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>RR = Risk Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>

``` r

## Incidence rate ratios 
gtsummary::tbl_uvregression(linelist, 
                            method = glm, 
                            y = death,
                            include = fever, 
                            method.args = list(family = poisson,
                                               offset = log(observation_time)),
                            exponentiate = TRUE,
                            hide_n = TRUE) |> 
  add_crosstabs(wide = TRUE)
```

<div id="ypwjlgqpns" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ypwjlgqpns table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ypwjlgqpns thead, #ypwjlgqpns tbody, #ypwjlgqpns tfoot, #ypwjlgqpns tr, #ypwjlgqpns td, #ypwjlgqpns th {
  border-style: none;
}
&#10;#ypwjlgqpns p {
  margin: 0;
  padding: 0;
}
&#10;#ypwjlgqpns .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#ypwjlgqpns .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ypwjlgqpns .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#ypwjlgqpns .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#ypwjlgqpns .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ypwjlgqpns .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ypwjlgqpns .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ypwjlgqpns .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#ypwjlgqpns .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#ypwjlgqpns .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ypwjlgqpns .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ypwjlgqpns .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#ypwjlgqpns .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ypwjlgqpns .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#ypwjlgqpns .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#ypwjlgqpns .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ypwjlgqpns .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ypwjlgqpns .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#ypwjlgqpns .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ypwjlgqpns .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#ypwjlgqpns .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ypwjlgqpns .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ypwjlgqpns .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ypwjlgqpns .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ypwjlgqpns .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ypwjlgqpns .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ypwjlgqpns .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ypwjlgqpns .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ypwjlgqpns .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ypwjlgqpns .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ypwjlgqpns .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ypwjlgqpns .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ypwjlgqpns .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ypwjlgqpns .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ypwjlgqpns .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ypwjlgqpns .gt_left {
  text-align: left;
}
&#10;#ypwjlgqpns .gt_center {
  text-align: center;
}
&#10;#ypwjlgqpns .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ypwjlgqpns .gt_font_normal {
  font-weight: normal;
}
&#10;#ypwjlgqpns .gt_font_bold {
  font-weight: bold;
}
&#10;#ypwjlgqpns .gt_font_italic {
  font-style: italic;
}
&#10;#ypwjlgqpns .gt_super {
  font-size: 65%;
}
&#10;#ypwjlgqpns .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ypwjlgqpns .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ypwjlgqpns .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ypwjlgqpns .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ypwjlgqpns .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ypwjlgqpns .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ypwjlgqpns .gt_indent_5 {
  text-indent: 25px;
}
&#10;#ypwjlgqpns .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#ypwjlgqpns div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="exposure_FALSE"><span class='gt_from_md'><strong>Total exposed (person-time)</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="n_event_FALSE"><span class='gt_from_md'><strong>Cases exposed (n)</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="exposure_TRUE"><span class='gt_from_md'><strong>Total unexposed (person-time)</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="n_event_TRUE"><span class='gt_from_md'><strong>Cases unexposed (n)</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate"><span class='gt_from_md'><strong>IRR</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="conf.low"><span class='gt_from_md'><strong>95% CI</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">fever</td>
<td headers="exposure_FALSE" class="gt_row gt_center">55239.4</td>
<td headers="n_event_FALSE" class="gt_row gt_center">21</td>
<td headers="exposure_TRUE" class="gt_row gt_center">129209.1</td>
<td headers="n_event_TRUE" class="gt_row gt_center">23</td>
<td headers="estimate" class="gt_row gt_center">2.14</td>
<td headers="conf.low" class="gt_row gt_center">1.17, 3.87</td>
<td headers="p.value" class="gt_row gt_center">0.012</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="8"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>IRR = Incidence Rate Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>

## Stratification - Cochran Mantel-Haenszel estimates

It is also possible to run stratified analysis to produce Cochran
Mantel-haenszel estimates.

``` r

tbl_cmh(data = linelist,
        case = death,
        exposure = fever,
        strata = age_group, 
        measure = "OR") |>
  add_crosstabs()
```

<div id="vrsxyxtynn" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#vrsxyxtynn table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#vrsxyxtynn thead, #vrsxyxtynn tbody, #vrsxyxtynn tfoot, #vrsxyxtynn tr, #vrsxyxtynn td, #vrsxyxtynn th {
  border-style: none;
}
&#10;#vrsxyxtynn p {
  margin: 0;
  padding: 0;
}
&#10;#vrsxyxtynn .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#vrsxyxtynn .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#vrsxyxtynn .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#vrsxyxtynn .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#vrsxyxtynn .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#vrsxyxtynn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#vrsxyxtynn .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#vrsxyxtynn .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#vrsxyxtynn .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#vrsxyxtynn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#vrsxyxtynn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#vrsxyxtynn .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#vrsxyxtynn .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#vrsxyxtynn .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#vrsxyxtynn .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#vrsxyxtynn .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#vrsxyxtynn .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#vrsxyxtynn .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#vrsxyxtynn .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vrsxyxtynn .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#vrsxyxtynn .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#vrsxyxtynn .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#vrsxyxtynn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vrsxyxtynn .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#vrsxyxtynn .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#vrsxyxtynn .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#vrsxyxtynn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vrsxyxtynn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#vrsxyxtynn .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#vrsxyxtynn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#vrsxyxtynn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#vrsxyxtynn .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#vrsxyxtynn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vrsxyxtynn .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#vrsxyxtynn .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vrsxyxtynn .gt_left {
  text-align: left;
}
&#10;#vrsxyxtynn .gt_center {
  text-align: center;
}
&#10;#vrsxyxtynn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#vrsxyxtynn .gt_font_normal {
  font-weight: normal;
}
&#10;#vrsxyxtynn .gt_font_bold {
  font-weight: bold;
}
&#10;#vrsxyxtynn .gt_font_italic {
  font-style: italic;
}
&#10;#vrsxyxtynn .gt_super {
  font-size: 65%;
}
&#10;#vrsxyxtynn .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#vrsxyxtynn .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#vrsxyxtynn .gt_indent_1 {
  text-indent: 5px;
}
&#10;#vrsxyxtynn .gt_indent_2 {
  text-indent: 10px;
}
&#10;#vrsxyxtynn .gt_indent_3 {
  text-indent: 15px;
}
&#10;#vrsxyxtynn .gt_indent_4 {
  text-indent: 20px;
}
&#10;#vrsxyxtynn .gt_indent_5 {
  text-indent: 25px;
}
&#10;#vrsxyxtynn .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#vrsxyxtynn div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stratifier"><span class='gt_from_md'><strong>Strata</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="n_event"><span class='gt_from_md'><strong>Case (n)</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="n_nonevent"><span class='gt_from_md'><strong>Control (n)</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate"><span class='gt_from_md'><strong>OR</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="conf.low"><span class='gt_from_md'><strong>95% CI</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="mh_estimate"><span class='gt_from_md'><strong>CMH estimate</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="mh_ci"><span class='gt_from_md'><strong>95% CI</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="woolf_p.value"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="stratifier" class="gt_row gt_center">Crude</td>
<td headers="label" class="gt_row gt_left">fever</td>
<td headers="n_event" class="gt_row gt_center"><br /></td>
<td headers="n_nonevent" class="gt_row gt_center"><br /></td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><br /></td>
<td headers="mh_estimate" class="gt_row gt_center"><br /></td>
<td headers="mh_ci" class="gt_row gt_center"><br /></td>
<td headers="woolf_p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="stratifier" class="gt_row gt_center"><br /></td>
<td headers="label" class="gt_row gt_left">    FALSE</td>
<td headers="n_event" class="gt_row gt_center">23</td>
<td headers="n_nonevent" class="gt_row gt_center">687</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td>
<td headers="mh_estimate" class="gt_row gt_center"><br /></td>
<td headers="mh_ci" class="gt_row gt_center"><br /></td>
<td headers="woolf_p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="stratifier" class="gt_row gt_center"><br /></td>
<td headers="label" class="gt_row gt_left">    TRUE</td>
<td headers="n_event" class="gt_row gt_center">21</td>
<td headers="n_nonevent" class="gt_row gt_center">269</td>
<td headers="estimate" class="gt_row gt_center">2.33</td>
<td headers="conf.low" class="gt_row gt_center">1.26, 4.29</td>
<td headers="p.value" class="gt_row gt_center">0.006</td>
<td headers="mh_estimate" class="gt_row gt_center">2.40</td>
<td headers="mh_ci" class="gt_row gt_center">1.30, 4.42</td>
<td headers="woolf_p.value" class="gt_row gt_center">0.9</td></tr>
    <tr><td headers="stratifier" class="gt_row gt_center">&lt;5</td>
<td headers="label" class="gt_row gt_left">fever</td>
<td headers="n_event" class="gt_row gt_center"><br /></td>
<td headers="n_nonevent" class="gt_row gt_center"><br /></td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><br /></td>
<td headers="mh_estimate" class="gt_row gt_center"><br /></td>
<td headers="mh_ci" class="gt_row gt_center"><br /></td>
<td headers="woolf_p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="stratifier" class="gt_row gt_center"><br /></td>
<td headers="label" class="gt_row gt_left">    FALSE</td>
<td headers="n_event" class="gt_row gt_center">1</td>
<td headers="n_nonevent" class="gt_row gt_center">58</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td>
<td headers="mh_estimate" class="gt_row gt_center"><br /></td>
<td headers="mh_ci" class="gt_row gt_center"><br /></td>
<td headers="woolf_p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="stratifier" class="gt_row gt_center"><br /></td>
<td headers="label" class="gt_row gt_left">    TRUE</td>
<td headers="n_event" class="gt_row gt_center">1</td>
<td headers="n_nonevent" class="gt_row gt_center">28</td>
<td headers="estimate" class="gt_row gt_center">2.07</td>
<td headers="conf.low" class="gt_row gt_center">0.08, 53.7</td>
<td headers="p.value" class="gt_row gt_center">0.6</td>
<td headers="mh_estimate" class="gt_row gt_center"><br /></td>
<td headers="mh_ci" class="gt_row gt_center"><br /></td>
<td headers="woolf_p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="stratifier" class="gt_row gt_center">15-29</td>
<td headers="label" class="gt_row gt_left">fever</td>
<td headers="n_event" class="gt_row gt_center"><br /></td>
<td headers="n_nonevent" class="gt_row gt_center"><br /></td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><br /></td>
<td headers="mh_estimate" class="gt_row gt_center"><br /></td>
<td headers="mh_ci" class="gt_row gt_center"><br /></td>
<td headers="woolf_p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="stratifier" class="gt_row gt_center"><br /></td>
<td headers="label" class="gt_row gt_left">    FALSE</td>
<td headers="n_event" class="gt_row gt_center">9</td>
<td headers="n_nonevent" class="gt_row gt_center">183</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td>
<td headers="mh_estimate" class="gt_row gt_center"><br /></td>
<td headers="mh_ci" class="gt_row gt_center"><br /></td>
<td headers="woolf_p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="stratifier" class="gt_row gt_center"><br /></td>
<td headers="label" class="gt_row gt_left">    TRUE</td>
<td headers="n_event" class="gt_row gt_center">6</td>
<td headers="n_nonevent" class="gt_row gt_center">56</td>
<td headers="estimate" class="gt_row gt_center">2.18</td>
<td headers="conf.low" class="gt_row gt_center">0.70, 6.31</td>
<td headers="p.value" class="gt_row gt_center">0.2</td>
<td headers="mh_estimate" class="gt_row gt_center"><br /></td>
<td headers="mh_ci" class="gt_row gt_center"><br /></td>
<td headers="woolf_p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="stratifier" class="gt_row gt_center">30-44</td>
<td headers="label" class="gt_row gt_left">fever</td>
<td headers="n_event" class="gt_row gt_center"><br /></td>
<td headers="n_nonevent" class="gt_row gt_center"><br /></td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><br /></td>
<td headers="mh_estimate" class="gt_row gt_center"><br /></td>
<td headers="mh_ci" class="gt_row gt_center"><br /></td>
<td headers="woolf_p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="stratifier" class="gt_row gt_center"><br /></td>
<td headers="label" class="gt_row gt_left">    FALSE</td>
<td headers="n_event" class="gt_row gt_center">5</td>
<td headers="n_nonevent" class="gt_row gt_center">146</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td>
<td headers="mh_estimate" class="gt_row gt_center"><br /></td>
<td headers="mh_ci" class="gt_row gt_center"><br /></td>
<td headers="woolf_p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="stratifier" class="gt_row gt_center"><br /></td>
<td headers="label" class="gt_row gt_left">    TRUE</td>
<td headers="n_event" class="gt_row gt_center">3</td>
<td headers="n_nonevent" class="gt_row gt_center">65</td>
<td headers="estimate" class="gt_row gt_center">1.35</td>
<td headers="conf.low" class="gt_row gt_center">0.27, 5.66</td>
<td headers="p.value" class="gt_row gt_center">0.7</td>
<td headers="mh_estimate" class="gt_row gt_center"><br /></td>
<td headers="mh_ci" class="gt_row gt_center"><br /></td>
<td headers="woolf_p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="stratifier" class="gt_row gt_center">45-59</td>
<td headers="label" class="gt_row gt_left">fever</td>
<td headers="n_event" class="gt_row gt_center"><br /></td>
<td headers="n_nonevent" class="gt_row gt_center"><br /></td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><br /></td>
<td headers="mh_estimate" class="gt_row gt_center"><br /></td>
<td headers="mh_ci" class="gt_row gt_center"><br /></td>
<td headers="woolf_p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="stratifier" class="gt_row gt_center"><br /></td>
<td headers="label" class="gt_row gt_left">    FALSE</td>
<td headers="n_event" class="gt_row gt_center">4</td>
<td headers="n_nonevent" class="gt_row gt_center">91</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td>
<td headers="mh_estimate" class="gt_row gt_center"><br /></td>
<td headers="mh_ci" class="gt_row gt_center"><br /></td>
<td headers="woolf_p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="stratifier" class="gt_row gt_center"><br /></td>
<td headers="label" class="gt_row gt_left">    TRUE</td>
<td headers="n_event" class="gt_row gt_center">4</td>
<td headers="n_nonevent" class="gt_row gt_center">42</td>
<td headers="estimate" class="gt_row gt_center">2.17</td>
<td headers="conf.low" class="gt_row gt_center">0.49, 9.57</td>
<td headers="p.value" class="gt_row gt_center">0.3</td>
<td headers="mh_estimate" class="gt_row gt_center"><br /></td>
<td headers="mh_ci" class="gt_row gt_center"><br /></td>
<td headers="woolf_p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="stratifier" class="gt_row gt_center">5-14</td>
<td headers="label" class="gt_row gt_left">fever</td>
<td headers="n_event" class="gt_row gt_center"><br /></td>
<td headers="n_nonevent" class="gt_row gt_center"><br /></td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><br /></td>
<td headers="mh_estimate" class="gt_row gt_center"><br /></td>
<td headers="mh_ci" class="gt_row gt_center"><br /></td>
<td headers="woolf_p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="stratifier" class="gt_row gt_center"><br /></td>
<td headers="label" class="gt_row gt_left">    FALSE</td>
<td headers="n_event" class="gt_row gt_center">2</td>
<td headers="n_nonevent" class="gt_row gt_center">123</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td>
<td headers="mh_estimate" class="gt_row gt_center"><br /></td>
<td headers="mh_ci" class="gt_row gt_center"><br /></td>
<td headers="woolf_p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="stratifier" class="gt_row gt_center"><br /></td>
<td headers="label" class="gt_row gt_left">    TRUE</td>
<td headers="n_event" class="gt_row gt_center">2</td>
<td headers="n_nonevent" class="gt_row gt_center">46</td>
<td headers="estimate" class="gt_row gt_center">2.67</td>
<td headers="conf.low" class="gt_row gt_center">0.31, 22.8</td>
<td headers="p.value" class="gt_row gt_center">0.3</td>
<td headers="mh_estimate" class="gt_row gt_center"><br /></td>
<td headers="mh_ci" class="gt_row gt_center"><br /></td>
<td headers="woolf_p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="stratifier" class="gt_row gt_center">60+</td>
<td headers="label" class="gt_row gt_left">fever</td>
<td headers="n_event" class="gt_row gt_center"><br /></td>
<td headers="n_nonevent" class="gt_row gt_center"><br /></td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><br /></td>
<td headers="mh_estimate" class="gt_row gt_center"><br /></td>
<td headers="mh_ci" class="gt_row gt_center"><br /></td>
<td headers="woolf_p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="stratifier" class="gt_row gt_center"><br /></td>
<td headers="label" class="gt_row gt_left">    FALSE</td>
<td headers="n_event" class="gt_row gt_center">2</td>
<td headers="n_nonevent" class="gt_row gt_center">86</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td>
<td headers="mh_estimate" class="gt_row gt_center"><br /></td>
<td headers="mh_ci" class="gt_row gt_center"><br /></td>
<td headers="woolf_p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="stratifier" class="gt_row gt_center"><br /></td>
<td headers="label" class="gt_row gt_left">    TRUE</td>
<td headers="n_event" class="gt_row gt_center">5</td>
<td headers="n_nonevent" class="gt_row gt_center">32</td>
<td headers="estimate" class="gt_row gt_center">6.72</td>
<td headers="conf.low" class="gt_row gt_center">1.37, 48.6</td>
<td headers="p.value" class="gt_row gt_center">0.027</td>
<td headers="mh_estimate" class="gt_row gt_center"><br /></td>
<td headers="mh_ci" class="gt_row gt_center"><br /></td>
<td headers="woolf_p.value" class="gt_row gt_center"><br /></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="10"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>
