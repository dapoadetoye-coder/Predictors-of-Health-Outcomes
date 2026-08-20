Predictors of Health Outcomes: A Linear Regression Analysis of Lifestyle
and Demographic Factors
================
Dapo
2026-08-05

## Objectives

This analysis aims to answer the following questions: 1. Which lifestyle
and demographic factors are most strongly associated with Health Score
2. Can a linear regression model reliably predict Health Score from
these factors

## Methodology

Data was cleaned and explored descriptively, followed by bivariate
visualizations of each predictor against Health Score. A multiple linear
regression model was fit using all available predictors, refined via
stepwise selection, and evaluated against standard regression
assumptions(linearity, homoscedasticity, normality, multicollinearity,
and influential points).

Introduction

``` r
knitr::opts_chunk$set(echo = TRUE, fig.path = "Health-R-markdown_files/figure-gfm")
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.2.1     ✔ readr     2.2.0
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.3     ✔ tibble    3.3.1
    ## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
    ## ✔ purrr     1.2.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(gtExtras)
```

    ## Loading required package: gt

``` r
dataset <- read_csv("C:/Users/Adedapo/Documents/synthetic_health_data.csv")
```

    ## Rows: 1000 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (8): Age, BMI, Exercise_Frequency, Diet_Quality, Sleep_Hours, Smoking_St...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
dataset %>% 
  head(10) %>% 
  gt()
```



``` r
glimpse(dataset)
```

    ## Rows: 1,000
    ## Columns: 8
    ## $ Age                 <dbl> 45.96057, 38.34083, 47.77226, 58.27636, 37.19016, …
    ## $ BMI                 <dbl> 31.99678, 29.62317, 25.29815, 21.76532, 28.49112, …
    ## $ Exercise_Frequency  <dbl> 5, 6, 5, 2, 2, 2, 5, 3, 4, 4, 1, 4, 4, 2, 2, 5, 1,…
    ## $ Diet_Quality        <dbl> 55.40327, 41.83836, 76.90495, 49.75677, 44.21874, …
    ## $ Sleep_Hours         <dbl> 7.300359, 7.012419, 6.028641, 5.802714, 7.912548, …
    ## $ Smoking_Status      <dbl> 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1,…
    ## $ Alcohol_Consumption <dbl> 2.8347071, 7.1995168, 4.0979438, 3.6493772, 2.8397…
    ## $ Health_Score        <dbl> 70.54212, 57.24464, 96.33372, 61.32178, 67.17589, …

The data set consists of 1000 observations across 8 variables: Age, BMI,
Excecise_Frequency, Diet_Quality, Sleep_Hours, Smoking_status,
Alcohol_Consumption, and the outcome, Health_Score. Package Tidyverse
was loaded for data cleaning, organization and visualization. Package
gtExtras was loaded to produce visually apealing tables.

Data Cleaning

``` r
library(naniar)
  miss_var_summary(dataset)
```

    ## # A tibble: 8 × 3
    ##   variable            n_miss pct_miss
    ##   <chr>                <int>    <num>
    ## 1 Age                      0        0
    ## 2 BMI                      0        0
    ## 3 Exercise_Frequency       0        0
    ## 4 Diet_Quality             0        0
    ## 5 Sleep_Hours              0        0
    ## 6 Smoking_Status           0        0
    ## 7 Alcohol_Consumption      0        0
    ## 8 Health_Score             0        0

Missing data check with naniar::miss_var summary() confirmed zero
missing values across every variable, so no imputation was needed.

``` r
dataset_edit <- dataset %>% 
  mutate(Alcohol_Consumption = if_else(Alcohol_Consumption <0, 0, Alcohol_Consumption)) %>% 
  mutate(Smoking_Status = factor(Smoking_Status, levels = c("0", "1"), labels = c("Non-smoker", "Smoker"))) %>% 
  mutate(Exercise_Frequency = factor(Exercise_Frequency, levels = c("0", "1", "2", "3", "4", "5", "6"), labels = c("None", "Once a week", "Twice a week", "Thrice a week", "Four times a week", "Five times a week", "Six times a week")))
```

Alcohol_Consumption contained some invalid negative entries, these were
corrected to 0, Since Alcohol Consumption cannot be a negative figure.
Smoking_status was converted to a factor, 0 was named Non-smoker, while
1 was Smoker. Exercise Frequency was also converted to a Factor and was
named appropriately for frequency of exercise weekly.

``` r
summary_table <- dataset_edit %>% 
  summarise(
    across(c(Health_Score, Age, Diet_Quality, BMI, Sleep_Hours, Alcohol_Consumption),
           list(Mean = ~mean(.x, na.rm = TRUE),
                Median = ~median(.x, na.rm = TRUE),
                SD = ~sd(.x, na.rm = TRUE),
                Min = ~min(.x, na.rm = TRUE),
                Max = ~max(.x, na.rm = TRUE)))) %>% 
  pivot_longer(everything(),
               names_to = c("Variable", "Statistic"),
               names_pattern = "^(.*)_(Mean|Median|SD|Min|Max)$") %>% 
  pivot_wider(names_from = Statistic,
              values_from = value)
  summary_table %>% 
    gt() %>% 
    fmt_number(columns = where(is.numeric),
               decimals = 2)
```

<div id="rchwwmiiaa" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#rchwwmiiaa table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#rchwwmiiaa thead, #rchwwmiiaa tbody, #rchwwmiiaa tfoot, #rchwwmiiaa tr, #rchwwmiiaa td, #rchwwmiiaa th {
  border-style: none;
}
&#10;#rchwwmiiaa p {
  margin: 0;
  padding: 0;
}
&#10;#rchwwmiiaa .gt_table {
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
&#10;#rchwwmiiaa .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#rchwwmiiaa .gt_title {
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
&#10;#rchwwmiiaa .gt_subtitle {
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
&#10;#rchwwmiiaa .gt_heading {
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
&#10;#rchwwmiiaa .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rchwwmiiaa .gt_col_headings {
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
&#10;#rchwwmiiaa .gt_col_heading {
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
&#10;#rchwwmiiaa .gt_column_spanner_outer {
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
&#10;#rchwwmiiaa .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#rchwwmiiaa .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#rchwwmiiaa .gt_column_spanner {
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
&#10;#rchwwmiiaa .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#rchwwmiiaa .gt_group_heading {
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
&#10;#rchwwmiiaa .gt_empty_group_heading {
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
&#10;#rchwwmiiaa .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#rchwwmiiaa .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#rchwwmiiaa .gt_row {
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
&#10;#rchwwmiiaa .gt_stub {
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
&#10;#rchwwmiiaa .gt_stub_row_group {
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
&#10;#rchwwmiiaa .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#rchwwmiiaa .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#rchwwmiiaa .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rchwwmiiaa .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#rchwwmiiaa .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#rchwwmiiaa .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rchwwmiiaa .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rchwwmiiaa .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#rchwwmiiaa .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#rchwwmiiaa .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#rchwwmiiaa .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rchwwmiiaa .gt_footnotes {
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
&#10;#rchwwmiiaa .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rchwwmiiaa .gt_sourcenotes {
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
&#10;#rchwwmiiaa .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rchwwmiiaa .gt_left {
  text-align: left;
}
&#10;#rchwwmiiaa .gt_center {
  text-align: center;
}
&#10;#rchwwmiiaa .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#rchwwmiiaa .gt_font_normal {
  font-weight: normal;
}
&#10;#rchwwmiiaa .gt_font_bold {
  font-weight: bold;
}
&#10;#rchwwmiiaa .gt_font_italic {
  font-style: italic;
}
&#10;#rchwwmiiaa .gt_super {
  font-size: 65%;
}
&#10;#rchwwmiiaa .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#rchwwmiiaa .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#rchwwmiiaa .gt_indent_1 {
  text-indent: 5px;
}
&#10;#rchwwmiiaa .gt_indent_2 {
  text-indent: 10px;
}
&#10;#rchwwmiiaa .gt_indent_3 {
  text-indent: 15px;
}
&#10;#rchwwmiiaa .gt_indent_4 {
  text-indent: 20px;
}
&#10;#rchwwmiiaa .gt_indent_5 {
  text-indent: 25px;
}
&#10;#rchwwmiiaa .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#rchwwmiiaa div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Variable">Variable</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Mean">Mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Median">Median</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="SD">SD</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Min">Min</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Max">Max</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Variable" class="gt_row gt_left">Health_Score</td>
<td headers="Mean" class="gt_row gt_right">85.48</td>
<td headers="Median" class="gt_row gt_right">87.50</td>
<td headers="SD" class="gt_row gt_right">13.63</td>
<td headers="Min" class="gt_row gt_right">29.11</td>
<td headers="Max" class="gt_row gt_right">100.00</td></tr>
    <tr><td headers="Variable" class="gt_row gt_left">Age</td>
<td headers="Mean" class="gt_row gt_right">40.23</td>
<td headers="Median" class="gt_row gt_right">40.30</td>
<td headers="SD" class="gt_row gt_right">11.75</td>
<td headers="Min" class="gt_row gt_right">1.10</td>
<td headers="Max" class="gt_row gt_right">86.23</td></tr>
    <tr><td headers="Variable" class="gt_row gt_left">Diet_Quality</td>
<td headers="Mean" class="gt_row gt_right">69.95</td>
<td headers="Median" class="gt_row gt_right">69.98</td>
<td headers="SD" class="gt_row gt_right">14.97</td>
<td headers="Min" class="gt_row gt_right">19.91</td>
<td headers="Max" class="gt_row gt_right">110.27</td></tr>
    <tr><td headers="Variable" class="gt_row gt_left">BMI</td>
<td headers="Mean" class="gt_row gt_right">25.35</td>
<td headers="Median" class="gt_row gt_right">25.32</td>
<td headers="SD" class="gt_row gt_right">4.99</td>
<td headers="Min" class="gt_row gt_right">10.30</td>
<td headers="Max" class="gt_row gt_right">40.97</td></tr>
    <tr><td headers="Variable" class="gt_row gt_left">Sleep_Hours</td>
<td headers="Mean" class="gt_row gt_right">6.97</td>
<td headers="Median" class="gt_row gt_right">6.99</td>
<td headers="SD" class="gt_row gt_right">1.52</td>
<td headers="Min" class="gt_row gt_right">2.43</td>
<td headers="Max" class="gt_row gt_right">11.64</td></tr>
    <tr><td headers="Variable" class="gt_row gt_left">Alcohol_Consumption</td>
<td headers="Mean" class="gt_row gt_right">3.14</td>
<td headers="Median" class="gt_row gt_right">3.06</td>
<td headers="SD" class="gt_row gt_right">1.97</td>
<td headers="Min" class="gt_row gt_right">0.00</td>
<td headers="Max" class="gt_row gt_right">11.11</td></tr>
  </tbody>
  &#10;</table>
</div>

Descriptive statistical analysis was performed to summarize the
distribution and variability of the numerical variables using the mean,
median, standard deviation, minimum, and maximum. The minimum health
score is 29.11 while the maximum is 100. Alcohol consumption has a
minimum of 0 and a maximum of 11.11. All variables have similar mean and
median which suggests that the distributions are generally not strongly
skewed.

``` r
dataset_edit %>% 
    count(Smoking_Status) %>%
    mutate(Percentage = n/sum(n)*100) %>% 
  gt()
```

<div id="oryfqfybjv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#oryfqfybjv table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#oryfqfybjv thead, #oryfqfybjv tbody, #oryfqfybjv tfoot, #oryfqfybjv tr, #oryfqfybjv td, #oryfqfybjv th {
  border-style: none;
}
&#10;#oryfqfybjv p {
  margin: 0;
  padding: 0;
}
&#10;#oryfqfybjv .gt_table {
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
&#10;#oryfqfybjv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#oryfqfybjv .gt_title {
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
&#10;#oryfqfybjv .gt_subtitle {
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
&#10;#oryfqfybjv .gt_heading {
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
&#10;#oryfqfybjv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#oryfqfybjv .gt_col_headings {
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
&#10;#oryfqfybjv .gt_col_heading {
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
&#10;#oryfqfybjv .gt_column_spanner_outer {
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
&#10;#oryfqfybjv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#oryfqfybjv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#oryfqfybjv .gt_column_spanner {
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
&#10;#oryfqfybjv .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#oryfqfybjv .gt_group_heading {
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
&#10;#oryfqfybjv .gt_empty_group_heading {
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
&#10;#oryfqfybjv .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#oryfqfybjv .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#oryfqfybjv .gt_row {
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
&#10;#oryfqfybjv .gt_stub {
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
&#10;#oryfqfybjv .gt_stub_row_group {
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
&#10;#oryfqfybjv .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#oryfqfybjv .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#oryfqfybjv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oryfqfybjv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#oryfqfybjv .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#oryfqfybjv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#oryfqfybjv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oryfqfybjv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#oryfqfybjv .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#oryfqfybjv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#oryfqfybjv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#oryfqfybjv .gt_footnotes {
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
&#10;#oryfqfybjv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oryfqfybjv .gt_sourcenotes {
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
&#10;#oryfqfybjv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oryfqfybjv .gt_left {
  text-align: left;
}
&#10;#oryfqfybjv .gt_center {
  text-align: center;
}
&#10;#oryfqfybjv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#oryfqfybjv .gt_font_normal {
  font-weight: normal;
}
&#10;#oryfqfybjv .gt_font_bold {
  font-weight: bold;
}
&#10;#oryfqfybjv .gt_font_italic {
  font-style: italic;
}
&#10;#oryfqfybjv .gt_super {
  font-size: 65%;
}
&#10;#oryfqfybjv .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#oryfqfybjv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#oryfqfybjv .gt_indent_1 {
  text-indent: 5px;
}
&#10;#oryfqfybjv .gt_indent_2 {
  text-indent: 10px;
}
&#10;#oryfqfybjv .gt_indent_3 {
  text-indent: 15px;
}
&#10;#oryfqfybjv .gt_indent_4 {
  text-indent: 20px;
}
&#10;#oryfqfybjv .gt_indent_5 {
  text-indent: 25px;
}
&#10;#oryfqfybjv .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#oryfqfybjv div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Smoking_Status">Smoking_Status</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="n">n</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Percentage">Percentage</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Smoking_Status" class="gt_row gt_center">Non-smoker</td>
<td headers="n" class="gt_row gt_right">501</td>
<td headers="Percentage" class="gt_row gt_right">50.1</td></tr>
    <tr><td headers="Smoking_Status" class="gt_row gt_center">Smoker</td>
<td headers="n" class="gt_row gt_right">499</td>
<td headers="Percentage" class="gt_row gt_right">49.9</td></tr>
  </tbody>
  &#10;</table>
</div>

``` r
dataset_edit %>% 
    count(Exercise_Frequency) %>% 
    mutate(Percentage = n/sum(n)*100) %>% 
  gt()
```

<div id="mdvokgrwsv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#mdvokgrwsv table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#mdvokgrwsv thead, #mdvokgrwsv tbody, #mdvokgrwsv tfoot, #mdvokgrwsv tr, #mdvokgrwsv td, #mdvokgrwsv th {
  border-style: none;
}
&#10;#mdvokgrwsv p {
  margin: 0;
  padding: 0;
}
&#10;#mdvokgrwsv .gt_table {
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
&#10;#mdvokgrwsv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#mdvokgrwsv .gt_title {
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
&#10;#mdvokgrwsv .gt_subtitle {
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
&#10;#mdvokgrwsv .gt_heading {
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
&#10;#mdvokgrwsv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mdvokgrwsv .gt_col_headings {
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
&#10;#mdvokgrwsv .gt_col_heading {
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
&#10;#mdvokgrwsv .gt_column_spanner_outer {
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
&#10;#mdvokgrwsv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#mdvokgrwsv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#mdvokgrwsv .gt_column_spanner {
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
&#10;#mdvokgrwsv .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#mdvokgrwsv .gt_group_heading {
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
&#10;#mdvokgrwsv .gt_empty_group_heading {
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
&#10;#mdvokgrwsv .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#mdvokgrwsv .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#mdvokgrwsv .gt_row {
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
&#10;#mdvokgrwsv .gt_stub {
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
&#10;#mdvokgrwsv .gt_stub_row_group {
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
&#10;#mdvokgrwsv .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#mdvokgrwsv .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#mdvokgrwsv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mdvokgrwsv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#mdvokgrwsv .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#mdvokgrwsv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mdvokgrwsv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mdvokgrwsv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#mdvokgrwsv .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#mdvokgrwsv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#mdvokgrwsv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mdvokgrwsv .gt_footnotes {
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
&#10;#mdvokgrwsv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mdvokgrwsv .gt_sourcenotes {
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
&#10;#mdvokgrwsv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mdvokgrwsv .gt_left {
  text-align: left;
}
&#10;#mdvokgrwsv .gt_center {
  text-align: center;
}
&#10;#mdvokgrwsv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#mdvokgrwsv .gt_font_normal {
  font-weight: normal;
}
&#10;#mdvokgrwsv .gt_font_bold {
  font-weight: bold;
}
&#10;#mdvokgrwsv .gt_font_italic {
  font-style: italic;
}
&#10;#mdvokgrwsv .gt_super {
  font-size: 65%;
}
&#10;#mdvokgrwsv .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#mdvokgrwsv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#mdvokgrwsv .gt_indent_1 {
  text-indent: 5px;
}
&#10;#mdvokgrwsv .gt_indent_2 {
  text-indent: 10px;
}
&#10;#mdvokgrwsv .gt_indent_3 {
  text-indent: 15px;
}
&#10;#mdvokgrwsv .gt_indent_4 {
  text-indent: 20px;
}
&#10;#mdvokgrwsv .gt_indent_5 {
  text-indent: 25px;
}
&#10;#mdvokgrwsv .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#mdvokgrwsv div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Exercise_Frequency">Exercise_Frequency</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="n">n</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Percentage">Percentage</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Exercise_Frequency" class="gt_row gt_center">None</td>
<td headers="n" class="gt_row gt_right">161</td>
<td headers="Percentage" class="gt_row gt_right">16.1</td></tr>
    <tr><td headers="Exercise_Frequency" class="gt_row gt_center">Once a week</td>
<td headers="n" class="gt_row gt_right">139</td>
<td headers="Percentage" class="gt_row gt_right">13.9</td></tr>
    <tr><td headers="Exercise_Frequency" class="gt_row gt_center">Twice a week</td>
<td headers="n" class="gt_row gt_right">147</td>
<td headers="Percentage" class="gt_row gt_right">14.7</td></tr>
    <tr><td headers="Exercise_Frequency" class="gt_row gt_center">Thrice a week</td>
<td headers="n" class="gt_row gt_right">149</td>
<td headers="Percentage" class="gt_row gt_right">14.9</td></tr>
    <tr><td headers="Exercise_Frequency" class="gt_row gt_center">Four times a week</td>
<td headers="n" class="gt_row gt_right">141</td>
<td headers="Percentage" class="gt_row gt_right">14.1</td></tr>
    <tr><td headers="Exercise_Frequency" class="gt_row gt_center">Five times a week</td>
<td headers="n" class="gt_row gt_right">134</td>
<td headers="Percentage" class="gt_row gt_right">13.4</td></tr>
    <tr><td headers="Exercise_Frequency" class="gt_row gt_center">Six times a week</td>
<td headers="n" class="gt_row gt_right">129</td>
<td headers="Percentage" class="gt_row gt_right">12.9</td></tr>
  </tbody>
  &#10;</table>
</div>

Descriptive analysis of the categorical variables: Smoking status, 50.1%
of the participants are Non-smokers, representing slightly more than
half, while 49.9% are smokers Exercise frequency, 16.1% of the
participants do not exercise, representing the highest percentage, while
the lowest is six times weekly, represented by 12.9% of the population.

``` r
dataset_age <- dataset_edit %>% 
  ggplot(aes(Age, Health_Score))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  labs(title = "Relationship between Age and Health Score", x = "Age", y = "Health Score")
dataset_age
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Health-R-markdown_files/figure-gfmunnamed-chunk-7-1.png)<!-- -->

There is a negative relationship between Age and Health Score, The older
people are, the lesser the Health Score

``` r
dataset_bmi <- dataset_edit %>% 
  ggplot(aes(BMI, Health_Score))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  labs(title = "Relationship between BMI and Health Score", x = "BMI", y = "Health Score")
dataset_bmi
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Health-R-markdown_files/figure-gfmunnamed-chunk-8-1.png)<!-- -->

There is a strong negative relationship between BMI and Health Score,
the higher the BMI, the lower the Health Score

``` r
dataset_diet <- dataset %>% 
  ggplot(aes(Diet_Quality, Health_Score))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  labs(title = "Relationship between Diet Quality and Health Score", x = "Diet Quality", y = "Health Score")
dataset_diet  
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Health-R-markdown_files/figure-gfmunnamed-chunk-9-1.png)<!-- -->

There is a strong positive relationship between Diet Quality and Health
Score. Hence, people with better diet tend to be healthier

``` r
dataset_sleep <- dataset %>% 
  ggplot(aes(Sleep_Hours, Health_Score))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  labs(title = "Relationship between Sleep Hours and Health Score", x = "Sleep Hours", y = "Health Score")
dataset_sleep
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Health-R-markdown_files/figure-gfmunnamed-chunk-10-1.png)<!-- -->

A positive relationship exists between Sleep Hours and Health Score,
More sleep is associated with a higher health score

``` r
dataset_alcohol <- dataset_edit %>% 
  ggplot(aes(Alcohol_Consumption, Health_Score))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  labs(title = "Relationship between Alcohol Consumption and Health Score", x = "Alcohol Consumption", y = "Health Score")
dataset_alcohol
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Health-R-markdown_files/figure-gfmunnamed-chunk-11-1.png)<!-- -->

There is a mild negative relationship between Alcohol Consumption and
Health Score, Increase in Alcohol intake is associated with a reduction
in Health Score.

``` r
dataset_smoking <- dataset_edit %>% 
  mutate(Smoking_Status = factor(Smoking_Status)) %>% 
  ggplot(aes(Smoking_Status, Health_Score))+
  geom_boxplot()+
  labs(title = "Relationship between Smoking Status and Health Score", x = "Smoking Status", y = "Health Score")
dataset_smoking  
```

![](Health-R-markdown_files/figure-gfmunnamed-chunk-12-1.png)<!-- -->

Non smokers show a higher median health score than smokers, which
signifies a negative relationship between Smoking and Health Score

``` r
dataset_exercise <- dataset_edit %>% 
  mutate(Exercise_Frequency = factor(Exercise_Frequency)) %>% 
  ggplot(aes(Exercise_Frequency, Health_Score))+
  geom_boxplot()+
  labs(title = "Relationship between Exercise Frequency and Health Score", x = "Exercise Frequency", y = "Health Score")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dataset_exercise
```

![](Health-R-markdown_files/figure-gfmunnamed-chunk-13-1.png)<!-- -->

Health Score rises from 0 through to 3-4 sessions, then plateau. Then
there is an increase in Health Score in 5-6 sessions weekly. The box
plot illustrates that higher Exercise frequency is associated with
Higher Health Scores

``` r
model <- dataset_edit %>% 
  lm(Health_Score ~ Exercise_Frequency + Diet_Quality + BMI + Age + Sleep_Hours 
     + Alcohol_Consumption + Smoking_Status, data = .)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = Health_Score ~ Exercise_Frequency + Diet_Quality + 
    ##     BMI + Age + Sleep_Hours + Alcohol_Consumption + Smoking_Status, 
    ##     data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -25.3916  -3.4025   0.5407   3.8974  14.8998 
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                         64.29356    1.71901  37.401  < 2e-16 ***
    ## Exercise_FrequencyOnce a week        1.56486    0.65132   2.403   0.0165 *  
    ## Exercise_FrequencyTwice a week       3.70167    0.63943   5.789  9.5e-09 ***
    ## Exercise_FrequencyThrice a week      5.34308    0.63638   8.396  < 2e-16 ***
    ## Exercise_FrequencyFour times a week  7.16609    0.64755  11.067  < 2e-16 ***
    ## Exercise_FrequencyFive times a week  9.08503    0.65490  13.872  < 2e-16 ***
    ## Exercise_FrequencySix times a week  10.54624    0.66281  15.911  < 2e-16 ***
    ## Diet_Quality                         0.60574    0.01187  51.044  < 2e-16 ***
    ## BMI                                 -1.13331    0.03581 -31.649  < 2e-16 ***
    ## Age                                 -0.23954    0.01512 -15.840  < 2e-16 ***
    ## Sleep_Hours                          2.43269    0.11710  20.775  < 2e-16 ***
    ## Alcohol_Consumption                 -0.98654    0.09026 -10.929  < 2e-16 ***
    ## Smoking_StatusSmoker                -3.66033    0.35552 -10.296  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.593 on 987 degrees of freedom
    ## Multiple R-squared:  0.8338, Adjusted R-squared:  0.8317 
    ## F-statistic: 412.5 on 12 and 987 DF,  p-value: < 2.2e-16

At an alpha level of 0.05. The model above shows that all predictors are
statistically significant (p\<0.05), and nearly all being less than
0.01. Increase in Exercise Frequency from None to Once a week increases
the Health Score by 1.56, increase to Twice a week increases Health
Score by 3.70, with each increase in Exercise Frequency increasing the
Health Score. An Increase in Diet Quality by 1 unit increases Health
Score by 0.6, An increase in BMI by 1kg/m^2 decreases Health Score by
1.13 on average. Each additional hour of Sleep is associated with an
increase in Health Score by 2.43. An increase in Age by 1 year leads to
an reduction in Health Score by 0.2. If Alcohol Consumption increases by
1, Health Score will experience a decrease by 0.9. Smokers on average,
have a Health Score 3.66 lower than Non-smokers, adjusting for the other
variables in the model. Adjusted R-squared of 0.8317 means 83.17% of the
Outcome variables can be explained by the seven predictor variables.

``` r
best_model <- step(model, direction = "both")
```

    ## Start:  AIC=3455.82
    ## Health_Score ~ Exercise_Frequency + Diet_Quality + BMI + Age + 
    ##     Sleep_Hours + Alcohol_Consumption + Smoking_Status
    ## 
    ##                       Df Sum of Sq    RSS    AIC
    ## <none>                              30871 3455.8
    ## - Smoking_Status       1      3315  34186 3555.8
    ## - Alcohol_Consumption  1      3736  34607 3568.1
    ## - Age                  1      7848  38719 3680.3
    ## - Exercise_Frequency   6     12752  43623 3789.6
    ## - Sleep_Hours          1     13499  44370 3816.6
    ## - BMI                  1     31329  62200 4154.4
    ## - Diet_Quality         1     81493 112364 4745.7

``` r
summary(best_model)
```

    ## 
    ## Call:
    ## lm(formula = Health_Score ~ Exercise_Frequency + Diet_Quality + 
    ##     BMI + Age + Sleep_Hours + Alcohol_Consumption + Smoking_Status, 
    ##     data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -25.3916  -3.4025   0.5407   3.8974  14.8998 
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                         64.29356    1.71901  37.401  < 2e-16 ***
    ## Exercise_FrequencyOnce a week        1.56486    0.65132   2.403   0.0165 *  
    ## Exercise_FrequencyTwice a week       3.70167    0.63943   5.789  9.5e-09 ***
    ## Exercise_FrequencyThrice a week      5.34308    0.63638   8.396  < 2e-16 ***
    ## Exercise_FrequencyFour times a week  7.16609    0.64755  11.067  < 2e-16 ***
    ## Exercise_FrequencyFive times a week  9.08503    0.65490  13.872  < 2e-16 ***
    ## Exercise_FrequencySix times a week  10.54624    0.66281  15.911  < 2e-16 ***
    ## Diet_Quality                         0.60574    0.01187  51.044  < 2e-16 ***
    ## BMI                                 -1.13331    0.03581 -31.649  < 2e-16 ***
    ## Age                                 -0.23954    0.01512 -15.840  < 2e-16 ***
    ## Sleep_Hours                          2.43269    0.11710  20.775  < 2e-16 ***
    ## Alcohol_Consumption                 -0.98654    0.09026 -10.929  < 2e-16 ***
    ## Smoking_StatusSmoker                -3.66033    0.35552 -10.296  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.593 on 987 degrees of freedom
    ## Multiple R-squared:  0.8338, Adjusted R-squared:  0.8317 
    ## F-statistic: 412.5 on 12 and 987 DF,  p-value: < 2.2e-16

Stepwise selection (step(), both directions) was run to check whether a
simpler model would perform better, it confirmed the full model as the
best fit, since dropping any single predictor increased the AIC. So the
final model kept all seven predictors.

``` r
library(sjPlot)
```

    ## 
    ## Attaching package: 'sjPlot'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     set_theme

``` r
mod <- tab_model(best_model, transform = NULL, show.aic = TRUE, show.stat = TRUE, p.style = "numeric")
mod
```

<table style="border-collapse:collapse; border:none;">

<tr>

<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">

 
</th>

<th colspan="4" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

Health_Score
</th>

</tr>

<tr>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">

Predictors
</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates
</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

CI
</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Statistic
</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

p
</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

(Intercept)
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

64.29
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

60.92 – 67.67
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

37.40
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001</strong>
</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

Exercise Frequency \[Once<br>a week\]
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

1.56
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.29 – 2.84
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

2.40
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>0.016</strong>
</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

Exercise Frequency \[Twice<br>a week\]
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

3.70
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

2.45 – 4.96
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

5.79
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001</strong>
</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

Exercise Frequency<br>\[Thrice a week\]
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

5.34
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

4.09 – 6.59
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

8.40
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001</strong>
</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

Exercise Frequency \[Four<br>times a week\]
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

7.17
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

5.90 – 8.44
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

11.07
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001</strong>
</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

Exercise Frequency \[Five<br>times a week\]
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

9.09
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

7.80 – 10.37
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

13.87
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001</strong>
</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

Exercise Frequency \[Six<br>times a week\]
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

10.55
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

9.25 – 11.85
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

15.91
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001</strong>
</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

Diet Quality
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.61
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.58 – 0.63
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

51.04
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001</strong>
</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

BMI
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

-1.13
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

-1.20 – -1.06
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

-31.65
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001</strong>
</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

Age
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

-0.24
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

-0.27 – -0.21
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

-15.84
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001</strong>
</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

Sleep Hours
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

2.43
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

2.20 – 2.66
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

20.77
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001</strong>
</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

Alcohol Consumption
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

-0.99
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

-1.16 – -0.81
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

-10.93
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001</strong>
</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

Smoking Status \[Smoker\]
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

-3.66
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

-4.36 – -2.96
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

-10.30
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001</strong>
</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">

Observations
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="4">

1000
</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">

R<sup>2</sup> / R<sup>2</sup> adjusted
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="4">

0.834 / 0.832
</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">

AIC
</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="4">

6295.695
</td>

</tr>

</table>

Package sjPlot, and function tab_model was used to generate a table of
the model.

Assumptions

``` r
library(car)
```

    ## Loading required package: carData

    ## 
    ## Attaching package: 'car'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

    ## The following object is masked from 'package:purrr':
    ## 
    ##     some

``` r
crPlots(best_model)
```

![](Health-R-markdown_files/figure-gfmunnamed-chunk-17-1.png)<!-- -->

crPlots from the car package was used to test the Component + Residual
plots. This is usually done to check whether each predictor has a linear
relationship with the outcome variable, after accounting for other
predictors in the model. We look out for a pink and a blue line. Diet
quality: There is a slight curve, the pink line follows the blue line
which shows linearity BMI: Pink and blue line are close with a downward
slope, shows linearity. Age: Decent match between the lines, linear
relationship Sleep Hours: Upward trend, Pink line curves slightly, still
linear Alcohol Consumption: Shows a downward trend, Pink and blue lines
show linearity Exercise Frequency: Box plots show component + residual
roughly increasing with increase in exercise frequency Smoking Status:
Box plot show a decrease from Non-smoker to Smoker

Overall: All predictors look linear. The assumption of linearity for
this regression model is met

``` r
library(lmtest)  
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
dwtest(best_model)  
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  best_model
    ## DW = 1.9385, p-value = 0.1652
    ## alternative hypothesis: true autocorrelation is greater than 0

The Durbin-Watson test was performed to check for autocorrelation in the
residuals. DW: 1.9385 (very close to 2, therefore, there is little to no
autocorrelation) p-value: 0.1652 (greater then 0.05, therefore, we fail
to reject the null hypothesis) There is no statistically significant
evidence of autocorrelation in the residuals.

``` r
plot(best_model, which = 1)  
```

![](Health-R-markdown_files/figure-gfmunnamed-chunk-19-1.png)<!-- -->

A Residuals vs Fitted plot was done to check for Linearity and
Homoscedasticity The plot shows that the points form a pattern and are
not randomly scattered around the horizontal line. There is a curve on
the red line, it rises gently from about 60 to 90, then bends sharply
downward after 90 - 100, which signals nonlinearity. Around fitted
values 90-120, there is a tight, sharp bend of points which all fall in
an almost perfectly diagonal straight line. This shows that there is a
cap on the outcome variable (Explained by the maximum health score,
which is 100). 356, 592 and 656 are outliers. Spread of residuals is not
fully constant across fitted values( it is tighter on the right, wider
in the middle), this is a sign of heteroscedasticity Conclusion, plot
shows nonlinearity and heteroscedaticity.

``` r
bptest(best_model)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  best_model
    ## BP = 31.926, df = 12, p-value = 0.001421

Breusch-Pagan test was done, this is to measure how much the residual’s
variance depeds on the fitted values p = 0.0014 (less than 0.05, hence,
the null hypothesis is rejected) Conclusion: Confirms the
heteroscedasticity seen in the residuals vs fitted plot.

``` r
shapiro.test(residuals(best_model))
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  residuals(best_model)
    ## W = 0.98281, p-value = 1.786e-09

The Shapiro-Wilk normality test was done to check if the model’s
residuals are normally distributed p-value is far below 0.05, the null
hypothesis is rejected. Therefore, the residuals are not normally
distributed.

``` r
vif(best_model)
```

    ##                         GVIF Df GVIF^(1/(2*Df))
    ## Exercise_Frequency  1.040736  6        1.003333
    ## Diet_Quality        1.008290  1        1.004137
    ## BMI                 1.018687  1        1.009300
    ## Age                 1.008492  1        1.004237
    ## Sleep_Hours         1.008154  1        1.004069
    ## Alcohol_Consumption 1.006446  1        1.003218
    ## Smoking_Status      1.010274  1        1.005124

The vif test was done to check for multicollinearity using Variance
Inflation Factors (VIF) Rsult shows every value is well below 5 (all
within 1.008 to 1.040). Therefore, there is no multicollinearity.

``` r
plot(best_model, which = 4)
```

![](Health-R-markdown_files/figure-gfmunnamed-chunk-23-1.png)<!-- -->

A Cook’s Distance plot was done, to measure the influence each
individual data point has on the regression model’s fitted coefficients.
Using the treshold of 4/n(n is the number of observations), therefore,
any point exceeding 0.004 warrant further investigation, since they may
reflect extreme or unusual combinations of predictor values. Point 356
and 592 which showed up as outliers in the residual vs fitted plot also
stands out, in addition to point 352

Limitations Some of the limitations include: 1. Data is synthetic 2. The
health score is capped at 100, which is likely the cause of the
nonlinearity and heteroscedaticity noted in the assumptions 3.
Assumption violations; Nonlinearity in the residuals vs fitted plot,
Heteroscedasticity(confirmed via Breusch-Pagan), Non-normal residuals
(confirmed via Shapiro-Wilk) 4. Influential points were not addressed,
points such as 356, 592 and 352 5. The cross-scetional nature of the
data makes establishing causation difficult.
