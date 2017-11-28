# Brad's functions (bfuncs)

This repository is a random smattering of functions I wrote for myself, but you can use them too.

<!-- Need to make help vignettes and link to them below -->

## Programing functions

Functions that help with writing functions, but on their own, don't produce any result that is useful for manipulating or analyzing data (think %>%).

## Simulation functions

Functions created specifically to help with simulations.

## Data wrangling functions

Functions that manipulate data in some way. But, in and of themselves, do not produce generally meaningful statistics of interest about the data.

* mean_center: mean center continuous variables.

## Data analysis functions

Functions that take data as an input and produces some numerical summary or visualization about the data.

* about_data: view number of observations and variables in a data frame, but not any of the data values.

* conf_matrix: create a confusion matrix.

* [freq_table: estimate percentages and 95 percent confidence intervals in dplyr pipelines](https://rawgit.com/brad-cannell/bfuncs/master/inst/doc/descriptive_analysis.html)

* [mean_table: estimate means and 95 percent confidence intervals in dplyr pipelines](https://rawgit.com/brad-cannell/bfuncs/master/inst/doc/descriptive_analysis.html)

* [tabstats: compact table of summary statistics for continuous variables.](https://rawgit.com/brad-cannell/bfuncs/master/inst/doc/using_tabstat_with_dplyr.html)

## Presentation and dissemination functions

Functions that help with assist with making the data itself, or the results of statistical analyses of the data, more accessible to people unfamiliar with the data (think tables and visualizations for publication).

* codebook: create a codebook / data dictionary from a data frame.

* get_group_n: To help with making tables. Given a tibble and a filter expression, get_group_n returns the group sample size formatted as "N = XXXX".


-------------------------------------------------------------------------------

#### Installation instructions:

``` r
devtools::install_github("brad-cannell/bfuncs", build_vignettes = TRUE)
```
