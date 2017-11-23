# Brad's functions (bfuncs)

This repository is a random smattering of functions I wrote for myself, but you can use them too.

<!-- Need to make help vignettes and link to them below -->

## Programing functions

## Simulation functions

## Data wrangling functions

* mean_center: mean center continuous variables.

## Data analysis functions

* conf_matrix: create a confusion matrix.

* [freq_table: estimate percents and 95 percent confidence intervals in dplyr pipelines](https://rawgit.com/brad-cannell/bfuncs/master/inst/doc/Frequency_Tables.html)

* mean_table: estimate means and 95 percent confidence intervals in dplyr pipelines

* [tabstats: compact table of summary statistics for continuous variables.](https://rawgit.com/brad-cannell/bfuncs/master/inst/doc/Using_tabstat_with_dplyr.html)

## Presentation and dissemination functions

* Add functions for creating tables

* about_data: view number of observations and variables in a data frame, but not any of the data values.

* codebook: create a codebook / data dictionary from a data frame.

## Other functions

-------------------------------------------------------------------------------

#### Installation instructions:

``` r
devtools::install_github("brad-cannell/bfuncs", build_vignettes = TRUE)
```
