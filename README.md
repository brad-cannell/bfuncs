<!-- README.md is generated from README.Rmd. Please edit that file -->
This package is a collection of simple functions I use sometimes
================================================================

``` r
library(tidyverse)
#> Loading tidyverse: ggplot2
#> Loading tidyverse: tibble
#> Loading tidyverse: tidyr
#> Loading tidyverse: readr
#> Loading tidyverse: purrr
#> Loading tidyverse: dplyr
#> Conflicts with tidy packages ----------------------------------------------
#> filter(): dplyr, stats
#> lag():    dplyr, stats
library(myFunctions)
```

Tools for data cleaning
-----------------------

``` r
data(mtcars)
```

**about\_data function**\*

``` r
# View number of observations and variables in a data frame, but not any of the data values
about_data(mtcars)
#> 32 observations and 11 variables in the data
```

**all\_na function**

``` r
# Which rows contain only NA values
df <- data.frame(
  var1 = c(1, NA, 2, NA, 5, NA),
  var2 = c(NA, "b", "c", NA, NA, NA),
  var3 = c(TRUE, FALSE, TRUE, NA, FALSE, NA)
)

all_na(df)
#> [1] 4 6
```

**check\_catvars function**

``` r
lapply(mtcars[c("cyl", "am", "mpg")], check_catvars)
#> $cyl
#> x
#>  4  6  8 
#> 11  7 14 
#> 
#> $am
#> x
#>  0  1 
#> 19 13 
#> 
#> $mpg
#> [1] "Not a categorical variable"
```

**codebook function**

``` r
# Optional: Create label vector
labels <- c("Miles Per Gallon", "Number of cylinders")

# Codebook with one variable
codebook(mtcars["mpg"], label = labels)
#> -------------------------------------------------------------------------------------------------------------- 
#> mpg 
#> -------------------------------------------------------------------------------------------------------------- 
#> Label: Miles Per Gallon 
#> Class: numeric 
#> Unique: 25 
#> Miss: 0 
#> Summary: 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   10.40   15.43   19.20   20.09   22.80   33.90

# Codebook with multiple variables
varlist <- c("mpg", "cyl")
codebook(mtcars[varlist], label = labels)
#> -------------------------------------------------------------------------------------------------------------- 
#> mpg 
#> -------------------------------------------------------------------------------------------------------------- 
#> Label: Miles Per Gallon 
#> Class: numeric 
#> Unique: 25 
#> Miss: 0 
#> Summary: 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   10.40   15.43   19.20   20.09   22.80   33.90 
#> 
#> 
#> -------------------------------------------------------------------------------------------------------------- 
#> cyl 
#> -------------------------------------------------------------------------------------------------------------- 
#> Label: Number of cylinders 
#> Class: numeric 
#> Unique: 3 
#> Miss: 0 
#> Summary: 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   4.000   4.000   6.000   6.188   8.000   8.000
```

**mean\_center function**

``` r
(mtcars$c.mpg <- mean_center(mtcars$mpg))
#>  [1]  0.909375  0.909375  2.709375  1.309375 -1.390625 -1.990625 -5.790625
#>  [8]  4.309375  2.709375 -0.890625 -2.290625 -3.690625 -2.790625 -4.890625
#> [15] -9.690625 -9.690625 -5.390625 12.309375 10.309375 13.809375  1.409375
#> [22] -4.590625 -4.890625 -6.790625 -0.890625  7.209375  5.909375 10.309375
#> [29] -4.290625 -0.390625 -5.090625  1.309375
```

**recode\_miss function**

``` r
recode_miss(mtcars$cyl, values = 4)
#>  [1]  6  6 NA  6  8  6  8 NA NA  6  6  8  8  8  8  8  8 NA NA NA NA  8  8
#> [24]  8  8 NA NA NA  8  6  8 NA
```

Tools for descriptive tables
----------------------------

``` r
# mean95 function
mean95(mtcars$mpg)
#>     mean    sd    lcl    ucl
#> 1 20.091 6.027 17.918 22.264

# props function
props(mtcars$cyl)
#>   variable level non_miss percent      lcl      ucl
#> 1      cyl     4       11  34.375 16.97690 51.77310
#> 2      cyl     6        7  21.875  6.73190 37.01810
#> 3      cyl     8       14  43.750 25.57828 61.92172

# stats function
stats(mtcars$mpg)
#>   Non_Missing Missing   Mean Mean_LCL Mean_UCL   SEM Median Variance    SD
#> 1          32       0 20.091   17.918   22.264 1.065   19.2   36.324 6.027
#>    Min  Max
#> 1 10.4 33.9

# tabstats function
tabstat(mtcars$mpg, stats = c("n", "nmiss", "ci", "sum", "max", "min",
"range", "sd", "var", "cv", "sem", "skew", "kurt", "p1", "p5", "p10",
"p25", "p50", "median", "p75", "p90", "p95", "p99", "iqr", "q"))
#>   variable  n nmiss   mean mean_lcl mean_ucl   sum  max  min range std_dev
#> 1      mpg 32     0 20.091   17.918   22.264 642.9 33.9 10.4  23.5   6.027
#>   variance coef_var   sem skewness kurtosis   p1     p5   p10    p25
#> 1   36.324      0.3 1.065     0.64    2.799 10.4 11.995 14.34 15.425
#>   median  p75   p90  p95    p99   iqr    p25  p50  p75
#> 1   19.2 22.8 30.09 31.3 33.435 7.375 15.425 19.2 22.8
```

Tools for evaluating model performace and diagnostic tests. This package was originally created to help with the development and evaluation of screening tools.
---------------------------------------------------------------------------------------------------------------------------------------------------------------

Tools for evaluating model performace and diagnostic tests. This package was originally created to help with the development and evaluation of screening tools.
---------------------------------------------------------------------------------------------------------------------------------------------------------------

**Create confusion matrix with conf\_matrix()**

``` r
utils::data(mtcars)

# Create "true" outcome vector:
truth <- mtcars$vs

# Create a vector meant to represent a set of predicted outcomes:
set.seed(123)
predicted <- as.numeric(sample(0:1, length(truth), replace = TRUE))

# Create confusion matrix
my_cm <- conf_matrix(truth = truth, prediction = predicted)
#> 
#>  
#>    Cell Contents
#> |-------------------------|
#> |                       N |
#> | Chi-square contribution |
#> |           N / Row Total |
#> |           N / Col Total |
#> |         N / Table Total |
#> |-------------------------|
#> 
#>  
#> Total Observations in Table:  32 
#> 
#>  
#>              | prediction 
#>        truth |        P1 |        P0 | Row Total | 
#> -------------|-----------|-----------|-----------|
#>           T1 |         9 |         5 |        14 | 
#>              |     0.004 |     0.007 |           | 
#>              |     0.643 |     0.357 |     0.438 | 
#>              |     0.429 |     0.455 |           | 
#>              |     0.281 |     0.156 |           | 
#> -------------|-----------|-----------|-----------|
#>           T0 |        12 |         6 |        18 | 
#>              |     0.003 |     0.006 |           | 
#>              |     0.667 |     0.333 |     0.562 | 
#>              |     0.571 |     0.545 |           | 
#>              |     0.375 |     0.188 |           | 
#> -------------|-----------|-----------|-----------|
#> Column Total |        21 |        11 |        32 | 
#>              |     0.656 |     0.344 |           | 
#> -------------|-----------|-----------|-----------|
#> 
#>  
#>   Sensativity Specificity  FPR  FNR  FDR Accuracy Misclassification
#> 1        0.64        0.33 0.67 0.36 0.57     0.47              0.53
#>   Precision
#> 1      0.43

# -----------------------------------------------------------------------------
# Example of using conf_matrix with multiple variables

df <- tibble(
  x1 = factor(c("No", "Yes")),
  x2 = factor(c("No", "Yes")),
  x3 = factor(c("Yes", "No"))
)

# Calculate measures
test <- conf_matrix(truth = df$x1, prediction = df$x2, show_matrix = FALSE)
#>   tp fp fn tn Sensativity Specificity FPR FNR FDR Accuracy
#> 1  1  0  0  1           1           1   0   0   0        1
#>   Misclassification Precision Prevalence
#> 1                 0         1        0.5
as_tibble(test)
#> # A tibble: 1 × 13
#>      tp    fp    fn    tn Sensativity Specificity   FPR   FNR   FDR
#>   <int> <int> <int> <int>       <dbl>       <dbl> <dbl> <dbl> <dbl>
#> 1     1     0     0     1           1           1     0     0     0
#> # ... with 4 more variables: Accuracy <dbl>, Misclassification <dbl>,
#> #   Precision <dbl>, Prevalence <dbl>

# Make empty table
results <- tibble(
  var = NA,
  tp = NA,
  fp = NA,
  fn = NA,
  tn = NA,
  sensativity = NA,
  specificity = NA,
  fpr = NA,
  fnr = NA,
  accuracy = NA,
  misclassification = NA,
  precision = NA,
  prevalence = NA
)

# for each screener item:
#   capture the name of the screener item
#   put the name into the first column of the results tibble
#   capture the performance measures of the screener item
#   put each performance measure into the coresponding element in the results tibble
var <- names(df[2:3])
r <- 1
for (i in var) {
  results[r, 1] <- i
  results[r, 2:13] <- conf_matrix(truth = df$x1, prediction = df[[i]], show_matrix = FALSE)
  r <- r + 1
}
#>   tp fp fn tn Sensativity Specificity FPR FNR FDR Accuracy
#> 1  1  0  0  1           1           1   0   0   0        1
#>   Misclassification Precision Prevalence
#> 1                 0         1        0.5
#> Warning in `[<-.data.frame`(`*tmp*`, r, 2:13, value = structure(list(tp =
#> 1L, : provided 13 variables to replace 12 variables
#>   tp fp fn tn Sensativity Specificity FPR FNR FDR Accuracy
#> 1  0  1  1  0           0           0   1   1   1        0
#>   Misclassification Precision Prevalence
#> 1                 1         0        0.5
#> Warning in `[<-.data.frame`(`*tmp*`, r, 2:13, value = structure(list(tp =
#> 0L, : provided 13 variables to replace 12 variables

print(results)
#> # A tibble: 2 × 13
#>     var    tp    fp    fn    tn sensativity specificity   fpr   fnr
#> * <chr> <int> <int> <int> <int>       <dbl>       <dbl> <dbl> <dbl>
#> 1    x2     1     0     0     1           1           1     0     0
#> 2    x3     0     1     1     0           0           0     1     1
#> # ... with 4 more variables: accuracy <dbl>, misclassification <dbl>,
#> #   precision <dbl>, prevalence <dbl>
```

#### Installation instructions:

``` r
devtools::install_github("brad-cannell/myFunctions")
```
