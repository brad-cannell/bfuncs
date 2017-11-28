## ------------------------------------------------------------------------
library(tidyverse)
library(bfuncs)

## ------------------------------------------------------------------------
data(mtcars)

## ----eval=FALSE----------------------------------------------------------
#  
#  ---
#  title: "Table 1. Descriptive Characteristics"
#  output:
#    word_document:
#      reference_docx: word_style_template_01.docx
#  ---
#  

## ------------------------------------------------------------------------
table <- tibble(
  variable = "", # Variable names
  class    = "", # Classes for categorical variables
  am_0     = "", # Group 1
  am_1     = ""  # Group 2
)

## ------------------------------------------------------------------------
# Same as above, add n's to table
table <- tibble(
  variable = "",
  class    = "",
  am_0     = mtcars %>% bfuncs::get_group_n(am == 0),
  am_1     = mtcars %>% bfuncs::get_group_n(am == 1)
)

## ------------------------------------------------------------------------
mtcars %>% group_by(am) %>% mean_table(mpg)

## ------------------------------------------------------------------------
mtcars %>% mean_table(mpg) %>% format_table()

## ------------------------------------------------------------------------
mtcars %>% mean_table(mpg) %>% format_table(stats = "n and mean")

## ------------------------------------------------------------------------
mtcars %>% group_by(am) %>% mean_table(mpg) %>% format_table()

## ------------------------------------------------------------------------
mtcars %>% group_by(am) %>% mean_table(mpg) %>% format_table(stats = "n and mean")

## ------------------------------------------------------------------------
mtcars %>% group_by(am) %>% mean_table(mpg)

