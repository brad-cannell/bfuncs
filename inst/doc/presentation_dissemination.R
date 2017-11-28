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
mtcars %>% group_by(am) %>% freq_table() %>% format_table()

## ------------------------------------------------------------------------
mtcars %>% group_by(am) %>% freq_table() %>% format_table(stats = "n and percent")

## ------------------------------------------------------------------------
mtcars %>% group_by(am, cyl) %>% freq_table() %>% format_table()

## ------------------------------------------------------------------------
mtcars %>% group_by(am, cyl) %>% freq_table(output = "all") %>% format_table(stats = "n and percent")

## ------------------------------------------------------------------------
mtcars %>% 
  group_by(am, cyl) %>% 
  freq_table() %>% 
  format_table() %>% 
  spread(key = am, value = percent_row_95)

## ------------------------------------------------------------------------
mtcars %>% 
  group_by(am, cyl) %>% 
  freq_table() %>% 
  format_table() %>% 
  spread(key = am, value = percent_row_95) %>% 
  mutate(variable = colnames(.)[1]) %>% 
  rename("class" = cyl, "am_0" = `0`, "am_1" = `1`)

## ------------------------------------------------------------------------
row <- mtcars %>% 
  group_by(am, cyl) %>% 
  freq_table() %>% 
  format_table() %>% 
  spread(key = am, value = percent_row_95) %>% 
  mutate(variable = colnames(.)[1]) %>% 
  rename("class" = cyl, "am_0" = `0`, "am_1" = `1`) %>% 
  mutate(class = as.character(class)) # Need for bind_rows below

bind_rows(table, row)

## ------------------------------------------------------------------------
# Select variables 
cat_vars <- quos(cyl, vs)

# Execute freq_table and row bind results to table
for (i in seq_along(cat_vars)) {
  
  # Calculate mean and 95% CI
  row <- mtcars %>% 
    group_by(am, !!cat_vars[[i]]) %>% 
    freq_table() %>% 
    format_table() %>% 
    spread(key = am, value = percent_row_95) %>% 
    mutate(variable = colnames(.)[1]) %>% 
    rename("class" = !!cat_vars[[i]], "am_0" = `0`, "am_1" = `1`) %>% 
    mutate(class = as.character(class)) # Need for bind_rows below

  # Append to bottom of table
  table <- bind_rows(table, row)
}

print(table)

## ------------------------------------------------------------------------
# Reset the table shell
# ---------------------
table <- tibble(
  variable = "",
  class    = "",
  am_0     = mtcars %>% bfuncs::get_group_n(am == 0),
  am_1     = mtcars %>% bfuncs::get_group_n(am == 1)
)


# Select variables
# ----------------
cont_vars <- quos(mpg, disp)
cat_vars  <- quos(cyl, vs)


# Fill in continuous variables
# ----------------------------
for (i in seq_along(cont_vars)) {
  
  # Calculate mean and 95% CI
  row <- mtcars %>% 
    group_by(am) %>% 
    bfuncs::mean_table(!!cont_vars[[i]]) %>% 
    bfuncs::format_table() %>% 
    spread(key = am, value = mean_95) %>% 
    rename("variable" = var, "am_0" = `0`, "am_1" = `1`)

  # Append to bottom of table
  table <- bind_rows(table, row)
}


# Fill in categorical variables
# -----------------------------
for (i in seq_along(cat_vars)) {
  
  # Calculate mean and 95% CI
  row <- mtcars %>% 
    group_by(am, !!cat_vars[[i]]) %>% 
    freq_table() %>% 
    format_table() %>% 
    spread(key = am, value = percent_row_95) %>% 
    mutate(variable = colnames(.)[1]) %>% 
    rename("class" = !!cat_vars[[i]], "am_0" = `0`, "am_1" = `1`) %>% 
    mutate(class = as.character(class)) # Need for bind_rows below

  # Append to bottom of table
  table <- bind_rows(table, row)
}

print(table)

## ------------------------------------------------------------------------
table <- table %>%
  mutate(
    variable = if_else(variable == "mpg", "Miles per gallon, mean (95% CI)", variable),
    variable = if_else(variable == "disp", "Displacement, mean (95% CI)", variable),
    variable = if_else(variable == "cyl", "Number of cylinders, percent (95% CI)", variable),
    variable = if_else(variable == "vs", "V/S, percent (95% CI)", variable)
  ) %>% 
  print

## ------------------------------------------------------------------------
table <- table %>%
  group_by(variable) %>%
  mutate(
    x = duplicated(variable),
    x = if_else(variable == "", NA, x)
  ) %>%
  ungroup() %>%
  mutate(
    variable = if_else(x == TRUE, "", variable),
    variable = if_else(is.na(variable), "", variable),
    x = NULL
  ) %>% 
  print

## ------------------------------------------------------------------------
# table %>% 
#   mutate(
#     class = stringr::str_replace(class, "^", "---"),
#     variable = if_else(variable == "", class, variable),
#     class = NULL
#   )

## ------------------------------------------------------------------------
table_kable <- knitr::kable(table, col.names = c(
  "Characteristic",
  "Class",
  "Automatic Transmission", 
  "manual Transmission")
)

print(table_kable)

