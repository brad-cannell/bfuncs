## ------------------------------------------------------------------------
library(tidyverse)
data(mtcars)

## ------------------------------------------------------------------------
mtcars %>% 
  group_by(am) %>% 
  summarise(n = n()) %>% 
  mutate(
    cumsum = sum(n),
    prop = n / cumsum,
    se = sqrt(prop * (1 - prop) / (cumsum - 1)),
    tcrit = stats::qt(0.975, df = cumsum - 1),
    lower = prop - tcrit * se,
    upper = prop + tcrit * se
  )

## ------------------------------------------------------------------------
mtcars %>% 
  group_by(am) %>% 
  summarise(n = n()) %>% 
    mutate(
      cumsum    = sum(n),
      prop      = n / cumsum,
      log_prop  = log(prop) - log(1 - prop),
      se        = sqrt(prop * (1 - prop) / (cumsum - 1)),
      log_se    = se / (prop * (1 - prop)),
      t_crit    = stats::qt(0.975, df = cumsum - 1),
      log_lower = log_prop - t_crit * log_se,
      log_upper = log_prop + t_crit * log_se,
      lower     = exp(log_lower) / (1 + exp(log_lower)),
      upper     = exp(log_upper) / (1 + exp(log_upper))
    ) %>% 
    select(1, n, prop, se, lower, upper)

## ------------------------------------------------------------------------
mtcars %>% 
  group_by(am, cyl) %>% 
  summarise(n = n()) %>% 
  mutate(
    cumsum = sum(n),
    prop = n / cumsum,
    percent = prop * 100,
    se = sqrt(prop * (1 - prop) / (cumsum)),
    sep = se * 100,
    tcrit = stats::qt(0.975, df = cumsum),
    lower = prop - tcrit * se,
    upper = prop + tcrit * se
  ) %>% select(1:4, percent, sep, lower, upper)

## ------------------------------------------------------------------------
mtcars %>% 
  group_by(am, cyl) %>% 
  summarise(n = n()) %>% 
  mutate(group_n = sum(n)) %>% 
  ungroup() %>%
  mutate(
    total_n   = sum(n),
    prop      = n / group_n,
    log_prop  = log(prop) - log(1 - prop),
    se        = sqrt(prop * (1 - prop) / (group_n - 1)), # group n - 1
    log_se    = se / (prop * (1 - prop)),
    t_crit    = stats::qt(0.975, df = total_n - 1), # overall n - 1
    log_lower = log_prop - t_crit * log_se,
    log_upper = log_prop + t_crit * log_se,
    lower     = exp(log_lower) / (1 + exp(log_lower)),
    upper     = exp(log_upper) / (1 + exp(log_upper))
  ) %>% 
  select(1:2, n, group_n, total_n, prop, se, lower, upper)  
  # map_at(.at = c(5:8), ~ . * 100) %>% 
  # as_tibble()

## ------------------------------------------------------------------------
freq_table <- function(x, t_prob = 0.975, ci_type = "log", ...) {
  
  # ===========================================================================
  # Check for grouped tibble
  # ===========================================================================
  if (!("grouped_df" %in% class(x))) {
    stop(paste("The x argument to freq_table must be a grouped tibble. 
               The class of the current x argument is", class(x)))
  } # No else
  
  # ===========================================================================
  # Check for number of group vars: 
  # If 1 var then use Wald and overall prop
  # If 2 vars then use logit transformation for CI's and give row prop in
  # addition to overall.
  # ===========================================================================
  out <- x %>% 
    summarise(n = n())
  
  # ===========================================================================
  # One-way tables
  # ===========================================================================
  if (ncol(out) == 2) { # else is in two-way tables.
    
    # Update out to include elements need for Wald and Log transformed CI's
    # One-way tables
    out <- out %>% 
      mutate(
        n_total = sum(n),
        prop    = n / n_total,
        se      = sqrt(prop * (1 - prop) / (n_total - 1)),
        t_crit  = stats::qt(t_prob, df = n_total - 1)
      )
        
        # Calculate Wald CI's
        # and put prop, se, and CI's on percent scale
        # One-way tables
        if (ci_type == "wald") {
          
          out <- out %>% 
            mutate(
              lcl_wald = prop - t_crit * se,
              ucl_wald = prop + t_crit * se,
              percent  = prop * 100,
              se       = se * 100, 
              lcl_wald = lcl_wald * 100,
              ucl_wald = ucl_wald * 100
            ) %>% 
            
            # Control output
            select(1, n, n_total, percent, se, t_crit, lcl_wald, ucl_wald)
        
        # Calculate log transformed CI's
        # and put prop, se, and CI's on percent scale
        # One-way tables
        } else if (ci_type == "log") {
          
          out <- out %>%
            mutate(
              prop_log = log(prop) - log(1 - prop),
              se_log   = se / (prop * (1 - prop)),
              lcl_log  = prop_log - t_crit * se_log,
              ucl_log  = prop_log + t_crit * se_log,
              lcl_log  = exp(lcl_log) / (1 + exp(lcl_log)),
              ucl_log  = exp(ucl_log) / (1 + exp(ucl_log)),
              percent  = prop * 100,
              se       = se * 100, 
              lcl_log  = lcl_log * 100,
              ucl_log  = ucl_log * 100 
            ) %>% 
          
            # Control output
            select(1, n, n_total, percent, se, t_crit, lcl_log, ucl_log)
        }


  # =========================================================================== 
  # Two-way tables
  # Only logged transformed CI's
  # Need percent and row percent
  # ===========================================================================
  } else if (ncol(out) == 3) { # if is one-way tables
    
    out <- out %>%
      # Calculate within row n
      mutate(n_group = sum(n)) %>%
      # Ungroup to get total_n
      ungroup() %>%
      mutate(
        
        # Estimate overall percent se and CI's
        n_total        = sum(n),
        prop_total     = n / n_total,
        se_total       = sqrt(prop_total * (1 - prop_total) / (n_total - 1)),
        t_crit_total   = stats::qt(t_prob, df = n_total - 1),
        prop_log_total = log(prop_total) - log(1 - prop_total),
        se_log_total   = se_total / (prop_total * (1 - prop_total)),
        lcl_total_log  = prop_log_total - t_crit_total * se_log_total,
        ucl_total_log  = prop_log_total + t_crit_total * se_log_total,
        lcl_total_log  = exp(lcl_total_log) / (1 + exp(lcl_total_log)),
        ucl_total_log  = exp(ucl_total_log) / (1 + exp(ucl_total_log)),
        percent_total  = prop_total * 100,
        se_total       = se_total * 100, 
        lcl_total_log  = lcl_total_log * 100,
        ucl_total_log  = ucl_total_log * 100,
        
        
        # Estimate row percent se and CI's
        prop_row     = n / n_group,
        se_row       = sqrt(prop_row * (1 - prop_row) / (n_group - 1)), # group n - 1
        t_crit_row   = stats::qt(t_prob, df = n_total - 1), # overall n - 1
        prop_log_row = log(prop_row) - log(1 - prop_row),
        se_log_row   = se_row / (prop_row * (1 - prop_row)),
        lcl_row_log  = prop_log_row - t_crit_row * se_log_row,
        ucl_row_log  = prop_log_row + t_crit_row * se_log_row,
        lcl_row_log  = exp(lcl_row_log) / (1 + exp(lcl_row_log)),
        ucl_row_log  = exp(ucl_row_log) / (1 + exp(ucl_row_log)),
        percent_row  = prop_row * 100,
        se_row       = se_row * 100, 
        lcl_row_log  = lcl_row_log * 100,
        ucl_row_log  = ucl_row_log * 100
      ) %>% 
          
      # Control output
      select(1:2, n, n_group, n_total, percent_total, se_total, lcl_total_log, 
             ucl_total_log, percent_row, se_row, lcl_row_log, ucl_row_log)
    
  } else { # Grouped by more than two variables, or not grouped.
    stop(
      paste(
        "Expecting x to be a grouped data frame with 2 or 3 columns. Instead
        x had", ncol(out) 
      )
    )
  }
  
  # Return tibble of results
  out
}

## ------------------------------------------------------------------------
mtcars %>% 
  group_by(am) %>% 
  freq_table()

## ------------------------------------------------------------------------
mtcars %>% 
  group_by(am, cyl) %>% 
  freq_table()

