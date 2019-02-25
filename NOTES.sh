# =============================================================================
# bfuncs
# Created: 2017-05-18
# =============================================================================

# View project progress and tasks:
open https://github.com/brad-cannell/bfuncs/projects/4

# Creating tests
https://github.com/r-lib/testthat


# Build check:
# -------------------------------------------------------------------------------

# 1. Warning: ‘inst/doc’ files ... ignored as vignettes have been rebuilt.
# Just ignore this one.

# 2. Warning in strptime(xx, f <- "%Y-%m-%d %H:%M:%OS", tz = tz) :
# unknown timezone 'zone/tz/2017c.1.0/zoneinfo/America/Chicago'
# Just ingnore this one. It's a problem with my computer, not with the package code.

# 3. Non-standard files/directories found at top level:
# ‘develop_test_table.Rmd’ ‘develop_test_table.nb.html’
# Need to delete these files when I'm done developing chi-square test.

# Prevents R CMD check: "no visible binding for global variable ‘.’"
# n = n_total = prop = t_crit = se = lcl_wald = ucl_wald = percent = NULL

# Checking dependencies in R code ... WARNING '::' or ':::' import not declared from: <package>
# Add <package> to imports section of DESCRIPTION
