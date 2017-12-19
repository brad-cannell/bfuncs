# Useful
# https://cran.r-project.org/web/packages/rprojroot/vignettes/rprojroot.html

# List every file in the current project
list.files()

# No, this only lists the files in the current directory, not the entire
# project.
list.files(recursive = TRUE)

# This works, but what if I'm not in a project. Is there a way to test and
# see if you are in a project or not?
rprojroot::is_rstudio_project()

# For each of those files, return the date it was created and the date it was
# last modified.
file.info("/Users/bradcannell/Dropbox/R/Packages/bfuncs/R/mean_table.R")
