## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(comment = NA)
Sys.setenv(TZ = "US/Central")

## ----load_packages, message=FALSE----------------------------------------
# Load packages
library(tidyverse)
library(bfuncs)

## ------------------------------------------------------------------------
data("mtcars")

## ------------------------------------------------------------------------
mtcars %>% 
  summarise(mean_mpg = mean(mpg))

## ------------------------------------------------------------------------
mtcars %>% 
  group_by(cyl) %>% 
  summarise(mean_mpg = mean(mpg))

## ------------------------------------------------------------------------
mtcars %>% 
  do(tabstat(.$mpg, stats = c("n", "ci")))

## ------------------------------------------------------------------------
mtcars %>% 
  group_by(cyl) %>% 
  do(tabstat(.$mpg, stats = c("n", "ci")))

## ----echo=FALSE----------------------------------------------------------
sessionInfo()

