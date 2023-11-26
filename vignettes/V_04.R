## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(ready4)

## ----include=T----------------------------------------------------------------
libraries_tb <- get_libraries_tb(gh_repo_1L_chr = "ready4-dev/ready4") %>% update_libraries_tb(include_1L_chr = "modules")

## -----------------------------------------------------------------------------
libraries_tb$Section %>% unique()

## ----echo=T-------------------------------------------------------------------
print_packages(libraries_tb %>% dplyr::filter(Section == "People"))

## ----echo=T-------------------------------------------------------------------
print_packages(libraries_tb %>% dplyr::filter(Section == "Places"))

## ----echo=T-------------------------------------------------------------------
print_packages(libraries_tb %>% dplyr::filter(Section == "Programs"))

