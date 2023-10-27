## 00_run_ME_to_install_or_update_packages.R
## olivier.duperrex@unisante.ch
## 2023-10-26


# install it if not already there
if (!require("pacman")) install.packages("pacman") 
if (!require("remotes")) install.packages("remotes")


pacman::p_load(quarto)

pacman::p_load(
    bookdown,
    crayon,
    data.table,
    flextable,
    formattable,
    fs,
    ftExtra,
    ggplot2,
    haven,
    here,
    magrittr,
    officedown,
    patchwork,
    purrr,
    REDCapR,
    sjlabelled,
    sjmisc,
    sjPlot,
    stringr,
    tidyverse,
    writexl)

## install.packages("remotes") ## If it's not already installed.
remotes::install_github("OuhscBbmc/OuhscMunge")
