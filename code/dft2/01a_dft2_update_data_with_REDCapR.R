## 01a_dft2_update_data_with_REDCapR.R ----
## olivier.duperrex@unisante.ch
## 2023-03-13
## 
## 

## Aim : Import data and metadata from a REDCap Project and save it as RData (in data.table format)
## Credit : most of the code is coming from the REDCapR vignette
 


## This script will:
## 1. read  raw data from REDCap with REDCapR
## . metadata
## . data_redcapr_raw
## 2. save the files


## Comments :
## . fast and simple
## . factors are transformed as numeric automatically (good for statistical summary, but not for if you want levels)
## . use of data.table mostly (and some tidyverse)
## . for levels : 
## .. https://ouhscbbmc.github.io/data-science-practices-1/coding.html#coding-simplify-recoding => lookup table: "It is feasible to recode 6 levels of race variable directly in R, but it’s less feasible to recode 200 provider names. Specify the mapping in a csv, then use readr to convert the csv to a data.frame, and finally left join it."
## => see 01b_dft2_recode_data.R which will create lookup tables

## Glossary
## . variable/field have names (raw) and labels (label)
## . multiple choice fields have (raw) coded values and labels (label)

## 0.a libraries ----------------------------------------------------
pacman::p_load(REDCapR, magrittr, data.table)



## 0.2 get your token fo r the database ------------------------------
## path_to_my_token is defined in '000_parameters.R'
source(here::here('code', '000_parameters.R'), encoding = 'UTF-8') 


source(path_to_my_token, encoding = 'UTF-8') # returns uri_redcap_your_institution and token_dft2_your_project


token <-  token_dft2

## 1.0 metadata ------------------------------------------------------
dft2_metadata <-
  REDCapR::redcap_metadata_read(
    redcap_uri = redcap_uri,
    token = token
    # forms = NULL,
    # forms_collapsed = "",
    # fields = NULL,
    # fields_collapsed = "",
    # verbose = TRUE,
    # config_options = NULL
  )$data

data.table::setDT(dft2_metadata)





## 1.1 select fields ------------------------------------------------
## all - activate line below for all fields
desired_fields_v2  <- c("")

## a selection - modify line below as needed
# desired_fields_v2 <- c("feil, uc_ser_bon_sero")


## 1.2 decide if data with raw values or labelled values ------------
raw_or_label <- "raw" # for options of multiple choice fields: raw or label
raw_or_label_headers <- "raw"  # variable/field names (raw) or the field labels (label).


## 1.3 load the data - REDCapR --------------------------------------

dft2_data_redcapr_raw <- REDCapR::redcap_read_oneshot(
  redcap_uri = redcap_uri,
  token      = token,
  #fields  = requested_fields_SHS,
  fields_collapsed = desired_fields_v2,
  raw_or_label = raw_or_label, 
  raw_or_label_headers = raw_or_label_headers
  #filter_logic = filters,
  #batch_size = guess_max
)$data %>%
  data.table::setDT()


str(dft2_data_redcapr_raw)

## . check structure of data -----------------------------------------
##  https://ouhscbbmc.github.io/OuhscMunge/
# remotes::install_github("OuhscBbmc/OuhscMunge")
OuhscMunge::verify_value_headstart(dft2_data_redcapr_raw)



## 2. save ----------------------------------------------------------
save(dft2_data_redcapr_raw, file = here::here('data', 'redcap_data_raw', 'dft2_data_redcapr_raw.RData'))

save(dft2_metadata, file = here::here('data', 'redcap_data_raw', 'dft2_metadata.RData'))


## 3. checks --------------------------------------------------------

## .. chk_type1_raw ----
chk_type1_raw <-
  dft2_data_redcapr_raw[, .SD, .SDcols = patterns('_type1$'), keyby = record_id] %>%
  data.table::transpose(keep.names = 'variable')

class(chk_type1_raw)
chk_type1_raw

chk_type1_raw %>%
  writexl::write_xlsx(path = here::here('output', 'checks', 'chk_dft2_type1_raw.xlsx'))



## .. chk_type2_raw ----
chk_type2_raw <-
  dft2_data_redcapr_raw[, .SD, .SDcols = patterns('_type2$'), keyby = record_id] %>%
  data.table::transpose(keep.names = 'variable')


chk_type2_raw %>%
  writexl::write_xlsx(path = here::here('output', 'checks', 'chk_dft2_type2_raw.xlsx'))


## .. chk_type3_raw ----
chk_type3_raw <-
  dft2_data_redcapr_raw[, .SD, .SDcols = patterns('_type3__'), keyby = record_id] %>%
  data.table::transpose(keep.names = 'variable')


chk_type3_raw %>%
  writexl::write_xlsx(path = here::here('output', 'checks', 'chk_dft2_type3_raw.xlsx'))

