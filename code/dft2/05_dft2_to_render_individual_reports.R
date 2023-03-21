## 05_dft2_to_render_individual_reports.R
## olivier.duperrex@unisante.ch
## 2022-09-13


here::here()

## .. print message ----
pacman::p_load(crayon)

timestamp_start <- lubridate::now()

message_00 <-
  glue::glue('{timestamp_start} => 0.0 starting script')
cat(crayon::green(message_00,'\n '))

## load package ----
pacman::p_load(rmarkdown, knitr, data.table)


## load dft2_data_clean ----
load(here::here('data', 'dft2', 'dft2_data_clean.RData'))
class(dft2_data_clean)
names(dft2_data_clean)

## list_record_id : list of individual record_id ----
list_record_id <- dft2_data_clean[order(record_id)][, record_id]
list_record_id
length(list_record_id)


## dft2_report_per_participant.Rmd ------------------------------------------
# list_record_id <- c(2,3)

list_record_id

for(i in 1:length(list_record_id)){
  
  input <-  here::here('analysis', 'dft2', 'dft2_report_per_participant.Rmd')
  
  output_file <- here::here('output', 'reports', 'dft2', 'report_by_participant',
                           stringr::str_glue("dft2_report_participant_{list_record_id[i]}_{Sys.Date()}.docx"))
  
  rmarkdown::render(
    input = input,
    output_file = output_file,
    params = list(record_id_selected = list_record_id[i]))

}


(duration <-  lubridate::now() - timestamp_start)

message_01 <- glue::glue('{lubridate::now()} => {length(list_record_id)} individuals reports created in {round(duration, 1)} minutes
                         in ../output/reports/dft2/report_by_participant/')
cat(crayon::green(message_01,'\n '))

