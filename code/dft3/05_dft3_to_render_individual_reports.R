## 05_dft3_to_render_individual_reports.R
## olivier.duperrex@unisante.ch
## 2023-10-24



here::here()

## .. print message ----
pacman::p_load(crayon)

timestamp_start <- lubridate::now()

message_00 <-
  glue::glue('{timestamp_start} => 0.0 starting script')
cat(crayon::green(message_00,'\n '))

## load package ----
pacman::p_load(rmarkdown, knitr, data.table)

## load dft3_data_clean ----
load(here::here('data', 'dft3',  'dft3_data_clean.RData'))
class(dft3_data_clean)
names(dft3_data_clean)

## list_record_id : list of individual record_id ----
list_record_id <- dft3_data_clean[order(record_id)][, record_id]
list_record_id

## dft3_report_per_participant.Rmd ------------------------------------------
## uncomment one of the line below to test with a few
## remember to comment it again when you are ready to create all the individual reports

# list_record_id <- head(list_record_id, n = 4)
# list_record_id <- tail(list_record_id, n = 10)
# list_record_id

list_record_id

for(i in 1:length(list_record_id)){

  input <-  "analysis/dft3/dft3_report_per_participant.Rmd"
  
  output_file <- here::here('output', 'reports', 'dft3', 'report_by_participant',
                           stringr::str_glue("dft3_report_participant_{list_record_id[i]}_{Sys.Date()}.docx"))
  
  rmarkdown::render(
    input = input,
    output_file = output_file,
    params = list(record_id_selected  = list_record_id[i]))
  
  ## Important step ---
  # clear most of what is in memory between loops (otherwise blocks with child sections)
  # but keep the list_record_id we are looping through (created above)
  # and the RData which were loaded by the first input file (list_files$files_to_load)
  # to save time ;-) 
  
  list_to_keep <- c('list_record_id', list_files$files_to_load, 'timestamp_start')
  
  # https://stackoverflow.com/a/15833737/6176250
  remove(list = ls()[!ls() %in% list_to_keep]) # remove from the list of objects the ones that are not in list_to_keep
}      


(duration <-  lubridate::now() - timestamp_start)

message_01 <- glue::glue('{lubridate::now()} => {length(list_record_id)} individuals reports created in {round(duration, 1)} minutes
                         in ../output/reports/dft3/report_by_participant/')
cat(crayon::green(message_01,'\n '))
