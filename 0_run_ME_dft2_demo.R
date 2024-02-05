## 0_run_ME_dft2_demo.R -------------------------------------------------------
## 2023-02-15
## olivier.duperrex@unisante.ch
## 
## This script will : 
## . NOT USED in demo - update data
## . load the demo data
## . identify if some emails are duplicated
## . recode the data
## . create summary table without participants (generic)
## . NOT USED in demo - update texts intro
## . publish the generic report
## . publish the individualised reports



## . ----
## .. print message ----
pacman::p_load(crayon)

timestamp_start <- lubridate::now()

message_00 <-
  glue::glue('{timestamp_start} => 0.0 starting script')
cat(crayon::green(message_00,'\n '))



## . check where you are ----
here::here()

## . load magrittr for pipe ----
pacman::p_load(magrittr, data.table)


## . check which packages are loaded ----
pacman::p_loaded()


## . ----
## . NOT USED in demo - update data : 01a_dft2_update_data_with_REDCapR.R ----
# source(here::here('code', 'dft2', '01a_dft2_update_data_with_REDCapR.R'),
#        encoding = 'UTF-8')

## . load dft2_data_redcapr_raw.RData ----
load(here::here('data', 'redcap_data_raw', 'dft2_data_redcapr_raw.RData'))

### ... total_rows ----
(total_rows <- dft2_data_redcapr_raw[,.N])


### .. number of rows with an email ----
dft2_data_redcapr_raw[!is.na(dft2_0_email), .N]

### ... emails_duplicated ----
(emails_duplicated <- dft2_data_redcapr_raw[, if (.N > 1L) .(N = .N), dft2_0_email])
(dt_emails_duplicated <- dft2_data_redcapr_raw[ , if (.N > 1L) .(record_id = record_id), keyby = .(dft2_0_email)])



## . ----
## . recode and summary tables  ----
### ..  01b_dft2_recode_data.R ----
source(here::here('code', 'dft2', '01b_dft2_recode_data.R'),
       encoding = 'UTF-8')

## .. 01c_dft2_define_cols.R ----
## DO NOT RUN : called by 02a_xx.R, xx_report_generic.Rmd and xx_report_per_participant.Rmd



### .. 02a_prepare_tables_without_participant_id.R ----
### ignore the warning about melt.data.table : it is due to the collapsing step, and is dealt with in the code
# suppressWarnings(
  source(here::here('code', 'dft2', '02a_dft2_prepare_tables_without_participant_id.R'),
       encoding = 'UTF-8')
# )

## .. 02b_dft2_prepare_tables_participants.R ----
## will be called when running individual reports to add the participant's own responses


## . ----
## . NOT USED in demo - update texts intro : 00_update_texts_intro.R ----
## 
# source(here::here('code', '00_update_texts_intro.R'),
#        encoding = 'UTF-8')

# message_02 <- glue::glue('{lubridate::now()} => 02 - {files_to_copy_N} texts copied from server in ../texts_intro/')
# cat(crayon::green(message_02,'\n '))

## . ----
### . publish Rmd ----
### .. dft2_report_generic.Rmd ----
input <-  "analysis/dft2/dft2_report_generic.Rmd"

output_file <- here::here('output', 'reports', 'dft2',
                          stringr::str_glue("dft2_report_generic_{Sys.Date()}.docx"))

rmarkdown::render(
  input = input,
  output_file = output_file)


### .. 05_dft2_to_render_individual_reports.R ------------------------------
### TAKES time ... have a coffee, a walk, a nice chat with someone ...
source(here::here('code', 'dft2', '05_dft2_to_render_individual_reports.R'),
       encoding = 'UTF-8')



## >>  .. table for individual emails << ----
load(here::here('data', 'dft2', 'dft2_data_clean.RData'))

dir_with_reports <- here::here('output', 'reports', 'dft2', 'report_by_participant')

dir_with_reports

foo1 <- 
  dir_with_reports |> 
  fs::dir_info() |> 
  setDT()

(date_produced <- foo1[, max(modification_time)] |> as.IDate())

dft2_table_for_sending_individual_reports <- 
  dft2_data_clean[, .(record_id, dft2_0_email)]

dft2_table_for_sending_individual_reports[, path_to_attachment := paste0(
  dir_with_reports,
  "/",
  "dft2_report_participant_",
  record_id,
  "_",
  date_produced,
  ".docx"
)]

dft2_table_for_sending_individual_reports %>%
  writexl::write_xlsx(path = here::here('output', 'checks', 'dft2_table_for_sending_individual_reports.xlsx'))


