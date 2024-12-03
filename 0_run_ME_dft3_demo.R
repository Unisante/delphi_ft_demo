## 0_run_ME_dft3_demo.R -------------------------------------------------------
## 2023-11-07
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
cat(crayon::green(message_00, '\n '))



## . check where you are ----
here::here()

## . load magrittr for pipe ----
pacman::p_load(magrittr, data.table)


## . check which packages are loaded ----
pacman::p_loaded()


## . ----
## . NOT USED in demo - update data : update data : 01a_dft3_update_data_with_REDCapR.R ----
# source(here::here('code', 'dft3','01a_dft3_update_data_with_REDCapR.R'),
#        encoding = 'UTF-8')

## . load dft3_data_redcapr_raw.RData ----
load(here::here('data', 'redcap_data_raw', 'dft3_data_redcapr_raw.RData'))


### .. total_rows ----
(total_rows <- dft3_data_redcapr_raw[, .N])

### .. number of rows with an email ----
dft3_data_redcapr_raw[!is.na(dft3_0_email), .N]

### ... dt_emails_duplicated ----

(dt_emails_duplicated <-
    dft3_data_redcapr_raw[, if (.N > 1L)
      .(record_id = record_id), keyby = .(tolower(dft3_0_email))])


## >> . checks --------------------------------------------------------

## .. chk_type1_raw ----
chk_type1_raw <-
  dft3_data_redcapr_raw[, .SD, .SDcols = patterns('_type1$'), keyby = record_id] %>%
  data.table::transpose(keep.names = 'variable')

class(chk_type1_raw)
chk_type1_raw

chk_type1_raw %>%
  writexl::write_xlsx(path = here::here('output', 'checks', 'chk_dft3_type1_raw.xlsx'))



## . ----
## . recode and summary tables  ----
### ..  01b_dft3_recode_data.R ----
source(here::here('code', 'dft3', '01b_dft3_recode_data.R'),
       encoding = 'UTF-8')

### .. total_participants_round_3 ----
(total_participants_round_3 <- dft3_data_clean[, .N])

(total_participants_round_3_complete <- 
    dft3_data_clean[round_3_equestionnaire_complete > 0,.N])

(total_participants_round_3_NA_n <- 
    dft3_data_clean[is.na(round_3_equestionnaire_complete) | round_3_equestionnaire_complete == 0, .N])  ## should be 0

dt_participants_round_3_NA <-
  dft3_data_clean[is.na(round_3_equestionnaire_complete) | round_3_equestionnaire_complete == 0, ]

(dt_emails_duplicated_after_cleaning <-
    dft3_data_clean[, if (.N > 1L)
      .(record_id = record_id), keyby = .(tolower(dft3_0_email))]) ## should be an empty table


## .. 01c_dft3_define_cols.R ----
## DO NOT RUN : called by 02a_xx.R, xx_report_generic.Rmd and xx_report_per_participant.Rmd

### .. 02a_dft3_prepare_tables_without_participant_id.R ----
suppressWarnings(source(
  here::here(
    'code',
    'dft3',
    '02a_dft3_prepare_tables_without_participant_id.R'
  ),
  encoding = 'UTF-8'
))

## .. 02b_dft3_prepare_tables_participants.R ----
## will be call when running individual reports to add the participant's own responses


## . ----
### . NOT USED in demo - 00_update_texts_intro.R ----
### obtain latest version of introduction texts from the server
# source(here::here('code', '00_update_texts_intro.R'),
#        encoding = 'UTF-8')
#
# message_02 <- glue::glue('{lubridate::now()} => 02 - {files_to_copy_N} texts_intro copied from server in ../texts_intro/')
# cat(crayon::green(message_02,'\n '))



## . ----
## . publish Rmd ----
### .. dft3_report_generic.Rmd ------------------------------------------
input <-  "analysis/dft3/dft3_report_generic.Rmd"

output_file <- here::here(
  'output',
  'reports',
  'dft3',
  stringr::str_glue("dft3_report_generic_{Sys.Date()}.docx")
)

rmarkdown::render(input = input,
                  output_file = output_file)



### . 05_dft3_to_render_individual_reports.R ------------------------------
### ### TAKES time ... have a coffee, a walk, a nice chat with someone ...
source(here::here('code', 'dft3', '05_dft3_to_render_individual_reports.R'),
       encoding = 'UTF-8')


## >>  .. table for individual emails << ----
load(here::here('data', 'dft3', 'dft3_data_clean.RData'))

dir_with_reports <- here::here('output', 'reports', 'dft3', 'report_by_participant')

dir_with_reports

foo1 <- 
  dir_with_reports |> 
  fs::dir_info() |> 
  setDT()

(date_produced <- foo1[, max(modification_time)] |> as.IDate())

dft3_table_for_sending_individual_reports <- 
  dft3_data_clean[, .(record_id, dft3_0_email)]

dft3_table_for_sending_individual_reports[, path_to_attachment := paste0(
  dir_with_reports,
  "/",
  "dft3_report_participant_",
  record_id,
  "_",
  date_produced,
  ".docx"
)]

dft3_table_for_sending_individual_reports %>%
  writexl::write_xlsx(
    path = here::here(
      'output',
      'checks',
      'dft3_table_for_sending_individual_reports.xlsx'
    )
  )

