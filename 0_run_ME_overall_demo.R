## 0_run_ME_overall.R -------------------------------------------------------
## 2023-11-07
## olivier.duperrex@unisante.ch

timestamp_generic_report_start <- lubridate::now()

## . executive summary ----

### . NOT USED in demo - 00_update_texts_intro.R ----
### obtain latest version of introduction texts from the server 
# source(here::here('code', '00_update_texts_intro.R'),
#        encoding = 'UTF-8')
# message_01 <- glue::glue('{lubridate::now()} => 02 - {files_to_copy_N} texts_intro copied from server in ../texts_intro/')
# cat(crayon::green(message_01,'\n '))


### .. 06_prepare_tables_combined_round_2_3.R ----
source(here::here('code', '06_prepare_tables_combined_round_2_3.R'),
       encoding = 'UTF-8')



### .. overall_executive_summary.Rmd ------------------------------------------
input <-  "analysis/overall/overall_executive_summary.Rmd"

output_file <- here::here('output', 'reports', 'overall',
                          stringr::str_glue("dft_overall_executive_summary_{Sys.Date()}.docx"))

rmarkdown::render(
  input = input,
  output_file = output_file)


### .. overall_executive_summary.Rmd ------------------------------------------
input <-  "analysis/overall/overall_executive_summary.Rmd"

output_file <- here::here(
  'output',
  'reports',
  'overall',
  stringr::str_glue("dft_overall_executive_summary_{Sys.Date()}.docx")
)

rmarkdown::render(input = input,
                  output_file = output_file)

timestamp_generic_report_done <- lubridate::now()
# |> as.ITime()
class(timestamp_generic_report_done)


(
  duration <-
    difftime(
      timestamp_generic_report_done,
      timestamp_generic_report_start,
      units = "secs"
    )
)

# as.numeric(duration, units = "mins")
message_03 <-
  glue::glue(
    '{lubridate::now()} => overall report created in {round(duration, 1)} seconds
                         in ../output/reports/dft3/'
  )
cat(crayon::green(message_03, '\n '))
