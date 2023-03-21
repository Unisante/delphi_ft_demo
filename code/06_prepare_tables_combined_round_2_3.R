## 06_prepare_tables_combined_round_2_3.R
## olivier.duperrex@unisante.ch
## 2023-03-13

## this script will combine only the type1 tables and graphs without individual data
## from dft2 (round 2) and dft3 (round 3)
## 
## 
## 0.a load libraries ----
pacman::p_loaded()

pacman::p_load(flextable, data.table, magrittr, ggplot2, ftExtra, sjPlot)

pacman::p_loaded()

## 0.b source 00_functions.R ----------------------------------------------
source(here::here('code', '00_functions.R'), encoding = 'UTF-8')

## 1. load RData ----
### 1.1 create a list of RData to load ----
files_to_load <- c(
  'dft2_type1_zz_combined',
  'dft3_type1_zz_combined'
)

### 1.2. put them in a tibble and create filename including path ----
list_files <- tibble::tibble(
  files_to_load = files_to_load,
  files = paste0(files_to_load, '.RData'),
  filename = here::here('output', 'RData', files)
)

### 1.3. purrr::walk the list to load the RData ----
purrr::walk(list_files$filename, load, envir = .GlobalEnv)




## 2. combine them ----
## use rbindlist to append the tables

# setdiff(names(type1_zz_combined_round_2), names(type1_zz_combined))

type1_zz_combined_round_2_3 <- rbindlist(list(dft2_type1_zz_combined, dft3_type1_zz_combined))


## 3. create variables for exec summary ----

type1_zz_combined_round_2_3[, dft_round := stringr::str_extract(variable, "[0-9]+")]

## .. agreement_and_consensus ----
## in both rounds
type1_zz_combined_round_2_3[agreement == 'ok' &
                              consensus == 'ok', 
                            agreement_and_consensus := 1]

## .. group_exec_summary ----
## agree_with_consensus in round 2 --
## make sure to update statement_numbers_in_dft2_to_keep_for_execsummary in 000_parameters.R
## some of the statements of round 2 have been refined in dft3, so are not kept in executive summary
type1_zz_combined_round_2_3[dft_round == 2 &
                              statement_number %in% statement_numbers_in_dft2_to_keep_for_execsummary,
                            group_exec_summary := 'agree_with_consensus']

## agree_with_consensus in round 3 --
type1_zz_combined_round_2_3[dft_round == 3 &
                              agreement == 'ok' &
                              consensus == 'ok', 
                            group_exec_summary := 'agree_with_consensus']

## disagree_with_consensus in round 3 --
type1_zz_combined_round_2_3[dft_round == 3 &
                              agreement == 'not_ok' &
                              consensus == 'ok', 
                            group_exec_summary := 'disagree_with_consensus']


## agree_without_consensus in round 3 --
type1_zz_combined_round_2_3[dft_round == 3 &
                              agreement == 'ok' &
                              is.na(consensus), 
                            group_exec_summary := 'agree_without_consensus']

## disagree_without_consensus in round 3 --
type1_zz_combined_round_2_3[dft_round == 3 &
                              agreement == 'not_ok' &
                              is.na(consensus), 
                            group_exec_summary := 'disagree_without_consensus']


## no_agree in round 3 --
type1_zz_combined_round_2_3[dft_round == 3 &
                              is.na(agreement), 
                            group_exec_summary := 'no_agree']

chk_recode_exec_summary <- type1_zz_combined_round_2_3[, .N, keyby = .(dft_round, agreement, consensus, group_exec_summary)]
chk_recode_exec_summary

writexl::write_xlsx(
  chk_recode_exec_summary,
  path = here::here('output', 'checks', 'chk_recode_exec_summary.xlsx')
)

## 4.1 add a row with section_txt as subheader ----

names(type1_zz_combined_round_2_3)

## .. get the list of values ----
list_group_exec_summary <- type1_zz_combined_round_2_3[!is.na(group_exec_summary), unique(group_exec_summary)]
list_section <- type1_zz_combined_round_2_3[!is.na(section), unique(section)]

## .. create a new empty table ----
## using Cross-Join from data.table
dt0 <- CJ(group_exec_summary = list_group_exec_summary, section = list_section)

## .. add type ---
dt0[, type := 'text']

## .. combine type1_zz_combined_round_2_3 with dt0 ----
type1_zz_combined_round_2_3 <-
  rbindlist(list(type1_zz_combined_round_2_3, dt0), fill = TRUE)

## .. reorder ----
type1_zz_combined_round_2_3 <- type1_zz_combined_round_2_3[order(group_exec_summary, section, type)]


## 4.2 recode section ----

type1_zz_combined_round_2_3[, .N, section]


type1_zz_combined_round_2_3[, section_txt := fcase(
  section == "Z", label_section_Z_long,
  section == "A", label_section_A,
  section == "B", label_section_B,
  section == "C", label_section_C
                            
)]


type1_zz_combined_round_2_3[, section_order := fcase(
  section == "Z", 1,
  section == "A", 2,
  section == "B", 3,
  section == "C", 4
  )]


chk_recode_section <- type1_zz_combined_round_2_3[, .N, keyby = .(section_order, section, section_txt)]
chk_recode_section


## 4.3 img_path ----
## define the path and the name of the image to correspond to variable name
type1_zz_combined_round_2_3[!is.na(variable), img_path := here::here('output', 'png', paste0(variable, ".png"))]


setcolorder(type1_zz_combined_round_2_3, c('dft_round', 'group_exec_summary', 'section_order', 'section', 'section_txt'))

setorder(type1_zz_combined_round_2_3, group_exec_summary, section_order)

## https://stackoverflow.com/a/37881577/6176250
## setorder(dumdt[, .r := order(to_ord)], .r)[, .r := NULL]

chk1 <-
  type1_zz_combined_round_2_3[, .(
    section,
    section_txt,
    dft_round,
    statement_number,
    variable,
    agreement,
    consensus,
    group_exec_summary,
    agreement_and_consensus
  )]

chk1 %>% sjmisc::frq(group_exec_summary)



## 4.4 recode glue variables ----
## transform class glue to as.character because flextable:as_grouped_data has problem with data.table::rbindlist when creating subtables
type1_zz_combined_round_2_3[, responses := as.character(responses)]
type1_zz_combined_round_2_3[, stats := as.character(stats)]



## 4.5 recode var_label ----
## the var_label contains the statement_number after a statement_txt
type1_zz_combined_round_2_3[statement_number == 10, var_label]

## . so we get rid of the elements with statement number for all the labels ----
## see add_var_label_exec_summary() in 00_functions.R for details

type1_zz_combined_round_2_3[, var_label_exec_summary := add_var_label_exec_summary(var_label, statement_txt, statement_number)]

type1_zz_combined_round_2_3[statement_number == 10, .(var_label, var_label_exec_summary)]


type1_zz_combined_round_2_3[type == 'text', var_label_exec_summary := section_txt]

## replace NULL by NA for the plot cols ----
type1_zz_combined_round_2_3[, names(type1_zz_combined_round_2_3) := lapply(.SD, function(x) replace(x, x=='NULL', ''))]


## .. and check that is what you want
chk2 <-
  type1_zz_combined_round_2_3[, .(
    section,
    type,
    variable,
    var_label,
    var_label_exec_summary)]





## 5.  save ----
## 
save(type1_zz_combined_round_2_3, file = here::here('output', 'RData', 'type1_zz_combined_round_2_3.RData'))

writexl::write_xlsx(type1_zz_combined_round_2_3,
                    path = here::here('output', 'checks', 'type1_zz_combined_round_2_3.xlsx'))
