## 06_prepare_tables_combined_round_2_3.R
## olivier.duperrex@unisante.ch
## 2023-10-25

## this script will combine only the type1 tables and graphs without individual data
## from dft2 (round 2) and dft3 (round 3)
##

## 0.a load libraries ----
pacman::p_loaded()

pacman::p_load(flextable, data.table, magrittr, ggplot2, ftExtra, sjPlot)

pacman::p_loaded()

## 0.b source 00_functions.R ----------------------------------------------
source(here::here('code', '00_functions.R'), encoding = 'UTF-8')

## 1. load RData ----
### 1.1 create a list of RData to load ----
files_to_load <-
  c('dft2_type1_zz_combined',
    'dft3_type1_zz_combined')

### 1.2. put them in a tibble and create filename including path ----
list_files <-
  tibble::tibble(
    files_to_load = files_to_load,
    files = paste0(files_to_load, '.RData'),
    filename = here::here('output', 'RData', files)
  )

### 1.3. purrr::walk the list to load the RData ----
purrr::walk(list_files$filename, load, envir = .GlobalEnv)


## 2. combine them ----
## use rbindlist to append the tables

# setdiff(names(type1_zz_combined_round_2), names(type1_zz_combined))

type1_zz_combined_round_2_3 <-
  rbindlist(list(dft2_type1_zz_combined, dft3_type1_zz_combined))


## 3. create variables for exec summary ----

type1_zz_combined_round_2_3[, dft_round := stringr::str_extract(variable, "[0-9]+")]

## . group_exec_summary ----
## agree_with_consensus in round 2 --
## !! make sure you have updated statement_numbers_in_dft2_to_keep_for_execsummary in section 4 of 000_parameters.R !! ----

## .. agree_with_consensus ----

type1_zz_combined_round_2_3[agreement == 'ok' &
                              consensus == 'ok',
                            `:=`(agreement_and_consensus = 1,
                                 group_exec_summary = 'agree_with_consensus')]

## .. disagree_with_consensus ----
type1_zz_combined_round_2_3[agreement == 'not_ok' &
                              consensus == 'ok',
                            group_exec_summary := 'disagree_with_consensus']


## .. agree_without_consensus ----
type1_zz_combined_round_2_3[agreement == 'ok' &
                              is.na(consensus),
                            group_exec_summary := 'agree_without_consensus']

## .. disagree_without_consensus ----
type1_zz_combined_round_2_3[agreement == 'not_ok' &
                              is.na(consensus),
                            group_exec_summary := 'disagree_without_consensus']


## .. no_agree ----
type1_zz_combined_round_2_3[is.na(agreement),
                            group_exec_summary := 'no_agree']

## .. add variable to_keep for overall report ----
type1_zz_combined_round_2_3[dft_round == 2 &
                              statement_number %in% statement_numbers_in_dft2_to_keep_for_execsummary, 
                            to_keep := 1][dft_round == 3, to_keep := 1]


chk_recode_exec_summary_all <- type1_zz_combined_round_2_3[, .N, keyby = .(dft_round, agreement, consensus, group_exec_summary, to_keep)]

chk_recode_exec_summary_all

writexl::write_xlsx(
  chk_recode_exec_summary_all,
  path = here::here('output', 'checks', 'chk_recode_exec_summary_all.xlsx')
)

## 4.0 dt_exec_summary ----
dt_exec_summary <- 
  type1_zz_combined_round_2_3[to_keep == 1,]

## 4.1 add a row with section_txt as subheader ----

names(dt_exec_summary)

## .. get the list of values ----
list_group_exec_summary <- 
  dt_exec_summary[!is.na(group_exec_summary), unique(group_exec_summary)]

list_section <- 
  dt_exec_summary[!is.na(section), unique(section)]

## .. create a new empty table ----
## using Cross-Join from data.table
dt0 <- 
  CJ(group_exec_summary = list_group_exec_summary, 
     section = list_section)

## .. add type ---
## will be used in the word document as tables headings !
dt0[, type := 'text']

## .. combine dt_exec_summary with dt0 ----
dt_exec_summary <-
  rbindlist(list(dt_exec_summary, dt0), fill = TRUE)

## .. reorder ----
dt_exec_summary |> 
  setcolorder(c('group_exec_summary', 'section', 'type'))

dt_exec_summary <- 
  dt_exec_summary[order(group_exec_summary, section, type)]


## 4.2 recode section ----

dt_exec_summary[, .N, section]


dt_exec_summary[, section_txt := fcase(
  section == "Z", label_section_Z_long,
  section == "A", label_section_A,
  section == "B", label_section_B,
  section == "C", label_section_C
  
)]


dt_exec_summary[, section_order := fcase(
  section == "Z", 1,
  section == "A", 2,
  section == "B", 3,
  section == "C", 4
)]


chk_recode_section <- 
  dt_exec_summary[, .N, keyby = .(section_order, section, section_txt)]

chk_recode_section


## 4.3 img_path ----
## define the path and the name of the image to correspond to variable name
dt_exec_summary[!is.na(variable), 
                img_path := here::here('output', 'png', paste0(variable, ".png"))]


setcolorder(dt_exec_summary, 
            c('dft_round', 'group_exec_summary', 'section_order', 'section', 'section_txt'))

setorder(dt_exec_summary, 
         group_exec_summary, section_order)

## https://stackoverflow.com/a/37881577/6176250
## setorder(dumdt[, .r := order(to_ord)], .r)[, .r := NULL]

chk1 <-
  dt_exec_summary[, .(
    to_keep,
    group_exec_summary,
    agreement_and_consensus,
    section,
    section_txt,
    dft_round,
    statement_number,
    variable,
    agreement,
    consensus
  )]

chk1 %>% sjmisc::frq(group_exec_summary)



## 4.4 recode glue variables ----
## transform class glue to as.character because flextable:as_grouped_data has problem with data.table::rbindlist when creating subtables
dt_exec_summary[, responses := as.character(responses)]
dt_exec_summary[, stats := as.character(stats)]



## 4.5 recode var_label ----
## the var_label contains the statement_number after a statement_txt
dt_exec_summary[statement_number == 1, var_label]

## . recode 'Affirmation' to 'Enoncé' ----
dt_exec_summary[var_label %like% 'Affirmation', var_label := stringr::str_replace_all(var_label, 'Affirmation', 'Enoncé')]

## . so we get rid of the elements with statement number for all the labels ----
## see add_var_label_exec_summary() in 00_functions.R for details

dt_exec_summary[, var_label_exec_summary := add_var_label_exec_summary(var_label, statement_txt, statement_number)]

dt_exec_summary[statement_number == 1, .(var_label, var_label_exec_summary)]


dt_exec_summary[type == 'text', var_label_exec_summary := section_txt]

## . and we replace NULL by NA to avoid errors in the plot cols ----
dt_exec_summary[, names(dt_exec_summary) := lapply(.SD, function(x) replace(x, x=='NULL', ''))]


## .. and check that is what you want
chk2 <-
  dt_exec_summary[, .(
    section,
    type,
    variable,
    var_label,
    var_label_exec_summary)]





## 5.  save ----
## 
save(dt_exec_summary, file = here::here('output', 'RData', 'dt_exec_summary.RData'))

writexl::write_xlsx(dt_exec_summary,
                    path = here::here('output', 'checks', 'dt_exec_summary.xlsx'))
