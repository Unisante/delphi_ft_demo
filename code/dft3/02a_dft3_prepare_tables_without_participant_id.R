## 02a_dft3_prepare_tables_without_participant_id.R
## olivier.duperrex@unisante.ch
## 2022-09-13

## this script will generate the tables and graphs without individual data
## in principle, only type 1

 
## 0.a load libraries ----
pacman::p_loaded()

pacman::p_load(flextable, data.table, magrittr, ggplot2, ftExtra, sjPlot)

pacman::p_loaded()

## 0.b source 00_functions.R ----------------------------------------------
source(here::here('code', '00_functions.R'), encoding = 'UTF-8')


## 1. load RData ----
### 1.1 create a list of RData to load ----
files_to_load <- c('dft3_data_clean',
                   'dft3_lookup_final')

### 1.2. put them in a tibble and create filename including path ----
list_files <- tibble::tibble(
  files_to_load = files_to_load,
  files = paste0(files_to_load, '.RData'),
  filename = here::here('data', 'dft3', files)
)

### 1.3. purrr::walk the list to load the RData ----
purrr::walk(tidyselect::all_of(list_files$filename), load, envir = .GlobalEnv)

names(dft3_data_clean)




### 1.4. total_participants ----
(total_participants <- dft3_data_clean[,.N])


### 1.5 source 01d_define_cols.R ----
source(here::here('code', 'dft3', '01c_dft3_define_cols.R'), encoding = 'UTF-8')




## . ----
## 2. > cols_type1 -------------------------------------------------
### select cols ending with _type1
# cols_type1 <- dft3_data_clean[, names(.SD), .SDcols = patterns('_type1$')]
cols_type1



### 2.1 dft3_dt_type1_m ----------------------------------------------------
### subset and melt in one go ---
dft3_dt_type1_m <- dft3_data_clean[, .SD, .SDcols = c('record_id', cols_type1)] %>%
   melt('record_id', value.name = "item", na.rm = T) %>%
  sjlabelled::remove_label()

dft3_dt_type1_m

## check all values are between 1 and 9 ---
dft3_dt_type1_m %>% sjmisc::frq(item)

testthat::expect_equal(
  dft3_dt_type1_m[item > 9, .N],
  0) # if error : need to recode


### 2.2 dft3_t1_type1 -------------------------------------------------------
## idea is to create separate tables and then merge them before using flextable
## but a more efficient way is to :
## add   summary_cols <- list( var_label	=	var_label, ...) in function create_summary_type1 (see 00_functions.R)
## and then use of rbindlist !
## credit : : https://stackoverflow.com/a/72750112/6176250
dft3_t1_type1 <-
  dft3_data_clean[, rbindlist(lapply(.SD, create_summary_type1),
                              idcol = "variable"),
                  .SDcols = cols_type1]

dft3_t1_type1
names(dft3_t1_type1) 



### 2.4. add section, responses and stats -------------------------------
## extract_section and others are defined in 00_functions.R

dft3_t1_type1[, `:=`(
  section = extract_section(variable),
  type = extract_type(variable),
  statement_number = extract_statement_number(variable),
  responses = glue::glue(
    "{reponses_count} ({formattable::percent(response_rate, 1)})",
    .envir = .SD
  ),
  # stats = glue::glue("{median} [{q1} - {q3}]\n({min} - {max})", .envir = .SD),
  # stats = glue::glue("[{q1} - {q3}]\n({min} - {max})", .envir = .SD),
  stats = glue::glue("[{q1}-{q3}]\n({min}-{max})", .envir = .SD),
  iqr_range = as.numeric(q3) - as.numeric(q1)
), keyby = variable]



setcolorder(dft3_t1_type1, 
            c('section', 'type', 'statement_number'))



### 2.5 dft3_type1_zz1 ------------------------------------------------------
dft3_type1_zz1 <- dft3_t1_type1

dft3_type1_zz1

dft3_type1_zz1[median >= agreement_threshold_type1, agreement := "ok"]
dft3_type1_zz1[median <= disagreement_threshold_type1, agreement := "not_ok"]

dft3_type1_zz1[agreement == "ok", agreement_icon := icon_agreement_reached]      
dft3_type1_zz1[agreement == "not_ok", agreement_icon := icon_disagreement_reached] 

dft3_type1_zz1[iqr_range <= consensus_threshold_type1, consensus := "ok"]
dft3_type1_zz1[consensus == "ok", consensus_icon := icon_ok] # icon_ok is for tick sign or ok sign



### 2.6 z_histo --------------------------------------------------------
z_histo <- dft3_dt_type1_m[,
                      lapply(.SD, function(x) list(gg_histo(x))),
                      by = c('variable'), .SDcols = c('item')
]


setnames(z_histo, 'item', 'histo')
z_histo



### 2.7 z_boxplot ------------------------------------------------------
z_boxplot <- dft3_dt_type1_m[,
                        lapply(.SD, function(x)
                          list(gg_boxplot(x))),
                        by = c('variable'), .SDcols = c('item')]

setnames(z_boxplot, 'item', 'boxplot')
z_boxplot


# ### 2.6_bis z_bar --------------------------------------------------------
z_bar <- dft3_dt_type1_m[,
                    lapply(.SD, function(x) list(gg_bar(x))),
                    by = c('variable'), .SDcols = c('item')
]


setnames(z_bar, 'item', 'bar')
z_bar



### 2.7_bis z_boxplot_bis ------------------------------------------------------
z_boxplot_bis <- dft3_dt_type1_m[,
                            lapply(.SD, function(x)
                              list(gg_boxplot_bis(x))),
                            by = c('variable'), .SDcols = c('item')]

setnames(z_boxplot_bis, 'item', 'boxplot_bis')
z_boxplot_bis



### 2.8 dft3_type1_zz_combined : join the three tables -------------------------------------------


dft3_type1_zz_combined <- dft3_type1_zz1[z_boxplot, on = 'variable'][z_histo, on = 'variable'][z_boxplot_bis, on = 'variable'][z_bar, on = 'variable']


dft3_type1_zz_combined
names(dft3_type1_zz_combined)
class(dft3_type1_zz_combined)


### 2.9 cols_type1_no_opinion ----
cols_type1_no_opinion <- dft3_data_clean[, names(.SD), .SDcols = patterns('_type1_no_op')]
cols_type1_no_opinion

### 2.10 dft3_type1_zz0_no_opinion -------------------------------------------

dft3_type1_zz0_no_opinion <- dft3_data_clean[, .(
  item_name = names(.SD), 
  item = no_op_short,
  n = lapply(.SD, function(x) {sum(!is.na(x))})
), .SDcols = c(cols_type1_no_opinion)]

dft3_type1_zz0_no_opinion[, n := as.numeric(n)]
dft3_type1_zz0_no_opinion[, variable := stringr::word(item_name, 1, sep = stringr::fixed("_no_op"))]
dft3_type1_zz0_no_opinion

## . ----
## 3. > dft3_dt_comments_m ----
## there are comments for each question and one for each section !



cols_comments <- dft3_data_clean[, names(.SD), .SDcols = patterns('_comment$')]
cols_comments


## subset ---
dft3_dt_comments <- dft3_data_clean[, .SD, .SDcols = c('record_id', cols_comments)]
class(dft3_data_clean$record_id)


## melt : wide to long ---
dft3_dt_comments_m <- dft3_dt_comments %>%
  melt('record_id', value.name = "item", na.rm = T) %>%
  sjlabelled::remove_label()

## add section and statement number ---
dft3_dt_comments_m[, section := extract_section(variable)]
dft3_dt_comments_m[, statement_number:= extract_statement_number(variable)]


## . ----
## 4.1 save RData ---------------------------------------------------

save(dft3_type1_zz_combined, file = here::here('output', 'RData', 'dft3_type1_zz_combined.RData'))

save(dft3_dt_comments_m, file = here::here('output', 'RData', 'dft3_dt_comments_m.RData'))


## 4.2 save to xlsx ----
writexl::write_xlsx(dft3_type1_zz0_no_opinion, path = here::here('output', 'checks', 'dft3_type1_zz0_no_opinion.xlsx'))
