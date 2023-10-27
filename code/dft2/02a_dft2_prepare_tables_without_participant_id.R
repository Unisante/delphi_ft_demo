## 02a_dft2_prepare_tables_without_participant_id.R
## olivier.duperrex@unisante.ch
## 2023-10-24

## this script will generate the tables and graphs without individual data

 
## 0. load libraries ----
pacman::p_loaded()

pacman::p_load(flextable, data.table, magrittr, ggplot2, ftExtra, sjPlot)

pacman::p_loaded()

## 1.1 load RData ----

load(here::here('data', 'dft2', 'dft2_data_clean.RData'))
load(here::here('data', 'dft2', 'dft2_lookup_final.RData'))
load(here::here('data', 'dft2', 'dft2_lookup_value_labels_final.RData'))

## 1.2 source 00_functions.R ----------------------------------------------
source(here::here('code', '00_functions.R'), encoding = 'UTF-8')
# source(here::here('code', '_sanpit_new_plots_bar_boxplot_bis.R'), encoding = 'UTF-8')


## 1.3 total_participants ----
(total_participants <- dft2_data_clean[,.N])


## 1.4 source 01d_define_cols.R ----
source(here::here('code', 'dft2', '01c_dft2_define_cols.R'), encoding = 'UTF-8')

# dft2_type3_zz0 ## see below - different approach because of radio buttons as separate variables in dft2_data_clean



## . ----
## 2. > cols_0_subset_for_table_1 : characteristics --------------------------------


cols_type0 <- c("dft2_0_gender",
                "dft2_0_job" ,
                # "dft2_0_job_o",
                "dft2_0_joblang", 
                "dft2_0_conflicts")
cols_type0


## . dft2_type0_value_labels_zz0 ----
## .. cols_0_subset_for_table_1
dft2_type0_value_labels_zz0 <-
  dft2_lookup_value_labels_final[field_name %in% cols_0_subset_for_table_1, 
                                 .(variable = field_name, 
                                   item = id_value_labels, 
                                   value_labels)]

## .. recode canton ----
dft2_type0_value_labels_zz0[, value_labels := stringr::str_remove(value_labels, 'Canton de |Canton du ')]

names(dft2_type0_value_labels_zz0)

## .. joblang ----
# dft2_type0_value_labels_zz0[variable == 'dft2_0_joblang',]

## .. job ----

# dft2_type0_value_labels_zz0[variable == 'dft2_0_job',]



## . dft2_dt_type0_value_labels_m ----

dft2_dt_type0_value_labels <- 
  dft2_data_clean[, .SD, .SDcols = c('record_id', cols_0_subset_for_table_1)]

dft2_dt_type0_value_labels_m <- 
  dft2_dt_type0_value_labels %>%
  melt('record_id', value.name = "item", na.rm = T)


## . dft2_type0_value_labels_counts ----
dft2_type0_value_labels_counts <-
  dft2_dt_type0_value_labels_m[, .N, keyby = .(variable, item)]

str(dft2_type0_value_labels_counts)

dft2_type0_value_labels_counts[, item := as.numeric(item)]
## . dft2_type0_value_labels_zz1 ------------------------------------------------------


dft2_type0_value_labels_zz1 <-
  dft2_type0_value_labels_zz0[dft2_type0_value_labels_counts, 
                              on = .(variable, item),
                              n := N]

### .. replace NA by 0 ----
dft2_type0_value_labels_zz1[is.na(n), n := 0]


### .. add prop ----
dft2_type0_value_labels_zz1[, prop := formattable::percent(n / total_participants, 1), keyby = variable]


### .. order rows ----
dft2_type0_value_labels_zz1 <-
  dft2_type0_value_labels_zz1[order(
    variable,
    value_labels %like% other_country,
    value_labels %like% other_please_short,
    -prop
  )]

### .. left join to add variable label ----
dft2_type0_value_labels_zz1[dft2_lookup_final, 
                            on = 'variable', 
                            variable_label := variable_label]

setcolorder(dft2_type0_value_labels_zz1, 'variable_label')



## . dft2_type0_3_zz0 -------------------------------------------------------
dft2_type0_3_zz0 <-
  dft2_data_clean[, .(
    item_name = names(.SD),
    value_labels = lapply(.SD, sjlabelled::get_label),
    n = lapply(.SD, sum)
  ), .SDcols = cols_0_subset_type3]

### .. extract variable from item_name ----
dft2_type0_3_zz0[, variable := stringr::word(item_name, 1, sep = stringr::fixed("__"))]
dft2_type0_3_zz0[, item := stringr::str_extract(item_name, "(\\d+$)")]

dft2_type0_3_zz0[, n := as.numeric(n)]

### .. replace NA by 0 ----
dft2_type0_3_zz0[is.na(n), n := 0]

### .. add prop ----
dft2_type0_3_zz0[, prop := formattable::percent(n / total_participants, 1), keyby = variable]



names(dft2_type0_value_labels_zz1)
names(dft2_type0_3_zz0)

### .. left join to add variable label ----
dft2_type0_3_zz0[dft2_lookup_final,
                 on = 'variable', 
                 variable_label := variable_label]
setcolorder(dft2_type0_3_zz0, 
            names(dft2_type0_value_labels_zz1))


## . dft2_type0_zz1 ------------------------------------------------------
dft2_type0_zz1 <-
  rbindlist(list(dft2_type0_value_labels_zz1 ,
                 dft2_type0_3_zz0[ , .SD, .SDcols = !'item_name']), fill = TRUE)

dft2_type0_zz1[, value_labels := as.character(value_labels)  ]

## .. sort according to questionnaire ----
## `order_dft2_0_table_1` is defined in '000_parameters.R'
## thanks to https://tim-tiefenbach.de/post/2023-ordering-rows/ !
dft2_type0_zz1 <- 
  dft2_type0_zz1[order(factor(variable, levels = order_dft2_0_table_1))]



## . ----
## 3. > cols_type1 -------------------------------------------------
### select cols ending with _type1
cols_type1  # defined in 01c_dft2_define_cols.R


### 2.1 dft2_dt_type1_m ----------------------------------------------------
### subset and melt in one go ---
dft2_dt_type1_m <- dft2_data_clean[, .SD, .SDcols = c('record_id', cols_type1)] %>%
  melt('record_id', value.name = "item", na.rm = T)  %>%
  sjlabelled::remove_label()

dft2_dt_type1_m

## check all values are between 1 and 9 ---
dft2_dt_type1_m %>% sjmisc::frq(item)

testthat::expect_equal(
  dft2_dt_type1_m[item > 9, .N],
  0) # if error : need to recode



## create separate tables and then merge them before using flextable
### 2.2 dft2_t1_type1 -------------------------------------------------------
## idea is to create separate tables and then merge them before using flextable
## but a more efficient way is to :
## add   summary_cols <- list( var_label	=	var_label, ...) in function create_summary_type1 (see 00_functions.R)
## and then use of rbindlist !
## credit : : https://stackoverflow.com/a/72750112/6176250
## 
dft2_t1_type1 <-
  dft2_data_clean[,
                  rbindlist(lapply(.SD, create_summary_type1),
                            idcol = "variable"),
                  .SDcols = cols_type1]
dft2_t1_type1


### 2.4. add section, responses and stats -------------------------------
## extract_section and others are defined in 00_functions.R
dft2_t1_type1[, `:=`(
  section = extract_section(variable),
  type = extract_type(variable),
  statement_number = extract_statement_number(variable),
  responses = glue::glue(
    "{reponses_count} ({formattable::percent(response_rate, 1)})",
    .envir = .SD
  ),
  # stats = glue::glue("{median} [{q1} - {q3}]\n({min} - {max})", .envir = .SD),
  # stats = glue::glue("[{q1} - {q3}]\n({min} - {max})", .envir = .SD),
  # stats = glue::glue("[{q1} - {q3}]\n({min} - {max})", .envir = .SD),
  stats = glue::glue("[{q1}-{q3}]\n({min}-{max})", .envir = .SD),
  iqr_range = as.numeric(q3) - as.numeric(q1)
), keyby = variable]


setcolorder(dft2_t1_type1, 
            c('section', 'type', 'statement_number'))


# dft2_t1_type1[variable %like% 's7']

### 2.5 dft2_type1_zz1 ------------------------------------------------------
dft2_type1_zz1 <- dft2_t1_type1

dft2_type1_zz1

dft2_type1_zz1[median >= agreement_threshold_type1, agreement := "ok"]
dft2_type1_zz1[median <= disagreement_threshold_type1, agreement := "not_ok"]

dft2_type1_zz1[agreement == "ok", agreement_icon := icon_agreement_reached]      
dft2_type1_zz1[agreement == "not_ok", agreement_icon := icon_disagreement_reached] 

dft2_type1_zz1[iqr_range <= consensus_threshold_type1, consensus := "ok"]
dft2_type1_zz1[consensus == "ok", consensus_icon := icon_ok] # icon_ok is for tick sign or ok sign




### 2.6 z_histo --------------------------------------------------------
z_histo <- dft2_dt_type1_m[,
                           lapply(.SD, function(x) list(gg_histo(x))),
                           by = c('variable'), .SDcols = c('item')
]


setnames(z_histo, 'item', 'histo')
z_histo



### 2.7 z_boxplot ------------------------------------------------------
z_boxplot <- dft2_dt_type1_m[,
                             lapply(.SD, function(x)
                               list(gg_boxplot(x))),
                             by = c('variable'), .SDcols = c('item')]

setnames(z_boxplot, 'item', 'boxplot')
z_boxplot

### 2.6_bis z_bar --------------------------------------------------------
z_bar <- dft2_dt_type1_m[,
                         lapply(.SD, function(x) list(gg_bar(x))),
                         by = c('variable'), .SDcols = c('item')
]


setnames(z_bar, 'item', 'bar')
z_bar



### 2.7_bis z_boxplot_bis ------------------------------------------------------
z_boxplot_bis <- dft2_dt_type1_m[,
                                 lapply(.SD, function(x)
                                   list(gg_boxplot_bis(x))),
                                 by = c('variable'), .SDcols = c('item')]

setnames(z_boxplot_bis, 'item', 'boxplot_bis')
z_boxplot_bis


### 2.8 dft2_type1_zz_combined : join the three tables -------------------------------------------

dft2_type1_zz_combined <- dft2_type1_zz1[z_boxplot, on = 'variable'][z_histo, on = 'variable'][z_boxplot_bis, on = 'variable'][z_bar, on = 'variable']

dft2_type1_zz_combined
names(dft2_type1_zz_combined)
# class(dft2_type1_zz_combined)


### 2.9 cols_type1_no_opinion ----
cols_type1_no_opinion <- dft2_data_clean[, names(.SD), .SDcols = patterns('_type1_no_op')]
cols_type1_no_opinion

### 2.10 dft2_type1_zz0_no_opinion -------------------------------------------

dft2_type1_zz0_no_opinion <- dft2_data_clean[, .(
  item_name = names(.SD), 
  item = no_op_short,
  n = lapply(.SD, function(x) {sum(!is.na(x))})
), .SDcols = c(cols_type1_no_opinion)]

dft2_type1_zz0_no_opinion[, n := as.numeric(n)]
dft2_type1_zz0_no_opinion[, variable := stringr::word(item_name, 1, sep = stringr::fixed("_no_op"))]
dft2_type1_zz0_no_opinion


## . ----
## 4. > cols_type2 --------------------------------------------------

### check list of cols type2
cols_type2

if (length(cols_type2) > 0) {
  ### 4.0 dft2_type2_zz0 ----
  ## create an empty table with all possible answers
  dft2_type2_zz0 <-
    dft2_lookup_value_labels_final[field_name %in% cols_type2, .(variable = field_name, item = id_value_labels, value_labels)]
  
  
  ### 4.1. dft2_dt_type2 -----------------------------------------------------
  dft2_dt_type2 <-
    dft2_data_clean[, lapply(.SD, sjlabelled::as_label), .SDcols = c('record_id', cols_type2)]
  head(dft2_dt_type2)
  
  
  ### 4.2 dft2_dt_type2_m : melted table by record id ----------------------
  dft2_dt_type2_m <- dft2_dt_type2 %>%
    melt('record_id', value.name = "item", na.rm = T) %>%
    sjlabelled::remove_label()
  
  dft2_dt_type2_m
  
  dft2_dt_type2_m[, item := clean_variable_label(item, thankyou_string)]
  
  ### 4.3 dft2_type2_counts ----
  dft2_type2_counts <-
    dft2_dt_type2_m[, .N, keyby = .(variable, item)]
  dft2_type2_counts[, item := as.numeric(item)]
  names(dft2_type2_counts)
  str(dft2_type2_counts)
  
  ### 4.4 dft2_type2_zz1 ------------------------------------------------------
  str(dft2_type2_zz0)
  
  dft2_type2_zz1 <- dft2_type2_zz0[dft2_type2_counts, on = .(variable,
                                                             item),
                                   n := N]
  str(dft2_type2_zz1)
  
  ### . replace NA by 0 ----
  dft2_type2_zz1[is.na(n), n := 0]
  
  ### 4.5 add section and variable ----
  ## extract_section and others are defined in 00_functions.R
  dft2_type2_zz1[, `:=`(
    section = extract_section(variable),
    type = extract_type(variable),
    statement_number = extract_statement_number(variable)
  )] %>%
    setcolorder(c('section', 'type', 'statement_number', 'variable', 'item'))
  
  dft2_type2_zz1[, prop := formattable::percent(n / total_participants, 1), keyby = variable]
  dft2_type2_zz1[, minibar := prop] # add a column for later plot
  
  dft2_type2_zz1[prop >= agreement_threshold_type2_3, agreement := "ok"]
  
  dft2_type2_zz1[agreement == "ok", agreement_icon := icon_ok] # icon_ok is for tick sign or ok sign
  
  
  ### 4.6 add variable_label as a column ----
  dft2_type2_zz1[dft2_lookup_final, on = .(variable), variable_label := variable_label]
  
  
  
  ### 4.7 order rows according to results ----
  ## this will sort the table by ... and keep no opinion and other at the end
  names(dft2_type2_zz1)
  
  ## this worked at one point in the development ...
  # dft2_type2_zz1 <- dft2_type2_zz1[order(variable,
  #                                        value_labels %like% no_op_short,
  #                                        value_labels %like% other_please_short,
  #                                        -prop)]
  
  ## ... but this is what is working correctly now
  dft2_type2_zz1 <- dft2_type2_zz1[order(
    variable,
    ## ... variable name
    value_labels %like% no_op_short,
    ## ... no opinion: no then yes
    value_labels %like% other_please_short,
    ## ... other : no then yes-prop                                    ## ... decreasing proportion
  )]
  
  dft2_type2_zz1[, item := as.numeric(item)]
}
## . ----
## 5. > cols_type3 --------------------------------------------------
### here we need to add no_opinion with a rbindlist as it is a separate variable for each question
if (length(cols_type3) > 0) {
  cols_type3_in_dft2_data_clean <-
    dft2_data_clean[, names(.SD), .SDcols = patterns('_type3___')]
  cols_type3_in_dft2_data_clean
  
  ### 5.1 cols_type3_no_opinion ----
  cols_type3_no_opinion <-
    dft2_data_clean[, names(.SD), .SDcols = patterns('_type3_no_op')]
  cols_type3_no_opinion
  
  
  ### 5.2 dft2_type3_zz0 ------------------------------------------------------
  dft2_type3_zz0 <- dft2_data_clean[, .(
    item_name = names(.SD),
    item = lapply(.SD, sjlabelled::get_label),
    n = lapply(.SD, sum)
  ), .SDcols = cols_type3_in_dft2_data_clean]
  
  ## extract variable from item_name
  dft2_type3_zz0[, variable := stringr::word(item_name, 1, sep = stringr::fixed("__"))]
  
  ### 5.3 dft2_type3_zz0_no_opinion -------------------------------------------
  
  dft2_type3_zz0_no_opinion <- dft2_data_clean[, .(
    item_name = names(.SD),
    item = no_op_short,
    n = lapply(.SD, function(x) {
      sum(!is.na(x))
    })
  ), .SDcols = c(cols_type3_no_opinion)]
  2
  dft2_type3_zz0_no_opinion[, n := as.numeric(n)]
  dft2_type3_zz0_no_opinion[, variable := stringr::word(item_name, 1, sep = stringr::fixed("_no_op"))]
  
  ### 5.4 dft2_type3_zz1 ------------------------------------------------------
  dft2_type3_zz1 <-
    rbindlist(list(dft2_type3_zz0, dft2_type3_zz0_no_opinion))
  
  
  
  ### 5.5 add section and variable ----
  ## extract_section is defined in 00_functions
  ## section = stringr::str_extract(item, '(?<=\\_)(.*?)(?=\\_)') %>% toupper(),
  dft2_type3_zz1[, `:=`(
    n = as.numeric(n),
    item = as.character(item),
    section = extract_section(item_name),
    type = extract_type(variable),
    statement_number = extract_statement_number(variable)
  )] %>%
    setcolorder(c(
      'section',
      'type',
      'statement_number',
      'variable',
      'item_name'
    ))
  
  
  
  dft2_type3_zz1[, prop := formattable::percent(n / total_participants, 1), keyby = variable]
  dft2_type3_zz1[, minibar := prop]
  # dft2_type3_zz1
  
  dft2_type3_zz1[prop >= agreement_threshold_type2_3, agreement := "ok"]
  
  dft2_type3_zz1[agreement == "ok", agreement_icon := icon_ok] # icon_ok is for tick sign or ok sign
  
  
  
  
  ### 5.6 rename item for tables --------------------------------------------------
  setnames(dft2_type3_zz1, 'item', 'value_labels')
  
  
  ### 5.7 order rows according to results ----
  ### this will sort the table by ... and keep no opinion and other at the end
  
  ## this worked at one point in the development ...
  # dft2_type3_zz1 <- dft2_type3_zz1[order(
  #   variable,                               ## ... variable name
  #   -prop,                                  ## ... decreasing proportion
  #   value_labels %like% no_op_short,        ## ... no opinion: no then yes
  #   value_labels %like% other_please_short  ## ... other : no then yes
  #   )]
  
  ## ... but this is what is working correctly now
  dft2_type3_zz1 <- dft2_type3_zz1[order(
    variable,
    ## ... variable name
    value_labels %like% no_op_short,
    ## ... no opinion: no then yes
    value_labels %like% other_please_short,
    ## ... other : no then yes-prop                                    ## ... decreasing proportion
  )]
  
  
}

## . ----
## 6. > dft2_dt_comments_m ----
## there are comments for each question and one for each section !
## 
cols_comments <- dft2_data_clean[, names(.SD), .SDcols = patterns('_comment$')]
cols_comments



dft2_dt_comments <- dft2_data_clean[, .SD, .SDcols = c('record_id', cols_comments)]

dft2_dt_comments_m <- dft2_dt_comments %>%
  melt('record_id', value.name = "item", na.rm = T) %>%
  sjlabelled::remove_label()

dft2_dt_comments_m[, section := extract_section(variable)]
dft2_dt_comments_m[, statement_number:= extract_statement_number(variable)]

## add the response to the question being commented on ----
dft2_dt_comments_m[ , variable_commented := stringr::str_replace_all(variable, "_comment", "")]


dft2_dt_type1_m |> 
  setnames(c('variable', 'item'),
           c('variable_commented', 'response'))

dft2_dt_type1_m[, variable_commented := stringr::str_replace_all(variable_commented, "_type1", "")]


## left join ---
dft2_dt_comments_m[dft2_dt_type1_m,
                   on = .(record_id, variable_commented),
                   response := response]

dft2_dt_comments_m <- 
  dft2_dt_comments_m[order(variable_commented, response)]



## . ----
## 7.1 save RData -------------------------------------------------------------


save(dft2_type0_zz1,
     file = here::here('output', 'RData', 'dft2_type0_zz1.RData'))

save(
  dft2_type1_zz_combined,
  file = here::here('output', 'RData', 'dft2_type1_zz_combined.RData')
)


if (length(cols_type2) > 0) {
  save(dft2_type2_zz1,
       file = here::here('output', 'RData', 'dft2_type2_zz1.RData'))
}

if (length(cols_type3) > 0) {
  save(dft2_type3_zz1,
       file = here::here('output', 'RData', 'dft2_type3_zz1.RData'))
}

save(dft2_dt_comments_m,
     file = here::here('output', 'RData', 'dft2_dt_comments_m.RData'))



## 7.2 save to xlsx ----
writexl::write_xlsx(
  dft2_type1_zz0_no_opinion,
  path = here::here('output', 'checks', 'dft2_type1_zz0_no_opinion.xlsx')
)

if (length(cols_type3) > 0) {
  writexl::write_xlsx(
    dft2_type3_zz0_no_opinion,
    path = here::here('output', 'checks', 'dft2_type3_zz0_no_opinion.xlsx')
  )
}

