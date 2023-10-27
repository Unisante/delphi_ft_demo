## 01b_dft2_recode_data.R ----
## olivier.duperrex@unisante.ch
## 2023-10-24


## 0. Loading -----------------------------------------------------
## .. load libraries ----
pacman::p_loaded()

pacman::p_load(data.table, magrittr, flextable)
# pacman::p_load( data.table, magrittr, ggplot2, ftExtra, sjPlot)

pacman::p_loaded()


## .. load data ----
load(here::here('data', 'redcap_data_raw', 'dft2_metadata.RData'))
load(here::here('data', 'redcap_data_raw', 'dft2_data_redcapr_raw.RData'))

## .. local functions ----
source(here::here('code', '00_functions.R'), encoding = 'UTF-8')

## .. Quick look at the dataset ----
## if lines below don't work, just comment them
## another nice way to look at it
## thanks to https://gt.albert-rapp.de/

setcolorder(dft2_data_redcapr_raw, c('record_id', 'dft2_0_email'))
names(dft2_data_redcapr_raw)


cols_to_exclude <- grep("_email|_c$|_comment", names(dft2_data_redcapr_raw), value = TRUE)
cols_to_exclude

# chk <- dft2_data_redcapr_raw[, .SD, .SDcols = !cols_to_exclude] |> names()
# chk

t0 <- 
  dft2_data_redcapr_raw[, .SD, .SDcols = !cols_to_exclude]  |> 
  gtsummary::tbl_summary()

t0

t0 |>
  gtsummary::as_gt() |>
  gt::tab_header(title = "Description of variables from dft2_data_redcapr_raw (without comments and email)") |>
  gt::gtsave(filename = here::here('output', 'checks', 't0_raw_data_descriptive.docx'))


## 1. recode some variables ----
## .. chk_participants_raw ----

# (id_to_check <- c(2,9))
# (email_to_check <- dft2_data_redcapr_raw[record_id %in% id_to_check, unique(dft2_0_email)])

## dft2_0_email tolower ----
dft2_data_redcapr_raw |> sjmisc::frq(dft2_0_email) ## some have capital letters

dft2_data_redcapr_raw[, dft2_0_email:= tolower(dft2_0_email)]  ## make all letters as lower

dft2_data_redcapr_raw |> sjmisc::frq(dft2_0_email) ## all lower letters


## .. replace email of mock record by NA ----
dft2_data_redcapr_raw[dft2_0_email %in% tolower(email_tester), dft2_0_email := NA]

dft2_data_redcapr_raw |> sjmisc::frq(dft2_0_email) # OK


## .. dft2_0_conflicts ----
## update according to responses --

dft2_data_redcapr_raw[, dft2_0_conflicts_raw := dft2_0_conflicts]

dft2_data_redcapr_raw[, dft2_0_conflicts] |> sjmisc::frq()

## Modified in 000_parameters.R --
# conflicts_keywords <- 'aucun|0|none|pas de|ras' 

dft2_data_redcapr_raw[tolower(dft2_0_conflicts) %like% conflicts_keywords, dft2_0_conflicts := 'Aucun']

dft2_data_redcapr_raw[, dft2_0_conflicts] |> sjmisc::frq()

## check recoding and save xlsx --
chk_dft2_0_conflicts <-
  dft2_data_redcapr_raw[, .N, .(dft2_0_conflicts_raw, dft2_0_conflicts)]

chk_dft2_0_conflicts


chk_dft2_0_conflicts %>%
  writexl::write_xlsx(path = here::here('output', 'checks', 'chk_dft2_0_conflicts.xlsx'))


## 2. dft2_metadata -------------------------------------------------
## .. create variable_label ----
## create short label adapted to the type of questions
## this will need to modified if logic of naming in REDCap changes changes

## type2 and type3 : keep what is before newline symbol
dft2_metadata[field_name %like% '_type2$|_type3$', 
              variable_label := stringr::word(field_label, 1, sep = "\\n")]


## type1 : simply field_label
dft2_metadata[field_name %like% '_type1$', 
              variable_label := field_label]

## comment of statements
dft2_metadata[field_name %like%  '[0-9]_comment', 
              variable_label := stringr::word(field_label, 1, sep = separator_comment_statements)]


## comment of section
dft2_metadata[field_name %like%  '[a-d]_comment$', 
              variable_label := paste0(stringr::word(field_label, 1, sep = "\\n"), 
                                       comment_txt_plural)]

## various fields

dft2_metadata[dt_labels_cols_0, on = .(field_name), variable_label := labels_cols_0_short]



## .. correct dft2_0b_s1_type1 ----
dft2_metadata[field_name == 'dft2_0b_s1_type1', variable_label := labels_0b_s1]

## .. clean_variable_label ----
dft2_metadata[, variable_label := clean_variable_label(variable_label, thankyou_string)]


## .. chk1 ----
chk1 <- 
  dft2_metadata[, .(field_name, variable_label, field_label)] 

## 3. Rename variables ----
## Sometimes there are errors in the naming of variables with the wrong section
## below is an example on how we dealt with this
## . we created a function in ./code/00_functions.R called `update_statement_name()`
## . we assign old and new elements of the name
## . we apply `update_statement_name()` 
## . we check it worked as we intended
## 
## Uncomment and adapt the lines in this part if you face similar problem
 

## .. rename variables with _0b_ to _z_ ------

## all variables with _0b_ should have named with _z_
## here we correct this by code, in dft2_data_redcapr_raw and in dft2_metadata

# pattern_colname_old <- '_0b'
# pattern_colname_new <- '_z'
# 
# ## check the names to update
# dft2_data_redcapr_raw[, names(.SD), .SDcols = patterns(pattern_colname_old)] # ok
# 
# 
# ## update the names (defined in 00_functions.R)
# update_statement_name(
#   dt = dft2_data_redcapr_raw,
#   metadata = dft2_metadata,
#   pattern_colname_old,
#   pattern_colname_new
# )
# 
# ## check update worked
# # dft2_data_redcapr_raw[, names(.SD), .SDcols = patterns(pattern_colname_old)] # ok
# dft2_data_redcapr_raw[, names(.SD), .SDcols = patterns(pattern_colname_new)] # ok
# 
# dft2_metadata[field_name %like% pattern_colname_old, .(field_name)] # empty
# dft2_metadata[field_name %like% pattern_colname_new, .(field_name)] # ok
# 
# ## .. rename two variables that are in 0b but had _a_ by mistake ------
# 
# ## ... dft2_a_s2 -> dft2_z_s2 ----
# statement_to_update <- '_s2_'
# 
# pattern_colname_old <- paste0('_a', statement_to_update)
# pattern_colname_new <- paste0('_z', statement_to_update)
# 
# ## check the names to update
# dft2_data_redcapr_raw[, names(.SD), .SDcols = patterns(statement_to_update)] # ok
# 
# ## update the names
# update_statement_name(
#   dt = dft2_data_redcapr_raw,
#   metadata = dft2_metadata,
#   pattern_colname_old,
#   pattern_colname_new
# )
# 
# ## check update worked
# dft2_data_redcapr_raw[, names(.SD), .SDcols = patterns(statement_to_update)] # ok
# 
# dft2_metadata[field_name %like% statement_to_update, .(field_name)]
# 
# ## ... dft2_a_s3 -> dft2_0z_s3 ----
# statement_to_update <- '_s3_'
# 
# pattern_colname_old <- paste0('_a', statement_to_update)
# pattern_colname_new <- paste0('_z', statement_to_update)
# 
# update_statement_name(
#   dt = dft2_data_redcapr_raw,
#   metadata = dft2_metadata,
#   pattern_colname_old,
#   pattern_colname_new
# )
# 
# dft2_data_redcapr_raw[, names(.SD), .SDcols = patterns(statement_to_update)] # ok


## .. rename variables 'comment_bis' ---------------------------------
## patterns ---
# pattern_colname_old <- '_comment_bis'
# pattern_colname_new <- '_comment'
# 
# cols_comments_bis <- dft2_data_redcapr_raw[, names(.SD), .SDcols = patterns(pattern_colname_old)]
# cols_comments_bis
# 
# cols_comments_to_rename <- cols_comments_bis %>% stringr::str_replace_all(pattern_colname_old, pattern_colname_new)
# cols_comments_to_rename
# 
# ## ... rename in dft2_data_redcapr_raw --- 
# dft2_data_redcapr_raw %>% setnames(cols_comments_bis, cols_comments_to_rename)
# 
# ## check---
# dft2_data_redcapr_raw[, names(.SD), .SDcols = patterns(pattern_colname_new)]
# 
# 
# ## ... rename in dft2_metadata ---
# ## check names old exists
# dft2_metadata[field_name %like% pattern_colname_old, ]
# 
# 
# 
# dft2_metadata[field_name %like% pattern_colname_old, 
#          field_name := stringr::str_replace_all(field_name, pattern_colname_old, pattern_colname_new)]
# 
# ## check ---
# dft2_metadata[field_name %like% pattern_colname_new, field_name]




## 4. Define some lists ---------------------------------------------
## . create list of names of variables per type of question ----
## which will be used below

## .. cols_0_subsets >> DONE in 000_parameters.R ----
## check in 01a_xx.R that there is no change


## .. cols_type1 ----
# cols <- grep("_type2$", names(dft2_data_redcapr_raw), value = TRUE)

cols_type1 <- dft2_data_redcapr_raw[, names(.SD), .SDcols = patterns('_type1$')]
cols_type1
# class(dft2_data_redcapr_raw$dft2_z_s1_type1)

## .. cols_type2 ----
cols_type2 <- grep("_type2$", names(dft2_data_redcapr_raw), value = TRUE)
  # dft2_data_redcapr_raw[, names(.SD), .SDcols = patterns('_type2$')]
cols_type2
# class(dft2_data_redcapr_raw$dft2_a_s10_type2)

## .. cols_type3 ----
cols_type3 <- grep("_type3___", names(dft2_data_redcapr_raw), value = TRUE)
  # dft2_data_redcapr_raw[, names(.SD), .SDcols = patterns('_type3___')]
cols_type3
# class(dft2_data_redcapr_raw$dft2_z_s2_type3___1)



## . list_vars_with_value_labels ----
## variables with value labels (levels)
## all except type3 : levels are separate variables with __
list_vars_with_value_labels <- c(cols_0_subset_value_labels,
                                 cols_type1,
                                 cols_type2)


list_vars_with_value_labels


## 5. Create a lookup table to recode variable name ----


## 5.a prepare for type3 ---- 
## radio buttons : 
## dft2_metadata : one variable name and one select_choices_or_calculations 
## data : several variables which names is xxx_type3___# (one binary for each possible response)

## condition added in case there is no cols_type3
if (length(cols_type3) > 0) {
  ## .. list_type3_unique ----
  ## create list with unique name of variable (as it will be in dft2_metadata)
  list_type3_unique <-
    cols_type3 %>%
    stringr::word(1, sep = '\\___') %>%
    unique()
  
  ## .. choices_type3 ----
  ## extract choices for type3 from dft2_metadata ---
  choices_type3 <-
    dft2_metadata[field_name %in% list_type3_unique, .(field_name, select_choices_or_calculations)]
  choices_type3
  
  ## >> SOLUTION : create empty list - for loop to add 'nested' tables - and use rbindlist to 'unnest' ---
  ## thanks to : https://stackoverflow.com/a/29419402/6176250
  
  ## .. dft2_lookup_type3 ----
  ## create an empty lists
  dft2_lookup_type3 <- list()
  # i = 2
  
  
  ## .. for loop to add 'nested' tables ----
  for (i in 1:nrow(choices_type3)) {
    # create temporary table
    dft2_lookup_0 <-
      REDCapR::regex_named_captures(pattern = pattern_boxes_fr,
                                    text = choices_type3$select_choices_or_calculations[i]) %>%
      setDT()
    
    # keep field_name
    dft2_lookup_0[, field_name :=  choices_type3$field_name[[i]]]
    
    # add variable name and label
    dft2_lookup_0[, variable := paste0(field_name, '___', id)] %>%
      setcolorder('variable')
    
    # rename label
    setnames(dft2_lookup_0, 'label', 'variable_label')
    
    # add it to your list
    dft2_lookup_type3[[i]] <- dft2_lookup_0
    
  }
  
  ## .. rbindlist to unnest ----
  dft2_lookup_type3 <- data.table::rbindlist(dft2_lookup_type3)
}

## 5.b dft2_lookup_initial ----
dft2_lookup_initial <-
  dft2_metadata[!is.na(variable_label), .(variable = field_name, variable_label)]
  
  
## 5.c dft2_lookup_final ----
if (length(cols_type3) > 0) {
  dft2_lookup_final <-
    rbindlist(list(dft2_lookup_initial,
                   dft2_lookup_type3[, .(variable, variable_label)]))
} else {
  dft2_lookup_final <- dft2_lookup_initial
}

(old <- names(dt_labels_cols_0))
(new <- names(dft2_lookup_final))
setnames(dt_labels_cols_0, old, new)



identical(names(dft2_lookup_final),
          names(dt_labels_cols_0))


if (length(dt_labels_cols_0) > 0) {
  dft2_lookup_final <- 
    rbind(dft2_lookup_final,
          dt_labels_cols_0
    )
}

dft2_lookup_final

dft2_lookup_final[, variable_label := clean_variable_label(variable_label, thankyou_string)]


dft2_lookup_final[!(variable_label %like% 'SECTION'), variable_label := stringr::str_replace_all(variable_label, comment_txt_singular, comment_txt_plural)]

dft2_lookup_final[!(variable_label %like% 'SECTION'), variable_label := stringr::str_replace_all(variable_label, ' ;', ' -')]

dft2_lookup_final[, variable_label := stringr::str_replace_all(variable_label, '"', '')]



## 6. deduplicate records by collapsing -----------------------------

### 6.a  first get rid of email that are empty ----
dft2_data_redcapr_1 <- dft2_data_redcapr_raw[!is.na(dft2_0_email), ]


### 6.b conditional deduplication ----
## .. check if any email is duplicated ----
# anyDuplicated returns a integer value with the index of first duplicate. If none exists, 0L is returned.
anyDuplicated(dft2_data_redcapr_1[, dft2_0_email])  ## index of first duplicate ! not number of duplicates !


if (anyDuplicated(dft2_data_redcapr_1[, dft2_0_email]) == 0) {
  ## . dft3_data_clean : if no duplicates just copy dft3_data_redcapr_1 ----
  dft2_data_clean <- dft2_data_redcapr_1
  
} else {
  ## . dft2_data_clean : if  duplicates, then ...  ----
  
  ##  .. dt1_dupes_yes ----
  dt1_dupes_yes <- dft2_data_redcapr_1[ , if (.N > 1L) .SD, keyby = .(dft2_0_email)]
 
  # head(dt1_dupes_yes)

  emails_duplicated <- dt1_dupes_yes[,unique(dft2_0_email)]

  id_duplicated <- dt1_dupes_yes[, record_id]
  id_duplicated


  message_emails_duplicated <- glue::glue("there are {length(emails_duplicated)} duplicate emails:
                                          - in rows {list(id_duplicated)}
                                          - {list(emails_duplicated)} ")  
  message_emails_duplicated


  ###  .. data_deduplicated ----
  ## collapse rows
  ## https://stackoverflow.com/a/61309525/6176250
  data_deduplicated <-
    dft2_data_redcapr_1[!is.na(dft2_0_email), lapply(.SD, function(x)
      paste0(unique(x), collapse = "; ")),
      # paste0(unique(na.omit(x)), collapse = "; ")),
      keyby = .(dft2_0_email)]

  data_deduplicated[dft2_0_email %in% emails_duplicated, .(dft2_0_email, record_id)]

  ### .  clean after collapse ... !!! ---------------------------------
  ## numeric cols loose their properties
  cols_numeric <- dft2_data_redcapr_1[, names(.SD), .SDcols = is.numeric]
  cols_numeric    # it contains record_id but we want to keep it with ';' for checks
  
  cols_numeric_not_id <- cols_numeric[! cols_numeric == 'record_id'] 
  cols_numeric_not_id    # record_id removed

  data_deduplicated <- 
    data_deduplicated %>% clean_after_collapse(cols_numeric)

  # chk_participants_deduplicated <- data_deduplicated[dft2_0_email %in% email_to_check, ]
  # foo4 <- data_deduplicated[, names(.SD), .SDcols = is.numeric] 
  # foo4
  # identical(cols_numeric, c('record_id', foo4)) ## FALSE because all the values with ';'

  ### .. dt0 - subset with ones like ';' and recode with get_last ----
  dt0 <- data_deduplicated[record_id %like% ';', ]
  
  ### .. cols_to_recode ----
  cols_to_recode <- c(cols_type1,
                      cols_type2)
  
  ## for cols_type1 and cols_type2 we get the last value that is not NA
  ## this allows respondants to change their mind
  dt0[, (cols_to_recode) := lapply(.SD, get_last), .SDcols = cols_to_recode, keyby = dft2_0_email]
  
  ## .. cols_type3 : to recode these we use the max value that is not NA
  ##  respondants coming back to finish the answering will often continue from latest question
  dt0[, (cols_type3) := lapply(.SD, get_max), .SDcols = cols_type3, keyby = dft2_0_email]
  
  

  ### . recode comments ----
  cols_comment <- dt0[, names(.SD), .SDcols = patterns('comment')]
  cols_to_keep <- c('dft2_0_email', cols_comment)
  
  ## check the duplicated comments
  foo1 <- dt0[,..cols_to_keep] %>% melt('dft2_0_email')
  
  foo1[value %like% 'NA;', .N] 
  foo1[value %like% '; NA', .N] 
  
  ## 
  dt0[, (cols_comment) := lapply(.SD, remove_NAs_after_collapsing), .SDcols = cols_comment]
  foo2 <- dt0[,..cols_to_keep] %>% melt('dft2_0_email')
  
  foo2[value %like% 'NA;', .N] 
  foo2[value %like% '; NA', .N] 
  
  ## . recode record_id : keep the latest ----
  dt0[, record_id := get_last(record_id), keyby = dft2_0_email]

  ### .. dft2_data_almost_clean -----------------------------------------------------
  ### add dt0 to data_deduplicated ---
  dft2_data_almost_clean <- 
      rbindlist(list(data_deduplicated, dt0))
  
  ## sort by email ---
  dft2_data_almost_clean <- dft2_data_almost_clean[order(dft2_0_email)]
  
  ## .. chk_deduplication ----
  chk_deduplication <- 
    dft2_data_almost_clean[dft2_0_email %in% emails_duplicated,]
  



  ### .. dft2_data_clean -----------------------------------------------------
  ### exclude the collapsed lines
  dft2_data_clean <- dft2_data_almost_clean[!(record_id %like% ';'),]
  
  
  
  ## check names old exists no more
  # dft2_lookup_final[variable %like% pattern_colname_old,, ]
  
  ## check names new exists 
  # dft2_lookup_final[variable %like% pattern_colname_new, ]



  ## .. update class to numeric ----
  # class(dft2_data_clean$dft2_z_s2_type3___1)

  foo3 <- dft2_data_clean[, names(.SD), .SDcols = is.numeric] 
  foo3
  identical(cols_numeric, foo3)

  dft2_data_clean[, (cols_numeric) := lapply(.SD, as.numeric), .SDcols = cols_numeric]

  foo4 <- dft2_data_clean[, names(.SD), .SDcols = is.numeric]
  foo4
  identical(cols_numeric, foo4)
  
  # class(dft2_data_clean$dft2_z_s2_type3___1)


}



### 6.c add variable label dft2_data_redcapr_raw -------------
## purrr solution thanks to https://stackoverflow.com/a/69838854/6176250
## for each name of variable (column) of dft2_data_clean, 
## we will find the row that matches in dft2_lookup_final (in the column 'variable') and return the corresponding variable_label, 
## and then assign it as the label in dft2_data_clean, which will be returned as a data.table
## 
## numeric variables will become character ones - this will be dealt with once cleaning is done
## 
dft2_data_clean <- purrr::map2_dfc(
  dft2_data_clean,
  dft2_lookup_final$variable_label[ match(names(dft2_data_clean), dft2_lookup_final$variable) ],
  ~ `attr<-`(.x, "label", .y)) %>% 
  data.table::setDT()



## 7. Create dft2_lookup table of value levels ----

list_vars_with_value_labels

## . dt_choices ----
## extract choices for type2 from dft2_metadata for list_vars_with_value_labels
dt_choices <- dft2_metadata[field_name %in% list_vars_with_value_labels, .(field_name, select_choices_or_calculations)]
dt_choices


## . dft2_lookup_value_labels  ----

## create an empty list
dft2_lookup_value_labels <- list()



## . for loop to add 'nested' tables to the list ----
for (i in 1:nrow(dt_choices)) {
  
  # create temporary data.table
  dft2_lookup_0 <-
    REDCapR::regex_named_captures(pattern = pattern_boxes_fr,
                                  text = dt_choices$select_choices_or_calculations[i]) %>% 
    setDT() 
  
  # convert id to numeric
  dft2_lookup_0[, id := as.numeric(id)]
  
  # keep field_name from dt_choices
  dft2_lookup_0[, field_name :=  dt_choices$field_name[[i]]]                 
  
  # add value_labels with a bit of cleaning
  dft2_lookup_0[, value_labels := clean_value_labels(label)] 
  
  # order cols and rename id
  dft2_lookup_0  %>%
    setcolorder(c('field_name', 'id', 'value_labels')) %>% 
    setnames('id', 'id_value_labels')
  
  
  
  dft2_lookup_value_labels[[i]] <- dft2_lookup_0 # add it to your list
  
}

## have a quick look at the 10th element of the list
dft2_lookup_value_labels[10]

## . dft2_lookup_value_labels_intermediate ----
## simple rbindlist to unnest !

dft2_lookup_value_labels_intermediate <- data.table::rbindlist(dft2_lookup_value_labels)

# dft2_data_clean[, .(dft2_0_gender, dft2_0_job, dft2_0_joblang)] %>% sjmisc::frq()


## . dft2_lookup_value_labels_final ----
## rbind if 'dft2_value_labels_manual' exists - this manual table could be created in 000_parameters.R in case of creating a new categorical variable

if(exists('dft2_value_labels_manual') == TRUE) {
  identical(names(dft2_lookup_value_labels_intermediate),
            names(dft2_value_labels_manual))
  
  ## rbind to add the manually created list of labels
  dft2_lookup_value_labels_final <-  
    rbind(dft2_lookup_value_labels_intermediate, dft2_value_labels_manual)
} else {
  dft2_lookup_value_labels_final <-  
    dft2_lookup_value_labels_intermediate
}



## 8.a CHECK values that are typos -----------------------------------
## type1 only 1 to 9

### select cols ending with _type1
dft2_dt_type1_m <- 
  dft2_data_redcapr_1[, .SD, .SDcols = c('record_id', cols_type1)] %>%
  melt('record_id', value.name = "item", na.rm = T) %>%
  sjlabelled::remove_label()

# dft2_dt_type1_m

dft2_dt_type1_m %>% sjmisc::frq(item)



if (dft2_dt_type1_m[item > max_scale_delphi, .N] > 0) {
  id_to_check <<- dft2_dt_type1_m[item > max_scale_delphi, record_id]
  
  chK_2 <-
    dft2_data_redcapr_1[record_id %in% id_to_check, .SD, .SDcols = patterns('_type1')]
  
  glue::glue(
    "Warning: at least one response is > {max_scale_delphi} (max_scale_delphi) in {length(id_to_check)} rows. 
    See *id_to_check* for the row ids and *chk_2* for the data.table with a subset type1 questions"
  ) %>%
    crayon::red() %>%
    cat()
}


## 8.b check structure of data ---------------------------------------
##  https://ouhscbbmc.github.io/OuhscMunge/
# remotes::install_github("OuhscBbmc/OuhscMunge")
OuhscMunge::verify_value_headstart(dft2_data_clean)


### 9. save plots ----
### some will be used in executive summary
pacman::p_load(tidyverse, ggplot2, flextable, patchwork)

## .. summarise data_set ----
## we create a tibble with the variables using group_by, the number and the median value, the path of the png to be saved and the plot ready to be interpreted by flextable
dft2_zzz_type1 <- dft2_dt_type1_m %>%
  group_by(variable) %>%
  summarise( n = n(),
             median = median(item),
             img_path = here::here('output', 'png', paste0( unique(variable), ".png")),
             gg = list(gg_duo(item))
  )



dft2_zzz_type1




## .. save the png ----
purrr::walk2(dft2_zzz_type1$gg, dft2_zzz_type1$img_path, function(gg, path){
  png(filename = path,  width = width_png_high_res, height = height_png_high_res)
  print(gg)
  dev.off()
})

## .. Quick look at the dataset ----
t0_clean <- 
  dft2_data_clean[, .SD, .SDcols = !cols_to_exclude]  |> 
  gtsummary::tbl_summary()

t0_clean

t0_clean |>
  gtsummary::as_gt() |>
  gt::tab_header(title = "Descrpition of variables from dft2_data_clean (without comments and email)") |>
  gt::gtsave(filename = here::here('output', 'checks', 't0_clean_data_descriptive.docx'))

## 10. save ----------------------------------------------------------


save(dft2_data_clean, file = here::here('data', 'dft2', 'dft2_data_clean.RData'))

save(dft2_lookup_final, file = here::here('data', 'dft2', 'dft2_lookup_final.RData'))

save(
  dft2_lookup_value_labels_final,
  file = here::here('data', 'dft2', 'dft2_lookup_value_labels_final.RData')
)


