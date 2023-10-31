## 01b_dft3_recode_data.R ----
## olivier.duperrex@unisante.ch
## 2023-10-24


## NOTE in round 3 only type1 => simplified code

## 0. Loading -----------------------------------------------------
here::here()

## .. load libraries ----
pacman::p_loaded()

pacman::p_load(data.table, magrittr, flextable)
# pacman::p_load( data.table, magrittr, ggplot2, ftExtra, sjPlot)

pacman::p_loaded()


## .. load data ----
load(here::here('data', 'redcap_data_raw', 'dft3_metadata.RData'))
load(here::here('data', 'redcap_data_raw', 'dft3_data_redcapr_raw.RData'))

## .. local functions and parameters ----
source(here::here('code', '00_functions.R'), encoding = 'UTF-8')

source(here::here('code', '000_parameters.R'), encoding = 'UTF-8')

## .. replace email of mock record by NA ----
dft3_data_redcapr_raw[dft3_0_email %in% email_tester, dft3_0_email := NA]


## .. Quick look at the dataset ----
## another nice way to look at it
## thanks to https://gt.albert-rapp.de/

setcolorder(dft3_data_redcapr_raw, c('record_id', 'dft3_0_email'))
names(dft3_data_redcapr_raw)


cols_to_exclude <- grep("_email|_c$|_comment", names(dft3_data_redcapr_raw), value = TRUE)
cols_to_exclude

# foo1 <-
#   dft3_data_redcapr_raw[, .SD, .SDcols = !cols_to_exclude] |>
#   gtExtras::gt_plt_summary()
# foo1

# chk <- dft3_data_redcapr_raw[, .SD, .SDcols = !cols_to_exclude] |> names()
# chk

t0 <- 
  dft3_data_redcapr_raw[, .SD, .SDcols = !cols_to_exclude]  |> 
  gtsummary::tbl_summary()

t0

t0 |>
  gtsummary::as_gt() |>
  gt::tab_header(title = "Description of variables from dft3_data_redcapr_raw (without comments and email)") |>
  gt::gtsave(filename = here::here('output', 'checks', 't0_raw_data_descriptive.docx'))

## 1. recode some variables ----
## .. chk_participants_raw ----

# (id_to_check <- c(2,9))
# (email_to_check <- dft3_data_redcapr_raw[record_id %in% id_to_check, unique(dft3_0_email)])

## dft3_0_email tolower ----
dft3_data_redcapr_raw |> sjmisc::frq(dft3_0_email) ## some have capital letters

dft3_data_redcapr_raw[, dft3_0_email:= tolower(dft3_0_email)]  ## make all letters as lower

dft3_data_redcapr_raw |> sjmisc::frq(dft3_0_email) ## all lower letters


## .. replace email of mock record by NA ----
dft3_data_redcapr_raw[dft3_0_email %in% tolower(email_tester), dft3_0_email := NA]

dft3_data_redcapr_raw |> sjmisc::frq(dft3_0_email) # OK


## 2. dft3_metadata -------------------------------------------------
## .. create variable_label ----

## type1 : simply field_label
dft3_metadata[field_name %like% '_type1$', 
              variable_label := field_label]

## comment of statements
dft3_metadata[field_name %like%  '[0-9]_comment', 
              variable_label := stringr::word(field_label, 1, sep = separator_comment_statements)]


## comment of section
dft3_metadata[field_name %like%  '[a-d]_comment$', 
              variable_label := paste0(stringr::word(field_label, 1, sep = "\\n"), 
                                       comment_txt_plural)]

## .. clean_variable_label ----
dft3_metadata[, variable_label := clean_variable_label(variable_label, thankyou_string)]

## .. chk1 ----
chk1 <- dft3_metadata[, .(field_name, variable_label, field_label)] 

## 3. Rename variables ----
cols_0 <- dft3_metadata[field_name %like% '_0_', field_name]
cols_0

# dft3_metadata[field_name == 'dft3_z_s3_type1', .(field_name, variable_label, field_label)]
# dft3_metadata[field_name == 'dft3_z_s4_type1', .(field_name, variable_label, field_label)]


## 3a. Statement 3 and 4 : missing beginning ----
dft3_metadata[field_name == 'dft3_z_s3_type1', variable_label := paste0('Enoncé 3 : ', field_label)]
dft3_metadata[field_name == 'dft3_z_s4_type1', variable_label := paste0('Enoncé 4 : ', field_label)]

## .. chk2 ----
chk2 <- dft3_metadata[, .(field_name, variable_label, field_label)]

dft3_metadata[field_name == 'dft3_z_s3_type1', .(field_name, variable_label, field_label)]
dft3_metadata[field_name == 'dft3_z_s4_type1', .(field_name, variable_label, field_label)]
 
 
# ## 3.b LPTab* ----
dft3_metadata[field_name == 'dft3_z_s6_type1', .(field_name, variable_label, field_label)]
dft3_metadata[field_name == 'dft3_z_s6_type1', variable_label := stringr::str_remove(variable_label, '\\*')]
dft3_metadata[field_name == 'dft3_z_s6_type1', .(field_name, variable_label, field_label)]



## 4. Define some lists ---------------------------------------------
## . create list of names of variables per type of question ----
## .. cols_0_subset ----
## check in 01a_xx.R that there is no change

# cols_0_subset <- c('dft3_0_gender', 'dft3_0_job', 'dft3_0_joblang')
# cols_0_subset

## .. cols_type1 ----
cols_type1 <- dft3_data_redcapr_raw[, names(.SD), .SDcols = patterns('_type1$')]
cols_type1
class(dft3_data_redcapr_raw$dft3_z_s1_type1)



## .. list_vars_with_value_labels ----
list_vars_with_value_labels <- c(cols_type1)


list_vars_with_value_labels


## 5. Create dft3_lookup_final to recode variable name ----

## . dft3_lookup_final ----


dft3_lookup_final <- dft3_metadata[!is.na(variable_label), .(variable = field_name, variable_label)]

dft3_lookup_final[, variable_label := clean_variable_label(variable_label, thankyou_string)]



dft3_lookup_final[!(variable_label %like% 'SECTION'), 
             variable_label := stringr::str_replace_all(variable_label, comment_txt_singular, comment_txt_plural)]

dft3_lookup_final[!(variable_label %like% 'SECTION'), 
             variable_label := stringr::str_replace_all(variable_label, ' ;', ' -')]

dft3_lookup_final[, variable_label := stringr::str_replace_all(variable_label, '"', '')]


## 6. clean and deduplicate by collapsing ---------------------------
### 6.a  first get rid of email that are empty ----
dft3_data_redcapr_1 <- dft3_data_redcapr_raw[!is.na(dft3_0_email), ]


### 6.b conditional deduplication ----
### inspired by : https://riptutorial.com/data-table/example/18443/handling-duplicates#undefined
### and probably others on stackoverflow

## .. check if any email is duplicated ----
# anyDuplicated returns a integer value with the index of first duplicate. If none exists, 0L is returned.
anyDuplicated(dft3_data_redcapr_1[, dft3_0_email])


if (anyDuplicated(dft3_data_redcapr_1[, dft3_0_email]) == 0) {
  ## . dft3_data_clean : if no duplicates just copy dft3_data_redcapr_1 ----
  dft3_data_clean <- dft3_data_redcapr_1
  
} else {
  ## . dft3_data_clean : if  duplicates, then ...  ----
  
  ##  .. dt1_dupes_yes ----
  dt1_dupes_yes <-
    dft3_data_redcapr_1[, if (.N > 1L)
      .SD, keyby = .(dft3_0_email)]
  
  # head(dt1_dupes_yes)
  
  
  emails_duplicated <- dt1_dupes_yes[, unique(dft3_0_email)]
  
  id_duplicated <- dt1_dupes_yes[, record_id]
  id_duplicated
  
  message_emails_duplicated <- glue::glue("there are {length(emails_duplicated)} duplicate emails:
                                          - in rows {list(id_duplicated)}
                                          - {list(emails_duplicated)} ")  
  message_emails_duplicated
 
  
  ##  .. data_deduplicated ----
  ## collapse rows
  ## https://stackoverflow.com/a/61309525/6176250
  data_deduplicated <-
    dft3_data_redcapr_1[!is.na(dft3_0_email), lapply(.SD, function(x)
      paste0(unique(x), collapse = "; ")),
      # paste0(unique(na.omit(x)), collapse = "; ")),
      keyby = .(dft3_0_email)]
  
  data_deduplicated[dft3_0_email %in% emails_duplicated, .(dft3_0_email, record_id)]
  
  #### ..  clean after collapse ... !!! ----
  cols_numeric <- dft3_data_redcapr_1[, names(.SD), .SDcols = is.numeric]
  cols_numeric    # it contains record_id but we want to keep it with ';' for checks
  
  cols_numeric_not_id <- cols_numeric[! cols_numeric == 'record_id'] 
  cols_numeric_not_id    # record_id removed
  
  data_deduplicated <-
    data_deduplicated %>% clean_after_collapse(cols_numeric_not_id)
  
 
  # foo4 <- data_deduplicated[, names(.SD), .SDcols = is.numeric]
  # foo4
  # identical(cols_numeric, c('record_id', foo4))

  ### .. dt0 - subset with ones like ';' and recode with get_last ----
  dt0 <- data_deduplicated[record_id %like% ';',]

  ### .. check questionnaire completed ----
  questionnaire_completed <- 
    dt0[, names(.SD), .SDcols = patterns('equestionnaire_complete')]
  dt0[, ..questionnaire_completed] |> sjmisc::frq()
  
  ### .. cols_to_recode ----
  ### need to be modified if also type2 or type3 questions - see 01b_dft2_recode_data.R
  cols_to_recode <- c(
    cols_type1,
    questionnaire_completed) 
  ## for cols_type1 we get the last value that is not NA
  ## this allows respondants to change their mind
  dt0[, (cols_to_recode) := lapply(.SD, get_last), .SDcols = cols_to_recode, keyby = dft3_0_email]

  
  ### .. recode comments ----
  cols_comment <- dt0[, names(.SD), .SDcols = patterns('comment')]
  cols_to_keep <- c('dft3_0_email', cols_comment)
  
  foo1 <- dt0[, ..cols_to_keep] %>% melt('dft3_0_email')
  
  foo1[value %like% 'NA;', .N]
  foo1[value %like% '; NA', .N]
  
  
  dt0[, (cols_comment) := lapply(.SD, remove_NAs_after_collapsing),
      .SDcols = cols_comment]
  foo2 <- dt0[, ..cols_to_keep] %>% melt('dft3_0_email')
  
  foo2[value %like% 'NA;', .N]
  foo2[value %like% '; NA', .N]
  
  ## .. recode record_id : keep the latest ----
  dt0[, record_id := get_last(record_id), keyby = dft3_0_email]
  
  
  ## .. dft3_data_almost_clean -----------------------------------------------------
  
  dft3_data_almost_clean <- 
    rbindlist(list(data_deduplicated, dt0))
  
  dft3_data_almost_clean <- dft3_data_almost_clean[order(dft3_0_email)]
  
  ## .. chk_deduplication ----
  chk_deduplication <-
    dft3_data_almost_clean[dft3_0_email %in% emails_duplicated, ]
  
  
  
  
  ## .. dft3_data_clean -----------------------------------------------------
  dft3_data_clean <- dft3_data_almost_clean[!(record_id %like% ';'), ]
  
  
  ## .. update class to numeric ----
  # chk_deduplication
  cols_numeric ## defined above before collapsing
  
  foo3 <- dft3_data_clean[, names(.SD), .SDcols = is.numeric]
  foo3
  identical(cols_numeric, foo3) # nope
  
  ## as.numeric to all numeric cols including record_id
  dft3_data_clean[, (cols_numeric) := lapply(.SD, as.numeric), .SDcols = cols_numeric]
  
  foo4 <- dft3_data_clean[, names(.SD), .SDcols = is.numeric]
  foo4
  identical(cols_numeric, foo4) # TRUE


}



### 6.c  add variable label  ----
## purrr solution  thanks to https://stackoverflow.com/a/69838854/6176250
## 
## for each name of variable (column) of dft3_data_clean, 
## we will find the row that matches in dft3_lookup_final (in the column 'variable') and return the corresponding variable_label, 
## and then assign it as the label in dft3_data_clean, which will be returned as a data.table
## 
## numeric variables will become character ones - this will be dealt with once cleaning is done
dft3_data_clean <- purrr::map2_dfc(
  dft3_data_clean,
  dft3_lookup_final$variable_label[ match(names(dft3_data_clean), dft3_lookup_final$variable) ],
  ~ `attr<-`(.x, "label", .y)) %>% 
  data.table::setDT()

class(dft3_data_clean)





## 7. Value levels ----

## .. cols_type1 ----
## . check levels are not there ---
dft3_data_clean %>% sjmisc::frq(cols_type1)

## . add labels (= levels) to all the variables finishing with '_type1' in one go ---
dft3_data_clean[, (cols_type1) := lapply(.SD, sjlabelled::set_labels, labels = levels_type1),
           .SDcols = cols_type1]

## . check levels have been updated not there ---
dft3_data_clean %>% sjmisc::frq(cols_type1)




## 8.a CHECK values that are typos -----------------------------------
## type1 only 1 to 9

### select cols ending with _type1
dft3_dt_type1_m <- dft3_data_clean[, .SD, .SDcols = c('record_id', cols_type1)] %>%
  melt('record_id', value.name = "item", na.rm = T) %>%
  sjlabelled::remove_label()

dft3_dt_type1_m


dft3_dt_type1_m %>% sjmisc::frq(item)

if (dft3_dt_type1_m[item > max_scale_delphi, .N] > 0) {
  id_to_check <<- dft3_dt_type1_m[item > max_scale_delphi, record_id]
  
  chK_3 <-
    dft3_data_redcapr_1[record_id %in% id_to_check, .SD, .SDcols = patterns('_type1')]
  
  
  glue::glue(
    "Warning: at least one response is > {max_scale_delphi} (max_scale_delphi) in {length(id_to_check)} rows. 
    See *id_to_check* for the row ids and *chk_3* for the data.table with a subset type1 questions"
  ) %>%
    crayon::red() %>%
    cat()
  
}




## 8.b check structure of data ---------------------------------------
##  https://ouhscbbmc.github.io/OuhscMunge/
# remotes::install_github("OuhscBbmc/OuhscMunge")
OuhscMunge::verify_value_headstart(dft3_data_clean)



## 9. save plots for use in executive summary ----
pacman::p_load(tidyverse, ggplot2, flextable, patchwork)

## .. summarise data_set ----
## we create a tibble with the variables using group_by, the number and the median value, the path of the png to be saved and the plot ready to be interpreted by flextable
dft3_zzz_type1 <- dft3_dt_type1_m %>%
  group_by(variable) %>%
  summarise( n = n(),
             median = median(item),
             img_path = here::here('output', 'png', paste0( unique(variable), ".png")),
             gg = list(gg_duo(item))
  )

dft3_zzz_type1




## .. save the png ----
## https://aosmith.rbind.io/2018/08/20/automating-exploratory-plots/
## and another stackoverflow comment ...

purrr::walk2(dft3_zzz_type1$gg, dft3_zzz_type1$img_path, function(gg, path) {
  png(filename = path,
      width = width_png_high_res,
      height = height_png_high_res)
  print(gg)
  dev.off()
})

## .. Quick look at the dataset ----
t0_clean <- 
  dft3_data_clean[, .SD, .SDcols = !cols_to_exclude]  |> 
  gtsummary::tbl_summary()

t0_clean

t0_clean |>
  gtsummary::as_gt() |>
  gt::tab_header(title = "Descrpition of variables from dft3_data_clean (without comments and email)") |>
  gt::gtsave(filename = here::here('output', 'checks', 't0_clean_data_descriptive.docx'))

## 10. save ----------------------------------------------------------

save(dft3_data_clean, file = here::here('data', 'dft3', 'dft3_data_clean.RData'))

save(dft3_lookup_final, file = here::here('data', 'dft3', 'dft3_lookup_final.RData'))



