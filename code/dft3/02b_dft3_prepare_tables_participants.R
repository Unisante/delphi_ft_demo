## 02b_dft3_prepare_tables_participants.R
## olivier.duperrex@unisante.ch
## 2022-09-26


## . ----
### . > cols_type1 ----
### select cols endign with _type1
cols_type1 


### 1.5 dft3_dt_type1_m ----
### subset and metl
dft3_dt_type1_m <- data_clean[, .SD, .SDcols = c('record_id', cols_type1)] %>%
  melt('record_id', value.name = "item", na.rm = T) %>%
  sjlabelled::remove_label()

# dft3_dt_type1_m


# ## dft3_type1_zz_combined ---------------------------------------
# str(dft3_type1_zz_combined)
 


## . dft3_type1_zz0_id_selected ----
## table with participant responses
dft3_type1_zz0_id_selected <- dft3_dt_type1_m[record_id == record_id_selected, .(record_id, variable,
                                                                              responses_participant = item)]

dft3_type1_zz0_id_selected
names(dft3_type1_zz0_id_selected)



## . dft3_type1_zz1_id_selected ---- 
## make sure not to keep responses_participant from previous round !!! ----

if('responses_participant' %in% names(dft3_type1_zz_combined)) {
  
    dft3_type1_zz_combined[, responses_participant := NULL]  
}

# names(dft3_type1_zz1)

## left join to add responses_participant
## last part with := needed to keep all variables
dft3_type1_zz_combined_id_selected <- dft3_type1_zz_combined[dft3_type1_zz0_id_selected, on = 'variable', responses_participant := responses_participant] 

dft3_type1_zz_combined_id_selected %>% names()





