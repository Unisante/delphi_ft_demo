## 02b_dft2_prepare_tables_participants.R
## olivier.duperrex@unisante.ch
## 2023-10-24

## . > cols_type1 ----
## select cols endign with _type1
cols_type1 


## .. dft2_dt_type1_m ----
## subset and metl
dft2_dt_type1_m <- data_clean[, .SD, .SDcols = c('record_id', cols_type1)] %>%
  melt('record_id', value.name = "item", na.rm = T) %>%
  sjlabelled::remove_label()
# dft2_dt_type1_m


## . dft2_type1_zz0_id_selected ----
## table with participant responses
dft2_type1_zz0_id_selected <- dft2_dt_type1_m[record_id == record_id_selected, .(record_id, variable,
                                                                                 responses_participant = item)]

dft2_type1_zz0_id_selected
names(dft2_type1_zz0_id_selected)



## . type1_zz1_id_selected ---- 

# names(type1_zz1)

## make sure not to keep responses_participant from previous round !!! ----

if('responses_participant' %in% names(dft2_type1_zz_combined)) {
  
  dft2_type1_zz_combined[, responses_participant := NULL]  
}


## left join to add responses_participant
## last part with := needed to keep all variables
dft2_type1_zz_combined_id_selected <- 
  dft2_type1_zz_combined[dft2_type1_zz0_id_selected, 
                         on = 'variable', 
                         responses_participant := responses_participant] 

dft2_type1_zz_combined_id_selected %>% names()


## . ----
## . > cols_type2 ----
cols_type2

if (length(cols_type2) > 0) {
  ## . dft2_dt_type2 -----------------------------------------------------
  dft2_dt_type2 <-
    data_clean[, lapply(.SD, sjlabelled::as_label), .SDcols = c('record_id', cols_type2)]
  
  ## . dft2_dt_type2_m : melted table by record id ----------------------
  dft2_dt_type2_m <- dft2_dt_type2 %>%
    melt('record_id', value.name = "item", na.rm = T) %>%
    sjlabelled::remove_label()
  
  
  ## . dft2_type2_zz1 ---------------------------------------------------------
  str(dft2_type2_zz1)
  # dft2_type2_zz1[, item := as.numeric(item)]
  
  ## . dft2_type2_zz0_id_selected ----
  dft2_type2_zz0_id_selected <-
    dft2_dt_type2_m[record_id == record_id_selected, .(responses_participant =
                                                         .N), keyby = .(variable, item)]
  
  dft2_type2_zz0_id_selected
  
  ## . dft2_type2_zz1_id_selected ----
  ## make sure not to keep responses_participant from previous round !!! ---
  
  if ('responses_participant' %in% names(dft2_type2_zz1)) {
    dft2_type2_zz1[, responses_participant := NULL]
  }
  
  
  dft2_type2_zz1_id_selected <-
    dft2_type2_zz1[dft2_type2_zz0_id_selected,
                   on = .(variable, item),
                   responses_participant := responses_participant
    ][order(variable,
            item %like% "Pas d'avis",
            -prop)] # here we reorder to make sure they come out in descending order
  
  dft2_type2_zz1_id_selected[, responses_participant := fifelse(responses_participant == 1, 'X', '')]
  # dft2_type2_zz1_id_selected
  
  ## . order rows according to results ----
  ## this will sort the table by ... and keep no opinion and other at the end
  names(dft2_type2_zz1_id_selected)
  
  dft2_type2_zz1_id_selected <- dft2_type2_zz1_id_selected[order(
    variable,                                 ## ... variable name
    value_labels %like% no_op_short,          ## ... no opinion: no then yes
    value_labels %like% other_please_short    ## ... other : no then yes-prop 
  )]
  
}

## . ----
## . > cols_type3 --------------------------------------------------
if (length(cols_type3) > 0) {
  cols_type3_in_data_clean <-
    data_clean[, names(.SD), .SDcols = patterns('_type3___')]
  cols_type3_in_data_clean
  
  
  cols_type3_no_opinion <-
    data_clean[, names(.SD), .SDcols = patterns('_type3_no_op')]
  cols_type3_no_opinion
  #
  # ## . dft2_type3_zz1 ----------------------------------------------------------
  str(dft2_type3_zz1)
  
  
  ## . dft2_type3_zz0_id_selected ----------------------------------------------
  dft2_type3_zz0_id_selected <-
    data_clean[record_id == record_id_selected, .(item_name = names(.SD),
                                                  # item = lapply(.SD, sjlabelled::get_label),
                                                  responses_participant = lapply(.SD, sum)), .SDcols = c(cols_type3_in_data_clean)]
  
  dft2_type3_zz0_id_selected
  
  dft2_type3_zz0_no_opinion_id_selected <-
    data_clean[record_id == record_id_selected, .(item_name = names(.SD),
                                                  # item = "Pas d'avis",
                                                  responses_participant = lapply(.SD, function(x) {
                                                    sum(!is.na(x))
                                                  })), .SDcols = c(cols_type3_no_opinion)]
  
  
  dft2_type3_zz1_id_selected <-
    rbindlist(list(
      dft2_type3_zz0_id_selected,
      dft2_type3_zz0_no_opinion_id_selected
    ))
  
  
  ## . dft2_type3_zz1_id_selected ----
  ## .. left join ---
  # dft2_type3_zz1_id_selected <- dft2_type3_zz1[dft2_type3_zz1_id_selected, on = 'item_name']
  dft2_type3_zz1_id_selected <-
    dft2_type3_zz1_id_selected[dft2_type3_zz1, on = 'item_name']
  # this will keep the sorting done in generic report
  
  
  dft2_type3_zz1_id_selected[, responses_participant := fifelse(responses_participant == 1, 'X', '')]
  # dft2_type3_zz1_id_selected
  
  ### .  order rows according to results ----
  ## this will sort the table by ... and keep no opinion and other at the end
  dft2_type3_zz1_id_selected <- dft2_type3_zz1_id_selected[order(
    variable,                                ## ... variable name
    value_labels %like% no_op_short,         ## ... no opinion: no then yes
    value_labels %like% other_please_short,  ## ... other : no then yes-prop
  )]
}

## . ----
## . > dft2_dt_comments_m ----

dft2_dt_comments_m[record_id == record_id_selected, responses_participant := 'X']








