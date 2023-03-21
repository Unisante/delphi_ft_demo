## 01c_dft2_define_cols.R
## olivier.duperrex@unisante.ch
## 2022-06-24
## 
## AIM : create lists of variables by type and section for more readabale code


## cols_0_subset ----
## check in 01a that there is no change
cols_0_subset <- c('dft2_0_gender', 'dft2_0_job', 'dft2_0_joblang')

## cols_type1 ----

# cols_type1 <- data_clean[, names(.SD), .SDcols = patterns('_type1$')]
cols_type1 <- dft2_lookup_final[variable %like% '_type1$', variable]
cols_type1

cols_type1_z <- cols_type1 %>% subset(cols_type1 %like% '_z_')

cols_type1_a <- cols_type1 %>% subset(cols_type1 %like% '_a_')
cols_type1_b <- cols_type1 %>% subset(cols_type1 %like% '_b_')
cols_type1_c <- cols_type1 %>% subset(cols_type1 %like% '_c_')


## cols_type2 ----
# cols_type2 <- data_clean[, names(.SD), .SDcols = patterns('_type2$')]
cols_type2 <- dft2_lookup_final[variable %like% '_type2$', variable]
cols_type2

cols_type2_z <- cols_type2 %>% subset(cols_type2 %like% '_z_')
cols_type2_a <- cols_type2 %>% subset(cols_type2 %like% '_a_')
cols_type2_b <- cols_type2 %>% subset(cols_type2 %like% '_b_')
cols_type2_c <- cols_type2 %>% subset(cols_type2 %like% '_c_')


## cols_type3 ----
# cols_type3 <- data_clean[, names(.SD), .SDcols = patterns('_type3$')]
cols_type3 <- dft2_lookup_final[variable %like% '_type3', variable]
cols_type3

cols_type3_z <- cols_type3 %>% subset(cols_type3 %like% '_z_')
cols_type3_a <- cols_type3 %>% subset(cols_type3 %like% '_a_')
cols_type3_b <- cols_type3 %>% subset(cols_type3 %like% '_b_')
cols_type3_c <- cols_type3 %>% subset(cols_type3 %like% '_c_')


cols_type3_item <- dft2_lookup_final[variable %like% '_type3___', variable]
cols_type3_item
