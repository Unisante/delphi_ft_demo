## 01c_dft3_define_cols.R
## olivier.duperrex@unisante.ch
## 2022-08-09
## 
## AIM : create lists of variables by type and section for more readabale code

## cols_type1 ----

# cols_type1 <- data_clean[, names(.SD), .SDcols = patterns('_type1$')]
cols_type1 <- dft3_lookup_final[variable %like% '_type1$', variable]
cols_type1

cols_type1_z <- cols_type1 %>% subset(cols_type1 %like% '_z_')

cols_type1_a <- cols_type1 %>% subset(cols_type1 %like% '_a_')
cols_type1_b <- cols_type1 %>% subset(cols_type1 %like% '_b_')
cols_type1_c <- cols_type1 %>% subset(cols_type1 %like% '_c_')
