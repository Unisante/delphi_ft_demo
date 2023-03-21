## 00_update_texts_intro.R ----
## olivier.duperrex@unisante.ch
## 2022-07-01


## Aim : copy locally latest version of textes_intro

pacman::p_load(data.table, magrittr)

source(here::here('code', '00_functions.R'), encoding = 'UTF-8')


## > files_on_server : select only the word documents ---------------
files_on_server <- fs::dir_info(path_texts_intro_server, glob = "*.docx") %>% setDT()

## . keep only modification_time and path ----
files_on_server_short <- files_on_server[, .(modification_time, path)]

## . replace complete path by shorter one ----
files_on_server_short[, file_name := stringr::str_replace_all(
  path,
  paste0(path_texts_intro_server, "/"),
  '')]

## reorder cols ---
files_on_server_short <-  setcolorder(files_on_server_short, 'file_name')
files_on_server_short[,.N]

## . dt0 : so the original is not modified during join below ----
dt0 <- files_on_server_short



## > local_files : select only the word documents -------------------
## path_texts_intro_local defined in 000_parameters.R

## . dt_txt_intro_local_files_short ----
## get the list of local files with a function created in 00_functions.R
get_txt_intro_local_files(path_texts_intro_local) # in 00_functions.R

names(dt_txt_intro_local_files_short)

## . cols_to_keep : for the join below ----
cols_to_keep <- dt_txt_intro_local_files_short[,names(.SD), .SDcols = !patterns('file_name')]

## . join the two ----
dt0[dt_txt_intro_local_files_short, on = 'file_name', (cols_to_keep) := mget(cols_to_keep)  ]
dt0[!(local_modification_time == modification_time) |
      is.na(local_modification_time), files_to_copy := 1]


## files_to_copy_N ----
files_to_copy_N <- dt0[files_to_copy == 1,.N][]
files_to_copy_N

## . copy from server ----
dt0[files_to_copy == 1, path] %>% 
  fs::file_copy(path_texts_intro_local, overwrite = TRUE)


## . rename files with 0b in their name ----
## not needed anymore, but kept as example how it can be done by code
# files_to_rename <- fs::dir_info(path_texts_intro_local, regex ='0b')$path
# files_to_rename
# 
# file.rename(files_to_rename,
#             stringr::str_replace(files_to_rename, '0b', 'Z'))



