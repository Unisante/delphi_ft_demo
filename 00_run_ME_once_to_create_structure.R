## 00_run_ME_once_to_create_structure.R -------------------------------------------------------
## 2022-09-19
## olivier.duperrex@unisante.ch
## 
## This script will : 
## . create the structure of the project


## > create_folder ----
##' ad hoc function that creates a folder if does not exists
##' will not modify existing folder even if subfolder is created
create_folder <- function(x) {
  if (!fs::dir_exists(x)) {
    fs::dir_create(here::here(x))
  }
}

## > create structure of project ----
# 0. list of folders to create ----
folders <- c(
  'analysis/dft2',
  'analysis/dft3',
  'analysis/overall',
  'texts_intro',
  'code/dft2',
  'code/dft3',
  'data/dft2',
  'data/dft3',
  'data/redcap_data_raw'
)

purrr::walk(folders, create_folder)


# 1. list of subfolders to create under output ----
folders <- c('checks',
             'png',
             'RData',
             'reports/dft2/report_by_participant',
             'reports/dft3/report_by_participant',
             'reports/overall')

# 2. tibble and create folder_name including path ----
list_folders <- tibble::tibble(folders = folders,
                               folder_name = here::here('output', folders))

# 3. create subfolders ---

purrr::walk(list_folders$folder_name, create_folder)



