## 00_functions.R
## olivier.duperrex@unisante.ch
## 2023-03-13
## 
## 
## 

## load some libraries ----------------------------------------------
pacman::p_load(data.table, magrittr, flextable)


## source 000_parameters.R ------------------------------------------
source(here::here('code', '000_parameters.R'), encoding = 'UTF-8')

## load_if_not_loaded -----------------------------------------------
#' Load a dataset if it is not already in the environment. Very useful to save time when processing iterative Rmd. 
#' 
#' @param files_to_load Name of dataset without extension (as it appears in Global env. once loaded)
#' @param filename Filename of dataset to load if it is not in the  Global env. with full path and extension
#'  
#' @examples \dontrun{
#' # with a single dataset ---
#' load_if_not_loaded('dt1', here::here('data', 'dt1.RData')
#' 
#' # with a list of dataset ---
#' files_to_load <- c('dt1, 'dt2')
#' 
#' list_files <- tibble::tibble(
#'   files_to_load = files_to_load,
#'   files = paste0(files_to_load, '.RData'),
#'   filename = here::here('output', 'RData', files)
#'   )
#' 
#' list_files %>% purrr::walk2(.x = .$files_to_load, .y = .$filename, .f = ~ load_if_not_loaded(.x, .y))
#' }
#' 
#' @source Inspired by https://stackoverflow.com/a/58981292/6176250
#' 
#' @export
load_if_not_loaded <- function(files_to_load, filename, ...) {
  if (!exists(files_to_load)) {
    load(filename, envir = .GlobalEnv)
  }
}

## > Documents ------------------------------------------------------

## get_txt_intro_local_files ----------------------------------------
#' Create a data.table with the list of local texts files for introduction. 
#' 
#' @param path_texts_intro_local Path that is defined in 000_parameters.R
#' 
#' @returns 
#' dt_txt_intro_local_files_short, a data.table with local_modification_time, local_path and file_name of local files in ../texts_intro/
#' dt_txt_intro_local_files A data.table with several elements descibing the local files
#' 
#' @examples \dontrun{
#' get_txt_intro_local_files(path_texts_intro_local)
#' dt_txt_intro_local_files[,.N]
#' dt_txt_intro_local_files_short[,.N]
#'  
#' }
#' @export
#' 
get_txt_intro_local_files <-
  function(path_texts_intro_local = path_texts_intro_local) {
    ## txt_intro_local_files : select only the word documents - and send to global env
    dt_txt_intro_local_files <<-
      fs::dir_info(path_texts_intro_local, glob = "*.docx") %>% setDT()
    
    ## keep and rename time and path columns
    dt_txt_intro_local_files_short <-
      dt_txt_intro_local_files[, .(local_modification_time = modification_time,
                                local_path = path)]
    
    ## add file_name without the path
    dt_txt_intro_local_files_short[, file_name := stringr::str_replace_all(local_path,
                                                                        paste0(path_texts_intro_local, "/"),
                                                                        '')]
    ## put file_name in front and send to global env
    dt_txt_intro_local_files_short <<-
      setcolorder(dt_txt_intro_local_files_short, 'file_name')
  }



## get_docx_to_pour -------------------------------------------------
#' Get the local docx file to use (to pour) in the report with officedown. Ideally, the parameters are defined in the Rmd.
#' 
#' @param current_round Current round. Should be 2 or 3 for second (dft2) and third (dft3) rounds, or '' for overall report. 
#' @param report_type Report type. Here 'ExecutiveSummary' or 'generic'. Could of course be different if your docx have different name structure
#' @param keyword Keyword for the section in which to insert the text. Here words are in french : 'situation', 'objectif', 'methode'. Could of course be different if your docx have different name structure and language
#' 
#' @returns docx_to_get Path and name of document to insert.
#'   
#' @export
#' 
get_docx_to_pour <- function(current_round = current_round, report_type = report_type, keyword = keyword) {
  
  docx_name <- dt_txt_intro_local_files_short[, file_name] %>% 
    stringr::str_subset(current_round) %>% 
    stringr::str_subset(report_type) %>% 
    stringr::str_subset(keyword)
  # path_texts_intro_local
  
  docx_to_get <<- paste0(path_texts_intro_local, '/', docx_name)
}


## > Variables and labels -------------------------------------------

## clean_variable_label ---------------------------------------------
#' Clean variable label from html and thank you wording.
#' 
#' Update and add string elements as needed. Can be used to clean a variable within a data.table
#' @param htmlString A string with html code and thank you words
#' @param thankyou_string A regex string including thank you word
#' 
#' @returns txt A cleaned string
#' 
#' 
#' @seealso https://stackoverflow.com/a/17227415/6176250
#' 
#' @examples 
#' 
#' @export
#' 
clean_variable_label <- function(htmlString, thankyou_string) {
 
  ## get rid of html tags and instructions
  txt <- gsub("<.*?>", "", htmlString)
  
  ## delete thank you words and whatever follows
  txt <- stringr::str_replace(txt, thankyou_string, '')

  return(txt)
}


## clean_value_labels ----
#' Clean value labels when preparing a lookup table
#' 
#' @param x A string to clean
#' 
#' @returns y A cleaned string. See comments in function to understand what is cleaned.
#' 
#' @example txt_to_clean <- c("1, La protection de l'environnement | 2, Les mineurs | 3, Les non-consommateurs de produits du tabac / de la nicotine ")
#' clean_value_labels(txt_to_clean)
#'  
#' @export
#' 
clean_value_labels <- function(x){
  y <- x %>% 
    
    # clean 'Autre' (Other)
    stringr::str_replace_all(other_please_long,
                             other_please_short) %>%
    
    # clean no_op (no opinion)
    stringr::str_replace_all(no_op_long,
                             no_op_short) %>% 
    
    # delete numbers directly followed by comma and spaces
    stringr::str_replace_all("[0-9]+\\,\\s+", "") %>%
    
    # replace any comma left by a dash
    stringr::str_replace_all(",", " -") %>%
    
    # replace vertical separator by comma
    stringr::str_replace_all("\\s+\\|", ",")
  
  return(y)
}


## update_statement_name ----
#' Update the name of a statement if there was an initial mistake in building it in REDCap
#' 
#' @param dt A data.table with variable names as columns
#' @param metadata A data.table with variable names as rows
#' @param statement_to_update A string with the statement number (i.e '_s2_')
#' @param pattern_colname_old String with old pattern to modify (i.e '_a_s2_')
#' @param pattern_colname_new String with new modified pattern (i.e '_z_s2_')
#' 
#' @returns dt and metadata with updated names of variables
#' @export
#' 
update_statement_name <- function(dt, metadata, pattern_colname_old, pattern_colname_new) {
  # list of old columns names
  cols_old <- dt[, names(.SD), .SDcols = patterns(pattern_colname_old)]
  cols_old
  
  # list of new columns names
  cols_new <- cols_old %>% stringr::str_replace_all(pattern_colname_old, pattern_colname_new)
  cols_new
  
  # replace the names in dt
  setnames(dt, cols_old, cols_new)
  
  # replace the names in metadata 
  metadata[field_name %like% pattern_colname_old,
           field_name := stringr::str_replace_all(field_name, pattern_colname_old, pattern_colname_new)]
  
}


## add_var_label_exec_summary ---------------------------------------------
#' Clean variable label from html and thank you wording.
#' 
#' Update and add string elements as needed. Can be used to clean a variable within a data.table
#' @param x A string with variable label containing a part to delete
#' @param statement_txt A string the statement text
#' @param statement_number A statement number
#' 
#' @returns txt A cleaned string
#' 
#' 
#' @examples 
#' 
#' @export
#' 
add_var_label_exec_summary <- function(x, statement_txt, statement_number) {
  
  ## string to be replaced

  txt <- paste0(statement_txt, " ", statement_number, colon_punctuation_txt)
  
  ## delete thank you words and whatever follows
  x_cleaned <- stringr::str_replace(x, txt, '')
  
  return(x_cleaned)
}

## > Clean data.table -----------------------------------------------

## clean_after_collapse --------------------------------------
## !!! need to sort the conditional formatting if date_cols provided

#' Create year_week and make correction for last or first days for some years
#'
#'
#' @param dt  data.table
#' @param date_cols List of variables that are dates ('%Y-%m-%d')
#'
#'
#' @returns A data.table with empty and text 'NA' replaced by <NA>, dates formatted as IDates
#' @export
#' 
clean_after_collapse <- function(dt, date_cols = NULL, ...) {
  
  ## .. replace empty by NA for any type of variable ----
  
  ## https://stackoverflow.com/a/53652776/6176250
  dt[, names(dt) := lapply(.SD, function(x) replace(x, x=="", NA))]
  
  
  ## .. define character cols ----
  # char_cols <- names(which(sapply(dt, is.character)))
  char_cols <- dt[, names(.SD), .SDcols = is.character]
  
  ## .. recode NA as <NA> ---
  ## inspired by https://stackoverflow.com/a/37422218/6176250
  dt[, (char_cols) := lapply(.SD, type.convert, as.is = TRUE, na.strings = "NA"), .SDcols = char_cols]
  
  # ## .. recode as.IDate ---
  # date_cols <- dt[, names(.SD), .SDcols = patterns('^date')]
  # # date_cols <- c('dob', dt[, names(.SD), .SDcols = patterns('^date')])
  # if (!is.null(date_cols)){ 
  # dt[, (date_cols) := lapply(.SD, as.IDate, format = '%Y-%m-%d', tz = "Europe/Paris"), .SDcols = date_cols]
  # }
}


## get_max after collapsing rows ------------------------------------
## adapted from https://stackoverflow.com/a/70543306/6176250
#' Get the maximum value of elements in a string
#' 
#' @param x A string obtained after collapsing with `;` as separator
#' 
#' @returns Maximum value of the string x
#' @examples 
#' x <- 	c("9; NA; 7; NA")
#' get_max(x)
#' @export
#' 
get_max = function(x) {
  vals = unlist(stringr::str_split(x, ";"))
  max(as.numeric(vals[vals != ""]), na.rm = TRUE)
}


## get_last after collapsing rows ------------------------------------
#' Get the latest value of elements in a string
#' 
#' @param x A string obtained after collapsing with `;` as separator
#' 
#' @returns Last value in the string x that is not NA
#' @examples 
#' x <- 	c("9; NA; 7; NA")
#' get_last(x)
#' 
#' @export
#' 
get_last = function(x) {
  # 
  vals <- x %>% 
    stringr::str_split(";") %>%
    unlist() %>%
    trimws() %>%      # trim white spaces 
    stringr::str_replace_all("NA", NA_character_) 
  
  last_value <<-
    vals %>%
    na.omit() %>%
    data.table::last()
}

# remove_NAs_after_collapsing --------------------------------------
#' Remove NAs with punctuation after collapsing
#' 
#' @param x A string obtained after collapsing with `;` as separator
#' 
#' @returns String without NAs 
#' @examples 
#' x <- 	c("9; NA; 7; NA")
#' remove_NAs_after_collapsing(x)
#' 
#' @export
#' 
remove_NAs_after_collapsing <- function(x) {
  pattern_1 <- "NA; "
  pattern_2 <- "; NA"
  x %>% 
    stringr::str_remove_all(pattern_1) %>% 
    stringr::str_remove_all(pattern_2) %>% 
    return()
}



## every_second_item ------------------------------------------------
#' Assign a blank label for every other item in a list
#'
#' Very useful for some graphs
#'
#' @param x A list
#' @param decreasing Logical. For direction of sorting. If left empty, FALSE is used so the order will be from lower to higher values
#' @returns x A list with a blank every second item
#'
#' @seealso
#' Inspired by https://community.rstudio.com/t/how-to-automatically-skip-some-x-labels/65702
#'
#' @examples
#' list_to_test <- c(3, 4, 5, 6)
#' every_second_item(list_to_test)
#' every_second_item(list_to_test, decreasing = FALSE)
#' every_second_item(list_to_test, decreasing = TRUE)
#' every_second_item(seq(1,9))
#'
#' # works also with strings
#' text_to_sort <- c('Leia', 'Beru','Luke', 'Anakin',  'R2-D2', 'C-3PO')
#' every_second_item(text_to_sort)
#' every_second_item(text_to_sort, decreasing = TRUE)
#'
#' @export
#' 
every_second_item <- function(x, decreasing = NULL) {
  if (is.null(decreasing)) {
    a <- FALSE
  } else {
    if (decreasing == TRUE | decreasing == T) {
      a  <- TRUE
    } else {
      a <- FALSE
    }
  }
  
  x <- sort(unique(x), decreasing = a)
  x[seq(2, length(x), 2)] <- ""
  x
}



## extract_section --------------------------------------------------
#' Extract the section
#' 
#' @param x A variable name with the section (letter) between two underscores
#' 
#' @returns Section, as the single character between two underscores
#' 
#' @examples 
#' extract_section('dft3_a_s3_type3___1')
#' extract_section('dft3_z_s1_type1')
#' 
#' @export
#' 
extract_section <- function(x){
  section <- stringr::str_extract(x, '(?<=\\_)(.*?)(?=\\_)') %>% toupper()  
}




## extract_type --------------------------------------------------
#' Extract the type of question
#' 
#' @param x A variable name with the type of question 
#' 
#' @returns Type of question with the number just after the word 'type'
#' 
#' @examples 
#' extract_type('dft3_a_s3_type3___1')
#' extract_type('dft3_a_s6_type2')
#' 
#' @export
#' 
extract_type <- function(x){
  type <- paste0('type', 
                 stringr::str_extract(x, "(?<=type)[0-9]*")
  )
}




## extract_statement_number --------------------------------------------------
#' Extract the statement number
#' 
#' @param x A variable name with the statement number just after '_s'
#' 
#' @returns Statement number
#' 
#' @examples 
#' extract_statement_number('dft3_a_s3_type3___1')
#' extract_statement_number('dft3_a_s6_type2')
#' 
#' @export
#' 
extract_statement_number <- function(x){
  statement_number <- stringr::str_extract(x, "(?<=_s)[0-9]*")
  
}


## extract_variable_root --------------------------------------------------
#' Extract the variable root
#' 
#' @param x A variable name
#' 
#' @returns A string as the variable root, without detail of the type of question
#' 
#' @examples 
#' extract_variable_root('dft3_a_s3_type3___1')
#' extract_variable_root('dft3_a_s6_type2')
#' 
#' @export
#' 
extract_variable_root <- function(x){
  variable_root <-  sub("_type.*", "", x) 
}




## > Plots ----------------------------------------------------------

## the following elements will be used in the plots below are defined and can be modified in 000_parameters.R
# breaks_gg
# limits_gg

## labels_x_gg_bar --------------------------------------------------
labels_x_gg_bar <- every_second_item(seq(1,9))


## gg_bar -------------------------------------------------------
#' Mini barplot to insert in a word table
#' 
#' @param z A list of values
#' 
#' @returns Elements of plots
#' 
#' @seealso https://ardata-fr.github.io/officeverse/index.html
gg_bar <- function(z) {
  # z <- scale(z)
  z <- na.omit(z)
  z <- data.frame(z)
  ggplot(z, aes(x = z)) +
    geom_bar(width = 0.5, show.legend = FALSE) +
    # scale_x_discrete(limits = c(1, 3, 5, 7, 9)) +
    scale_x_continuous(breaks = breaks_gg,
                       limits = limits_gg,
                       labels = labels_x_gg_bar) +
    sjPlot::theme_blank(base_size = 20) + 
    theme(axis.text.y = element_blank(),
          axis.title = element_blank()
    )
  
}

## gg_boxplot -------------------------------------------------------
#' Mini boxplot to insert in a word table
#' 
#' @param z A list of values
#' 
#' @returns Elements of plots
#' 
#' @seealso https://ardata-fr.github.io/officeverse/index.html
gg_boxplot <- function(z) {
  z <- scale(z)
  z <- na.omit(z)
  z <- data.frame(z)
  # z <- data.frame(x = seq_along(z), z = z, w = z < 0)
  ggplot(z, aes(x = z)) +
    geom_boxplot(show.legend = FALSE) +
    theme_void()
}

## gg_boxplot_bis -------------------------------------------------------
#' Mini boxplot to insert in a word table
#' 
#' @param z A list of values
#' 
#' @returns Elements of plots
#' 
#' @seealso https://ardata-fr.github.io/officeverse/index.html
gg_boxplot_bis <- function(z) {
  z <- na.omit(z)
  z <- data.frame(z)
  ggplot(z, aes(x = z)) +
    geom_boxplot(show.legend = FALSE) +
    scale_x_continuous(breaks = breaks_gg,
                       limits = limits_gg, 
                       labels = NULL
                       # labels = labels_x_gg_bar
    )  +
    sjPlot::theme_blank(base_size = 20) +
    theme(axis.text.y = element_blank(),
          axis.title = element_blank()
    )
}

## gg_duo ----
#' Duo of plots (mini bar + mini boxplot) to insert in a word table
#' 
#' @param z A list of values
#' 
#' @returns Elements of plots
#' 
#' @seealso https://ardata-fr.github.io/officeverse/index.html
gg_duo <- function(z) {
  z <- na.omit(z)
  z <- data.frame(z)
  
  p1_bar <- ggplot(z, aes(x = z)) +
    geom_bar(width = 0.5, show.legend = FALSE) +
    # scale_x_discrete(limits = c(1, 3, 5, 7, 9)) +
    scale_x_continuous(breaks = breaks_gg,
                       limits = limits_gg,
                       labels = labels_x_gg_bar) +
    sjPlot::theme_blank(base_size = 72) + 
    theme(axis.text.y = element_blank(),
          axis.title = element_blank()
    )
  
  
  p2_bxp <- ggplot(z, aes(x = z)) +
    geom_boxplot(show.legend = FALSE, lwd = 2) +
    scale_x_continuous(breaks = breaks_gg,
                       limits = limits_gg, 
                       labels = NULL
                       # labels = labels_x_gg_bar
    )  +
    sjPlot::theme_blank(base_size = 20) +
    theme(axis.text.y = element_blank(),
          axis.title = element_blank()
    )
  
  # use of patchwork to combine them
  p_duo <- p1_bar / p2_bxp +
    plot_layout(heights = c(4, 1))
  
  return(p_duo)
} 



## gg_histo ---------------------------------------------------------
#' Mini histogram to insert in a word table
#' 
#' @param z A list of values
#' 
#' @returns Elements of plots
#' 
#' @seealso https://ardata-fr.github.io/officeverse/index.html
gg_histo <- function(z) {
  z <- z / total_participants
  z <- scale(z)
  z <- na.omit(z)
  z <- data.frame(z)
  ggplot(z, aes(x = z)) +
    geom_histogram(show.legend = FALSE,  bins = 30) +
    theme_void()
}



## > Flextables --------------------------------------------------------

## set_flextable_defaults ----
## voir https://github.com/davidgohel/flextable/issues/383 pour hansi.family
flextable::set_flextable_defaults(
  font.family = "Calibri",
  hansi.family = "Calibri",
  eastasia.family = "Calibri",
  cs.family = "Calibri",
  font.size = 10,
  big.mark = "'",
  # padding.top = 7,
  # padding.bottom = 7,
  padding.bottom = 3, 
  padding.top = 3,
  padding.left = 3,
  padding.right = 3,
  theme_fun = theme_alafoli
)
flextable::get_flextable_defaults()


## cols_for_simple_type1_participant -----
cols_for_simple_type1_participant <- c(
  'var_label',
  'agreement_icon',
  'consensus_icon',
  'responses',
  'median',
  'stats',
  'responses_participant'
)

## cols_for_histo_box_type1_participant -----
cols_for_histo_box_type1_participant <- c(
  'var_label',
  'agreement_icon',
  'consensus_icon',
  'responses',
  'median',
  'stats',
  'responses_participant',
  'histo_box'
)

## cols_for_histo_box_type1_generic -----
cols_for_histo_box_type1_generic <- c(
  'var_label',
  'agreement_icon',
  'consensus_icon',
  'responses',
  'median',
  'stats',
  'histo_box'
)



## cols_for_minibar_type2_participant ----
cols_for_minibar_type2_3_participant <-
  c('value_labels', 'agreement_icon', 'n', 'prop', 'minibar', 'responses_participant')


## cols_for_minibar_type2_3_generic ----
cols_for_minibar_type2_3_generic <-
  c('value_labels', 'agreement_icon', 'n', 'prop', 'minibar')


# ## cols_for_minibar_type3_generic ----
# cols_for_minibar_type3_generic <-
#   c('item', 'agreement_icon', 'n', 'prop', 'minibar')


## cols_for_comments_participant ----
cols_for_comments_participant <- c('item') # , 'responses_participant'

## cols_for_comments_participant_type1 ----
# cols_for_comments_participant_type1 <- c('item', 'value_labels') # , 'responses_participant'


## cols_for_type_1_exec_summary -----
cols_for_type_1_exec_summary <- c(
  'section',
  'var_label_exec_summary',
  'median',
  'stats',
  'img_path'
)

## create_summary_type1 ---------------------------------------------
## more efficient : https://stackoverflow.com/a/72750112/6176250
## ... and use with rbindlist 
## t1_type1 <- data_clean[, rbindlist(lapply(.SD, create_summary_type1), idcol = "variable"), .SDcols = cols_type1]

#' Create statistical summary for type 1 question
#' 
#' @param z A variable that is a type 1 question
#' 
#' @returns summary_cols A list of summary values : 
#'   var_label Variable label 
#'   participants_count Number of participants
#'   reponses_count Number of responses
#'   response_rate Response rate
#'   reponses_na Number of non-responders
#'   min Minimum response value
#'   q1 Quartile 1 : 25% of responses are at the value or below
#'   median Median : 50% of responses are at the value or below
#'   q3 Quartile 3 : 75% of responses are at the value or below
#'   max Maximum response value

#' 
#' @seealso Efficient solution found in https://stackoverflow.com/a/72750112/6176250
#' 
#' @examples \dontrun{
#' # it can be used as function inside a data.table
#' # the line below will create separate lists of summary values of the column 'variable' for type1 columns in data_clean and then bind the lists into a new summary data.table 
#' t1_type1 <- data_clean[, rbindlist(lapply(.SD, create_summary_type1), idcol = "variable"), .SDcols = cols_type1]
#' }
#' 
create_summary_type1 <- function(x) {
  var_label = sjlabelled::get_label(x)
  participants_count = length(x)
  reponses_count = sum(!is.na(x))   # with chek if is.na or not and then count them
  response_rate = round(reponses_count / participants_count, 3)
  reponses_na = sum(is.na(x))
  min    = min(x, na.rm = TRUE)
  q1     = quantile(x, 0.25,  na.rm = TRUE)
  median = median(x, na.rm = TRUE)
  q3     = quantile(x, 0.75,  na.rm = TRUE)
  max    = max(x, na.rm = TRUE)
  
  summary_cols <- list(
    var_label	=	 var_label,
    participants_count	= participants_count,
    reponses_count	= reponses_count,
    response_rate	= response_rate,
    reponses_na	= reponses_na,
    min	= min,
    q1	= q1,
    median	= median,
    q3	= q3,
    max	= max
  )
  
}


## create_flextable_table1 --------------------------------------------------
#' Create table 1 description of participants
#' 
#' @param dt Data.table
#' 
#' @param cols_for_table1_generic List of columns ; variables of participants characteristics
#' 
#' @param label_proportion Label for proportion
#' 
#' @param variable_label Label for variable (a column in dt)

create_flextable_table1 <- function(dt,
                          cols_for_table1_generic,
                          label_proportion) {
  dt %>%
    as_grouped_data(groups = "variable_label") %>%
    as_flextable(col_keys = cols_for_table1_generic, hide_grouplabel = TRUE) %>%
    set_formatter(
      prop = function(x)
        fifelse(is.na(x), "",
                sprintf("%.1f%%", x * 100))
    ) %>%
    set_header_labels(
      # variable_label = 'Variable',
      # value_labels   = item_txt_type_2_3,
      value_labels   = '',
      n = 'n',
      prop = label_proportion
    ) %>%
    set_table_properties(layout = "autofit", width = .8) %>%
    # color(part = "footer", color = "#800000") %>%
    flextable::bold(bold = TRUE, part = "header") %>%
    align(i = ~ !is.na(variable_label), align = "left") %>%
    flextable::bold(i = ~ !is.na(variable_label))%>% 
    padding(i = ~ is.na(variable_label), padding.left = 12)
}


## create_flextable_type_1_exec_summary ----------------------------------
#' Create a flextable of type 1 question for the executive summary
#' 
#' @param dt Data.table containing summary elements and 
create_flextable_type_1_exec_summary <- function(dt) {
  dt %>%
    flextable::flextable(col_keys = cols_for_type_1_exec_summary) %>% 
        
    line_spacing(space = 1.2, part = "all") %>%
    
    align(part = 'all', j = cols_for_type_1_exec_summary[-2], align = 'center') %>%
    
    ## adjust values below until satisfying result is obtained 
	
    colformat_image(i = ~ !is.na(img_path), 
                    j = "img_path", 
                    width = width_exec_table, 
                    height = height_exec_table) %>%

    padding(
      j = 'img_path',
      part = "body",
      padding.top = 3,
      padding.bottom = 3
    ) %>%
    
    set_table_properties(layout = "autofit", width = .8) %>%

    flextable::bold(j = c('section', 'median'), bold = TRUE, part = "body") %>%
    flextable::bold(bold = TRUE, part = "header") %>%
    flextable::bold(i = ~ var_label_exec_summary %in% list_labels_sections, 
         j = 'var_label_exec_summary', bold = TRUE, part = "body") %>%

    fontsize(j = 'var_label_exec_summary', size = 16, part = "header") %>% 
    
    set_header_labels(
      section = '',
      var_label_exec_summary = label_var_execsummary,
      median = label_median,
      stats = label_stats,
      img_path = label_scale
    ) %>% 
    merge_v(j = 'section') 
  
}

# dt <- type1_zz_combined_round_2_3

## create_flextable_histo_box_type_1_generic ----------------------------------
create_flextable_histo_box_type_1_generic <- function(dt) {
  dt %>%
    flextable::flextable(col_keys = cols_for_histo_box_type1_generic) %>%
    # qflextable::flextable(col_keys = c('var_label', 'responses', 'stats', 'histo_box')) %>%
    flextable::compose(j = 'agreement_icon',
                       value = as_paragraph(as_chunk(
                         agreement_icon, fp_text_default(color = green_ok, bold = TRUE)
                       ))) %>%
    flextable::compose(j = 'consensus_icon',
                       value = as_paragraph(as_chunk(
                         consensus_icon, fp_text_default(color = green_ok, bold = TRUE)
                       ))) %>%
    flextable::compose(j = 'histo_box',
                       value = as_paragraph(
                         gg_chunk(histo, height = .25, width = 1),
                         "\n",
                         # gg_chunk(linerange, height = .5, width = 1),
                         linerange(
                           value = '',
                           min = min_scale_delphi,
                           max = max_scale_delphi,
                           height = .1,
                           stickcol = red_unisante,
                           width = 1
                         ),
                         "\n",
                         gg_chunk(boxplot, height = .1, width = 1)
                       )) %>%
    
    
    line_spacing(space = 1.2, part = "all") %>%
    # align(j = c('responses', 'stats', 'histo_box'), align = "center", part = "all") %>%
    align(part = 'all', j = cols_for_histo_box_type1_generic[-1], align = 'center') %>%
    padding(
      j = 'histo_box',
      part = "body",
      padding.top = 12,
      padding.bottom = 12
    ) %>%
    set_table_properties(layout = "autofit", width = 1) %>%
    flextable::bold(j = 'median', bold = TRUE, part = "body") %>%
    set_header_labels(
      # var_label = statement_txt,
      var_label = '',
      agreement_icon = label_agreement,
      consensus_icon = label_consensus_2lines,
      responses = label_n_pct,
      median = label_median_short,
      stats = label_stats,
      
      histo_box = label_scale
    ) %>%
    
    flextable::footnote(
      j = c('agreement_icon', 'consensus_icon'),
      value = as_paragraph(as_chunk(
        c(footnote_agreement_column_type1,
          footnote_consensus_column),
        fp_text_default(color = green_ok)
      )),
      ref_symbols = c("a", "b"),
      part = 'header'
    ) %>%
    
    # width(width = 1)  %>% 
    width(j = c("var_label", 'stats'), 
          width = c(4, 2))
  
}


## create_flextable_histo_box_type_1_participant ------------------------------------------
create_flextable_histo_box_type_1_participant <- function(dt) {
  dt %>%
    flextable::flextable(col_keys = cols_for_histo_box_type1_participant) %>%
    # qflextable::flextable(col_keys = c('var_label', 'responses', 'stats', 'histo_box')) %>%
    flextable::compose(j = 'agreement_icon',
                       value = as_paragraph(as_chunk(
                         agreement_icon, fp_text_default(color = green_ok, bold = TRUE)
                       ))) %>%
    flextable::compose(j = 'consensus_icon',
                       value = as_paragraph(as_chunk(
                         consensus_icon, fp_text_default(color = green_ok, bold = TRUE)
                       ))) %>%
    flextable::compose(j = 'histo_box',
                       value = as_paragraph(
                         gg_chunk(histo, height = .25, width = 1),
                         "\n",
                         # gg_chunk(linerange, height = .5, width = 1),
                         linerange(
                           value = responses_participant,
                           min = min_scale_delphi,
                           max = max_scale_delphi,
                           height = .1,
                           stickcol = red_unisante,
                           width = 1
                         ),
                         "\n",
                         gg_chunk(boxplot, height = .1, width = 1)
                       )) %>%
    
    flextable::compose(j = "responses_participant",
                       value = as_paragraph(as_chunk(
                         responses_participant,
                         fp_text_default(color = red_unisante, bold = TRUE)
                       ))) %>%
    
    line_spacing(space = 1.2, part = "all") %>%
    # align(j = c('responses', 'stats', 'histo_box'), align = "center", part = "all") %>%
    align(part = 'all', j = cols_for_histo_box_type1_participant[-1], align = 'center') %>%
    padding(
      j = 'histo_box',
      part = "body",
      padding.top = 12,
      padding.bottom = 12
    ) %>%
    set_table_properties(layout = "autofit", width = 1) %>%
    colformat_num(col_keys = c("responses_participant"),
                  digits = 0) %>%
    color(j = "responses_participant", color = red_unisante) %>%
    flextable::bold(j = 'median', bold = TRUE, part = "body") %>%
    set_header_labels(
      # section = 'Section',
      # variable = 'Variable',
      # var_label = statement_txt,
      var_label = '',
      agreement_icon = label_agreement,
      consensus_icon = label_consensus_2lines,
      responses = label_n_pct,
      median = label_median_short,
      stats = label_stats,
      responses_participant = label_responses_participant_plural,
      
      histo_box = label_scale
    ) %>%
    
    flextable::footnote(
      j = c('agreement_icon', 'consensus_icon'),
      value = as_paragraph(as_chunk(
        c(footnote_agreement_column_type1,
          footnote_consensus_column),
        fp_text_default(color = green_ok)
      )),
      ref_symbols = c("a", "b"),
      part = 'header'
    ) %>%
    
    # width(width = 1)  %>%
    width(j = c("var_label", 'stats'), 
          width = c(4, 2))
  
}


## create_flextable_comments_participant ----------------------------------------------
create_flextable_comments_participant <- function(dt) {
  
  dt %>%
    flextable::flextable(col_keys = cols_for_comments_participant) %>%
    set_header_labels(
      item   = label_comment
    ) %>%
    # align(part = 'all', j = cols_for_comments_participant[-1], align = 'center') %>%
    width(j = 1, width = 5)
}




## create_flextable_minibar_type_2_3_generic ----------------------------------------------


create_flextable_minibar_type_2_3_generic <- function(dt) {
  type <- dt[, unique(type)]
  dt %>%
    flextable::flextable(col_keys = cols_for_minibar_type2_3_generic) %>%
    # qflextable() %>%
    flextable::compose(j = "minibar",
                       value = as_paragraph(value = minibar(
                         value = minibar,
                         max = 1,
                         barcol = barcol_color,
                         bg = bg_color
                       ))) %>%
    flextable::compose(
      i = ~ agreement == "ok",
      j = "agreement_icon",
      value = as_paragraph(as_chunk(
        agreement_icon, fp_text_default(color = green_ok, bold = TRUE)
      ))) %>%
    set_formatter( prop = function(x) fifelse(is.na(x), "",
                                              sprintf( "%.1f%%", x * 100 )) ) %>% 
    set_header_labels(
      value_labels   = item_txt_type_2_3,
      n = 'n',
      agreement_icon = label_agreement,
      prop = label_proportion,
      minibar = label_proportion
    ) %>%
    
    flextable::footnote(
      i = 1,
      j = 'agreement_icon',
      value = as_paragraph(as_chunk(
        footnote_agreement_column_type2_3,
        fp_text_default(color = green_ok)
      )),
      ref_symbols = 'a',
      part = 'header'
    ) %>%
    
    
    merge_h(part = "header") %>%
    align(part = "header", j = 'prop', align = "center") %>%
    # align(part = 'all', j = c('agreement_icon', 'reponses_generic'), align = 'center') %>% 
    align(part = 'all', j = cols_for_minibar_type2_3_generic[-1], align = 'center') %>%
    width(j = c('value_labels', 'minibar'), width = c(3, 1.5))
}


## create_flextable_minibar_type_2_3_participant ----------------------------------------------


create_flextable_minibar_type_2_3_participant <- function(dt) {
  type <- dt[, unique(type)]
  dt %>%
    flextable::flextable(col_keys = cols_for_minibar_type2_3_participant) %>%
    # qflextable() %>%
    flextable::compose(j = "minibar",
                       value = as_paragraph(value = minibar(
                         value = minibar,
                         max = 1,
                         barcol = barcol_color,
                         bg = bg_color
                       ))) %>%
    
    # flextable::compose(
    #   j = "agreement_icon",
    #   value = as_paragraph(as_chunk(
    #     agreement_icon, fp_text_default(color = fcase(agreement == "not_ok", 'orange',
    #                                                   agreement == "ok", green_ok),
    #                                     bold = TRUE)
    #   ))) %>%
    
    flextable::compose(
      i = ~ agreement == "ok",
      j = "agreement_icon",
      value = as_paragraph(as_chunk(
        agreement_icon, fp_text_default(color = green_ok, bold = TRUE)
      ))) %>%
    
    # flextable::compose(
    #   i = ~ agreement == "not_ok",
    #   j = "agreement_icon",
    #   value = as_paragraph(as_chunk(
    #     agreement_icon, fp_text_default(color = 'orange', bold = TRUE)
    #   ))) %>%
    
    flextable::compose(j = "responses_participant",
                       value = as_paragraph(as_chunk(
                         responses_participant,
                         fp_text_default(color = red_unisante, bold = TRUE)
                       ))) %>%
    
    set_formatter( prop = function(x) fifelse(is.na(x), "",
                                              sprintf( "%.1f%%", x * 100 )) ) %>% 
    
    set_header_labels(
      value_labels   = item_txt_type_2_3,
      agreement_icon = label_agreement,
      prop = label_proportion,
      minibar = label_proportion,
      responses_participant = fifelse(type == 'type2', label_responses_participant_single, label_responses_participant_plural)
    ) %>%
    
    flextable::footnote(
      i = 1,
      j = 2, # 'agreement_icon'
      value = as_paragraph(as_chunk(
        footnote_agreement_column_type2_3,
        fp_text_default(color = green_ok)
      )),
      ref_symbols = 'a',
      part = 'header'
    ) %>%
    
    merge_h(part = "header") %>%
    align(part = "header", j = 'prop', align = "center") %>%
    # align(part = 'all', j = c('agreement_icon', 'responses_participant'), align = 'center') %>% 
    align(part = 'all', j = cols_for_minibar_type2_3_participant[-1], align = 'center') %>%
    width(j = c('value_labels', 'minibar'), width = c(3, 1.5))
}

 
# ft_participant <-
#   get(selected_table)[variable %like% selected_variable,] %>%  create_flextable_minibar_type_2_3_participant()
# ft_participant

