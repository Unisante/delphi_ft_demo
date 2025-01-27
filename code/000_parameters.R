## 000_parameters.R
## Prepared by olivier.duperrex@unisante.ch
## Modified on 2023-11-07
##
## Elements that can be modified according to the topic, the language,
## and desired adjustments on the output
##
## To modify the structure of the tables and plots: 00_functions.R

## variable : is a question
## item : is the possible answers
## value_labels : labels of possible answers
## cols : columns


## 0. general elements --------------------------------------------------------
## . paths ---------------------------------------------------------

# path_to_my_token <- here::here(token_delphi_ft_this_project.R)        # not used in demo
## if you see an error message : object 'token_delphi_ft_this_project.R' not found,
## open 'token_delphi_ft_this_project.R.example' and follow the instructions

# source(path_to_my_token, encoding = 'UTF-8') # returns redcap_uri, token_dft2 and token_dft3 - not used in demo


path_texts_intro_local <- here::here('texts_intro')

# path_texts_intro_server <- "//path/to/your/server/project/texts_intro" # replace with yours - not used in demo



## . authors ----
authors_text <-
  "Camille Velarde Crézé, Luc Lebon, Vincent Faivre, Olivier Duperrex - Unisanté" # replace with yours

## . email_tester ---------------------------------------------------
email_tester <-
  c('participant_27@some.email',
    'tester_2@some.email')

## >> .. exclude participants according to record_id ----
## if several use c(111, 222)
dft2_record_id_to_ignore <- ""
dft3_record_id_to_ignore <- ""

## .. cols_to_ignore ----
## this will be used when excluding participants who did not answer any question apart from their email
cols_to_ignore <- c("dft3_0_email",
                    "record_id",
                    "round_3_equestionnaire_complete")


## . reports titles -----------------------------------------------
project_text <- "Delphi fast-track – Puff Bars"

title_round_2 <-   glue::glue("{project_text} – 2e étape")
title_round_3 <-   glue::glue("{project_text} – 3e étape")



subtitle_generic <- 'Rapport générique'
subtitle_participant <- 'Rapport pour participant∙e'

heading_situation <- "Situation et objectif"
heading_method <- "Méthode"
heading_toc <- "Table des matières {#toc}"
heading_list_tables <- "Liste des tables de résultats"

heading_participants_characteristics <- "Profil des participant∙e∙s"

heading_questions_type1 <- "Questions de type 1"
heading_questions_type2 <- "Questions de type 2"
heading_questions_type3 <- "Questions de type 3"

heading_comment_section <- "Commentaires sur "

email_recipient_text <-
  "Destinataire de ce rapport : " # used in individual reports


## 1. define metrics --------------------------------------------
## . delphi scale -------------------------------------------------
min_scale_delphi <- 1
max_scale_delphi <- 9



## . consensus and agreement thresholds ----------------------------
# . agreement reached if median >=7 : OK  sign bold and green
# . disagreement reached if median <=3 : OK with exclamation mark
#
# . consensus reached if IQR_range <= 3


agreement_threshold_type1 <- 7      # agreement if median >=7
disagreement_threshold_type1 <- 3   # disagreement if median <=3

consensus_threshold_type1 <- 3      # iqr_range <= 3

agreement_threshold_type2_3 <-  0.66
# agreement_threshold_type3 <-  0.66



## . icons ----
icon_ok <- "\U2714" # tick mark

icon_agreement_reached    <- icon_ok
icon_disagreement_reached <- paste0(icon_ok, " !")

## . define colors ----
red_unisante <- "#D91136" ## rgb(217, 17, 54)
color_txt <- "rouge"

green_ok     <- "#5BB951" ## rgb(91, 185, 91)

bg_color     <- 'grey92'  ## rgb(235, 235, 235)	#EBEBEB
barcol_color <- 'grey70'  ## rgb(179, 179, 179)	#B3B3B3
border_color <- "grey40"  ## rgb(102, 102, 102)	#666666


## 2. labels and levels ---------------------------------------------
## . pattern_boxes_fr -----------------------------------------------
## to use with REDCapR::regex_named_captures

## \x7f-\xff was added to take french caracters into account () - worked with R 4.1 but not R4.2 ???
# pattern_boxes_fr <- "(?<=\\A| \\| )(?<id>\\d{1,}), (?<label>[\x20-\x7B\x7D-\x7E\x7f-\xff]{1,})(?= \\| |\\Z)"


## new solution : add À-ÖØ-öø-ÿ at the beginning !! ---
##https://stackoverflow.com/a/48925757/6176250
pattern_boxes_fr <-
  "(?<=\\A| \\| )(?<id>\\d{1,}), (?<label>[À-ÖØ-öø-ÿ\x20-\x7B\x7D-\x7E]{1,})(?= \\| |\\Z)"
# pattern_boxes_fr <- stringr::regex("
#                                    (?<=\\A| \\| )       # positive lookbehind :
#                                    (?<id>\\d{1,}),      #
#                                    (?<label>[À-ÖØ-öø-ÿ\x20-\x7B\x7D-\x7E]{1,})
#                                    (?= \\| |\\Z)
#                                    ", comments = TRUE)
## please go to https://regex101.com/ and paste this line in it to understand it



# example
# REDCapR::regex_named_captures(pattern = pattern_boxes_fr, text = choices_1)



## . thankyou_string ----
## Some thank you sentences are sometimes part of the variable label or the levels labels.
## Here we have two different patterns as regex that will be used in `clean_variable_label()`
##
## For english, use `[Pp]lease.*`
## For french,  use `[Mm]erci.* `
## For german,  use `[Dd]anke.*`
##
## Adjust if necessary the blank line just before comment = TRUE
## https://stringr.tidyverse.org/articles/regular-expressions.html


thankyou_string <- stringr::regex(
  "
  \\s?          # optional whitespace
  [[:punct:]]   # any punctuation symbol
  \\s?          # optional whitespace
  [Mm]erci.*    # word 'Merci' with upper- or lower-case for the first letter and anything following it

                                 ",
  comments = TRUE
)



## . sections: lists and labels ----


list_sections_round_2 <- c('Z', 'A', 'B', 'C')
list_sections_round_3 <- c('Z', 'A', 'B', 'C')


label_section_Z_long  <- "Problématique et interdiction de vente"
label_section_Z       <- "Préambule"
label_section_A       <-
  "Section A - Composition du produit & aspects marketing"
label_section_B       <-
  "Section B - Vente, consommation, taxation & prix"
label_section_C       <-
  "Section C - Aspects écologiques & éléments transversaux"

list_labels_sections_round_2  <-
  c(label_section_Z_long,
    label_section_A,
    label_section_B,
    label_section_C)

list_labels_sections_round_3  <-
  c(label_section_Z_long,
    label_section_A,
    label_section_B,
    label_section_C)

list_sections_overall <- list_sections_round_2
list_labels_sections_overall <- list_labels_sections_round_2


## . label txt ----
## .. colon_punctuation_txt ----
##  english ": " (no space before)
##  french " : " (one space before)
colon_punctuation_txt <- " : "

statement_txt         <- 'Enoncé'
item_txt_type_2_3     <- 'Options'

comment_txt_singular  <- 'Commentaire'
comment_txt_plural    <- 'Commentaires'

## . label short for various variables ----
# label_gender_short    <- 'Genre'
# label_job_short       <- 'Activité professionnelle'
# label_job_o_short     <- 'Activité professionnelle - préciser'
# label_joblang_short   <- 'Canton activité professionnelle'
# label_email_short     <- 'email'
# label_conflicts_short <- "Conflits d'intérêts"

label_agreement       <- 'Accord'

footnote_agreement_column_type1 <-
  paste0(
    " ",
    icon_agreement_reached,
    "  = accord atteint (médiane ≥ ",
    agreement_threshold_type1,
    ")  ; ",
    icon_disagreement_reached,
    " = désaccord atteint (médiane ≤ ",
    disagreement_threshold_type1,
    ")"
  )

footnote_agreement_column_type2_3 <-
  paste0(
    " ",
    icon_agreement_reached,
    "  = accord atteint (≥ ",
    agreement_threshold_type2_3 * 100,
    "% des réponses)"
  )


label_consensus        <- 'Consensus'
label_consensus_2lines <-
  'Con-sensus'  ## '-' allows a narrower column in word

footnote_consensus_column <-
  paste0(" Consensus atteint (IQR ≤ ", consensus_threshold_type1, ")")

label_execsummary_agree_consensus_yes     <-
  'Accord consensuel atteint'

label_execsummary_disagree_consensus_yes  <-
  'Désaccord consensuel atteint'

label_execsummary_consensus_no            <- 'Absence de consensus'

label_response <-
  'Réponse\n(1 à 9)' ## used in comments for questions type_1

label_responses_participant_single <-
  'Votre\nréponse'  ## '\n' creates a new line

label_responses_participant_plural <-
  'Vos\nréponses'   ## '\n' creates a new line


## . labels for no opinion ----
no_op_long  <-
  "Je ne peux ou ne souhaite pas exprimer mon niveau d'accord avec cet énoncé."

no_op_short <- "Pas d'avis"


## . labels for 'Other (please comment...)' ----
## double escape \\ are required because it will be used in a regex
other_please_long  <-
  "Autre \\(merci de préciser votre réponse dans le commentaire ci-dessous\\)"
other_please_short <- "Autre"

other_country <- "Hors Suisse"

## . levels_type1_fr ----
levels_type1_fr <- c(
  "Désaccord total (1)" = 1,
  "(2)"                 = 2,
  "Désaccord (3)"       = 3,
  "(4)"                 = 4,
  "Neutre (5)"          = 5,
  "(6)"                 = 6,
  "Accord (7)"          = 7,
  "(8)"                 = 8,
  "Accord total (9)"    = 9
)

levels_type1_en <- c(
  "Total disagreement (1)" = 1,
  "(2)"                    = 2,
  "Disagreement (3)"       = 3,
  "(4)"                    = 4,
  "Neutral (5)"            = 5,
  "(6)"                    = 6,
  "Agreement (7)"          = 7,
  "(8)"                    = 8,
  "Total agreement (9)"    = 9
)


levels_type1 <- levels_type1_en

# labels_job <- c(
#   "Prévention du tabagism"                 = 1,
#   "Tabacologie clinique"                   = 2,
#   "Promotion de la santé et prévention"    = 3,
#   "Médecine de famille / Médecine communautaire / Psychiatrie" =   4,
#   "Recherche fondamentale ou clinique"     = 5,
#   "Autre" = 6
# )
#
#
# labels_canton <- c(
#   'Genève'       = 1,
#   'Vaud'         = 2,
#   'Valais'       = 3,
#   'Neuchâtel'    = 4,
#   'Jura'         = 5,
#   'Fribourg'     = 6,
#   'Bern'         = 7,
#   'Autre canton' = 8,
#   'Hors Suisse'  = 9
# )
#

labels_0b_s1 <-
  'Enoncé 1 : Les Puff Bars représentent un problème pour la santé des populations.' ## used in demo


## . metadata : separator for comments in field_name ----

separator_comment_statements <- "destin" ## used with demo data
# separator_comment_statements <- " to qualify"

## . text_comment_end_variable : possible end of comment variables ----
## if other than _comment$ might need to be recoded in 01b_dft2_recode_data.R
text_comment_end_variable <-
  "_comment$|_comment_bis$"

## 3. elements for plots  -------------------------------------------
##
## . tablec_aption_type1 ----
table_caption_type1 <- "Réponses aux questions de type 1"

## . table_caption_type1_participant ----
## no closing bracket because it is add in 04a_create_flextable_results_type_1_participants.R
table_caption_type1_participant <-
  glue::glue("({color_txt} = Particip.")


##  . high_res ----
## use high values for width and height to retain ability to read after reimporting !
## these values will be divided by a multiple of 72
## during re-importation by create_type_1_exec_summary()

width_png_high_res  <- 1600
height_png_high_res <-  600

# multiple_pixels   <- 720
# png_percent_for_inclusion_in_table <- 0.10

width_exec_table   <- 1.7
height_exec_table  <- 0.65

label_n_pct        <- "n (%)"
label_median       <- 'Médiane'
label_median_short <- 'Méd.'
label_stats        <- "[IQR]\n(min-max)"
label_scale        <-
  "(1) Désaccord total ...\n... Accord total (9)"


breaks_gg <- min_scale_delphi:max_scale_delphi
limits_gg <- c(min_scale_delphi - 1,
               max_scale_delphi + 1)








## 4. elements for executive summary --------------------------------
title_overall <-
  "Cigarettes électroniques jetables : réglementer?"  # for overall executive summary
subtitle_overall <-
  "Consensus d'expert selon une approche 'Delphi-Fast track'"


## . statement_numbers_in_dft2_to_keep_for_execsummary ----
## some of the statements of round 2 have been refined in dft3, so are not kept in executive summary
## update this line after round 2 is finished

# statement_numbers_in_dft2_to_keep_for_execsummary <- ""
statement_numbers_in_dft2_to_keep_for_execsummary <- c(4, 11, 18)


## 5. define the descriptive variables by type  ----
## . create a temporary table depending on which one is available

if (exists('dft2_data_redcapr_raw') == TRUE) {
  dt0 <- dft2_data_redcapr_raw
}

if (exists('dft2_data_clean') == TRUE) {
  dt0 <- dft2_data_clean
}


## . cols_0 : descriptive variables ----
## .. START condition 1 ----
if (exists('dt0') == TRUE) {
  cols_0 <- grep("_0_", names(dt0), value = TRUE)
  
  cols_0
  class(cols_0)
  data.table(cols_0)
  
  ## . cols_0_subset_type3 : characteristics variables with value labels in type3 format ----
  ## this will be added to the lookup table
  cols_0_subset_type3 <- grep("__", cols_0, value = TRUE)
  
  if (length(cols_0_subset_type3) > 0) {
    cols_0_subset_type3_unique <-
      cols_0_subset_type3 %>%
      stringr::word(1, sep = '\\___') %>%
      unique()
  }
  
  
  ## . cols_0_subset_value_labels : initial variables with value labels ----
  cols_0_subset_value_labels <- c('dft2_0_gender',
                                  'dft2_0_job',
                                  'dft2_0_joblang')
  
  
  ## . create a data.table with short variable labels ----
  ## note the added categorical variable 'dft2_0_nb_patients_cat'
  
  dt_labels_cols_0 <-
    fread(
      "
  field_name, labels_cols_0_short
  dft2_0_gender,   Genre
  dft2_0_job,   Activité professionnelle
  dft2_0_job_o,   Activité professionnelle - préciser
  dft2_0_joblang,   Canton activité professionnelle
  dft2_0_email,   email
  dft2_0_conflicts,   Conflits d'intérêts
  dft2_0_conflicts_raw,   Conflits d'intérêts - réponse initiale
"
    )
  
  
  writexl::write_xlsx(dt_labels_cols_0,
                      path = here::here('output', 'checks', 'dt_labels_cols_0.xlsx'))
  save(dt_labels_cols_0,
       file = here::here('output', 'RData', 'dt_labels_cols_0.RData'))
  
  
  
  ## . cols_0_subset_for_table_1 ----
  ## used in '02a_dft2_prepare_tables_without_participant_id.R'
  ## define here the variable to include in the Table 1
  cols_0_subset_for_table_1 <-
    c(cols_0_subset_value_labels,
      # cols_0_subset_continuous_cat,
      cols_0_subset_type3)
  
  cols_0_subset_for_table_1
  
  ## . order_dft2_0_table_1 ----
  ## used in '02a_dft2_prepare_tables_without_participant_id.R'
  ## define here the order in which the variables will be listed
  order_dft2_0_table_1 <- c('dft2_0_gender',
                            'dft2_0_job',
                            'dft2_0_joblang')
  order_dft2_0_table_1
  
}
## .. END condition 1 ----

caption_table_1 <- "
NB: Proportions are calculated on number of participants.
* Plusieurs réponses possibles - la somme des proportions peut donc dépasser le 100%"


## 6. conflicts_keywords ----
## Need to update according to the responses - either in 01b_ft2_recode_data.R or 'output/checks/chk_dft2_0_conflicts.xlsx'
conflicts_keywords <- 'aucun|0|none|pas de|ras'
