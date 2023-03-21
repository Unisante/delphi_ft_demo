## 03a_create_flextable_results_type_1_generic.R
## olivier.duperrex@unisante.ch
## 2022-06-24

## selected_table ----
selected_table <- glue::glue('{current_round}_{selected_type}_zz_combined')

## label_section ----
label_section <- glue::glue("label_section_{selected_section}")

## table_caption ----
## simpler in round 3
if(current_round_number == 3) {
table_caption <- glue::glue("{selected_section} - {get(label_section)}") %>%
  stringr::str_replace_all("Z - ", "")

} else {
table_caption <- glue::glue("{selected_section} - {table_caption_type1}") %>%
  stringr::str_replace_all("Z", label_section_Z)

}

# table_caption

## ft_generic ----
ft_generic <-
  get(selected_table)[section == selected_section,] %>%  
  create_flextable_histo_box_type_1_generic()


## comments ----
## create a list of variables with comments for the section to be used in a for loop
selected_comments <- get(selected_table)[section == selected_section, paste0(extract_variable_root(variable), '_comment')]
# selected_comments

