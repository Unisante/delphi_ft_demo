## 03b_create_flextable_results_type_2_3_generic.R
## olivier.duperrex@unisante.ch
## 2022-08-11

## selected_table ----
selected_table <- glue::glue('{current_round}_{selected_type}_zz1')

## label_selected_variable ----
label_selected_variable <- lookup_final[variable == selected_variable, variable_label]


## table_caption ----
table_caption <- glue::glue("{selected_section} - {label_selected_variable}") %>%
  stringr::str_replace_all("Z", label_section_Z)

## ft_generic ----
ft_generic <-
  get(selected_table)[variable %like% selected_variable, ] %>%
  create_flextable_minibar_type_2_3_generic()  %>%
  set_caption(table_caption)


# ft_generic

## comments ----
selected_variable_comment <- paste0(extract_variable_root(selected_variable), '_comment')

selected_comments <- dt_comments_m[variable == selected_variable_comment, item]

label_comment <- lookup_final[variable == selected_variable_comment, variable_label]

ft_comments <- dt_comments_m[variable == selected_variable_comment] %>%
  create_flextable_comments_participant()
