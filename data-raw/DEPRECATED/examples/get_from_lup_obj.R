lookup_tb <- tibble::tibble(Name = c("Sajid","Siobhan"),
                            Treat = c("Cake", "Chocolate"))
get_from_lup_obj(lookup_tb, match_value_xx = "Siobhan",
                 match_var_nm_1L_chr = "Name", target_var_nm_1L_chr = "Treat")
get_from_lup_obj(lookup_tb, match_value_xx = "Cake",
                 match_var_nm_1L_chr = "Treat", target_var_nm_1L_chr = "Name")

