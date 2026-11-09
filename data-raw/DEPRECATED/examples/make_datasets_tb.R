if (interactive()) {
  # Likely to take more than one minute to execute.
  make_datasets_tb("ready4")
  dvs_tb <- get_datasets_tb("ready4-dev/ready4")
  make_datasets_tb("ready4", dvs_tb = dvs_tb)
  make_datasets_tb("ready4", dvs_tb = dvs_tb, what_1L_chr = "real")
  make_datasets_tb("ready4", dvs_tb = dvs_tb, what_1L_chr = "fakes")
  make_datasets_tb("ready4", dvs_tb = dvs_tb, type_1L_chr = "datasets")
  make_datasets_tb("ready4", dvs_tb = dvs_tb, type_1L_chr = "datasets", what_1L_chr = "real")
  make_datasets_tb("ready4", dvs_tb = dvs_tb, type_1L_chr = "datasets", what_1L_chr = "fakes")
  }
