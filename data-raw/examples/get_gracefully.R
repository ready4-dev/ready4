if (interactive()) {
  # Likely execution time greater than current CRAN limit.
get_gracefully(paste0("https://github.com/ready4-dev/ready4/",
                      "releases/download/Documentation_0.0/ready4_badges_lup.RDS"))
  get_gracefully("DOES NOT EXIST")
if(requireNamespace("dataverse", quietly = TRUE)) {
  get_gracefully("https://doi.org/10.7910/DVN/RIQTKK", fn = dataverse::dataset_files,
                 args_ls = list(key = NULL, server = "dataverse.harvard.edu"))
    get_gracefully("https://doi.org/10.7910/DVN/RIQTKK", fn = dataverse::dataset_files,
                   args_ls = list(key = NULL, server = "DOES_NOT_EXIST"))
    get_gracefully("DOES_NOT_EXIST", fn = dataverse::dataset_files,
                   args_ls = list(key = NULL, server = "dataverse.harvard.edu"))

}
if (requireNamespace("gh", quietly = TRUE)) {
  get_gracefully("/orgs/ready4-dev/repos", fn = gh::gh, args_ls=list(type = "public"))
      get_gracefully("DOES_NOT_EXIST", fn = gh::gh, args_ls=list(type = "public"))
}
if(requireNamespace("piggyback", quietly = TRUE)) {
  get_gracefully(NULL, fn = piggyback::pb_download_url,
                 args_ls = list(repo = "ready4-dev/ready4",
                                tag = "Documentation_0.0",
                                .token = ""))
    get_gracefully(NULL, fn = piggyback::pb_download_url,
                   args_ls = list(repo = "DOES_NOT_EXIST",
                                  tag = "DOES_NOT_EXIST",
                                  .token = ""))
}
if(requireNamespace("rvest", quietly = TRUE)) {
  get_gracefully("https://ready4-dev.github.io/ready4/index.html", fn=rvest::read_html)
    get_gracefully("DOES_NOT_EXIST", fn=rvest::read_html)
}

if(requireNamespace("tidyRSS", quietly = TRUE)) {
  get_gracefully("https://github.com/ready4-dev/ready4/releases.atom",
                 fn = tidyRSS::tidyfeed)
    get_gracefully("DOES_NOT_EXIST", fn = tidyRSS::tidyfeed)
}
}
