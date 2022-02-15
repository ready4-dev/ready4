meta <- packageDescription("ready4")
citation_chr <- readLines("inst/CITATION")
publisher_1L_chr <- "Zenodo"

write_citation_cff(packageDescription("ready4"),
                   citation_chr = readLines("inst/CITATION"))
