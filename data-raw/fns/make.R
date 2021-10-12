make_gnrc_imports <- function(){
  generics_chr <- c(
    "author","authorClasses","authorData","authorFunctions",
    "characterize","enhance","ingest","investigate","manufacture",
    "metamorphose","procure","prognosticate",
    "reckon","renew","ratify","report","share")
  gnrc_imports_chr <- rep("ready4",length(generics_chr)) %>% stats::setNames(generics_chr)
  more_generics_chr <- ls("package:generics")
  more_generics_chr <- setdiff(more_generics_chr, generics_chr)
  gnrc_imports_chr <- c(gnrc_imports_chr,
                        rep("generics",length(more_generics_chr)) %>% stats::setNames(more_generics_chr))

  return(gnrc_imports_chr)
}
