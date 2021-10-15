#' Make list phrase
#' @description make_list_phrase() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make list phrase. The function returns List phrase (a character vector of length one).
#' @param items_chr Items (a character vector)
#' @return List phrase (a character vector of length one)
#' @rdname make_list_phrase
#' @export 
#' @importFrom stringr str_c
#' @importFrom stringi stri_replace_last
#' @keywords internal
make_list_phrase <- function (items_chr) 
{
    list_phrase_1L_chr <- items_chr %>% stringr::str_c(sep = "", 
        collapse = ", ") %>% stringi::stri_replace_last(fixed = ",", 
        replacement = " and")
    return(list_phrase_1L_chr)
}
#' Make prompt
#' @description make_prompt() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make prompt. The function returns Response (a character vector of length one).
#' @param prompt_1L_chr Prompt (a character vector of length one)
#' @param options_chr Options (a character vector), Default: NULL
#' @param force_from_opts_1L_chr Force from opts (a character vector of length one), Default: F
#' @return Response (a character vector of length one)
#' @rdname make_prompt
#' @export 
#' @keywords internal
make_prompt <- function (prompt_1L_chr, options_chr = NULL, force_from_opts_1L_chr = F) 
{
    acknowledgement_1L_chr <- "This function is based on: https://debruine.github.io/posts/interactive-test/"
    con_conn <- getOption("prompt_opts.con", stdin())
    options_1L_chr <- paste(options_chr, collapse = "|")
    prompt_with_options_1L_chr <- paste0(prompt_1L_chr, " [", 
        options_1L_chr, "]\n")
    cat(prompt_with_options_1L_chr)
    response_1L_chr <- readLines(con = con_conn, n = 1)
    if (!is.null(options_chr) & !response_1L_chr %in% options_chr & 
        force_from_opts_1L_chr) {
        response_1L_chr <- make_prompt(prompt_1L_chr, options_chr, 
            force_from_opts_1L_chr = T)
    }
    return(response_1L_chr)
}
