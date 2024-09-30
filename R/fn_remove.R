#' Remove labels from dataframe
#' @description remove_lbls_from_df() is a Remove function that edits an object, removing a specified element or elements. Specifically, this function implements an algorithm to remove labels from dataframe. The function returns Unlabelled data (a data.frame).
#' @param data_df Data (a data.frame)
#' @return Unlabelled data (a data.frame)
#' @rdname remove_lbls_from_df
#' @export 
#' @importFrom purrr reduce
#' @keywords internal
remove_lbls_from_df <- function (data_df) 
{
    acknowledgement_1L_chr <- "This function is based on: https://rdrr.io/github/dlindholm/doctoR/src/R/clear_labels.R"
    unlabelled_data_df <- purrr::reduce(1:ncol(data_df), .init = data_df, 
        ~{
            class(.x[[.y]]) <- setdiff(class(.x[[.y]]), "labelled")
            attr(.x[[.y]], "label") <- NULL
            .x
        })
    return(unlabelled_data_df)
}
