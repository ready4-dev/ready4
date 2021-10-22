#' Remove labels from data.frame
#' @description remove_lbls_from_df() is a Remove function that edits an object, removing a specified element or elements. Specifically, this function implements an algorithm to remove labels from data.frame. Function argument data_df specifies the object to be updated. The function returns Unlabelled data (a data.frame).
#' @param data_df Data (a data.frame)
#' @return Unlabelled data (a data.frame)
#' @rdname remove_lbls_from_df
#' @export
#' @importFrom lifecycle deprecate_soft
#' @importFrom purrr reduce
#' @keywords internal
remove_lbls_from_df <- function (data_df)
{
  lifecycle::deprecate_soft("0.0.0.9446", "ready4fun::remove_lbls_from_df()",
                            "remove_lbls_from_df()")
  unlabelled_data_df <- purrr::reduce(1:ncol(data_df), .init = data_df,
                                      ~{
                                        class(.x[[.y]]) <- setdiff(class(.x[[.y]]), "labelled")
                                        attr(.x[[.y]], "label") <- NULL
                                        .x
                                      })
  return(unlabelled_data_df)
}
