#' Author and save files
#' @rdname author-methods
#' @description author() is a method that authors and saves files.
#' @param x An object
#' @param ... Additional arguments
#' @export
author <- function (x, ...)
{
  UseMethod("author", x)
}
methods::setGeneric("author")
#' Author and document classes
#' @rdname authorClasses-methods
#' @description authorClasses() is a method that authors and saves code files for creating and documenting classes in an R package directory.
#' @param x An object
#' @param ... Additional arguments
#' @export
authorClasses <- function (x, ...)
{
  UseMethod("authorClasses", x)
}
methods::setGeneric("authorClasses")
#' Author and document package datasets
#' @rdname authorData-methods
#' @description authorData() is a method that authors and saves code files for creating and documenting datasets in an R package directory.
#' @param x An object
#' @param ... Additional arguments
#' @export
authorData <- function (x, ...)
{
  UseMethod("authorData", x)
}
methods::setGeneric("authorData")
#' Author and document functions
#' @rdname authorFunctions-methods
#' @description authorFunctions() is a method that authors and saves files necessary for creating and documenting functions in an R package directory.
#' @param x An object
#' @param ... Additional arguments
#' @export
authorFunctions <- function (x, ...)
{
  UseMethod("authorFunctions", x)
}
methods::setGeneric("authorFunctions")
#' Author and save a report
#' @rdname authorReport-methods
#' @description authorReport() is a method that authors and saves a report.
#' @param x An object
#' @param ... Additional arguments
#' @export
authorReport <- function (x, ...)
{
  UseMethod("authorReport", x)
}
methods::setGeneric("authorReport")
#' Characterize the features of an object
#' @rdname characterize-methods
#' @description characterize() is a method that generates descriptive information about the features of an object.
#' @param x An object
#' @param ... Additional arguments
#' @export
characterize <- function (x, ...)
{
  UseMethod("characterize", x)
}
methods::setGeneric("characterize")
#' Characterize one element of an object
#' @rdname characterizeSlot-methods
#' @description characterizeSlot() is a method that generates descriptive information about data stored in a slot of an S4 object.
#' @param x An object
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @export
characterizeSlot <- function (x,
                              slot_nm_1L_chr,
                              ...)
{
  UseMethod("characterizeSlot", x)
}
methods::setGeneric("characterizeSlot")
#' Depict (plot) an object
#' @rdname depict-methods
#' @description depict() is a method that plots an object.
#' @param x An object
#' @param ... Additional arguments
#' @export
depict <- function (x, ...)
{
  UseMethod("depict", x)
}
methods::setGeneric("depict")
#' Enhance an object with new data items
#' @rdname enhance-methods
#' @description enhance() is a method that adds new data fields and values to objects contained in that instance.
#' @param x An object
#' @param ... Additional arguments
#' @export
enhance <- function (x, ...)
{
  UseMethod("enhance", x)
}
methods::setGeneric("enhance")
#' Enhance an element of an object with new data items
#' @rdname enhanceSlot-methods
#' @description enhanceSlot() is a method that adds new data fields and values to an object stored in a slot of that instance.
#' @param x An object
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @export
enhanceSlot <- function (x,
                         slot_nm_1L_chr,
                         ...)
{
  UseMethod("enhanceSlot", x)
}
methods::setGeneric("enhanceSlot")
#' Exhibit (print to console) an object
#' @rdname exhibit-methods
#' @description exhibit() is a method that prints salient features of an object to console.
#' @param x An object
#' @param ... Additional arguments
#' @export
exhibit <- function (x, ...)
{
  UseMethod("exhibit", x)
}
methods::setGeneric("exhibit")
#' Ingest data
#' @rdname ingest-methods
#' @description ingest() is a method that ingests data saved in external files into R objects stored in working memory.
#' @param x An object
#' @param ... Additional arguments
#' @export
ingest <- function (x, ...)
{
  UseMethod("ingest", x)
}
methods::setGeneric("ingest")
#' Investigate a dataset
#' @rdname investigate-methods
#' @description investigate() is a method that applies an algorithm to solve an inverse problem to data contained in an object.
#' @param x An object
#' @param ... Additional arguments
#' @export
investigate <- function (x, ...)
{
  UseMethod("investigate", x)
}
methods::setGeneric("investigate")
#' Manufacture a new object
#' @rdname manufacture-methods
#' @description manufacture() is a method that creates a new object (other than a ready4 class instance).
#' @param x An object
#' @param ... Additional arguments
#' @export
manufacture <- function (x, ...)
{
  UseMethod("manufacture", x)
}
methods::setGeneric("manufacture")
#' Metamorphose an object
#' @rdname metamorphose-methods
#' @description metamorphose() is a method that transforms one ready4 object into a ready4 object of a different class.
#' @param x An object
#' @param ... Additional arguments
#' @export
metamorphose <- function (x, ...)
{
  UseMethod("metamorphose", x)
}
methods::setGeneric("metamorphose")
#' Metamorphose an element of an object
#' @rdname metamorphoseSlot-methods
#' @description metamorphoseSlot() is a method that transforms a ready4 object stored in a slot of an object into a ready4 object of a different class.
#' @param x An object
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @export
metamorphoseSlot <- function (x,
                              slot_nm_1L_chr,
                              ...)
{
  UseMethod("metamorphoseSlot", x)
}
methods::setGeneric("metamorphoseSlot")
#' Procure (get) data
#' @rdname procure-methods
#' @description procure() is a method that retrieves data contained within an object.
#' @param x An object
#' @param ... Additional arguments
#' @export
procure <- function (x, ...)
{
  UseMethod("procure", x)
}
methods::setGeneric("procure")
#' Procure (get) data from an element of an object
#' @rdname procureSlot-methods
#' @description procureSlot() is a "getter" method that retrieves data contained in a slot of an S4 object.
#' @param x An object
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @export
procureSlot <- function (x,
                         slot_nm_1L_chr,
                         ...)
{
  UseMethod("procureSlot", x)
}
methods::setGeneric("procureSlot")
#' Prognosticate (predict)
#' @rdname prognosticate-methods
#' @description prognosticate() is a method that uses statistical or simulation models to solve a forward problem.
#' @param x An object
#' @param ... Additional arguments
#' @export
prognosticate <- function (x, ...)
{
  UseMethod("prognosticate", x)
}
methods::setGeneric("prognosticate")
#' Ratify (validate) an object
#' @rdname ratify-methods
#' @description ratify() is a method that validates that an object conforms to specified criteria, potentially modifying the object so that these criteria are met.
#' @param x An object
#' @param ... Additional arguments
#' @export
ratify <- function (x, ...)
{
  UseMethod("ratify", x)
}
methods::setGeneric("ratify")
#' Ratify an element of an object
#' @rdname ratifySlot-methods
#' @description ratifySlot() is a method that validates that data in a slot of an S4 object conforms to specified criteria, potentially modifying that data so that these criteria are met.
#' @param x An object
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @export
ratifySlot <- function (x,
                        slot_nm_1L_chr,
                        ...)
{
  UseMethod("ratifySlot", x)
}
methods::setGeneric("ratifySlot")
#' Reckon (calculate) a value
#' @rdname reckon-methods
#' @description reckon() is a method that  performs a calculation using data contained in an object.
#' @param x An object
#' @param ... Additional arguments
#' @export
reckon <- function (x, ...)
{
  UseMethod("reckon", x)
}
methods::setGeneric("reckon")
#' Renew (update) an object
#' @rdname renew-methods
#' @description renew() is a method that updates an object with new data.
#' @param x An object
#' @param ... Additional arguments
#' @export
renew <- function (x, ...)
{
  UseMethod("renew", x)
}
methods::setGeneric("renew")
#' Renew (update) an element of an object
#' @rdname renewSlot-methods
#' @description renewSlot() is a "setter" method that updates a slot of a S4 object with new data.
#' @param x An object
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param new_val_xx New value (an object)
#' @param ... Additional arguments
#' @export
renewSlot <- function (x,
                       slot_nm_1L_chr,
                       new_val_xx,
                       ...)
{
  UseMethod("renewSlot", x)
}
methods::setGeneric("renewSlot")
#' Share (publish) open data
#' @rdname share-methods
#' @description share() is a method that uploads data to an online repository. If requested, the method will also publish the updated repository.
#' @param x An object
#' @param ... Additional arguments
#' @export
share <- function (x, ...)
{
  UseMethod("share", x)
}
methods::setGeneric("share")
#' Share (publish) an element of an object
#' @rdname shareSlot-methods
#' @description shareSlot() is a method that uploads data contained in a slot of an S4 object to an online repository. If requested, the method will also publish the updated repository.
#' @param x An object
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @export
shareSlot <- function (x,
                       slot_nm_1L_chr,
                       ...)
{
  UseMethod("shareSlot", x)
}
methods::setGeneric("shareSlot")
