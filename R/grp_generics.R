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
#' Author and document datasets
#' @rdname authorData-methods
#' @description authorData() is a method that authors and saves code files for creating and documenting datasets.
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
#' Characterize the features of a ready4 framework module / sub-module
#' @rdname characterize-methods
#' @description characterize() is a method that generates descriptive information about the features of a ready4 framework module / sub-module.
#' @param x An object
#' @param ... Additional arguments
#' @export
characterize <- function (x, ...)
{
  UseMethod("characterize", x)
}
methods::setGeneric("characterize")
#' Depict (plot) features of a ready4 framework module / sub-module
#' @rdname depict-methods
#' @description depict() is a method that plots features of a ready4 framework module / sub-module.
#' @param x An object
#' @param ... Additional arguments
#' @export
depict <- function (x, ...)
{
  UseMethod("depict", x)
}
methods::setGeneric("depict")
#' Enhance a ready4 framework module / sub-module with new data items
#' @rdname enhance-methods
#' @description enhance() is a method that adds new data fields and values to extendable objects in a ready4 framework module / sub-module.
#' @param x An object
#' @param ... Additional arguments
#' @export
enhance <- function (x, ...)
{
  UseMethod("enhance", x)
}
methods::setGeneric("enhance")

#' Exhibit (print to console) features of a ready4 framework module / sub-module
#' @rdname exhibit-methods
#' @description exhibit() is a method that prints salient features of a ready4 framework module / sub-module to console.
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
#' @description ingest() is a method that ingests data saved in external files into a ready4 framework module / sub-module.
#' @param x An object
#' @param ... Additional arguments
#' @export
ingest <- function (x, ...)
{
  UseMethod("ingest", x)
}
methods::setGeneric("ingest")
#' Investigate solutions to an inverse problem
#' @rdname investigate-methods
#' @description investigate() is a method that applies an algorithm to solve an inverse problem (ie, given the data,  identify a statistical model that can generate approximations of that data) to data contained in a ready4 framework module / sub-module.
#' @param x An object
#' @param ... Additional arguments
#' @export
investigate <- function (x, ...)
{
  UseMethod("investigate", x)
}
methods::setGeneric("investigate")
#' Manufacture a (non ready4 framework) object
#' @rdname manufacture-methods
#' @description manufacture() is a method that creates a new object (other than a ready4 framework module / sub-module).
#' @param x An object
#' @param ... Additional arguments
#' @export
manufacture <- function (x, ...)
{
  UseMethod("manufacture", x)
}
methods::setGeneric("manufacture")
#' Metamorphose from one ready4 framework module / sub-module class to another
#' @rdname metamorphose-methods
#' @description metamorphose() is a method that transforms a ready4 framework module / sub-module into a different class of ready4 framework module / sub-module.
#' @param x An object
#' @param ... Additional arguments
#' @export
metamorphose <- function (x, ...)
{
  UseMethod("metamorphose", x)
}
methods::setGeneric("metamorphose")

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
#' Prognosticate (predict) solutions to a forward problem
#' @rdname prognosticate-methods
#' @description prognosticate() is a method that uses statistical or simulation models to solve a forward problem (i.e., given a model, calculate what is expected to be observed).
#' @param x An object
#' @param ... Additional arguments
#' @export
prognosticate <- function (x, ...)
{
  UseMethod("prognosticate", x)
}
methods::setGeneric("prognosticate")
#' Ratify (validate) a ready4 framework module / sub-module
#' @rdname ratify-methods
#' @description ratify() is a method that validates that a ready4 framework module / sub-module conforms to specified criteria, potentially modifying the object so that these criteria are met.
#' @param x An object
#' @param ... Additional arguments
#' @export
ratify <- function (x, ...)
{
  UseMethod("ratify", x)
}
methods::setGeneric("ratify")
#' Reckon (calculate) a value
#' @rdname reckon-methods
#' @description reckon() is a method that  performs a calculation using data contained in a ready4 framework module / sub-module.
#' @param x An object
#' @param ... Additional arguments
#' @export
reckon <- function (x, ...)
{
  UseMethod("reckon", x)
}
methods::setGeneric("reckon")
#' Renew (update) a ready4 framework module / sub-module
#' @rdname renew-methods
#' @description renew() is a method that updates a ready4 framework module / sub-module with new values.
#' @param x An object
#' @param ... Additional arguments
#' @export
renew <- function (x, ...)
{
  UseMethod("renew", x)
}
methods::setGeneric("renew")
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

#' Apply the author method to a slot
#' @rdname authorSlot-methods
#' @description authorSlot() is a convenience method that applies the author method to a specified slot of an object.
#' @param x An object
#' @param ... Additional arguments
#' @export
authorSlot <- function (x,
                        slot_nm_1L_chr,
                        ...)
{
  UseMethod("authorSlot", x)
}
methods::setGeneric("authorSlot")

#' Apply the characterize method to a slot
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

#' Apply the depict method to a slot
#' @rdname depictSlot-methods
#' @description depictSlot() is a convenience method that applies the depict method to a specified slot of an object.
#' @param x An object
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @export
depictSlot <- function (x,
                        slot_nm_1L_chr,
                        ...)
{
  UseMethod("depictSlot", x)
}
methods::setGeneric("depictSlot")

#' Apply the enhance method to a slot
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


#' Apply the exhibit method to a slot
#' @rdname exhibitSlot-methods
#' @description exhibitSlot() is a convenience method that applies the exhibit method to a specified slot of an object.
#' @param x An object
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @export
exhibitSlot <- function (x,
                         slot_nm_1L_chr,
                         ...)
{
  UseMethod("exhibitSlot", x)
}
methods::setGeneric("exhibitSlot")

#' Apply the ingest method to a slot
#' @rdname ingestSlot-methods
#' @description ingestSlot() is a convenience method that applies the ingest method to a specified slot of an object.
#' @param x An object
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @export
ingestSlot <- function (x,
                        slot_nm_1L_chr,
                        ...)
{
  UseMethod("ingestSlot", x)
}
methods::setGeneric("ingestSlot")

#' Apply the investigate method to a slot
#' @rdname investigateSlot-methods
#' @description investigateSlot() is a convenience method that applies the investigate method to a specified slot of an object.
#' @param x An object
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @export
investigateSlot <- function (x,
                             slot_nm_1L_chr,
                             ...)
{
  UseMethod("investigateSlot", x)
}
methods::setGeneric("investigateSlot")

#' Apply the manufacture method to a slot
#' @rdname manufactureSlot-methods
#' @description manufactureSlot() is a convenience method that applies the manufacture method to a specified slot of an object.
#' @param x An object
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @export
manufactureSlot <- function (x,
                             slot_nm_1L_chr,
                             ...)
{
  UseMethod("manufactureSlot", x)
}
methods::setGeneric("manufactureSlot")
#' Apply the metamorphose method to a slot
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
methods::setGeneric("procure")
#' Procure (get) data from a slot
#' @rdname procureSlot-methods
#' @description procureSlot() is a "getter" method that procures (gets) the value of a specified slot (default behaviour) or the value returned by applying the procure method to the slot.
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
#' Apply the prognosticate method to a slot
#' @rdname prognosticateSlot-methods
#' @description prognosticateSlot() is a convenience method that applies the prognosticate method to a specified slot of an object.
#' @param x An object
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @export
prognosticateSlot <- function (x,
                               slot_nm_1L_chr,
                               ...)
{
  UseMethod("prognosticateSlot", x)
}
methods::setGeneric("prognosticateSlot")
#' Apply the ratify method to a slot
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
#' Apply the reckon method to a slot
#' @rdname reckonSlot-methods
#' @description reckonSlot() is a convenience method that applies the reckon method to a specified slot of an object.
#' @param x An object
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @export
reckonSlot <- function (x,
                        slot_nm_1L_chr,
                        ...)
{
  UseMethod("reckonSlot", x)
}
methods::setGeneric("reckonSlot")
#' Apply the renew method to a slot
#' @rdname renewSlot-methods
#' @description renewSlot() is a "setter" method that renews (sets) the value of a specified slot with either a supplied new value or the value returned by applying the renew method to the slot (default behaviour).
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
#' Apply the share method to a slot
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
