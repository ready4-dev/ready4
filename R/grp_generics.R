#' Author and save files
#' @rdname author-methods
#' @description author() is a method that authors and saves files.
#' @param x A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance)
#' @param ... Additional arguments
#' @return Either a model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance) of the same class as that supplied to the method or no return value (when called for side-effects only).
#' @export
author <- function (x, ...)
{
  UseMethod("author", x)
}
methods::setGeneric("author")
#' Author and document classes
#' @rdname authorClasses-methods
#' @description authorClasses() is a method that authors and saves R package files for creating and documenting classes to describe the data structures of model modules.
#' @param x A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance)
#' @param ... Additional arguments
#' @return Either a model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance) of the same class as that supplied to the method or no return value (when called for side-effects only).
#' @export
authorClasses <- function (x, ...)
{
  UseMethod("authorClasses", x)
}
methods::setGeneric("authorClasses")
#' Author and document datasets
#' @rdname authorData-methods
#' @description authorData() is a method that authors, documents and saves model module datasets.
#' @param x A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance)
#' @param ... Additional arguments
#' @return Either a model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance) of the same class as that supplied to the method or no return value (when called for side-effects only).
#' @export
authorData <- function (x, ...)
{
  UseMethod("authorData", x)
}
methods::setGeneric("authorData")
#' Author and document functions
#' @rdname authorFunctions-methods
#' @description authorFunctions() is a method that authors and saves R package files files necessary for creating and documenting functions that implement model module algorithms.
#' @param x A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance)
#' @param ... Additional arguments
#' @return Either a model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance) of the same class as that supplied to the method or no return value (when called for side-effects only).
#' @export
authorFunctions <- function (x, ...)
{
  UseMethod("authorFunctions", x)
}
methods::setGeneric("authorFunctions")
#' Author and save a report
#' @rdname authorReport-methods
#' @description authorReport() is a method that authors and saves a report.
#' @param x A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance)
#' @param ... Additional arguments
#' @return Either a model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance) of the same class as that supplied to the method or no return value (when called for side-effects only).
#' @export
authorReport <- function (x, ...)
{
  UseMethod("authorReport", x)
}
methods::setGeneric("authorReport")
#' Characterize model module data by generating (tabular) descriptive statistics
#' @rdname characterize-methods
#' @description characterize() is a method that generates descriptive tabular summaries about data contained in a model module.
#' @param x A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance)
#' @param ... Additional arguments
#' @return A data.frame, tibble or other table based class.
#' @export
characterize <- function (x, ...)
{
  UseMethod("characterize", x)
}
methods::setGeneric("characterize")
#' Depict (plot) features of model module data
#' @rdname depict-methods
#' @description depict() is a method that plots features of data contained in a model module (or sub-module).
#' @param x A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance)
#' @param ... Additional arguments
#' @return A ggplot, gg or other plot type class.
#' @export
depict <- function (x, ...)
{
  UseMethod("depict", x)
}
methods::setGeneric("depict")
#' Enhance a model module by adding new elements
#' @rdname enhance-methods
#' @description enhance() is a method that adds new data fields (columns for tabular data, elements for arrays) and values to a model module by transforming it into a module of an inheriting class.
#' @param x A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance)
#' @param ... Additional arguments
#' @return A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance) of the same class as that supplied to the method.
#' @export
enhance <- function (x, ...)
{
  UseMethod("enhance", x)
}
methods::setGeneric("enhance")

#' Exhibit features of model module data by printing them to the R console
#' @rdname exhibit-methods
#' @description exhibit() is a method that prints to console selected features of data contained in a model module.
#' @param x A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance)
#' @param ... Additional arguments
#' @return A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance) of the same class as that supplied to the method or no return value (when called for side-effects only).
#' @export
exhibit <- function (x, ...)
{
  UseMethod("exhibit", x)
}
methods::setGeneric("exhibit")
#' Ingest data
#' @rdname ingest-methods
#' @description ingest() is a method that ingests data saved in external files into a model module or submodule.
#' @param x A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance)
#' @param ... Additional arguments
#' @return A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance).
#' @export
ingest <- function (x, ...)
{
  UseMethod("ingest", x)
}
methods::setGeneric("ingest")
#' Investigate solutions to an inverse problem
#' @rdname investigate-methods
#' @description investigate() is a method that applies an algorithm to data contained in a model module in order to solve an inverse problem (ie, identify a statistical model that can generate approximations of that data).
#' @param x A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance)
#' @param ... Additional arguments
#' @return A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance).
#' @export
investigate <- function (x, ...)
{
  UseMethod("investigate", x)
}
methods::setGeneric("investigate")
#' Manufacture a new object
#' @rdname manufacture-methods
#' @description manufacture() is a method that used data contained in a model module or submodule to create a new object (other than a model module).
#' @param x A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance)
#' @param ... Additional arguments
#' @return An object other than a model module (an instance of a class that inherits from Ready4Module).
#' @export
manufacture <- function (x, ...)
{
  UseMethod("manufacture", x)
}
methods::setGeneric("manufacture")
#' Metamorphose a model module to a model module of a different (non-inheriting) class
#' @rdname metamorphose-methods
#' @description metamorphose() is a method that transforms a model module into a model module of a different (non-inheriting) class.
#' @param x A model module (an instance of a class that inherits from Ready4Module)
#' @param ... Additional arguments
#' @return A model module (an instance of a class that inherits from Ready4Module) of a different class to that supplied to the method.
#' @export
metamorphose <- function (x, ...)
{
  UseMethod("metamorphose", x)
}
methods::setGeneric("metamorphose")

#' Procure items from a dataset
#' @rdname procure-methods
#' @description procure() is a "getter"  method that retrieves data contained within a model module or sub-module.
#' @param x A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance)
#' @param ... Additional arguments
#' @return An object of the same class as that supplied to the method or of one of the same classes that constitute the input object's slots or elements.
#' @export
procure <- function (x, ...)
{
  UseMethod("procure", x)
}
#' Prognosticate (make predictions) by solving a forward problem
#' @rdname prognosticate-methods
#' @description prognosticate() is a method that applies an algorithm to data contained in a model module to solve a forward problem (i.e., use simulation and statistical methods to make predictions).
#' @param x A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance)
#' @param ... Additional arguments
#' @return A model module (an instance of a class that inherits from Ready4Module).
#' @export
prognosticate <- function (x, ...)
{
  UseMethod("prognosticate", x)
}
methods::setGeneric("prognosticate")
#' Ratify that input or output data meet validity criteria
#' @rdname ratify-methods
#' @description ratify() is a method that validates that a model module or submodule conforms to specified internal consistency criteria, potentially updating the invalid values in the model module so that these criteria are met.
#' @param x A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance)
#' @param ... Additional arguments
#' @return A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance) of the same class as that supplied to the method.
#' @export
ratify <- function (x, ...)
{
  UseMethod("ratify", x)
}
methods::setGeneric("ratify")
#' Reckon (calculate) a value
#' @rdname reckon-methods
#' @description reckon() is a method that performs a calculation using data contained in a model module (or sub-module).
#' @param x A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance)
#' @param ... Additional arguments
#' @return A numeric class.
#' @export
reckon <- function (x, ...)
{
  UseMethod("reckon", x)
}
methods::setGeneric("reckon")
#' Renew (update) values
#' @rdname renew-methods
#' @description renew() is a "setter" method that updates values of selected data contained in a model module or sub-module.
#' @param x A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance)
#' @param ... Additional arguments
#' @return A model module (an instance of a class that inherits from Ready4Module)  or submodule (any S3 class instance) of the same class as that supplied to the method.
#' @export
renew <- function (x, ...)
{
  UseMethod("renew", x)
}
methods::setGeneric("renew")
#' Share data via an online repository
#' @rdname share-methods
#' @description share() is a method that uploads data contained in a model module to an online repository. If requested, the method will also publish the updated repository.
#' @param x A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance)
#' @param ... Additional arguments
#' @return A model module (an instance of a class that inherits from Ready4Module) or submodule (any S3 class instance) of the same class as that supplied to the method or no return value (when called for side-effects only).
#' @export
share <- function (x, ...)
{
  UseMethod("share", x)
}
methods::setGeneric("share")

#' Apply the author method to a model module slot
#' @rdname authorSlot-methods
#' @description authorSlot() is a convenience method that applies the author method to a specified slot of a model module.
#' @param x A model module (an instance of a class that inherits from Ready4Module)
#' @param ... Additional arguments
#' @return Either a model module (an instance of a class that inherits from Ready4Module) of the same class as that supplied to the method or no value (when called for side effects only).
#' @export
authorSlot <- function (x,
                        slot_nm_1L_chr,
                        ...)
{
  UseMethod("authorSlot", x)
}
methods::setGeneric("authorSlot")

#' Apply the characterize method to a model module slot
#' @rdname characterizeSlot-methods
#' @description characterizeSlot() is a convenience method that applies the characterize method to a specified slot of a model module.
#' @param x A model module (an instance of a class that inherits from Ready4Module)
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return Either a model module (an instance of a class that inherits from Ready4Module) of the same class as that supplied to the method or a data.frame, tibble or other table class.
#' @export
characterizeSlot <- function (x,
                              slot_nm_1L_chr,
                              ...)
{
  UseMethod("characterizeSlot", x)
}
methods::setGeneric("characterizeSlot")

#' Apply the depict method to a model module slot
#' @rdname depictSlot-methods
#' @description depictSlot() is a convenience method that applies the depict method to a specified slot of a model module.
#' @param x A model module (an instance of a class that inherits from Ready4Module)
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return Either a model module (an instance of a class that inherits from Ready4Module) of the same class as that supplied to the method or no value (when called for side effects only).
#' @export
depictSlot <- function (x,
                        slot_nm_1L_chr,
                        ...)
{
  UseMethod("depictSlot", x)
}
methods::setGeneric("depictSlot")

#' Apply the enhance method to a model module slot
#' @rdname enhanceSlot-methods
#' @description enhanceSlot() is a convenience method that applies the enhance method to a specified slot a model module.
#' @param x A model module (an instance of a class that inherits from Ready4Module)
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return A model module (an instance of a class that inherits from Ready4Module) of the same class as that supplied to the method.
#' @export
enhanceSlot <- function (x,
                         slot_nm_1L_chr,
                         ...)
{
  UseMethod("enhanceSlot", x)
}
methods::setGeneric("enhanceSlot")


#' Apply the exhibit method to a model module slot
#' @rdname exhibitSlot-methods
#' @description exhibitSlot() is a convenience method that applies the exhibit method to a specified slot a model module.
#' @param x A model module (an instance of a class that inherits from Ready4Module)
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return Either a model module (an instance of a class that inherits from Ready4Module) of the same class as that supplied to the method or no return value (when called purely for side effects).
#' @export
exhibitSlot <- function (x,
                         slot_nm_1L_chr,
                         ...)
{
  UseMethod("exhibitSlot", x)
}
methods::setGeneric("exhibitSlot")

#' Apply the ingest method to a model module slot
#' @rdname ingestSlot-methods
#' @description ingestSlot() is a convenience method that applies the ingest method to a specified slot of a model module.
#' @param x A model module (an instance of a class that inherits from Ready4Module)
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return A model module (an instance of a class that inherits from Ready4Module) of the same class as that supplied to the method.
#' @export
ingestSlot <- function (x,
                        slot_nm_1L_chr,
                        ...)
{
  UseMethod("ingestSlot", x)
}
methods::setGeneric("ingestSlot")

#' Apply the investigate method to a model module slot
#' @rdname investigateSlot-methods
#' @description investigateSlot() is a convenience method that applies the investigate method to a specified slot of a model module.
#' @param x A model module (an instance of a class that inherits from Ready4Module)
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return A model module (an instance of a class that inherits from Ready4Module).
#' @export
investigateSlot <- function (x,
                             slot_nm_1L_chr,
                             ...)
{
  UseMethod("investigateSlot", x)
}
methods::setGeneric("investigateSlot")

#' Apply the manufacture method to a model module slot
#' @rdname manufactureSlot-methods
#' @description manufactureSlot() is a convenience method that applies the manufacture method to a specified slot of a model module.
#' @param x A model module (an instance of a class that inherits from Ready4Module)
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return An object that is not the the same class as that supplied to the method.
#' @export
manufactureSlot <- function (x,
                             slot_nm_1L_chr,
                             ...)
{
  UseMethod("manufactureSlot", x)
}
methods::setGeneric("manufactureSlot")
#' Apply the metamorphose method to a model module slot
#' @rdname metamorphoseSlot-methods
#' @description metamorphoseSlot() is a convenience method that applies the metamorphose method to a specified slot of a model module.
#' @param x A model module (an instance of a class that inherits from Ready4Module)
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return A model module (an instance of a class that inherits from Ready4Module).
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
#' @description procureSlot() is a "getter" method that, depending on input arguments, retrieves either data contained in a specified model module slot (the default behaviour) or the value returned by applying the procure method to the specified slot.
#' @param x A model module (an instance of a class that inherits from Ready4Module)
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return A model module (an instance of a class that inherits from Ready4Module) of the same class as that supplied to the method or an instance of a class contained in that Ready4Module's slots.
#' @export
procureSlot <- function (x,
                         slot_nm_1L_chr,
                         ...)
{
  UseMethod("procureSlot", x)
}
methods::setGeneric("procureSlot")
#' Apply the prognosticate method to a model module slot
#' @rdname prognosticateSlot-methods
#' @description prognosticateSlot() is a convenience method that applies the prognosticate method to a specified slot of a model module.
#' @param x A model module (an instance of a class that inherits from Ready4Module)
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return A model module (an instance of a class that inherits from Ready4Module).
#' @export
prognosticateSlot <- function (x,
                               slot_nm_1L_chr,
                               ...)
{
  UseMethod("prognosticateSlot", x)
}
methods::setGeneric("prognosticateSlot")
#' Apply the ratify method to a model module slot
#' @rdname ratifySlot-methods
#' @description ratifySlot() is a convenience method that applies the ratify method to a specified slot of a model module.
#' @param x A model module (an instance of a class that inherits from Ready4Module)
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return A model module (an instance of a class that inherits from Ready4Module) of the same class as that supplied to the method.
#' @export
ratifySlot <- function (x,
                        slot_nm_1L_chr,
                        ...)
{
  UseMethod("ratifySlot", x)
}
methods::setGeneric("ratifySlot")
#' Apply the reckon method to a model module slot
#' @rdname reckonSlot-methods
#' @description reckonSlot() is a convenience method that applies the reckon method to a specified slot of a model module.
#' @param x A model module (an instance of a class that inherits from Ready4Module)
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return A numeric class.
#' @export
reckonSlot <- function (x,
                        slot_nm_1L_chr,
                        ...)
{
  UseMethod("reckonSlot", x)
}
methods::setGeneric("reckonSlot")
#' Apply the renew method to a model module slot
#' @rdname renewSlot-methods
#' @description renewSlot() is a "setter" method that renews (sets) the value of a specified model module slot with either the value returned by applying the renew method to that slot (the default behaviour) or a supplied new value.
#' @param x A model module (an instance of a class that inherits from Ready4Module)
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param new_val_xx New value (slot dependent object type), Default 'use_renew_mthd'
#' @param ... Additional arguments
#' @return A model module (an instance of a class that inherits from Ready4Module) of the same class as that supplied to the method.
#' @export
renewSlot <- function (x,
                       slot_nm_1L_chr,
                       new_val_xx = "use_renew_mthd",
                       ...)
{
  UseMethod("renewSlot", x)
}
methods::setGeneric("renewSlot")
#' Apply the share method to a model module slot
#' @rdname shareSlot-methods
#' @description shareSlot() is a convenience method that applies the share method to a specified slot of a model module.
#' @param x A model module (an instance of a class that inherits from Ready4Module)
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return Either a model module (an instance of a class that inherits from Ready4Module) of the same class as that supplied to the method or no value (when called purely for side effects).
#' @export
shareSlot <- function (x,
                       slot_nm_1L_chr,
                       ...)
{
  UseMethod("shareSlot", x)
}
methods::setGeneric("shareSlot")
