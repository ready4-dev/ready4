#' Write files to local or remote locations.
#' @rdname author-methods
#' @description author() is a method that writes files to local or remote locations.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
author <- function (x, ...)
{
  UseMethod("author", x)
}
methods::setGeneric("author")
#' Author and save files necessary for creating and documenting classes.
#' @rdname authorClasses-methods
#' @description authorClasses() is a method that authors and saves files necessary for creating and documenting classes.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
authorClasses <- function (x, ...)
{
  UseMethod("authorClasses", x)
}
methods::setGeneric("authorClasses")
#' Author and save files necessary for creating and documenting datasets.
#' @rdname authorData-methods
#' @description authorData() is a method that authors and saves files necessary for creating and documenting datasets.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
authorData <- function (x, ...)
{
  UseMethod("authorData", x)
}
methods::setGeneric("authorData")
#' Author and save files necessary for creating and documenting functions.
#' @rdname authorFunctions-methods
#' @description authorFunctions() is a method that authors and saves files necessary for creating and documenting functions.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
authorFunctions <- function (x, ...)
{
  UseMethod("authorFunctions", x)
}
methods::setGeneric("authorFunctions")
#' Generate descriptive information about a dataset.
#' @rdname characterize-methods
#' @description characterize() is a method that generates descriptive information about a dataset.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
characterize <- function (x, ...)
{
  UseMethod("characterize", x)
}
methods::setGeneric("characterize")
#' Enhance an instance of a class by adding one or more elements.
#' @rdname enhance-methods
#' @description enhance() is a method that enhances an instance of a class by adding one or more elements.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
enhance <- function (x, ...)
{
  UseMethod("enhance", x)
}
methods::setGeneric("enhance")
#' Import objects in external file formats into R objects stored in working memory.
#' @rdname ingest-methods
#' @description ingest() is a method that imports objects in external file formats into R objects stored in working memory.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
ingest <- function (x, ...)
{
  UseMethod("ingest", x)
}
methods::setGeneric("ingest")
#' Execute an algorithm so solve an inverse problem, often through estimating statistical models.
#' @rdname investigate-methods
#' @description investigate() is a method that executes an algorithm so solve an inverse problem, often through estimating statistical models.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
investigate <- function (x, ...)
{
  UseMethod("investigate", x)
}
methods::setGeneric("investigate")
#' Create a novel R object.
#' @rdname manufacture-methods
#' @description manufacture() is a method that creates a novel R object.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
manufacture <- function (x, ...)
{
  UseMethod("manufacture", x)
}
methods::setGeneric("manufacture")
#' Transform an instance of a class into an object with different structural properties.
#' @rdname metamorphose-methods
#' @description metamorphose() is a method that transforms an instance of a class into an object with different structural properties.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
metamorphose <- function (x, ...)
{
  UseMethod("metamorphose", x)
}
methods::setGeneric("metamorphose")
#' Search and retrieve requested data from a specified source.
#' @rdname procure-methods
#' @description procure() is a method that searches and retrieves requested data from a specified source.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
procure <- function (x, ...)
{
  UseMethod("procure", x)
}
methods::setGeneric("procure")
#' Execute an algorithm for solving forward problems through simulation or prediction.
#' @rdname prognosticate-methods
#' @description prognosticate() is a method that executes an algorithm for solving forward problems through simulation or prediction.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
prognosticate <- function (x, ...)
{
  UseMethod("prognosticate", x)
}
methods::setGeneric("prognosticate")
#' Check whether an instance of a class conforms to required criteria, potentially modifying that instance to ensure that it is valid.
#' @rdname ratify-methods
#' @description ratify() is a method that checks whether an instance of a class conforms to required criteria, potentially modifying that instance to ensure that it is valid.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
ratify <- function (x, ...)
{
  UseMethod("ratify", x)
}
methods::setGeneric("ratify")
#' Perform a calculation.
#' @rdname reckon-methods
#' @description reckon() is a method that performs a calculation.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
reckon <- function (x, ...)
{
  UseMethod("reckon", x)
}
methods::setGeneric("reckon")
#' Update an instance of a class with new values.
#' @rdname renew-methods
#' @description renew() is a method that updates an instance of a class with new values.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
renew <- function (x, ...)
{
  UseMethod("renew", x)
}
methods::setGeneric("renew")
#' Update a specified slot of a class instance with new values.
#' @rdname renewSlot-methods
#' @description renewSlot() is a method that updates a specified slot of a class instance with new values.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
renewSlot <- function (x, ...)
{
  UseMethod("renew", x)
}
methods::setGeneric("renewSlot")
#' Author a report.
#' @rdname report-methods
#' @description report() is a method that authors a report.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
report <- function (x, ...)
{
  UseMethod("report", x)
}
methods::setGeneric("report")
#' Process output for public disemination and upload to an online repository along with required metadata.
#' @rdname share-methods
#' @description share() is a method that processes output for public disemination and uploads to an online repository along with required metadata.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
share <- function (x, ...)
{
  UseMethod("share", x)
}
methods::setGeneric("share")
