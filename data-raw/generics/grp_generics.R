#' Author
#' @rdname author-methods
#' @description author() is an Author function that writes files to local or remote locations. Specifically, this function implements an algorithm to author. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
author <- function (x, ...)
{
  UseMethod("author", x)
}
methods::setGeneric("author")
#' AuthorClasses
#' @rdname authorClasses-methods
#' @description authorClasses() is an AuthorClasses function that authors and saves files necessary for creating and documenting classes. Specifically, this function implements an algorithm to authorclasses. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
authorClasses <- function (x, ...)
{
  UseMethod("authorClasses", x)
}
methods::setGeneric("authorClasses")
#' AuthorData
#' @rdname authorData-methods
#' @description authorData() is an AuthorData function that authors and saves files necessary for creating and documenting datasets. Specifically, this function implements an algorithm to authordata. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
authorData <- function (x, ...)
{
  UseMethod("authorData", x)
}
methods::setGeneric("authorData")
#' AuthorFunctions
#' @rdname authorFunctions-methods
#' @description authorFunctions() is an AuthorFunctions function that authors and saves files necessary for creating and documenting functions. Specifically, this function implements an algorithm to authorfunctions. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
authorFunctions <- function (x, ...)
{
  UseMethod("authorFunctions", x)
}
methods::setGeneric("authorFunctions")
#' Characterize
#' @rdname characterize-methods
#' @description characterize() is a Characterize function that generates descriptive information about a dataset. Specifically, this function implements an algorithm to characterize. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
characterize <- function (x, ...)
{
  UseMethod("characterize", x)
}
methods::setGeneric("characterize")
#' Enhance
#' @rdname enhance-methods
#' @description enhance() is a method that enhances an instance of a class by adding new data fields and values to objects contained in that instance.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
enhance <- function (x, ...)
{
  UseMethod("enhance", x)
}
methods::setGeneric("enhance")
#' Ingest
#' @rdname ingest-methods
#' @description ingest() is an Ingest function that imports objects in external file formats into R objects stored in working memory. Specifically, this function implements an algorithm to ingest. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
ingest <- function (x, ...)
{
  UseMethod("ingest", x)
}
methods::setGeneric("ingest")
#' Investigate
#' @rdname investigate-methods
#' @description investigate() is an Investigate function that executes an algorithm so solve an inverse problem, often through estimating statistical models. Specifically, this function implements an algorithm to investigate. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
investigate <- function (x, ...)
{
  UseMethod("investigate", x)
}
methods::setGeneric("investigate")
#' Manufacture
#' @rdname manufacture-methods
#' @description manufacture() is a Manufacture function that creates a novel R object. Specifically, this function implements an algorithm to manufacture. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
manufacture <- function (x, ...)
{
  UseMethod("manufacture", x)
}
methods::setGeneric("manufacture")
#' Metamorphose
#' @rdname metamorphose-methods
#' @description metamorphose() is a Metamorphose function that transforms an instance of a class into an object with different structural properties. Specifically, this function implements an algorithm to metamorphose. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
metamorphose <- function (x, ...)
{
  UseMethod("metamorphose", x)
}
methods::setGeneric("metamorphose")
#' Procure
#' @rdname procure-methods
#' @description procure() is a Procure function that searches and retrieves requested data from a specified source. Specifically, this function implements an algorithm to procure. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
procure <- function (x, ...)
{
  UseMethod("procure", x)
}
methods::setGeneric("procure")
#' Prognosticate
#' @rdname prognosticate-methods
#' @description prognosticate() is a Prognosticate function that executes an algorithm for solving forward problems through simulation or prediction. Specifically, this function implements an algorithm to prognosticate. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
prognosticate <- function (x, ...)
{
  UseMethod("prognosticate", x)
}
methods::setGeneric("prognosticate")
#' Ratify
#' @rdname ratify-methods
#' @description ratify() is a Ratify function that checks whether an instance of a class conforms to required criteria, potentially modifying that instance to ensure that it is valid. Specifically, this function implements an algorithm to ratify. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
ratify <- function (x, ...)
{
  UseMethod("ratify", x)
}
methods::setGeneric("ratify")
#' Reckon
#' @rdname reckon-methods
#' @description reckon() is a Reckon function that performs a calculation. Specifically, this function implements an algorithm to reckon. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
reckon <- function (x, ...)
{
  UseMethod("reckon", x)
}
methods::setGeneric("reckon")
#' Renew
#' @rdname renew-methods
#' @description renew() is a Renew function that updates an instance of a class with new values. Specifically, this function implements an algorithm to renew. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
renew <- function (x, ...)
{
  UseMethod("renew", x)
}
methods::setGeneric("renew")
#' Report
#' @rdname report-methods
#' @description report() is a Report function that authors a report. Specifically, this function implements an algorithm to report. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
report <- function (x, ...)
{
  UseMethod("report", x)
}
methods::setGeneric("report")
#' Share
#' @rdname share-methods
#' @description share() is a Share function that processes output for public disemination and uploads to an online repository along with required metadata. Specifically, this function implements an algorithm to share. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export
share <- function (x, ...)
{
  UseMethod("share", x)
}
methods::setGeneric("share")
