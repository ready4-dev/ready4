#' Author and save files to local or remote locations
#' @rdname author-methods
#' @description author() is a method that authors and saves files to local or remote locations.
#' @param x An object
#' @param ... Additional arguments
#' @export
author <- function (x, ...)
{
  UseMethod("author", x)
}
methods::setGeneric("author")
#' Author and save files necessary for creating and documenting classes
#' @rdname authorClasses-methods
#' @description authorClasses() is a method that authors and saves files necessary for creating and documenting classes.
#' @param x An object
#' @param ... Additional arguments
#' @export
authorClasses <- function (x, ...)
{
  UseMethod("authorClasses", x)
}
methods::setGeneric("authorClasses")
#' Author and save files necessary for creating and documenting datasets
#' @rdname authorData-methods
#' @description authorData() is a method that authors and saves files necessary for creating and documenting datasets.
#' @param x An object
#' @param ... Additional arguments
#' @export
authorData <- function (x, ...)
{
  UseMethod("authorData", x)
}
methods::setGeneric("authorData")
#' Author and save files necessary for creating and documenting functions
#' @rdname authorFunctions-methods
#' @description authorFunctions() is a method that authors and saves files necessary for creating and documenting functions.
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
#' Characterize a class instance by generating descriptive information
#' @rdname characterize-methods
#' @description characterize() is a method that characterizes a class instance by generating descriptive information.
#' @param x An object
#' @param ... Additional arguments
#' @export
characterize <- function (x, ...)
{
  UseMethod("characterize", x)
}
methods::setGeneric("characterize")
#' Characterize an object stored in a slot of a class instance by generating descriptive information
#' @rdname characterizeSlot-methods
#' @description characterizeSlot() is a method that characterizes an object stored in a slot of a class instance by generating descriptive information.
#' @param x An object
#' @param ... Additional arguments
#' @export
characterizeSlot <- function (x, ...)
{
  UseMethod("characterizeSlot", x)
}
methods::setGeneric("characterizeSlot")
#' Depict features of an instance of a class by generating a plot
#' @rdname depict-methods
#' @description depict() is a method that depicts features of an instance of a class by generating a plot.
#' @param x An object
#' @param ... Additional arguments
#' @export
depict <- function (x, ...)
{
  UseMethod("depict", x)
}
methods::setGeneric("depict")
#' Enhance an instance of a class by adding new data fields and values to an object stored in a slot of that instance
#' @rdname enhance-methods
#' @description enhance() is a method that enhances an instance of a class by adding new data fields and values to an object stored in a slot of that instance.
#' @param x An object
#' @param ... Additional arguments
#' @export
enhance <- function (x, ...)
{
  UseMethod("enhance", x)
}
methods::setGeneric("enhance")
#' Enhance an instance of a class by adding new data fields and values to an object stored in a slot of that instance
#' @rdname enhanceSlot-methods
#' @description enhanceSlot() is a method that enhances an instance of a class by adding new data fields and values to an object stored in a slot of that instance.
#' @param x An object
#' @param ... Additional arguments
#' @export
enhanceSlot <- function (x, ...)
{
  UseMethod("enhanceSlot", x)
}
methods::setGeneric("enhanceSlot")
#' Exhibit features of a class instance by printing to console
#' @rdname exhibit-methods
#' @description exhibit() is a method that exhibits features of a class instance by printing to console.
#' @param x An object
#' @param ... Additional arguments
#' @export
exhibit <- function (x, ...)
{
  UseMethod("exhibit", x)
}
methods::setGeneric("exhibit")


#' Ingest data saved in external files into R objects stored in working memory
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
#' Investigate data stored in an instance of a class by executing an algorithm to solve an inverse problem
#' @rdname investigate-methods
#' @description investigate() is a method that investigates data stored in an instance of a class by executing an algorithm to solve an inverse problem.
#' @param x An object
#' @param ... Additional arguments
#' @export
investigate <- function (x, ...)
{
  UseMethod("investigate", x)
}
methods::setGeneric("investigate")
#' Manufacture a novel R object using data contained in an instance of a class
#' @rdname manufacture-methods
#' @description manufacture() is a method that manufactures a novel R object using data contained in an instance of a class.
#' @param x An object
#' @param ... Additional arguments
#' @export
manufacture <- function (x, ...)
{
  UseMethod("manufacture", x)
}
methods::setGeneric("manufacture")
#' Metamorphose an instance of a class into an instance of a different (non-child) class
#' @rdname metamorphose-methods
#' @description metamorphose() is a method that metamorphoses an instance of a class into an instance of a different (non-child) class.
#' @param x An object
#' @param ... Additional arguments
#' @export
metamorphose <- function (x, ...)
{
  UseMethod("metamorphose", x)
}
methods::setGeneric("metamorphose")
#' Metamorphose data stored in a slot of a class instance into an object of a different (non-child) class
#' @rdname metamorphoseSlot-methods
#' @description metamorphoseSlot() is a method that metamorphoses data stored in a slot of a class instance into an object of a different (non-child) class.
#' @param x An object
#' @param ... Additional arguments
#' @export
metamorphoseSlot <- function (x, ...)
{
  UseMethod("metamorphoseSlot", x)
}
methods::setGeneric("metamorphoseSlot")
#' Procure data by executing a search and retrieval algorithm using data contained in an instance of a class
#' @rdname procure-methods
#' @description procure() is a method that procures data by executing a search and retrieval algorithm using data contained in an instance of a class.
#' @param x An object
#' @param ... Additional arguments
#' @export
procure <- function (x, ...)
{
  UseMethod("procure", x)
}
methods::setGeneric("procure")
#' Prognosticate by applying an algorithm to solve a forward problem through simulation or prediction
#' @rdname prognosticate-methods
#' @description prognosticate() is a method that prognosticates by applying an algorithm to solve a forward problem through simulation or prediction.
#' @param x An object
#' @param ... Additional arguments
#' @export
prognosticate <- function (x, ...)
{
  UseMethod("prognosticate", x)
}
methods::setGeneric("prognosticate")
#' Ratifie that an instance of a class conforms to specified criteria
#' @rdname ratify-methods
#' @description ratify() is a method that ratifies that an instance of a class conforms to specified criteria.
#' @param x An object
#' @param ... Additional arguments
#' @export
ratify <- function (x, ...)
{
  UseMethod("ratify", x)
}
methods::setGeneric("ratify")
#' Ratifie that data contained in a slot of a class instance conforms to specified criteria
#' @rdname ratifySlot-methods
#' @description ratifySlot() is a method that ratifies that data contained in a slot of a class instance conforms to specified criteria.
#' @param x An object
#' @param ... Additional arguments
#' @export
ratifySlot <- function (x, ...)
{
  UseMethod("ratifySlot", x)
}
methods::setGeneric("ratifySlot")
#' Reckon a value by performing a calculation using data contained in an instance of a class
#' @rdname reckon-methods
#' @description reckon() is a method that reckons a value by performing a calculation using data contained in an instance of a class.
#' @param x An object
#' @param ... Additional arguments
#' @export
reckon <- function (x, ...)
{
  UseMethod("reckon", x)
}
methods::setGeneric("reckon")
#' Renew an instance of a class by updating it with new data
#' @rdname renew-methods
#' @description renew() is a method that renews an instance of a class by updating it with new data.
#' @param x An object
#' @param ... Additional arguments
#' @export
renew <- function (x, ...)
{
  UseMethod("renew", x)
}
methods::setGeneric("renew")
#' Renew a slot of a class instance by updating it with new data
#' @rdname renewSlot-methods
#' @description renewSlot() is a method that renews a slot of a class instance by updating it with new data.
#' @param x An object
#' @param ... Additional arguments
#' @export
renewSlot <- function (x, ...)
{
  UseMethod("renewSlot", x)
}
methods::setGeneric("renewSlot")
#' Share data contained in an instance of a class via an online repository
#' @rdname share-methods
#' @description share() is a method that shares data contained in an instance of a class via an online repository.
#' @param x An object
#' @param ... Additional arguments
#' @export
share <- function (x, ...)
{
  UseMethod("share", x)
}
methods::setGeneric("share")
#' Share data contained in a slot of a class instance via an online repository
#' @rdname shareSlot-methods
#' @description shareSlot() is a method that shares data contained in a slot of a class instance via an online repository.
#' @param x An object
#' @param ... Additional arguments
#' @export
shareSlot <- function (x, ...)
{
  UseMethod("shareSlot", x)
}
methods::setGeneric("shareSlot")
#' report
#' @rdname report-methods
#' @description report() is a method that reports.
#' @param x An object
#' @param ... Additional arguments
#' @export
report <- function (x, ...)
{
  UseMethod("report", x)
}
methods::setGeneric("report")
