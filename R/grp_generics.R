#' Create and save reusable artefacts
#'
#' @rdname author-methods
#' @description author() creates, documents and/or saves artefacts
#' associated with an object.
#' @param x A Ready4 object or other object for which an appropriate
#' author method exists.
#' @param ... Additional arguments.
#' @return The object returned by the applicable author method.
#' @export
author <- function (x, ...)
{
  UseMethod("author", x)
}
methods::setGeneric("author")

#' Create and document classes
#'
#' @rdname authorClasses-methods
#' @description authorClasses() creates and documents class definitions.
#'
#' This generic is primarily maintained for compatibility with legacy
#' ready4 package development workflows.
#' @param x A Ready4 object or other object for which an appropriate
#' authorClasses method exists.
#' @param ... Additional arguments.
#' @return The object returned by the applicable authorClasses method.
#' @export
authorClasses <- function (x, ...)
{
  UseMethod("authorClasses", x)
}
methods::setGeneric("authorClasses")

#' Create and document datasets
#'
#' @rdname authorData-methods
#' @description authorData() creates, documents and saves datasets.
#' @param x A Ready4 object or other object for which an appropriate
#' authorData method exists.
#' @param ... Additional arguments.
#' @return The object returned by the applicable authorData method.
#' @export
authorData <- function (x, ...)
{
  UseMethod("authorData", x)
}
methods::setGeneric("authorData")

#' Create and document functions
#'
#' @rdname authorFunctions-methods
#' @description authorFunctions() creates, documents and saves function
#' definitions.
#'
#' This generic is primarily maintained for compatibility with legacy
#' ready4 package development workflows.
#' @param x A Ready4 object or other object for which an appropriate
#' authorFunctions method exists.
#' @param ... Additional arguments.
#' @return The object returned by the applicable authorFunctions method.
#' @export
authorFunctions <- function (x, ...)
{
  UseMethod("authorFunctions", x)
}
methods::setGeneric("authorFunctions")

#' Create reports
#'
#' @rdname authorReport-methods
#' @description authorReport() generates and saves reports derived from
#' an object.
#' @param x A Ready4 object or other object for which an appropriate
#' authorReport method exists.
#' @param ... Additional arguments.
#' @return The object returned by the applicable authorReport method.
#' @export
authorReport <- function (x, ...)
{
  UseMethod("authorReport", x)
}
methods::setGeneric("authorReport")

#' Generate descriptive summaries
#'
#' @rdname characterize-methods
#' @description characterize() generates descriptive summaries of an
#' object's contents.
#' @param x An object.
#' @param ... Additional arguments.
#' @return The object returned by the applicable characterize method.
#' @export
characterize <- function (x, ...)
{
  UseMethod("characterize", x)
}
methods::setGeneric("characterize")

#' Visualise object features
#'
#' @rdname depict-methods
#' @description depict() visualises selected features of an object.
#' @param x An object.
#' @param ... Additional arguments.
#' @return The object returned by the applicable depict method.
#' @export
depict <- function (x, ...)
{
  UseMethod("depict", x)
}
methods::setGeneric("depict")

#' Extend an object's structure
#'
#' @rdname enhance-methods
#' @description enhance() augments an object by adding new data,
#' properties, components or functionality. The resulting object will
#' typically belong to the same class or an inheriting class.
#' @param x An object.
#' @param ... Additional arguments.
#' @return An enhanced version of the supplied object.
#' @export
enhance <- function (x, ...)
{
  UseMethod("enhance", x)
}
methods::setGeneric("enhance")

#' Display object contents
#'
#' @rdname exhibit-methods
#' @description exhibit() displays selected features of an object.
#' @param x An object.
#' @param ... Additional arguments.
#' @return The object returned by the applicable exhibit method.
#' @export
exhibit <- function (x, ...)
{
  UseMethod("exhibit", x)
}
methods::setGeneric("exhibit")

#' Import external data
#'
#' @rdname ingest-methods
#' @description ingest() reads data from external sources into an object.
#' @param x An object.
#' @param ... Additional arguments.
#' @return The object returned by the applicable ingest method.
#' @export
ingest <- function (x, ...)
{
  UseMethod("ingest", x)
}
methods::setGeneric("ingest")

#' Investigate inverse problems
#'
#' @rdname investigate-methods
#' @description investigate() applies methods for inferring models,
#' structures or generating mechanisms from observed data.
#' @param x An object.
#' @param ... Additional arguments.
#' @return The object returned by the applicable investigate method.
#' @export
investigate <- function (x, ...)
{
  UseMethod("investigate", x)
}
methods::setGeneric("investigate")

#' Create a derived object
#'
#' @rdname manufacture-methods
#' @description manufacture() uses an object to create a new derived
#' object that is not itself required to inherit from Ready4.
#' @param x An object.
#' @param ... Additional arguments.
#' @return A derived object that is not required to inherit from Ready4.
#' @export
manufacture <- function (x, ...)
{
  UseMethod("manufacture", x)
}
methods::setGeneric("manufacture")

#' Transform a Ready4 object into another Ready4 object
#'
#' @rdname metamorphose-methods
#' @description metamorphose() converts a Ready4 object into a different
#' Ready4 object representation. The resulting object will typically
#' belong to a different class that is not an inheriting class of the
#' input object.
#' @param x A Ready4 object.
#' @param ... Additional arguments.
#' @return A Ready4 object representing a transformed version of the
#' supplied object.
#' @export
metamorphose <- function (x, ...)
{
  UseMethod("metamorphose", x)
}
methods::setGeneric("metamorphose")

#' Retrieve information from an object
#'
#' @rdname procure-methods
#' @description procure() retrieves data, components or metadata from
#' an object.
#' @param x An object.
#' @param ... Additional arguments.
#' @return The object returned by the applicable procure method.
#' @export
procure <- function (x, ...)
{
  UseMethod("procure", x)
}
methods::setGeneric("procure")

#' Generate predictions
#'
#' @rdname prognosticate-methods
#' @description prognosticate() generates predictions by applying
#' forecasting, simulation or statistical methods.
#' @param x An object.
#' @param ... Additional arguments.
#' @return The object returned by the applicable prognosticate method.
#' @export
prognosticate <- function (x, ...)
{
  UseMethod("prognosticate", x)
}
methods::setGeneric("prognosticate")

#' Validate and reconcile object contents
#'
#' @rdname ratify-methods
#' @description ratify() assesses whether an object satisfies specified
#' validity criteria and may update the object so that those criteria are
#' met.
#' @param x An object.
#' @param ... Additional arguments.
#' @return An updated version of the supplied object.
#' @export
ratify <- function (x, ...)
{
  UseMethod("ratify", x)
}
methods::setGeneric("ratify")

#' Compute derived quantities
#'
#' @rdname reckon-methods
#' @description reckon() performs calculations using the contents of an
#' object.
#' @param x An object.
#' @param ... Additional arguments.
#' @return The object returned by the applicable reckon method.
#' @export
reckon <- function (x, ...)
{
  UseMethod("reckon", x)
}
methods::setGeneric("reckon")

#' Update object contents
#'
#' @rdname renew-methods
#' @description renew() updates values contained in an object without
#' changing its structure.
#' @param x An object.
#' @param ... Additional arguments.
#' @return An updated version of the supplied object.
#' @export
renew <- function (x, ...)
{
  UseMethod("renew", x)
}
methods::setGeneric("renew")

#' Share data or outputs
#'
#' @rdname share-methods
#' @description share() disseminates data, results or other outputs
#' through repositories, reports, publications or other channels.
#' @param x An object.
#' @param ... Additional arguments.
#' @return The object returned by the applicable share method.
#' @export
share <- function (x, ...)
{
  UseMethod("share", x)
}
methods::setGeneric("share")

#' Apply the author method to a specified slot
#'
#' @rdname authorSlot-methods
#' @description authorSlot() is a convenience method that applies the
#' author method to a specified slot of a Ready4 object.
#' @param x A Ready4 object.
#' @param slot_nm_1L_chr Name of the slot to which the author method
#' should be applied (a character vector of length one).
#' @param ... Additional arguments.
#' @return The object returned by applying author to the specified slot.
#' @export
authorSlot <- function (x,
                        slot_nm_1L_chr,
                        ...)
{
  UseMethod("authorSlot", x)
}
methods::setGeneric("authorSlot")

#' Apply the characterize method to a specified slot
#'
#' @rdname characterizeSlot-methods
#' @description characterizeSlot() is a convenience method that applies
#' the characterize method to a specified slot of a Ready4 object.
#' @param x A Ready4 object.
#' @param slot_nm_1L_chr Name of the slot to which the characterize
#' method should be applied (a character vector of length one).
#' @param ... Additional arguments.
#' @return The object returned by applying characterize to the specified
#' slot.
#' @export
characterizeSlot <- function (x,
                              slot_nm_1L_chr,
                              ...)
{
  UseMethod("characterizeSlot", x)
}
methods::setGeneric("characterizeSlot")

#' Apply the depict method to a specified slot
#'
#' @rdname depictSlot-methods
#' @description depictSlot() is a convenience method that applies the
#' depict method to a specified slot of a Ready4 object.
#' @param x A Ready4 object.
#' @param slot_nm_1L_chr Name of the slot to which the depict method
#' should be applied (a character vector of length one).
#' @param ... Additional arguments.
#' @return The object returned by applying depict to the specified slot.
#' @export
depictSlot <- function (x,
                        slot_nm_1L_chr,
                        ...)
{
  UseMethod("depictSlot", x)
}
methods::setGeneric("depictSlot")

#' Apply the enhance method to a specified slot
#'
#' @rdname enhanceSlot-methods
#' @description enhanceSlot() is a convenience method that applies the
#' enhance method to a specified slot of a Ready4 object and updates the
#' object with the result.
#' @param x A Ready4 object.
#' @param slot_nm_1L_chr Name of the slot to which the enhance method
#' should be applied (a character vector of length one).
#' @param ... Additional arguments.
#' @return An updated version of the supplied object.
#' @export
enhanceSlot <- function (x,
                         slot_nm_1L_chr,
                         ...)
{
  UseMethod("enhanceSlot", x)
}
methods::setGeneric("enhanceSlot")


#' Apply the exhibit method to a specified slot
#'
#' @rdname exhibitSlot-methods
#' @description exhibitSlot() is a convenience method that applies the
#' exhibit method to a specified slot of a Ready4 object.
#' @param x A Ready4 object.
#' @param slot_nm_1L_chr Name of the slot to which the exhibit method
#' should be applied (a character vector of length one).
#' @param ... Additional arguments.
#' @return The object returned by applying exhibit to the specified slot.
#' @export
exhibitSlot <- function (x,
                         slot_nm_1L_chr,
                         ...)
{
  UseMethod("exhibitSlot", x)
}
methods::setGeneric("exhibitSlot")

#' Apply the ingest method to a specified slot
#'
#' @rdname ingestSlot-methods
#' @description ingestSlot() is a convenience method that applies the
#' ingest method to a specified slot of a Ready4 object.
#' @param x A Ready4 object.
#' @param slot_nm_1L_chr Name of the slot to which the ingest method
#' should be applied (a character vector of length one).
#' @param ... Additional arguments.
#' @return The object returned by applying ingest to the specified slot.
#' @export
ingestSlot <- function (x,
                        slot_nm_1L_chr,
                        ...)
{
  UseMethod("ingestSlot", x)
}
methods::setGeneric("ingestSlot")

#' Apply the investigate method to a specified slot
#'
#' @rdname investigateSlot-methods
#' @description investigateSlot() is a convenience method that applies
#' the investigate method to a specified slot of a Ready4 object.
#' @param x A Ready4 object.
#' @param slot_nm_1L_chr Name of the slot to which the investigate
#' method should be applied (a character vector of length one).
#' @param ... Additional arguments.
#' @return The object returned by applying investigate to the specified
#' slot.
#' @export
investigateSlot <- function (x,
                             slot_nm_1L_chr,
                             ...)
{
  UseMethod("investigateSlot", x)
}
methods::setGeneric("investigateSlot")

#' Apply the manufacture method to a specified slot
#'
#' @rdname manufactureSlot-methods
#' @description manufactureSlot() is a convenience method that applies
#' the manufacture method to a specified slot of a Ready4 object.
#' @param x A Ready4 object.
#' @param slot_nm_1L_chr Name of the slot to which the manufacture method
#' should be applied (a character vector of length one).
#' @param ... Additional arguments.
#' @return The object returned by applying manufacture to the specified
#' slot.
#' @export
manufactureSlot <- function (x,
                             slot_nm_1L_chr,
                             ...)
{
  UseMethod("manufactureSlot", x)
}
methods::setGeneric("manufactureSlot")

#' Apply the metamorphose method to a specified slot
#'
#' @rdname metamorphoseSlot-methods
#' @description metamorphoseSlot() is a convenience method that applies
#' the metamorphose method to a specified slot of a Ready4 object.
#' @param x A Ready4 object.
#' @param slot_nm_1L_chr Name of the slot to which the metamorphose
#' method should be applied (a character vector of length one).
#' @param ... Additional arguments.
#' @return The object returned by applying metamorphose to the specified
#' slot.
#' @export
metamorphoseSlot <- function (x,
                              slot_nm_1L_chr,
                              ...)
{
  UseMethod("metamorphoseSlot", x)
}
methods::setGeneric("metamorphoseSlot")


#' Retrieve the value stored in a specified slot
#'
#' @rdname procureSlot-methods
#' @description procureSlot() retrieves the value stored in a specified
#' slot of a Ready4 object. Optionally, the procure method can be applied
#' to the retrieved value before it is returned.
#' @param x A Ready4 object.
#' @param slot_nm_1L_chr Name of the slot to retrieve
#' (a character vector of length one).
#' @param ... Additional arguments.
#' @return The value stored in the specified slot, or the object returned
#' by applying procure to that value.
#' @export
procureSlot <- function (x,
                         slot_nm_1L_chr,
                         ...)
{
  UseMethod("procureSlot", x)
}
methods::setGeneric("procureSlot")

#' Apply the prognosticate method to a specified slot
#'
#' @rdname prognosticateSlot-methods
#' @description prognosticateSlot() is a convenience method that applies
#' the prognosticate method to a specified slot of a Ready4 object.
#' @param x A Ready4 object.
#' @param slot_nm_1L_chr Name of the slot to which the prognosticate
#' method should be applied (a character vector of length one).
#' @param ... Additional arguments.
#' @return The object returned by applying prognosticate to the specified
#' slot.
#' @export
prognosticateSlot <- function (x,
                               slot_nm_1L_chr,
                               ...)
{
  UseMethod("prognosticateSlot", x)
}
methods::setGeneric("prognosticateSlot")

#' Apply the ratify method to a specified slot
#'
#' @rdname ratifySlot-methods
#' @description ratifySlot() is a convenience method that applies the
#' ratify method to a specified slot of a Ready4 object and updates the
#' object with the result.
#' @param x A Ready4 object.
#' @param slot_nm_1L_chr Name of the slot to which the ratify method
#' should be applied (a character vector of length one).
#' @param ... Additional arguments.
#' @return An updated version of the supplied object.
#' @export
ratifySlot <- function (x,
                        slot_nm_1L_chr,
                        ...)
{
  UseMethod("ratifySlot", x)
}
methods::setGeneric("ratifySlot")

#' Apply the reckon method to a specified slot
#'
#' @rdname reckonSlot-methods
#' @description reckonSlot() is a convenience method that applies the
#' reckon method to a specified slot of a Ready4 object.
#' @param x A Ready4 object.
#' @param slot_nm_1L_chr Name of the slot to which the reckon method
#' should be applied (a character vector of length one).
#' @param ... Additional arguments.
#' @return The object returned by applying reckon to the specified slot.
#' @export
reckonSlot <- function (x,
                        slot_nm_1L_chr,
                        ...)
{
  UseMethod("reckonSlot", x)
}
methods::setGeneric("reckonSlot")

#' Update the value stored in a specified slot
#'
#' @rdname renewSlot-methods
#' @description renewSlot() updates the value stored in a specified slot
#' of a Ready4 object. If no replacement value is supplied, the renew
#' method is first applied to the current slot value and the result is
#' written back to the slot.
#' @param x A Ready4 object.
#' @param slot_nm_1L_chr Name of the slot to update
#' (a character vector of length one).
#' @param new_val_xx New value to assign to the specified slot.
#' @param ... Additional arguments.
#' @return An updated version of the supplied object.
#' @export model module (an instance of a class that inherits from Ready4) of the same class as that supplied to the method.
#' @export
renewSlot <- function (x,
                       slot_nm_1L_chr,
                       new_val_xx = "use_renew_mthd",
                       ...)
{
  UseMethod("renewSlot", x)
}
methods::setGeneric("renewSlot")

#' Apply the share method to a specified slot
#'
#' @rdname shareSlot-methods
#' @description shareSlot() is a convenience method that applies the
#' share method to a specified slot of a Ready4 object.
#' @param x A Ready4 object.
#' @param slot_nm_1L_chr Name of the slot to which the share method
#' should be applied (a character vector of length one).
#' @param ... Additional arguments.
#' @return The object returned by applying share to the specified slot.
#' @export
shareSlot <- function (x,
                       slot_nm_1L_chr,
                       ...)
{
  UseMethod("shareSlot", x)
}
methods::setGeneric("shareSlot")
