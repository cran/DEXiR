#' @include DexiClasses.R

EnumOrder <- c("ascending", "descending", "none")
EnumQuality <- c("good", "bad", "none")

#' DexiScale
#'
#' `DexiScale` is a base RC class representing value scales in R.
#'
#' A value scale defines the type and set of values that can be assigned to some [DexiAttribute].
#' [DexiScale] is a base scale class that defines fields and methods common to all scales:
#' \itemize{
#'   \item whether or not the scale is preferentially ordered (and in which direction),
#'   \item scale type (discrete or continuous),
#'   \item the number of scale elements, if countable,
#'   \item partition of scale elements in three preferential classes: `"bad"`, `"good"` and `"none"`,
#'   \item helper methods `value_index()` and `full_range()`.
#' }
#'
#' DEXiR implements two other scale classes derived from `DexiScale`:
#' [DexiContinuousScale] and [DexiDiscreteScale].
#'
#' @field order character. Preferential order of the scale.
#' Possible values: `"ascending"`, `"descending"` or `"none"`.
#'
#' @export
#'
DexiScale <- setRefClass(DexiScaleClass,
  fields = list(
    order = "character" # of EnumOrder
  ),
  methods = list(
    initialize = function(order = EnumOrder, ...) {
      "Initialize a \\code{DexiScale} object."

      order <<- match.arg(order)
    },

    verify = function() {
      "Check the correctnes of this scale object and its fields.
      Result: \\code{error()} or \\code{TRUE}."

      stopifnot(order %in% EnumOrder)
      TRUE
    },

    equal = function(scl) {
      "Check if this scale is equal to scale \\code{scl}. Needed for attribute linking."

      return(!is.null(scl) && inherits(scl, DexiScaleClass) && all(order == scl$order))
    },

    to_string = function() {
      "Return a string representation of this scale for printing."

      switch(order,
             "ascending" = "+",
             "descending" = "-",
             "none" = "*"
      )
    },

    is_discrete = function() {
      "Logical: Is this scale discrete?"

      FALSE
    },

    is_continuous = function() {
      "Logical: Is this scale continuos?"

      !is_discrete()
    },

    count = function() {
      "Return the number of scale elements.
      Equal to \\code{NA} for \\code{DexiScale}, \\code{0} for \\code{DexiContinuousScale}, and
      equal to \\code{nvals >= 0} for \\code{DexiDiscreteScale}."

      NA
    },

    value_index = function(value) {
      "Find the index of \\code{value} (character(1)) on this scale.
      Equal to \\code{NA} for \\code{DexiScale} and \\code{DexiContinuousScale}.
      With \\code{DexiDiscreteScale} objects, it returns a numeric index or \\code{NA}
      of \\code{value} in \\code{scale$values}."

      NA
    },

    value_quality = function(value) {
      "Return the quality (preferential class) of \\code{value} on this scale:
      one of the strings \\code{\"bad\"}, \\code{\"none\"} or \\code{\"good\"}.
      Always \\code{\"none\"} for \\code{DexiScale} and scales with \\code{order = \"none\"}."

      "none"
    },

    full_range = function() {
      "Return the vector that represents the full range of values on this scale.
      Equal to \\code{NA} for \\code{DexiScale} and \\code{DexiContinuousScale},
      and \\code{1 : scale$nvals} for \\code{DexiDiscreteScale}."

      NA
    }
  )
)

#' DexiContinuousScale
#'
#' `DexiContinuousScale` is a RC class, derived from `DexiScale`,
#' representing continuous value scales in R.
#'
#' An attribute associated with a continuous scale can take any single numeric value from
#' `[-Inf, +Inf]`.
#'
#' `DexiContinuousScale` defines two numeric bounds, called `low_point` and
#' `high_point`, such that `low_point <= high_point`. These values partition
#' preferentially ordered scales in three preferential classes ("qualities"):
#' `"bad"`, `"none"` (in the sense of `"neutral"`), and `"good"`.
#' For a scale with `order = "ascending"`, the three corresponding intervals are
#' `[-Inf, low_point]`, `(low_point, high_point)` and `[high_point, +Inf]`.
#' For `order = "descending"`, the order of qualities is reversed.
#' Scales with `order = "none"` have only one associated quality,
#' `"none"`, for the whole range of values.
#'
#' Continuous scales are supported in DEXi Suite software (DEXiWin),
#' but not in older DEXi Classic software (DEXi).
#'
#' @field low_point numeric. A bound for the quality interval `[-Inf, low_point]`.
#' @field high_point numeric. A bound for the quality interval `[high_point, +Inf]`.
#'
#' @export DexiContinuousScale
#'
DexiContinuousScale <- setRefClass(DexiContinuousScaleClass,
  contains = DexiScaleClass,

  fields = list(
    low_point = "numeric",
    high_point = "numeric"
  ),

  methods = list(
    initialize = function(low_point = -Inf, high_point = +Inf, ...) {
      "Initialize a \\code{DexiContinuousScale} object."

      callSuper(...)
      low_point <<- low_point
      high_point <<- high_point
    },

    verify = function() {
      "Check the correctnes of this scale object and its fields.
      Result: \\code{error()} or \\code{TRUE}."

      callSuper()
      stopifnot(is_single(low_point))
      stopifnot(is_single(high_point))
      stopifnot(low_point <= high_point)
      TRUE
    },

    equal = function(scl) {
      "Check if this scale is equal to \\code{scl}."

      return(callSuper(scl) && inherits(scl, DexiContinuousScaleClass) &&
             low_point == scl$low_point && high_point == scl$high_point)
    },

    to_string = function() {
      "Return a string representation of this scale for printing."

      paste0(low_point, ':', high_point, " (", callSuper(), ")")
    },

    count = function() {
      "Return the number of scale elements. Always \\code{0} for \\code{DexiContinuousScale}."

      0
    },

    value_quality = function(value) {
      "\\code{value} is numeric(1). Return: the quality of \\code{value}, which can be one of the strings
      \\code{\"bad\"}, \\code{\"none\"} or \\code{\"good\"}, depending on this scale's \\code{order},
      \\code{low_point} and \\code{high_point}."

      if (!is_single_numeric(value)) return(NA)

      switch(order,
       "none" = "none",
       "ascending"= if (value < low_point) "bad" else if (value > high_point) "good" else "none",
       "descending"= if (value > high_point) "bad" else if (value < low_point) "good" else "none",
      )
    }
  )
)

#' DexiDiscreteScale
#'
#' `DexiDiscreteScale` is a RC class, derived from [DexiScale],
#' representing qualitative (symbolic, discrete, verbal) value scales in R. Such scales are
#' typical for DEXi models and are the only scale type supported by the DEXi software.
#' DEXiWin software supports both continuous and discrete scales.
#'
#' An attribute associated with a discrete scale can take values from a finite (and usually small)
#' set of string values contained in the character vector `values`. Additionally, each of these values is
#' associated with one of the qualities `"bad"`, `"none"` or `"good"`.
#' The latter are contained in the character vector `quality`,
#' which is of the same length as `values`.
#'
#' @field values character. Vector of qualitative scale values.
#' Example: `scale$values <- c("low", "medium", "high")`.
#' @field nvals integer. Equal to `length(values)`.
#' @field quality character. Vector of qualities, corresponding to `values`.
#' Should be the of the same length as `values`.
#' Example: `scale$quality <- c("bad", "none", "good")`.
#' @field descriptions character. A vector of textual descriptions of the corresponding
#' `values`. Should be of the same length as `values`.
#'
#' @export DexiDiscreteScale
#'
#' @examples
#' # Load "Car.dxi"
#' CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
#' Car <- read_dexi(CarDxi)
#'
#' # For example, consider the scale of attribute PRICE
#' scl <- Car$attrib("PRICE")$scale
#'
#' # Print fields and basic properties of scl
#' scl$verify()
#' scl$values
#' scl$quality
#' scl$descriptions
#' scl$nvals
#' scl$count()
#' scl$is_discrete()
#' scl$is_continuous()
#' scl$to_string()
#' scl$full_range()
#'
#' # Find value indices
#' scl$value_index("medium")
#' scl$value_index("med")
#'
#' # Is scl equal to the scale of BUY.PRICE?
#' scl$equal(Car$attrib("PRICE")$scale)
#'
DexiDiscreteScale <- setRefClass(DexiDiscreteScaleClass,
   contains = DexiScaleClass,

   fields = list(
     values = "character",
     nvals = "integer",
     quality = "character", # of EnumQuality
     descriptions = "character"
   ),

   methods = list(
     initialize = function(values = NULL, descriptions = NULL, quality = NULL, ...) {
       "Initialize a \\code{DexiDiscreteScale} object."

       callSuper(...)
       values <<- as.character(unlist(values))
       nvals <<- length(values)
       descriptions <<- if (is.null(descriptions)) rep("", nvals) else descriptions
       quality <<- if (is.null(quality)) default_quality(order, nvals) else quality
     },

     verify = function() {
      "Check the correctnes of this scale object and its fields.
      Result: \\code{error()} or \\code{TRUE}."
       callSuper()
       stopifnot(nvals >= 1)
       stopifnot(length(values) == nvals)
       stopifnot(is.character(values))
       stopifnot(length(quality) == nvals)
       stopifnot(is.character(quality))
       stopifnot(length(descriptions) == nvals)
       TRUE
     },

     to_string = function() {
       "Return a string representation of this scale for printing."

       vals <- paste(values, sep = ",", collapse = "; ")
       paste0(vals, " (", callSuper(), ")")
     },

     equal = function(scl) {
       "Check if this scale is equal to \\code{scl}."

       return(callSuper(scl) && inherits(scl, DexiDiscreteScaleClass) &&
         all(values == scl$values) && all(descriptions == scl$descriptions) &&
         all(quality == scl$quality))
     },

     count = function() nvals,

     is_discrete = function() TRUE,

     value_index = function(value) {
       if (is.null(value)) return(NA)
       match(value, values)
     },

     value_quality = function(value) {
       stopifnot(is_single(value))
       index <- value
       if (!is.numeric(index)) index <- value_index(value)
       if (is.na(index) || is.null(index) || !is_in_range(index, 1, nvals)) NA else quality[[index]]
     },

     full_range = function() seq_increasing(1, nvals)

   )
)

#' default_quality
#'
#' Make a default discrete scale quality vector depending on the scale's `order` and `nvals`.
#'
#' @param order `character(1)1, one of "ascending", "descending" or "none".
#' @param nvals `integer(1)`. The number of qualitative values of considered [DexiDiscreteScale].
#'
#' @return character vector of length `nvals`, containing
#' `"bad"`, `"none"` or `"good"`.
#' @export
#'
#' @examples
#' default_quality("ascending", 5)
#' default_quality("descending", 5)
#' default_quality("none", 5)
#' default_quality("ascending", 2)
#' default_quality("ascending", 1)
#'
default_quality <- function (order = EnumOrder, nvals) {
  order <- match.arg(order)
  quality <- rep("none", nvals)
  if (order == "none") return(quality)
  if (nvals > 1) {
    if (order == "ascending") {
      quality[[1]] <- "bad"
      quality[[length(quality)]] <- "good"
    }
    else
    {
      quality[[1]] <- "good"
      quality[[length(quality)]] <- "bad"
    }
  }
  quality
}

#' equal_scales
#'
#' Check if two scales are equal. `NULL` arguments, indicating undefined scales, are allowed.
#' Two `NULL` scales are considered equal.
#'
#' @param scl1 A [DexiScale] (or derived) object, or `NULL`.
#' @param scl2 A [DexiScale] (or derived) object, or `NULL`.
#'
#' @return `logical(1)`.
#' @export
#'
equal_scales <- function(scl1, scl2) {
  if (is.null(scl1)) return(is.null(scl2))
  if (is.null(scl2)) return(FALSE)
  scl1$equal(scl2)
}
