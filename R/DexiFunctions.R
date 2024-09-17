#' @include DexiClasses.R
#' @exportClass DexiFunction
#' @exportClass DexiTabularFunction
#' @exportClass DexiDiscretizeFunction

NULL

#' DexiFunction
#'
#' `DexiFunction` is a base RC class for representing DEXi aggregation and discretization functions in R.
#'
#' DEXi functions are generally associated with aggregate attributes. For some aggregate attribute `att`,
#' `att$funct` defines the mapping from values of `att$inputs` to values of `att`.
#'
#' [DexiFunction] is a base class that defines fields and methods common to all functions:
#' \itemize{
#'   \item method `value(x)`: returns the function value for arguments `x`. Arguments are
#'     assumed to be a numeric vector of length equal to `att$inputs`.
#'   \item method `evaluate(x)` is a silent wrapper around `value(x)`; it returns `NULL` when
#'     `value(x)` fails with an error.
#' }
#'
#' DEXiR implements two other function classes derived from `DexiFunction`:
#' [DexiTabularFunction] and [DexiDiscretizeFunction].
#'
#' @export
#'
DexiFunction <- setRefClass(DexiFunctionClass,
  fields = list(),

  methods = list(
   initialize = function(...) {
     "Initialize a \\code{DexiFunction} object."
   },

   verify = function() {
     "Check the correctnes of this function object and its fields.
      Result: \\code{error()} or \\code{TRUE}."

     TRUE
   },

   value = function(x) {
     "Return the function value for arguments \\code{x}, where arguments are
     a numeric vector of length equal to \\code{att$inputs}.
     Additionally, arguments of a \\code{DexiTabularFunctions$value()} must be integer numbers,
     and the argument of \\code{DexiDiscretizeFunctions$value()} must be a single number."

     NA
    },

   evaluate = function(x) {
     "A silent wrapper around \\code{value(x)}; it returns \\code{NULL} when
     \\code{value(x)} fails with an error."

     NULL
   }
 )
)

#' DexiTabularFunction
#'
#' `DexiTabularFunction` is a RC class, derived from [DexiFunction].
#' Functions of this type aggregate attribute values according to \emph{decision rules},
#' defined in terms of a \emph{decision table}.
#'
#' A decision table contains as many decision rules as there are possible combinations of
#' input attributes' values. For instance, if some `attribute` has two inputs whose
#' discrete scales have three and four values, respectively (i.e., `attribute$dim() == c(3,4)`),
#' then the number of rules is equal to `prod(attribute$dim()) == 12`. Each rule defines the
#' value of `attribute` for one of the possible combinations of values of `attribute$inputs`.
#' Thus, a decision table can be interpreted as a lookup table that, given a vector
#' of values of `attribute$inputs` (i.e., function arguments) returns the corresponding
#' `attribute` value.
#'
#' Objects of class `DexiTabularFunction` store decision rules in `values`, a multi-dimensional
#' list that contains rule values. In most cases, a rule value is a single integer,
#' representing an ordinal number of some value from `attribute$scale`. In a general case, however,
#' a rule value can be an integer vector, representing a (sub)set of values from `attribute$scale`.
#'
#' @field attribute [DexiAttribute]. The attribute this function is associated with.
#' Both the attribute and its inputs are required to be discrete (i.e., associated with
#' `DexiDiscreteScale`s).

#' @field values A multi-dimensional list of rule values. The dimensions of the list are equal to
#' `attribute$dim()`, and the length of the list is `nvals() == prod(dim)`. The list
#' contains rule values that are in general value sets, i.e., integer vectors of value indices
#' w.r.t. `attribute$scale`.
#'
#' @field args A list of integer vectors, containing all possible combinations of values of `attribute$inputs`.
#' `args` and `values` are of the same length and ordered so that, for each `i`, `args[[i]]`
#' defines function arguments that map to `values[[i]]`).
#'
#' @seealso [dexi_index()], [dexi_table()], [make_args()]
#'
#' @export DexiTabularFunction
#'
#' @examples
#' # Load "Car.dxi"
#' CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
#' Car <- read_dexi(CarDxi)
#'
#' # For example, consider the function of attribute CAR
#' fnc <- Car$attrib("CAR")$funct
#'
#' # Print fields and basic properties of fnc
#' fnc$verify()
#' att_names(fnc$attribute)
#' fnc$values
#' fnc$args
#' fnc$nargs()
#' fnc$nvals()
#' fnc$to_string()
#'
#' # Try some args to value mappings
#' fnc$evaluate(c(1, 1))
#' fnc$evaluate(c(2, 2))
#' fnc$evaluate(c(3, 4))
#' fnc$evaluate(c(4, 4)) # the first argument is out of bounds, returns NULL
#'
DexiTabularFunction <- setRefClass(DexiTabularFunctionClass,
  contains = DexiFunctionClass,

  fields = list(
    attribute = "ANY",
    values = "ANY",
    args = "ANY"
  ),

  methods = list(

    initialize = function(attribute = NULL, dim = NULL, values = NULL, low = NULL, high = NULL, ...) {
      "Initialize a \\code{DexiTabularFunction} object."

      stopif(is.null(attribute) && is.null(dim))
      stopifnot(is_class_or_null(attribute, DexiAttributeClass))
      if (!is.null(attribute) && is.null(dim)) dim <- attribute$dim()
      stopif(is.null(dim))
      nargs <- length(dim)
      nvals <- prod(dim)
      if (!is.null(attribute)) {
        att_dim <- attribute$dim()
        att_inpcount <- attribute$count()
        stopifnot(att_inpcount == nargs)
        stopifnot(all(att_dim == dim, na.rm = TRUE))
      }
      vals <- values
      if (is.null(vals)) {
        if (is.null(low))
        {
          # creating NULL values
          vals <- vector("list", length = nvals)
        }
        else
        {
          # using DEXi vectors
          vals <- dexi_table(dim, low, high)
        }
      }
      else
      {
        # using values
        stopifnot(is.list(vals))
        stopifnot(length(vals) == nvals)
      }
      dim(vals) <- dim

      attribute <<- attribute
      values <<- vals
      args <<- make_args(dim)
    },

    verify = function() {
      "Check the correctnes of this function object and its fields.
      Result: \\code{error()} or \\code{TRUE}."

      stopifnot(is_class_or_null(attribute, DexiAttributeClass))
      stopifnot(is.list(values))
      stopifnot(length(values) == nvals())
      stopifnot(length(args) == nvals())
      stopifnot(length(args) == length(values))
      stopifnot(nvals() == prod(dim(values)))
      TRUE
    },

    show = function () {
      "Prints out function's \\code{values}."

      print(values)
    },

    to_string = function() {
      "Return a short informative string about the size and dimensions of \\code{values}."

      paste0(length(values), " ", paste0(dim(values), collapse = "x"))
    },

    nargs = function() {
      "Return the number of function arguments."

      length(dim(values))
    },

    nvals = function() {
      "Return the function size (number of rules)."

      length(values)
    },

    value = function(x) {
      stopifnot(length(x) == nargs())
      if (is.list(x)) x <- unlist(x)
      stopifnot(is_all_integer(x))
      stopif(any(x < 1))
      stopif(any(x > dim(values)))
      unlist(values[matrix(x, 1)])
    },

    evaluate = function(x) {
      catch_error(value(x))
    }
  )
)

#' DexiDiscretizeFunction
#'
#' `DexiDiscretizeFunction` is a RC class, derived from [DexiFunction].
#' Functions of this type discretize numerical values of continuous attributes to qualitative
#' values of discrete attributes. More precisely, a `DexiDiscretizeFunction` can be defined
#' only for a discrete attribute that has exactly one continuous input. Then, the function discretizes
#' numeric values of the input attribute and maps them to discrete values of the parent attribute.
#'
#' Objects of class `DexiDiscretizeFunction`  define discretization rules in terms of three lists:
#' `values`, `bounds` and `assoc`. Using `n <- nvals()` to denote the length of
#' `values`, the required lengths of `bounds` and `assoc` are `n - 1`.
#'
#' The list `bounds` refers to values of the input attribute and partitions its scale in `n` intervals
#' `[-Inf, bound[[1]]]`, `[bound[[1]], bound[[2]]],` ..., `[bound[[n - 1]]], +Inf]`.
#' The list `values` then defines the output values for each interval.
#' The list `assoc` contains strings `"up"` or `"down"` that indicate to which interval,
#' lower or higher, belong the corresponding `bounds`.
#'
#'
#' @field attribute [DexiAttribute]. The attribute this function is associated with.
#' Requirements: `attribute` must be discrete (i.e., associated with a [DexiDiscreteScale]) and
#' must have exactly one continuous input attribute (i.e., associated with a [DexiContinuousScale]).
#'
#' @field values A list of output values corresponding to each interval defined by `bounds`.
#' List elements are in general value sets, i.e., integer vectors of value indices w.r.t. `attribute$scale`.
#'
#' @field bounds A vector of numeric values that partitions the input scale in intervals.
#'
#' @field assoc A vector of strings `"up"` or `"down"`.
#'  For each `i in 1:n-1`, `assoc[[i]]` indicates how to map the value of
#'  `bounds[[i]]`: to `value[[i]]` (`"down"`) or `value[[i + 1]]` (`"up"`).
#'
#' @export DexiDiscretizeFunction
#'
#' @examples
#' # Create a DexiDiscretizeFunction (without association to any attributes or scales)
#' fnc <- DexiDiscretizeFunction(bounds = c(-1, 2), values = list(1, 3, 5), assoc = c("up", "down"))
#'
#' # Print fields and basic properties of fnc
#'
#' fnc$verify()
#' fnc$nargs()
#' fnc$nvals()
#' fnc$to_string()
#'
#' fnc$bound_assoc(1)
#' fnc$bound_assoc(2)
#'
#' # Try some discretizations
#' sapply(c(-1.1, -1, 0, 1, 2, 3), fnc$evaluate)
#'
DexiDiscretizeFunction <- setRefClass(DexiDiscretizeFunctionClass,
   contains = DexiFunctionClass,

   fields = list(
     attribute = "ANY",
     values = "ANY",
     bounds = "ANY",
     assoc = "ANY"
   ),

   methods = list(

     initialize = function(attribute = NULL, bounds = NULL, assoc = NULL, values = NULL, ...) {
       "Initialize a \\code{DexiTabularFunction} object."

       stopifnot(is_class_or_null(attribute, DexiAttributeClass))
       vals <- values
       if (is.null(vals)) vals <- vector("list", length = length(bounds) + 1)
       if (!is.list(vals)) vals <- as.list(vals)
       nvals <- length(vals);
       nargs <- 1
       stopifnot(length(bounds) == (nvals - 1))
       if (!is.null(attribute)) {
         stopifnot(attribute$ninp() == nargs)
       }
       attribute <<- attribute
       values <<- vals
       bounds <<- bounds
       assoc <<- assoc
     },

     verify = function() {
      "Check the correctnes of this function object and its fields.
      Result: \\code{error()} or \\code{TRUE}."

       stopifnot(is_class_or_null(attribute, DexiAttributeClass))
       stopifnot(is.null(bounds) || is.numeric(unlist(bounds)))
       stopifnot(is.list(values))
       stopifnot(length(bounds) == nvals() - 1)
       TRUE
     },

     show = function () {
       "Prints out function's fields."

       list(values = values, bounds = bounds, assoc = assoc)
     },

     to_string = function() {
      "Return an informative string about this function's \\code{values} and \\code{bounds}."

       result <- vector("character", length = length(values) + length(bounds))
       j <- 1
       for (i in seq_increasing(1, length(values))) {
         result[[j]] <- values[[i]]
         j <- j + 1
         if (i <= length(bounds)) {
           result[[j]] <-
             if (assoc[[i]] == "up") paste0("[", bounds[[i]], ">")
             else paste0("<", bounds[[i]], "]")
           j <- j + 1
         }
       }
       paste(result, sep=" ", collapse=" ")
     },

     nargs = function() {
       "Return the number of function arguments."

       1
      },

     nvals = function() {
       "Return the length of \\code{values}."

       length(values)
     },

     bound_assoc = function(idx, default = "down") {
       "Given \\code{idx}, a \\code{bounds} index, return the corresponing association
       (\\code{\"down\"} or \\code{\"up\"})."

       result <- tryCatch(assoc[[idx]], error = function(e) default)
       if (!is_single_character((result))) result <- default
       result
     },

     value = function(x) {
       x <- x[[1]]
       if (is.null(x) || nvals() <= 0) {
         return(NULL)
       }
       else if (is.na(x)) {
         return(NA)
       }
       else if (nvals() == 1) {
         return(values[[1]])
       }
       else
       {
         lb <- -Inf
         lassoc <- bound_assoc(1)
         i <- 1
         while (i <= nvals()) {
           hb <- if (i < nvals()) bounds[[i]] else +Inf
           hassoc <- bound_assoc(i)
           if (is_in_range(x, lb, hb, lassoc = lassoc, hassoc = hassoc)) {
             return(values[[i]])
           }
           i <- i + 1
           lb <- hb
           lassoc <- hassoc
         }
       }
       NULL
     },

     evaluate = function(x) {
       catch_error(value(x))
     }

   )
)

#' dexi_index
#'
#' Return the index of argument vector `vec` in the decision space `dim`.
#' The index is calculated according to DEXi's sorting rules, which are different to R's.
#'
#' @param vec Integer vector, representing arguments of some decision rule.
#' @param dim Integer vector, representing dimensions of the corresponding decision space.
#'  Assumptions: `length(vec) == length(dim)` and, for each `i`,
#'  `1 <= vec[[i]] <= dim[[i]]`.
#'
#' @return Integer, index of `vec`.
#'
#' @export
#'
#' @examples
#' dexi_index(c(1,1,1), c(2,2,3))
#' dexi_index(c(1,1,2), c(2,2,3))
#' dexi_index(c(1,2,3), c(2,2,3))
#'
dexi_index <- function(vec, dim) {
  base <- 1
  index <- 0
  for (i in length(dim):1) {
    index <- index + base * (vec[i] - 1)
    base <- base * dim[i]
  }
  index + 1
}

#' dexi_table
#'
#' Create a representation of DEXi's decision table in R.
#'
#' @param low `character(1)`. A string normally read from a `.dxi` file, representing the lower bounds
#' of the corresponding decision rule values (assuming the order according to [dexi_index()]).
#' Notice that the string contains zero-based characters, which are converted to one-based integer values used in R.
#' @param high `character(1)` or `NULL`. A string representing the upper bounds
#'  of corresponding decision rule values. If `high = NULL`, `high` is assumed
#'  to be equal to `low`.
#' @param dim An integer vector, representing dimensions of the underlying decision space.
#'
#' @return `length(dim)`-dimensional matrix of rule values, which are normally single integer values,
#' but might also be sets of values. Each set is represented by a numeric vector.
#' @export
#'
#' @examples
#' # Converting DEXi's value strings to R's numeric vectors.
#' dexi_table(c(2, 3), "011012")
#' dexi_table(c(2, 3), "011012", "012112")
#'
dexi_table <- function(dim, low, high = NULL) {
  stopifnot(is_single_character(low))
  stopifnot(is_single_character_or_null(high))
  lowvals <- rule_values(low, 1)
  highvals <- if (is.null(high)) lowvals else rule_values(high, 1)

  n <- length(lowvals)
  stopifnot(n == length(highvals))
  stopifnot(n == prod(dim))
  table <- vector("list", length = n)
  for (i in seq_increasing(1, n)) {
    args <- arrayInd(i, dim, useNames = FALSE)
    x <- dexi_index(args, dim)
    table[[i]] <- lowvals[[x]]:highvals[[x]]
  }
  dim(table) <- dim
  table
}

#' make_args
#'
#' Make a list of all possible combinations of values in a decision space defined by `dim`.
#'
#' @param dim A numeric vector containing upper bounds of the corresponding decision space dimensions.
#' For example, `dim = c(3, 4)` defines the space of `3 * 4 == 12` combinations.
#'
#' @return A list containing all possible value combinations. List elements are numeric vectors
#' of length equal to `length(dim)`.
#' @export
#'
#' @examples
#' make_args(c(3, 4))
#'
make_args <- function(dim) {
  stopif(length(dim) == 0)
  stopifnot(all(dim > 0))
  len <- prod(dim)
  args <- vector("list", length = len)
  for (x in seq_increasing(1, len)) {
    arg <- arrayInd(x, dim)[1,]
    args[[x]] <- arg
  }
  args
}
