# A collection of various helper functions concerned with DEXi values.

#' @include DexiClasses.R
#' @include DexiUtils.R
#' @include DexiScales.R
NULL

# Helper functions for reading DexiTabularFunction

#' rule_value
#'
#' Values of decision rules are in `.dxi` files encoded using character strings, where each
#' individual character encodes some function value. The encoding is zero-based, so that
#' `"0"` represents the lowest ordinal number on the corresponding discrete scale.
#' `rule_value(char)` converts a single character to the corresponding ordinal value.
#'
#' @param ch A single character, such as `"3"` or `"Z"`.
#'
#' @return Corresponding integer value.
#'
#' @export
#'
#' @examples
#' rule_value("1")
#' rule_value("Z")
#'
rule_value <- function(ch) {
  utf8ToInt(ch) - utf8ToInt("0")
}

#' rule_values
#'
#' Values of decision rules are in `.dxi` files encoded using character strings, where each
#' individual character encodes some function value. The encoding is zero-based, so that
#' the character `"0"` represents the lowest ordinal number on the corresponding discrete scale.
#' Encoding of characters is according to ASCII, starting with `"0"`.
#' `rule_values(str)` converts the character string to a numeric vector of corresponding
#' ordinal values.
#'
#' @param str `character(1)`, a DEXi encoding of a vector of ordinal numbers.
#' @param add An integer constant to be added to the resulting vector. The default is `add = 0`,
#' however DEXi's ordinal numbers should normally be converted to R's using `add = 1`.
#'
#' @return A numeric vector of the same length as `str`.

#' @export
#'
#' @examples
#' rule_values("01122:")
#' rule_values("01122:", add = 1)
#'
rule_values <- function (str, add = 0) {
  result <- as.integer(sapply(unlist(strsplit(str, "")[[1]]), rule_value))
  if (add != 0) result <- result + add
  result
}

#' values_to_str
#'
#' Convert numbers to a DEXi string. Implements the reverse operation of [rule_values()].
#'
#' @param vals Numeric vector, containing ordinal values.
#' @param add  An integer constant to be added to `vals` prior to conversion.
#'
#' @return A string representing DEXi's representation of ordinal values. Fails when
#' `vals + add` contains negative numbers.
#' @export
#'
#' @examples
#' values_to_str(c(0, 1, 1, 2, 2, 10, 12))
#' values_to_str(c(1, 2, 2, 3, 3, 11, 13), -1)
#'
values_to_str <- function(vals, add = 0) {
  intToUtf8(vals + utf8ToInt("0") + add)
}

# Helper functions for reading other parts of .dxi files rather than functions


#' dexi_bool
#'
#' Convert a DEXi string to logical. `"TRUE"`, `"T"` and `"1"` are interpreted as `TRUE`,
#' all other strings as `FALSE`.
#'
#' @param x character(1).
#'
#' @return `logical(1)`.
#'
#' @export
#'
#' @examples
#' dexi_bool("TRUE")
#' sapply(c("TRUE", "T", "1", TRUE, 1, "FALSE", "F", "0", NULL, NA, NaN), dexi_bool)
#'
dexi_bool <- function(x) {
  if (is_single(x)) toupper(x) %in% c("TRUE", "T", "1") else FALSE
}

#' dexi_vector
#'
#' Interpret a string, composed of `";"`-separated numbers, as a numeric vector.
#'
#' @param x `character(1)`.
#'
#' @return Numeric vector.
#' @export
#'
#' @examples
#' dexi_vector("1;2")
#' dexi_vector("1.2; 2.3")
#'
dexi_vector <- function(x) {
  stopifnot(is_single(x) && is.character(x) && length(x) > 0)
  vals <- strsplit(x, ";", fixed = TRUE)
  as.numeric(vals[[1]])
}

#' dexi_value
#'
#' Conversion of a string to a "DEXi value" (see [DEXiR-package]) according to "new"
#' DEXi syntax rules. In `.dxi` files, this syntax is used in `ALTERNATIVE` and
#' `RULE` XML tags. Examples of possible options include:
#' \preformatted{
#'  x                              result
#'  ------------------------------+-------------------------------------------------
#'  NULL or ""                     NULL
#'  "*"                            "*"
#'  a string starting with "undef" NA
#'  "2"                            a single ordinal value, c(2) in this case
#'  "2.1"                          a single number, c(2.1) in this case
#'  "1:3"                          interval, equivalent to c(1, 2, 3)
#'  "{0;2;3}"                      a value set, equivalent to c(0, 2, 3)
#'  "<0;0.3;0.7>"                  a value distribution, distribution(0.0, 0.3, 0.7)
#' }
#'
#' @param x character(1).
#' @param add A numeric constant to be added to the result. Useful
#' when converting DEXi's zero-based representation to one-based representation used in R,
#' which requires the setting `add = 1`.
#'
#' @return A single integer or real number, an integer numeric vector, or a [distribution].
#' @export
#' @seealso [DEXiR-package], [dexi_option_value()], [distribution]
#'
#' @examples
#' dexi_value("")
#' dexi_value(NULL)
#' dexi_value("*")
#' dexi_value("UNDEF")
#' dexi_value("2")
#' dexi_value("2.1")
#' dexi_value("1:3")
#' dexi_value("{0;2;3}")
#' dexi_value("{0;2;3}", add = 1)
#' dexi_value("<0;0.3;0.7>")
#'
dexi_value <- function(x, add = 0) {
  if (is.null(x)) return(NULL)
  stopifnot(is_single(x) && is.character(x))
  if (x == "") return(NULL)
  if (x == "*") return("*")
  x <- tolower(x)
  if (startsWith(x, "undef")) return(NA)
  if (startsWith(x, "{") && endsWith(x, "}")) {
    # set
    set <- dexi_vector(substr(x[[1]], 2, nchar(x) - 1))
    stopifnot(is_all_integer(set))
    return (set + add)
  }
  else if (startsWith(x, "<") && endsWith(x, ">")) {
    # distribution
    distr <- distribution(dexi_vector(substr(x, 2, nchar(x) - 1)))
    return(distr)
  }
  else
  {
    lh <- strsplit(x, ":", fixed = TRUE)[[1]]
    if (length(lh) == 1) {
      # assuming single numeric
      value <- as.numeric(lh[1])
      return (value + add)
    }
    else
    {
      # assuming interval
      lvalue <- as.numeric(lh[1])
      hvalue <- as.numeric(lh[2])
      stopifnot(is_all_integer(lvalue) && is_all_integer(hvalue))
      return(seq(lvalue, hvalue) + add)
    }
  }
}

#' dexi_option_value
#'
#' Conversion of a string to a "DEXi value" (see [DEXiR-package]) according to "old"
#' DEXi syntax. In `.dxi` files, the old syntax is used with `OPTION` XML tags.
#' The reason for replacing the old with the new syntax (see [dexi_value()]) was
#' that the old syntax can not unambiguously represent value distributions.
#'
#' @param x `character(1)`. Contains a sequence of characters, each of which represents an individual ordinal number.
#'
#' @return A numeric vector. The conversion uses `rule_values(x, add = 1)`.
#' For special-type parameters, the conversion results are:
#' \preformatted{
#'  x                              result
#'  ------------------------------+------
#'  NULL                           NULL
#'  a non-character object         NA
#'  "" or "*"                      "*"
#'  a string starting with "undef" NA
#' }
#'
#' @seealso [DEXiR-package], [dexi_value()], [rule_value()]
#' @export
#'
#' @examples
#' dexi_option_value(NULL)
#' dexi_option_value(NA)
#' dexi_option_value("")
#' dexi_option_value("*")
#' dexi_option_value("undef")
#' dexi_option_value("1")
#' dexi_option_value("012")
#'
#'
dexi_option_value <- function (x) {
  if (is.null(x)) return(NULL)
  if (length(x) == 0) return(NA)
  if(!is_single(x) || !is.character(x)) return(NA)
  if (x == "") return("*")
  if (x == "*") return("*")
  x <- tolower(x)
  if (startsWith(x, "undef")) return(NA)
  rule_values(x, 1)
}

DistributionClass <- "distribution"

#' distribution
#'
#' Create an object as a S3 class `distribution`.
#'
#' @param ... Expected a comma-separated list of numeric values.
#'
#' @return An object, call it `obj`, such that `all(obj == c(...))` and
#'   `class(obj) == "distribution"`.
#' @export
#' @seealso [DEXiR-package], [set_to_distr()], [distr_to_set()]
#'
#' @examples
#' distribution(0.1, 0.2, 0.7)
#'
distribution <- function(...) {
  distr <- c(...)
  stopifnot(is.numeric(distr))
  class(distr) <- DistributionClass
  distr
}

#' is_distribution
#'
#' Checks whether `value` is of `DexDistributionClass` or not.
#'
#' @param value Any value or object to be checked.
#'
#' @return `logical(1)`. Returns `TRUE` if `value` is distribution.
#' @export
#'
#' @examples
#' is_distribution(NULL)
#' is_distribution(3)
#' is_distribution("text")
#' is_distribution(c(1,2,3))
#' is_distribution(distribution(1,0,2))
#'
is_distribution <- function(value) {
  inherits(value, DistributionClass)
}

#' set_to_distr
#'
#' Convert a DEXi value set to DEXi value distribution.
#'
#' @param set Normally a numeric vector containing integer numbers.
#' @param length The required length of the resulting distribution vector. The actual length
#' is determined as `max(length, max(set))`, so the `length` is extended when too small
#' to hold the whole distribution.
#'
#' @return A [distribution] object of length `length`. Arguments that
#' are already distributions are returned "as is". Input vectors of length 0 and other types
#' of objects return `NA`.
#'
#' @export
#' @seealso [DEXiR-package], [distribution], [distr_to_set()]
#'
#' @examples
#' set_to_distr(c(1, 3, 4))
#' set_to_distr(c(1, 3, 4), length = 5)
#' set_to_distr(c(1, 3, 4), length = 0)
#'
set_to_distr <- function(set, length = 0) {
  if(length(set) == 1 && is.na(set)) return(NA)
  if (inherits(set, DistributionClass)) return(set)
  if (length(set) == 1 && inherits(set[[1]], DistributionClass)) return(set[[1]])
  set <- unlist(set)
  stopifnot(is.numeric(set))
  if (!is_all_integer(set)) return(distribution(set))
  set <- set[set > 0]
  if (is_empty(set)) return(NA)
  len <- if (length > 0) length else max(1, max(set))
  distr <- rep(0.0, len)
  distr[set] <- 1.0
  distribution(distr)
}

#' distr_to_set
#'
#' Convert a DEXi value distribution to a DEXi value set.
#'
#' @param distr An S3 object of class `distribution`.
#' @param eps A numeric value representing the threshold value of \eqn{p} (see [DEXiR-package])
#' above which the corresponding elements are considered set members.
#'
#' @return A numeric vector determined as `which(distr > eps)`. Notice that `distr_to_set`
#' is generally a lossy conversion, so that multiple different `distr`s are converted to the same sets.
#' @export
#' @seealso [DEXiR-package], [distribution], [set_to_distr()]
#'
#' @examples
#' distr_to_set(distribution(0.2, 0, 0.5, 0.3))
#' distr_to_set(distribution(0.1, 0, 0.7, 0.2))
#' distr_to_set(distribution(0.1, 0, 0.7, 0.2), eps = 0.5)
#'
distr_to_set <- function(distr, eps = .Machine$double.eps) {
  if(!inherits(distr, DistributionClass)) return(distr)
  stopifnot(is.numeric(distr))
  which(distr > eps)
}

#' value_to_set
#'
#' @param value A DEXi value, internal representation: numeric value or vector, or [distribution].
#' @param scale A [DexiScale] or derived object.
#'
#' @return An integer vector or `NA` for: non-discrete scale, `NA`/`NULL` value(s), non-integer value(s).
#' @export
#'
#' @examples
#' scl <- DexiDiscreteScale(values = c("low", "med", "high"))
#' value_to_set(1, scl)                       # 1
#' value_to_set(1:2, scl)                     # c(1, 2)
#' value_to_set(c(1,3), scl)                  # c(1, 3)
#' value_to_set(distribution(1, 0, 0.5), scl) # c(1, 3)
value_to_set <- function(value, scale) {
  scale <- scale_of(scale)
  if (is_empty(value)) return(NA)
  stopifnot(is_class(scale, DexiScaleClass))
  if (!scale$is_discrete()) return(NA)
  if(inherits(value, DistributionClass)) value <- distr_to_set(value)
  value <- value[!is.na(value)]
  if (!is_all_integer(value)) return(NA)
  return(value)
}

#' scale_value
#'
#' Check and interpret `value` on `scale`.
#'
#' @param value A wide range of possible value types, including  integer, double, character and list vectors.
#' @param scale A [DexiScale] or derived object.
#'
#' @return The result is produced depending on `value` and `scale` according to the following tables.
#' For any scale type:
#' \preformatted{
#' value                          result
#' ------------------------------+------------------
#' NULL                           NULL
#' length(value == 0)             NULL
#' NA                             scale$full_range()
#' other types                    ERROR
#' value contains any NULL or NA  ERROR
#' ------------------------------+------------------
#' }
#'
#' For continuous scales:
#' \preformatted{
#' value                          result
#' ------------------------------+------------------
#' length(value != 1)             ERROR
#' character                      ERROR
#' named object                   ERROR
#' length(value == 1)             unclass(value)
#' ------------------------------+------------------
#' }
#'
#' For discrete scales:
#' \preformatted{
#' value                          result
#' ------------------------------+------------------
#' distribution class             value
#' all-integer numeric vector     value
#' non all-integer numeric vector distribution(value)
#' "*" or "undef"...              scale$full_range()
#' list of value names            matched value set
#' list of name=p                 distribution(value)
#' ------------------------------+------------------
#' }
#'
#' @export
#'
#' @examples
#' # Examples of successfully checked (witout error) values on a continuous scale
#' scl <- DexiContinuousScale()
#' scale_value(NULL, scl)               # NA
#' scale_value(c(), scl)                # NA
#' scale_value(list(), scl)             # NA
#' scale_value(character(), scl)        # NA
#' scale_value(NA, scl)                 # NA
#' scale_value(c(NA), scl)              # NA
#' scale_value(15.5, scl)               # 15.5
#' scale_value(distribution(15.5), scl) # 15.5
#'
#' # Examples of successfully checked (without error) values on a discrete scale
#' scl <- DexiDiscreteScale(values = c("low", "med", "high"))
#' scale_value(NULL, scl)                        # NA
#' scale_value(c(), scl)                         # NA
#' scale_value(list(), scl)                      # NA
#' scale_value(NA, scl)                          # NA
#' scale_value("*", scl)                         # 1:3
#' scale_value("Undefined", scl)                 # 1:3
#' scale_value(2, scl)                           # 2
#' scale_value(c(-1, 2, 4), scl)                 # c(-1, 2, 4))
#' scale_value(distribution(c(-1, 2, 4)), scl)   # distribution(c(-1, 2, 4)))
#' scale_value(c(-1, 2.2, 4), scl)               # distribution(c(-1, 2.2, 4)))
#' scale_value("high", scl)                      # 3
#' scale_value(c("low", "high"), scl)            # c(1,3))
#' v <- c(0.5, 0.4)
#' names(v) <- c("low", "high")
#' scale_value(v, scl)                           # distribution(c(0.5, 0, 0.4)))
#' scale_value(list(high = 1.1, low = 2.2), scl) # distribution(c(2.2, 0, 1.1)))
#'
scale_value <- function(value, scale) {
  scale <- scale_of(scale)
  if (is_empty(value)) return(NA)
  stopifnot(is_class(scale, DexiScaleClass))
  len <- length(value)
  discrete_scale <- scale$is_discrete()
  continuous_scale <- scale$is_continuous()
  stopifnot(discrete_scale | continuous_scale)
  type <- typeof(value)
  stopifnot(type %in% c("integer", "double", "character", "list"))
  stopifnot(all(!is_empty(value)))
  stopifnot(all(!is.na(value)))
  names <- names(value)
  if (continuous_scale) {
    stopifnot(continuous_scale, is.null(names), is_single_numeric(value))
    class(value) <- NULL
    return(value)
  }
  # discrete_scale
  if (is.null(names)) {
    # unnamed value
    if (is.numeric(value)) {
      if (inherits(value, DistributionClass)) return(value)
      else if (is_all_integer(value, na.rm = FALSE)) return(value)
      else return(distribution(value))
    }
    if (is.character(value)) {
      if (len == 1) if (value == "*" | startsWith(tolower(value), "undef")) return(scale$full_range())
      match <- match(value, scale$values)
      if (length(match) == 0 | any(is.na(match))) stop("Unknown value(s) ", paste(value, collapse = ", "), " for scale ", scale$to_string())
      return(match)
    }
    stop("Cannot determine scale_value of ", paste(value, collapse = ", "), " on ", scale$to_string())
  }
  else
  {
    #named value
    stopifnot(discrete_scale, is.numeric(unlist(value)), max(lengths(value)) == 1)
    match <- match(names, scale$values)
    if (length(match) == 0 | any(is.na(match))) stop("Value name(s) ", paste(names, collapse = ", "), " do not match scale ", scale$to_string())
    distr <- rep(0.0, scale$count())
    distr[match] <- unlist(value)
    distr <- unlist(distr)
    return(distribution(distr))
  }
}

#' scale_values
#'
#' A vectorized version of `scale_value`.
#'
#' @param values A list of values. For possible value types, see [scale_value()].
#' @param scale A [DexiScale] or derived object.
#'
#' @return A list determined as `lapply(values, function (v) scale_value(v, scale))`.
#' @seealso [scale_value()]
#' @export
#'
scale_values <- function(values, scale) {
  lapply(values, function (v) scale_value(v, scale))
}

#' bounded_scale_value
#'
#' `bounded_scale_value` is a wrapper around [scale_value()] that makes sure that
#' the resulting values lie within the bounds set up by the `scale`.
#'
#' @param value Any DEXi value, including value sets and distributions.
#' @param scale A [DexiScale] or derived object.
#'
#' @return For continuous scales, `value` is returned "as is". For discrete scales, all elements
#' of `value` that lie outside of `scale$full_range()` are removed. If this results in
#' an empty value set or distribution, `NULL` is returned.
#'
#' @export
#' @seealso [scale_value()]
#'
#' @examples
#' scl <- DexiDiscreteScale(values = c("low", "med", "high"))
#' bounded_scale_value(NA, scl)                               # NA
#' bounded_scale_value(1, scl)                                # 1
#' bounded_scale_value(4, scl)                                # NULL
#' bounded_scale_value(c(0, 1, 3,  4, 5), scl)                # c(1, 3)
#' bounded_scale_value(distribution(0.1, 0.2, 0.3, 0.4), scl) # distribution(0.1, 0.2, 0.3)
#'
bounded_scale_value <- function(value, scale) {
  scale <- scale_of(scale)
  value <- scale_value(value, scale)
  continuous_scale <- inherits(scale, DexiContinuousScaleClass)
  if (continuous_scale) return(value)
  len <- length(value)
  distr <- inherits(value, DistributionClass)
  nvals <- scale$count()
  if (distr) {
    if (length(value) > nvals) return(distribution(value[1:nvals]))
    else return(value)
  }
  else {
    value <- value[value >= 1 & value <= nvals]
    if (length(value) == 0) value <- NULL
    return(value)
  }
}

#' value_text
#'
#' Converts a DEXi `value` to a human-readable character string that can be printed.
#' Used, for instance, by `DexiModel$as_character()`.
#'
#' @param value Any DEXi value type (see [DEXiR-package]).
#' @param scale A [DexiScale] or derived object.
#' @param round An integer number. Indicates the number of decimals for
#' rounding numeric values prior to printing. If `NULL`, no rounding takes place.
#'
#' @return character.
#' @export
#'
#' @examples
#' scl <- DexiDiscreteScale(values = c("low", "med", "high"))
#' value_text(NA, scl)
#' value_text(1, scl)
#' value_text(c(1, 3), scl)
#' value_text(distribution(0.1, 0.2, 0.3), scl)
#'
value_text <- function(value, scale, round = NULL) {
  scale <- scale_of(scale)
  if (is.null(value)) return("NULL")
  if (length(value) == 0) return(flat_text(value))
  if (is.character(value)) return(flat_text(value))
  if (any(is.na(value))) return(flat_text(value))
  if (is.logical(value)) return(flat_text(value))
  if (scale$is_continuous()) return(flat_text(value))
  if (inherits(value, DistributionClass) || is.list(value)) {
    names <- names(value)
    value <- unlist(value)
    if (length(names) == 0) {
      names(value) <- scale$values[1:length(value)]
      names <- names(value)
    }
    result <- ""
    for (i in seq_increasing(1, length(names))) {
      val <- value[[i]]
      if (val != 0) {
        if (!is.null(round)) val <- round(val, round)
        text <- paste0("'", names[[i]], "'=", val)
        result <- if (result == "") text else paste(result, text, sep = ", ")
      }
    }
    return(result)
  }
  else
  {
    value <- unlist(value)
    value[value <= 0 | value != as.integer(value)] <- NA
    full <- sort(scale$full_range())
    if (length(value) == length(full) && (all(sort(value) == full))) return("*")
    return(paste(scale$values[value], collapse = ", "))
  }
}

#' export_dexi_value
#'
#' Convert a DEXi `value` to a character string that is understood by DEXi/DEXiWin software
#' while importing data about alternatives.
#'
#' @param value A DEXi value: `NA`, `NULL`, a single number, integer vector (a set) or a distribution.
#'
#' @return A string representation of `value`.
#' @export
#'
#' @examples
#' export_dexi_value(NULL)                            # "<undefined>"
#' export_dexi_value(NA)                              # "<undefined>"
#' export_dexi_value(1)                               # "1"
#' export_dexi_value(3.2)                             # "3.2"
#' export_dexi_value(c(1, 3, 5))                      # "1;3;5"
#' export_dexi_value(distribution(0.1, 0.9))          # "1/0.1;2/0.9"
#' export_dexi_value(distribution(0, 0.1, 0, 0.9, 0)) # "2/0.1;4/0.9"
#'
export_dexi_value <- function(value) {
  if (is.null(value)) "<undefined>"
  else if (length(value) <= 0) "<undefined>"
  else if (length(value) == 1) {
    if (is.na(value)) "<undefined>" else toString(value)
  }
  else if (is_distribution(value)) {
    s <- sapply(seq_increasing(1, length(value)), function(x) if (value[x] <= 0) NA else paste0(x, "/", value[x]))
    s <- s[!is.na(s)]
    paste0(s, sep="", collapse = ";")
  }
  else paste0(as.character(unlist(value)), sep="", collapse = ";")
}

# Normalization functions

#' norm_none
#'
#' A "do nothing" normalization function.
#'
#' @param values A numeric vector.
#'
#' @return Returns unchanged `values`.
#' @export
#' @seealso [norm_max()], [norm_sum()]
#'
#' @examples
#' norm_none(c(0, 0.5, 0.7))
#'
norm_none <- function(values) {
  values
}

#' norm_max
#'
#' Normalize `values` so that `max(values) == max`.
#'
#' @param values A numeric vector.
#' @param max `numeric(1)`.
#'
#' @return `values` normalized so that `max(result) == max`.
#' Returns unchanged `values` when `max(values) == 0`.
#' @export
#' @seealso [norm_none()], [norm_sum()]
#'
#' @examples
#' norm_max(c(0, 0.5, 0.7))
#'
norm_max <- function (values, max = 1) {
  if (length(values) == 0) return(values)
  stopifnot(is.numeric(values))
  max_values <- max(values)
  if (is.na(max_values)) return(NA)
  if (max_values == 0) return(values)
  result <- values * (max/max_values)
  class(result) <- class(values)
  result
}

#' norm_sum
#'
#' Normalize `values` so that `sum(values) == sum`.
#'
#' @param values A numeric vector.
#' @param sum `numeric(1)`.
#'
#' @return `values` normalized so that `sum(result) == sum`.
#' Returns unchanged `values` when `sum(values) == 0`
#' @export
#' @seealso [norm_none()], [norm_max()]
#'
#' @examples
#' norm_sum(c(0, 0.5, 0.7))
#'
norm_sum <- function (values, sum = 1) {
  if (length(values) == 0) return(values);
  stopifnot(is.numeric(values))
  sum_values <- sum(values)
  if (is.na(sum_values)) return(NA)
  if (sum_values == 0) return(values)
  result <- values * (sum/sum_values)
  class(result) <- class(values)
  result
}

#' value_qualities
#'
#' Returns a vector of qualities corresponding to consecutive elements of `value`.
#' In contrast with `DexiScale$value_quality(value)`, which can handle only single values,
#' this function can handle `value` arguments that contain multiple elements,
#' such as value sets and distributions.
#'
#' @param value A DEXi value, internal representation: numeric value or vector, or [distribution].
#' @param scale A [DexiScale] or derived object.
#'
#' @return A vector consisting of `EnumQuality` elements corresponding to individual `value` elements.
#' @export
#'
#' @examples
#' scl <- DexiDiscreteScale(values = c("low", "med", "high"))
#' value_qualities(1, scl)       # "bad"
#' value_qualities(1:3, scl)     # c("bad", "none", "good")
#' value_qualities(c(3, 2), scl) # c("good", "none")
value_qualities <- function(value, scale) {
  scale <- scale_of(scale)
  stopifnot(is_class(scale, DexiScaleClass))
  if (scale$is_discrete() && inherits(value, DistributionClass)) value <- distr_to_set(value)
  sapply(un_list(value), scale$value_quality)
}

#' has_quality
#'
#' @param quality A character string from `EnumQuality`.
#' @param value A DEXi value.
#' @param scale A [DexiScale] or derived object.
#'
#' @return `logical(1)`. Whether or not `value_qualities(value, scale)` contains `quality`.
#' @export
has_quality <- function(quality = EnumQuality, value, scale) {
  quality <- match.arg(quality)
  quals <- value_qualities(value, scale)
  return(quality %in% quals)
}

#' has_bad
#'
#' @param value A DEXi value.
#' @param scale A [DexiScale] or derived object.
#'
#' @return `logical(1)`. Whether or not `value_qualities(value, scale)` contains `"bad"`.
#' @export
#'
has_bad <- function(value, scale) {
  return(has_quality("bad", value, scale))
}

#' has_good
#'
#' @param value A DEXi value.
#' @param scale A [DexiScale] or derived object.
#'
#' @return `logical(1)`. Whether or not `value_qualities(value, scale) contains `"good"`.
#' @export
#'
has_good <- function(value, scale) {
  return(has_quality("good", value, scale))
}

#' has_none
#'
#' @param value A DEXi value.
#' @param scale A [DexiScale] or derived object.
#'
#' @return `logical(1)`. Whether or not `value_qualities(value, scale)` contains `"none"`.
#' @export
#'
has_none <- function(value, scale) {
  return(has_quality("none", value, scale))
}

#' compare_values
#'
#' Compare two DEXi values.
#' Internal representation is assumed for `value1` and `value2`, i.e.,
#' a single number, an integer vector representing a set or [distribution()].
#' Distributions are compared as sets.
#'
#' @param value1 First value.
#' @param value2 Second value.
#'
#' @return `0` if values are equal, `-1` if `value1 < value2`, `+1` if `value1 > value2`
#' and `NA` if values are incomparable. Values are incomparable if they are of a non-DEXiValue
#' type or if they represent two overlapping sets.
#' @export
#'
#' @examples
#' compare_values(c(1,2), c(1,2))       # 0
#' compare_values(c(1,2), c(1,3))       # NA
#' compare_values(c(1,2), c(3,4))       # -1
#' compare_values(c(1,2), c(2,4))       # NA
#' compare_values(c(1,2), c(2.1,4))     # -1
#' compare_values(c(1,2.05), c(2.1,4))  # -1
#' compare_values(c(3,4), c(3,4))       # 0
#' compare_values(c(5,5), c(3,4))       # +1
#' compare_values(c(5,5), 2)            # +1
#' compare_values(c(5,2), 2)            # NA
#' compare_values(c(5,3), 2)            # +1
#' compare_values(distribution(5,3), 2) # NA
#' compare_values(distribution(5,3), 5) # -1
compare_values <- function (value1, value2) {
  value1 <- distr_to_set(value1)
  value2 <- distr_to_set(value2)
  if (is.numeric(value1) && is.numeric(value2)) {
    if (all(value1 == value2)) 0
    else {
      l1 <- min(value1)
      h1 <- max(value1)
      l2 <- min(value2)
      h2 <- max(value2)
      if (h1 < l2) -1
      else if (h2 < l1) 1
      else NA
    }
  }
  else NA
}

#' compare_values_by_preference
#'
#' Compare values, considering preference `order`. For value arguments, see [compare_values()].
#'
#' @param value1 First value.
#' @param value2 Second value.
#' @param order `EnumOrder`, i.e., one of the strings `"ascending", "descending", "none"`.
#'
#' @return [compare_values()] result, modified according to `order`.
#' Results `0` (equal values) and `NA` (incomparable values) are always retained.
#' Results `-1` and `+1` are retained when `order="ascending"` and
#' reversed when `order="descending"`.
#' When `order="none"`, non-equal values return `NA`.
#' @export
#' @seealso [compare_values()]
#'
#' @examples
#' compare_values_by_preference(1, 1, "none")       # 0
#' compare_values_by_preference(1, 2, "none")       # NA
#' compare_values_by_preference(3, 2, "none")       # NA
#' compare_values_by_preference(1, 1, "ascending")  # 0
#' compare_values_by_preference(1, 2, "ascending")  # -1
#' compare_values_by_preference(3, 2, "ascending")  # +1
#' compare_values_by_preference(1, 1, "descending") # 0
#' compare_values_by_preference(1, 2, "descending") # +1
#' compare_values_by_preference(3, 2, "descending") # -1
compare_values_by_preference <- function (value1, value2, order = EnumOrder) {
  order <- match.arg(order)
  result <- compare_values(value1, value2)
  if (!is.na(result) && result != 0) {
    if (order == "ascending") {}
    else if (order == "descending") {result <- -result}
    else {result <- NA}
  }
  return(result)
}

#' compare_values_on_scale
#'
#' Compare values `value1` and `value2` considering `scale$order`.
#' Internal DEXi representation is assumed for values, i.e.,
#' a single number, an integer vector representing a set or [distribution()].
#' Distributions are compared as sets.
#'
#' @param value1 First value.
#' @param value2 Second value.
#' @param scale Normally a `DEXiScale` object or a `DexiAttribute` object with defined `$scale`.
#' @param force_compare `logical(1)`. Applies when `scale` is `NULL` or anything other than expected.
#' When `force_compare = TRUE`, comparison is enforced, assuming `"ascending"` scale order.
#' When `force_compare = FALSE`, NA is returned.
#'
#' @return [compare_values()] result, modified according to `scale$order`.
#' @export
#' @seealso [compare_values()], [compare_values_by_preference()]
#'
#' @examples
#' compare_values_on_scale(1, 2, NULL)                       # NA
#' compare_values_on_scale(2, 1, "")                         # NA
#' compare_values_on_scale(1, 2, NULL, force_compare = TRUE) # -1
#' compare_values_on_scale(2, 1, "", force_compare = TRUE)   # +1
#'
#' scl <- DexiDiscreteScale(values = c("a", "b", "c"))
#' compare_values_on_scale(1, 1, scl)             # 0
#' compare_values_on_scale(1, 2, scl)             # -1
#' compare_values_on_scale(3, 2, scl)             # +1
#' compare_values_on_scale(c(1, 2), c(1, 2), scl) # 0
#' compare_values_on_scale(c(1, 2), c(2, 3), scl) # NA
#'
#' scl <- DexiDiscreteScale(order = "descending", values = c("a", "b", "c"))
#' compare_values_on_scale(1, 1, scl)             # 0
#' compare_values_on_scale(1, 2, scl)             # +1
#' compare_values_on_scale(3, 2, scl)             # -1
#' compare_values_on_scale(c(1, 2), c(1, 2), scl) # 0
#' compare_values_on_scale(c(1, 2), c(2, 3), scl) # NA
#'
#' scl <- DexiDiscreteScale(order = "none", values = c("a", "b", "c"))
#' compare_values_on_scale(1, 1, scl)             # 0
#' compare_values_on_scale(1, 2, scl)             # NA
#' compare_values_on_scale(3, 2, scl)             # NA
#' compare_values_on_scale(c(1, 2), c(1, 2), scl) # 0
#' compare_values_on_scale(c(1, 2), c(2, 3), scl) # NA
compare_values_on_scale <- function(value1, value2, scale, force_compare = FALSE) {
  scl <- scale_of(scale)
  result <-
    if (is_class(scl, DexiScaleClass)) compare_values_by_preference(value1, value2, scl$order)
    else if (force_compare) compare_values(value1, value2)
    else NA
  return(result)
}

#' expand_value_to_points
#'
#' Expand a DEXi `value` to a sequence of individual elements (points).
#' Particularly aimed for graphic functions that display DEXi values
#' with dots of different sizes and colors.
#'
#' @param value A DEXi value: a single value (integer or float), a set (integer vector) or a [distribution].
#' @param scale A [DexiScale] object.
#' @param colors `numeric(3)` representing colors to display "bad", "neutral" and "good" values, respectively.
#'
#' @return A `data.frame` consisting of:
#' \describe{
#' \item{`points`}{`numeric()`. `value` expanded to a vector of ordinal values.}
#' \item{`sizes`}{`numeric()`. Numeric values assigned to each corresponding ordinal values.
#' Normally `1.0` for set elements and in the `(0,1]` interval for distribution membership values.}
#' \item{`colors`}{Colors assigned to corresponding value qualities.}
#' }
#' @export
#'
#' @examples
#' scl <- DexiDiscreteScale(values = c("L", "M", "H"))
#'
#' expand_value_to_points(c(1, 3), scl)
#' # points sizes colors
#' # 1      1     1    red
#' # 2      3     1  green
#'
#' expand_value_to_points(distribution(0.1, 0, 0.9), scl)
#' # points sizes colors
#' # 1      1   0.1    red
#' # 2      3   0.9  green
#'
expand_value_to_points <- function(value, scale, colors = c("red", "black", "green")) {
  scale <- scale_of(scale)
  if (is.null(value)) return(NULL)
  if (is_empty(value)) return(NA)
  if (is_single(value) && is.na(value)) return(NA)
  if (is.list(value) && length(value) == 1) value <- value[[1]]
  if (is_single_numeric(value)) {
    points <- value
    sizes <- 1.0
  }
  else if (is_distribution(value)) {
    points <- distr_to_set(value)
    sizes <- as.numeric(un_list(value[points]))
  }
  else {
    points <- un_list(value)
    sizes <- rep(1.0, length(points))
  }
  colors <- sapply(points, function (v) {
    q <- scale$value_quality(v)
    if (q == "bad") colors[1]
    else if (q == "good") colors[3]
    else colors[2]
  })
  data.frame(points = points, sizes = sizes, colors = colors)
}
