# A collection of various helper functions used in DEXiR.

# Type & Class checking

is_single <- function(obj) {
  length(obj) == 1
}

is_all_integer <- function(obj, na.rm = TRUE) {
  if (is.list(obj)) obj <- unlist(obj)
  result <- suppressWarnings(all(obj == as.integer(obj), na.rm = na.rm))
  if (is.na(result)) result <- FALSE
  result
}

is_single_character <- function(str) {
  (is.character(str) && is_single(str))
}

is_single_character_or_null <- function(str) {
  (is.null(str) || is_single_character(str))
}

is_single_numeric <- function(num) {
  (is.numeric(num) && is_single(num))
}

is_single_integer <- function(num) {
  (is_single(num) && is.numeric(num) && num == as.integer(num))
}

is_empty <- function(obj) {
  (length(obj) == 0 || identical(obj, NA))
}

is_class <- function(obj, classes) {
  inherits(obj, classes)
}

is_class_or_null <- function(obj, classes) {
  (is.null(obj) || is_class(obj, classes))
}

# List helpers

un_list <- function (list) {
  if (is_empty(list)) NA else if (is_single(list)) list[[1]] else list
}

# Program flow helpers

stopif <- function(cond) {
  stopifnot(!cond)
}

catch_error <- function (code, on_error = NULL) {
  tryCatch(
    code,
    error = function(e) on_error
  )
}

# Sequence generators

seq_increasing <- function(lb, ub, by = 1) {
  if (ub < lb)
    c()
  else
    seq(lb, ub, by = by)
}

seq_decreasing <- function(ub, lb, by = -1) {
  if (ub < lb)
    c()
  else
    seq(ub, lb, by = by)
}

# Range checking

#' is_in_range
#'
#' Check whether or not `x` lies the specified range.
#'
#' @param x Any object type, but using a non-numeric argument always returns `FALSE`.
#' @param lb `numeric(1)`. Lower bound of the interval.
#' @param hb `numeric(1)`. Ipper bound of the interval.
#' @param lassoc `"up"` or `"down"`, indicating whether `lb` is included
#' in the `[lb:hb]` interval (`"up"`) or not (`"down"`). The default is `"up"`.
#' @param hassoc `"up"` or `"down"`, indicating whether `hb` is included
#' in the `[lb:hb]` interval (`"down"`) or not (`"up"`). The default is `"down"`.
#'
#' @return `logical(1)`, indicating whether or not `x` lies in the interval `[lb:hb]`
#' according to function arguments.
#'
#' @export
#' @examples
#' is_in_range(3, 2, 5)
#' is_in_range(7, 2, 5)
#' is_in_range(3, 3, 5)
#' is_in_range(3, 3, 5, lassoc = "down")
#'
is_in_range <- function(x, lb, hb, lassoc = c("up", "down"), hassoc = c("down", "up")) {
  lassoc <- match.arg(lassoc)
  hassoc <- match.arg(hassoc)
  if (!is_single(x) || is.na(x) || is.null(x) || !is.numeric(x)) return(FALSE)
  lc <- x > lb
  if (!lc && lassoc == "up") lc <- x == lb
  hc <- x < hb
  if (!hc && hassoc == "down") hc <- x == hb
  lc && hc
}

# String helpers

# Unique IDs generator

#' unique_names
#'
#' Convert `names` strings to ID strings that are unique and conformant
#' with R's syntactic rules for variable names.
#'
#' @param names `character()`. Names to be converted to IDs.
#' @param reserved `character()`. Reserved names that should not be used as IDs.
#'
#' @return `character()`.
#' @seealso [base::make.unique()]]
#'
unique_names <- function(names, reserved = c()) {
  names <- c(reserved, names)
  result <- make.unique(make.names(names))
  result[(1 + length(reserved)):length(result)]
}

#' flat_text
#'
#' "Flatten" the function argument using `c(value)`, concatenate the elements and separate them by
#' a single space.
#'
#' @param value Any object that can occur as an argument of [c()] and [as.character()].
#'
#' @return `character(1)`.
#' @export
#'
flat_text <- function(value) {
  paste(as.character(c(value)), sep = " ", collapse = " ")
}

# Numeric value mapping

#' lin_map
#'
#' Map value `x` linearly from interval `[imin:imax]` to `[omax:omax]`.
#'
#' @param x `numeric()`. Value(s) to be mapped.
#' @param imin `numeric()`. Lower bound of the input range.
#' @param imax `numeric()`. Upper bound of the input range.
#' @param omin `numeric()`. Lower bound of the output range.
#' @param omax `numeric()`. Upper bound of the output range.
#'
#' @return `numeric()`. Mapped value(s).
#' @export
#'
#' @examples
#' lin_map(2, 1, 3) # 0.5
lin_map <- function(x, imin, imax, omin = 0.0, omax = 1.0) {
  k <- (omax - omin) / (imax - imin)
  n <- omin - k * imin
  k * x + n
}

#' reverse_value
#'
#' Numeric value(s) `x` are assumed to lie within the `[lb:hb]` interval.
#' The function "reverses" `x` linearly so that `x = lb` maps to `hb` and `x = hb` maps to `lb`.
#' In DEXiR, this function is used to reverse values defined on a [DexiScale] from
#' "ascending" to "descending" order or vice versa.
#'
#' @param x `numeric()`. Value(s) to be reversed.
#' @param lb `numeric()`. Lower interval bound(s).
#' @param hb `numeric()`. Upper interval bound(s).
#'
#' @return `numeric()`. Reversed `value`.
#' @export
#'
#' @examples
#' reverse_value(1, 1, 5) # 5
#' reverse_value(3, 1, 5) # 3
#' reverse_value(5, 1, 5) # 1
#' reverse_value(c(1, 3, 5), 1, 5) # c(5, 3, 1)
reverse_value <- function(x, lb, hb){
  hb + lb - x
}
