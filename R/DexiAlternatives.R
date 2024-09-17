#' @include DexiClasses.R
NULL

#' set_alternative
#'
#' Set values of a single decision alternative and represent it with a data frame.
#' Usually, only input values are set in this way. The data frame can then be
#' [evaluate]d to set the values of output attributes.
#'
#' @param model A [DexiModel] object. Required.
#' @param alternative `character(1)` or `data.frame`. The first form sets the name of the
#' newly created decision alternative. The second form copies values from `alternative[1, ]`
#' to initialize the corresponding columns of the resulting data frame.
#' @param ... A list of parameters specifying the values of the newly created decision alternative.
#' Each parameter is expected to be in the form `attribute_id=attribute_value`, or is a list of
#' elements of the same form.
#'
#' There are several possible ways to specify `attribute_value`.
#' Taking the scale \code{CAR = \{"unacc"; "acc"; "good"; "exc"\}} as an example, the options are:
#' \describe{
#'   \item{\code{CAR="unacc"}}{A single qualitative value.}
#'   \item{\code{CAR=2}}{An ordinal number, indicating \code{"acc"} in this case.}
#'   \item{\code{CAR=c("good", "exc")}}{A set of qualitative values.}
#'   \item{\code{CAR=c(3, 4)}}{A set of ordinal numbers, equivalent to the above.}
#'   \item{\code{CAR=list("good", 4)}}{A set specified by a mixture of qualitative values and ordinal numbers.}
#'   \item{\code{CAR="*"}}{A full range of ordinal numbers, in this case equivalent to \code{1:4}.}
#'   \item{\code{CAR=distribution(0, 0, 0.7, 0.3)}}{A value distribution.}
#'   \item{\code{CAR=list("good"=0.7, "exc"=0.3)}}{A value distribution, equivalent to the above.}
#'   \item{\code{CAR="undef"}}{An unknown value, interpreted as \code{NA}.}
#' }
#' For attributes associated with continuous scales,
#' only `numeric(1)` `attribute_value`s are allowed.
#'
#' @return A one-row data frame with columns corresponding to `model`'s attributes, collectively
#' representing a single decision alternative. The columns not copied from `alternative` (as a data frame)
#' nor set by any parameter contain `NA`s.
#'
#' @seealso [DEXiR-package] notes on values in DEXi models.
#'
#' @export
#'
set_alternative <- function(model, alternative, ...) {
  stopifnot(is_class(model, DexiModelClass))
  alt <-
    if (is_single_character(alternative)) model$alternative(alternative)
    else alternative[1,]
  if(missing(...)) return(alt)
  values <- list(...)
  if (length(values) == 1 && is.list(values[[1]])) values <- values[[1]]
  valnames <- names(values)
  for (i in seq_increasing(1, length(values))) {
    val <- values[[i]]
    if (is.data.frame(val)) {
      stopifnot(nrow(val) >= 1)
      vals <- as.list(val[1,])
      alt <- set_alternative(model, alt, vals)
    }
    else if (is.list(val)) {
      alt <- set_alternative(model, alt, val)
    } else {
      attname <- valnames[[i]]
      att <- model$attrib(attname)
      if (is_empty(att)) {
        if (attname %in% names(alt)) alt[[attname]] <- val
        else warning('Unmached attribute name "', attname, '" ignored')
      }
      else
      {
        alt[[attname]] <- vector("list", length = 1)
        alt[[1, attname]] <- scale_value(val, att)
      }
    }
  }
  alt
}

#' alt_values
#'
#' Make a list of `alternative`'s values corresponding to `attributes`.
#'
#' @param alt `data.frame` representing a single alternative.
#' @param attributes A vector of [DexiAttribute] objects.
#' @param as_character `logical(1).`. Determines whether to represent alternative values numerically
#' ("internal representation") (`FALSE`) or as character strings (using [value_text()]) (`TRUE`).
#' @param round A single integer. An optional argument to [value_text()].
#'
#' @return `character(length(attributes))`. String representation of `alt`'s values.
#' @export
#' @seealso [value_text()]
#'
#' @examples
#' # Load "Car.dxi"
#' CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
#' Car <- read_dexi(CarDxi)
#'
#' unlist(alt_values(Car$alternatives[1,], Car$attributes, as_character = TRUE))
#' # c("NULL", "exc", "low", "medium", "low", "exc", "high", "more", "4", "big", "high")
alt_values <- function(alt,
                       attributes,
                       as_character = TRUE,
                       round = NULL) {
  lapply(attributes, function (att) {
    value <- alt[[att$id]][[1]]
    result <-
      if (as_character) value_text(value, att$scale, round)
    else toString(value)
    return(result)
  })
}

#' compare_two_alternatives
#'
#' Compare alternatives `alt1` and `alt2` with respect to `attributes`.
#'
#' @param alt1 `data.frame`. First alternative.
#' @param alt2 `data.frame`. Second alternative.
#' @param attributes Vector of [DexiAttribute] objects.
#' @param deep `logical(1)`. When `TRUE` and compared values are equal,
#' input attributes are additionally investigated for possible preferential differences.
#'
#' @return `numeric(length(attributes))`.
#' Each element represents the outcome of comparison w.r.t. the corresponding attribute.
#' Possible outcomes:
#' \describe{
#' \item{`0`}{Values are equal.}
#' \item{`-1`}{`alt1`'s value is worse than `alt2`'s.}
#' \item{`+1`}{`alt1`'s value is better than `alt2`'s.}
#' \item{`NA`}{Values are incomparable.}
#' }
#'
#' When `deep = TRUE`, the so-called deep comparison is performed:
#' when the compared attribute's values are equal,
#' subordinate attributes are checked for differences, possibly returning
#' `-0.5` (indicating the weak preference relation "<=") or
#' `+0.5` (indicating the weak preference relation ">=").
#' @export
#' @seealso [compare_values_on_scale()]
#'
#' @examples
#' # Load "Car.dxi"
#' CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
#' Car <- read_dexi(CarDxi)
#'
#' compare_two_alternatives(Car$alternatives[1,], Car$alternatives[2,], Car$attributes)
#' # c(NA, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1)
compare_two_alternatives <- function(alt1, alt2, attributes, deep = TRUE) {
  sapply(attributes, function (att) {
    stopifnot(is_class(att, DexiAttributeClass))
    scl <- scale_of(att)
    value1 <- alt1[[att$id]][[1]]
    value2 <- alt2[[att$id]][[1]]
    comp <- compare_values_on_scale(value1, value2, scl, force_compare = TRUE)
    if (deep && !is.na(comp) && comp == 0 && att$is_aggregate()) {
      comp_inp <- sapply(att$inputs, function (inp) {
        compare_values_on_scale(alt1[[inp$id]][[1]], alt2[[inp$id]][[1]], inp$scale,
                                force_compare = TRUE)
      })
      if (!any(is.na(comp_inp))) {
        if (all(comp_inp <= 0) && any(comp_inp < 0)) comp <- -0.5
        else if (all(comp_inp >= 0) && any(comp_inp > 0)) comp <- +0.5
      }
    }
    return(comp)
  })
}

#' select_quality
#'
#' Select from `alt` only those attributes whose values have the given `quality`.
#' Used primarily in `selective_explanation()`.
#'
#' @param model A [DexiModel] object.
#' @param alt `data.frame`. A single DEXi alternative.
#' @param quality Requested `EnumQuality`: `"bad"`, `"good"` or `"none"`.
#'
#' @return `alt` containing only values that have the requested quality.
#' @export
#' @seealso [value_qualities()], [selective_explanation()]
#'
#' @examples
#' # Load "Car.dxi"
#' CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
#' Car <- read_dexi(CarDxi)
#'
#' alt <- Car$alternative("MyCar",
#'        BUY.PRICE="low", MAINT.PRICE="*", X.PERS="more", X.DOORS="4", LUGGAGE=2, SAFETY=c(1, 3))
#' alte <- Car$evaluate(alt)
#' alts <- select_quality(Car, alte, "bad")
#' names(alts)
#' # c("CAR", "PRICE", "MAINT.PRICE", "TECH.CHAR.", "SAFETY")
#' alts <- select_quality(Car, alte, "none")
#' names(alts)
#' # c("MAINT.PRICE", "X.DOORS", "LUGGAGE")
#' alts <- select_quality(Car, alte, "good")
#' names(alts)
#' # c("CAR", "PRICE", "BUY.PRICE", "MAINT.PRICE", "TECH.CHAR.", "COMFORT", "X.PERS", "SAFETY")
select_quality <- function(model, alt, quality) {
  stopifnot(nrow(alt) == 1)
  att_ids <- names(alt)
  select <- c()
  for (i in seq_increasing(1, length(alt))) {
    att <- model$attrib(att_ids[i])
    if (inherits(att, DexiAttributeClass)) {
      value <- alt[1, i]
      selected <- has_quality(quality, value, att$scale)
      select <- c(select, selected)
    } else {
      select <- c(select, FALSE)
    }
  }
  alt[select]
}

#' convert_data_continuous
#'
#' A helper function for converting individual columns of alternatives' data.
#' It is assumed that `data`  contains numeric data corresponding to a continuous
#' [DexiAttribute]. During conversion,
#' values are optionally converted from some interval to another, using [lin_map()], and/or
#' reversed using [reverse_value()] for scales whose `$order = "descending"`.
#'
#' @param data A vector containing floating point numbers.
#' Typically a `data.frame` column of DEXi alternatives' data.
#' @param scale A [DexiContinuousScale] object or a continuous [DexiAttribute] object.
#' @param imin Lower input bound. Default: determined as `min(data)`.
#' @param imax Upper input bound. Default: determined as `max(data)`.
#' @param omin Lower output bound for [lin_map()] value scaling.
#' @param omax Upper output bound for [lin_map()] value scaling.
#' @param map_values `logical(1)`. Whether or not to perform value scaling using [lin_map()].
#' @param reverse_descending `logical(1)`. Whether or not to reverse values of a "descending" scale.
#'
#' @return `numeric()`. Vector of converted values.
#' @export
#' @seealso [lin_map()], [reverse_value()]
#'
#' @examples
#' scl <- DexiContinuousScale()
#' convert_data_continuous(c(1, 2, 5), scl) # c(0.0, 0.25, 1.00)
#' convert_data_continuous(c(1, 2, 5), scl, imin = 0, imax = 10, omin = 0, omax = 100)
#' # c(10, 20, 50)
#'
convert_data_continuous <- function(data, scale,
                                     imin = NULL, imax = NULL,
                                     omin = 0, omax = 1,
                                     map_values = TRUE,
                                     reverse_descending = TRUE) {
  scale <- scale_of(scale)
  stopifnot(inherits(scale, DexiContinuousScaleClass))
  if (is.null(imin)) imin <- min(unlist(data), na.rm = TRUE)
  if (is.null(imax)) imax <- max(unlist(data), na.rm = TRUE)
  sapply(data, function(value) {
    if (map_values) value <- lin_map(unlist(value), imin, imax, omin = omin, omax = omax)
    if (reverse_descending && scale$order == "descending") value <- reverse_value(value, omin, omax)
    value
  })
}

#' convert_data_discrete
#'
#' #' A helper function for converting individual columns of alternatives' data.
#' It is assumed that `data`  contains data corresponding to a discrete
#' [DexiAttribute]. During conversion, `data` elements are converted either
#' to sets or distributions, and function `aggregate` if applied on them.
#' When `interpret = "set"`, values are also optionally converted to the interval `[omin:omax]`,
#' and reversed using [reverse_value()] for scales whose `$order = "descending"`.
#' @param data A vector containing DEXi values: single numbers, integer vectors or distribuions.
#' Typically a `data.frame` column of DEXi alternatives' data.
#' @param scale A [DexiDiscreteScale] object or a discrete [DexiAttribute] object.
#' @param interpret Either `"set"` (default), `"distribution"` or `"none"`.
#' Determines how are individual `data` elements interpreted: as sets or distributions.
#' Actually, each element is converted either to a set or distribution prior do applying `aggregate()`.
#' When `interpret = "none"`, just `aggregate()` is applied on the original `value` from `data`,
#' without any value scaling or reversal.
#' @param aggregate A function applied on each interpreted `data` element.
#' Normally a function that maps a numeric vector (set or distribution) to a single number.
#' Default: [min()].
#' @param omin Lower output bound for [lin_map()] value scaling. Applies only when `interpret = "set"`.
#' @param omax Upper output bound for [lin_map()] value scaling  Applies only when `interpret = "set"`.
#' @param map_values `logical(1)`. Whether or not to perform value scaling using [lin_map()].
#'  Applies only when `interpret = "set"`.
#' @param reverse_descending `logical(1)`. Whether or not to reverse values of a "descending" scale.
#'
#' @return Vector of converted values.
#' @export
#' @seealso [lin_map()], [reverse_value()]
#'
#' @examples
#' scla <- DexiDiscreteScale(values = c("L", "M", "H"))
#' scld <- DexiDiscreteScale(values = c("L", "M", "H"), order = "descending")
#' convert_data_discrete(c(1, 2, 3), scla)    # 0.0 0.5 1.0
#' convert_data_discrete(c(1, 2, 3), scld)    # 1.0 0.5 0.0
#' convert_data_discrete(list(1, 2, 3), scla) # 0.0 0.5 1.0
#' convert_data_discrete(list(1, 2, 3), scld) # 1.0 0.5 0.0
#' convert_data_discrete(list(1, 2, 3), scld, omax=10) # 10  5  0
#' data <- list(1, c(1,2), distribution(0.2, 0, 0.8), NA)
#' convert_data_discrete(data, scla, omax=10) #  0  0  0 NA
#' convert_data_discrete(data, scld, omax=10) # 10 10 10 NA
#' convert_data_discrete(data, scla, aggregate=max, omax=10)  #  0  5 10 NA
#' convert_data_discrete(data, scla, aggregate=mean, omax=10) # 0.0 2.5 5.0  NA
#'
convert_data_discrete <- function(data, scale,
                                    interpret = c("set", "distribution", "none"),
                                    aggregate = min,
                                    omin = 0, omax = 1,
                                    map_values = TRUE,
                                    reverse_descending = TRUE) {
  interpret <- match.arg(interpret)
  scale <- scale_of(scale)
  stopifnot(inherits(scale, DexiDiscreteScaleClass))
  nvals <- scale$count()
  sapply(data, function(value) {
    value <- unlist(value)
    if (interpret == "set") {
      value <- distr_to_set(value)
      if (!is.null(aggregate)) value <- aggregate(value)
      if (reverse_descending && scale$order == "descending") value <- reverse_value(value, 1, nvals)
      if (map_values) value <- lin_map(value, 1, nvals, omin = omin, omax = omax)
    }
    else if (interpret == "distribution") {
      value <- set_to_distr(value, nvals)
      if (is_distribution(value)) {
        if (reverse_descending && scale$order == "descending") value <- distribution(rev(value))
      }
      if (!is.null(aggregate)) value <- aggregate(value)
    }
    else {
      if (!is.null(aggregate)) value <- aggregate(value)
    }
    value
  })
}

#' convert_alternatives
#'
#' Converts a `data.frame` of alternatives' data to another `data.frame`. The conversion
#' generally involves: aggregating DEXi values originally represented by sets or distributions,
#' scaling aggregated values to a given interval and/or reversing values assigned to "descending"
#' [DexiScale]s.
#'
#' The rationale for `convert_alternatives()` is that data frames representing alternatives,
#' particularly those produced by [evaluate()], generally contain DEXi values of various
#' and mixed data types, such as numbers and numeric vectors (sets and distributions).
#' As such, this data is difficult to work with in R,
#' as most R functions expect simpler and more uniform data structures.
#' `convert_alternatives()` produces data frames that are more suitable for standard R data analysis
#' and graph drawing. However, as the conversion generally involves aggregation and mapping
#' of DEXi values, it may distort or lose information along the way.
#'
#' @param model A [DexiModel] object. Required.
#' @param alternatives A `data.frame` of alternatives (normally an output of [evaluate()]) or
#' indices to `model$alternatives`. The default value `NULL` selects `model$alternatives`.
#' @param interpret `character(1)`. Determines how the original values in `alternatives` are
#' interpreted, i.e., converted prior to submitting them to `aggregate()`:
#' \describe{
#'  \item{`"set"`}{As a set of values. Any [distribution]-type value is converted to a set,
#'  thus discarding the numeric membership information.}
#'  \item{`"distribution"`}{As a value distribution, i.e., a numeric vector of membership values.}
#'  \item{`"none"`}{No conversion.}
#' }
#' Values corresponding to continuous attributes are not converted nor affected by these settings.
#' @param aggregate A function accepting the interpreted DEXi value (see `interpret`) and converting
#' it to become part of the output data frame. Normally, this function is assumed to accept
#' a numeric vector argument and aggregate it in a single numeric value.
#' The default aggregation function is [min()]. Typical alternatives include [max()] and [mean()].
#' @param omin `numeric(1)`. Lower bound of the output value interval (see `map_values`). Default: `0`.
#' @param omax `numeric(1)`. Upper bound of the output value interval (see `map_values`). Default: `1`.
#' @param map_values `logical(1).` When `TRUE`, values produced by `aggregate()` are further scaled
#' to the interval `[omin:omax]`. Input bounds are determined from
#' the corresponding attribute scales (for discrete attributes) or
#' as minimum/maximum values from `alternatives` (for continuous attributes).
#' @param reverse_descending `logical(1).` Whether or not to reverse the values of attributes
#' whose scales are of a "descending" preference order.
#' @param verbatim `character()`. Names of `alternatives`' data columns
#' that are included in the output without conversion. Default: `"name"`.
#' @param skip `character()`. Names of `alternatives`' data columns
#' that are ignored in the process. Default: `NULL`.
#' @param continuous A function converting a data column that corresponds to a continuous attribute.
#' Default: [convert_data_continuous()].
#' Setting `continuous` to `NULL` excludes all continuous attributes from conversion.
#' @param discrete A function converting a data column that corresponds to a discrete attribute.
#' Default: [convert_data_discrete()].
#' Setting `discrete` to `NULL` excludes all discrete attributes from conversion.
#'
#' @return A converted `data.frame`.
#' @export
#' @seealso [convert_data_continuous()], [convert_data_discrete()], [scale_alternatives()],
#' [DEXiR-package] notes on values in DEXi models.
#'
#' @examples
#' # Load "Car.dxi"
#' CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
#' Car <- read_dexi(CarDxi)
#'
#' # Map Car$alternatives' values to the [0, 1] interval.
#' convert_alternatives(Car)
#'
#' #  name     CAR.1 PRICE BUY.PRICE MAINT.PRICE TECH.CHAR. COMFORT X.PERS   X.DOORS LUGGAGE SAFETY
#' # 1 Car1 1.0000000   1.0       0.5         1.0  1.0000000       1      1 0.6666667       1    1.0
#' # 2 Car2 0.6666667   0.5       0.5         0.5  0.6666667       1      1 0.6666667       1    0.5
#'
convert_alternatives <- function(model,
                                 alternatives = NULL,
                                 interpret = c("set", "distribution", "none"),
                                 aggregate = min,
                                 omin = 0, omax = 1,
                                 map_values = TRUE,
                                 reverse_descending = TRUE,
                                 verbatim = "name",
                                 skip = NULL,
                                 continuous = convert_data_continuous,
                                 discrete = convert_data_discrete) {
  interpret <- match.arg(interpret)
  stopifnot(inherits(model, DexiModelClass))
  if (is.null(alternatives)) alternatives <- model$alternatives
  altinterval <- !is.list(alternatives) && is_all_integer(alternatives)
  if (altinterval) alternatives <- model$alternatives[as.integer(alternatives),]
  result <- data.frame(row.names = row.names(alternatives))
  not_converted <- character()
  for (id in names(alternatives)) {
    data <- alternatives[[id]]
    if (id %in% skip) next
    if (id %in% verbatim) {
      result[[id]] <- unlist(data)
      next
    }
    attribute <- model$attrib(id)
    if (!inherits(attribute, DexiAttributeClass)) not_converted <- append(not_converted, id)
    else if (attribute$is_discrete()) {
      if (!is.null(discrete))
        result[[id]] <- discrete(data, attribute,
                          interpret = interpret,
                          aggregate = aggregate,
                          omin = omin, omax = omax,
                          map_values = map_values,
                          reverse_descending = reverse_descending)
    }
    else if (attribute$is_continuous()) {
      if (!is.null(continuous))
        result[[id]] <- continuous(data, attribute,
                        omin = omin, omax = omax,
                        map_values = map_values,
                        reverse_descending = reverse_descending)
    }
    else not_converted <- append(not_converted, id)
  }
  if (length(not_converted) > 0) warning(paste("Columns not converted:", not_converted))
  result
}

#' export_alternatives
#'
#' Convert `alternatives`' data to a data frame formatted so that it can be
#' imported by DEXi/DEXiWin software.
#'
#' In order to import the output of `export_alternative()` in DEXi/DEXiWin software,
#' proper `Import/Export` settings must be ensured in these programs:
#' \describe{
#'   \item{DEXi}{Option values: "base 1", Attributes: "all", Orientation: "normal",
#'   Indent: "indent".}
#'   \item{DEXiWin}{Option values: "Base 1", Attributes: "All",
#'   Orientation: "Attributes \ Alternatives",
#'   Indent: "Indent tree levels",
#'   CSV Format: "Invariant" when `format = "csv"` and "Local" when `format = "csv2"`.}
#' }
#' If `alternatives` contain value distributions,
#' they can be imported only by DEXiWin and not by DEXi.
#'
#' @param model A [DexiModel] object. Required.
#' @param alternatives A `data.frame` of alternatives (normally an output of [evaluate()]) or
#' indices to `model$alternatives`. The default value `NULL` selects `model$alternatives`.
#'
#' @return A data frame consisting of character strings that can be further written out by [write_alternatives()].
#' @export
#' @seealso [write_alternatives()]
#'
#' @examples
#' # Load "Car.dxi"
#' CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
#' Car <- read_dexi(CarDxi)
#'
#' export_alternatives(Car)    # export both alternatives from Car
#' export_alternatives(Car, 1) # export only the first alternative
#'
export_alternatives <- function(model, alternatives = NULL) {
  stopifnot(inherits(model, DexiModelClass))
  if (is.null(alternatives)) alternatives <- model$alternatives
  altinterval <- !is.list(alternatives) && is_all_integer(alternatives)
  if (altinterval) alternatives <- model$alternatives[as.integer(alternatives),]
  stopifnot(is.data.frame(alternatives))
  nalt <- nrow(alternatives)
  altnames <- alternatives[["name"]]
  if (is.null(altnames)) altnames <- row.names(alternatives)
  if (is.null(altnames)) atnames <- as.character(1:nalt)
  stopifnot(nalt == length(altnames))

  attids <- model$att_ids[model$att_ids != model$root$id]
  attributes <- sapply(attids, function(id) model$attrib(id))

  outnames <- sapply(attributes,
          function (att) paste0(paste0(rep(". ", att$level()), sep = "", collapse = ""), att$name, sep = "", collapse = "")
  )

  data <- data.frame(name = outnames)

  for (a in seq_increasing(1, nalt)) {
    outvalues <- sapply(attributes,
      function (att) {
        value <- alternatives[a, att$id][[1]]
        export_dexi_value(value)
      })
    data[[altnames[[a]]]] <- outvalues
  }

  data
}

#' write_alternatives
#'
#' Write out `alternatives`' data. First convert DEXi alternatives to a data frame using
#' [export_alternatives()] and then write it to a file.
#'
#' @param model A [DexiModel] object. Required.
#' @param alternatives A `data.frame` of alternatives (normally an output of [evaluate()]) or
#' indices to `model$alternatives`. The default value `NULL` selects `model$alternatives`.
#' @param file Write the data frame contents to a file.
#' When `file = ""`, the contents is written to the console (default).
#' `file = "clipboard"` might also work to copy the contents to the clipboard.
#' @param quote `logical(1)`. Whether or not to quote output character strings.
#' @param format One of `"tab"`, `"csv"` or `"csv2"` to
#' invoke [write.table()], [write.csv()] or [write.csv2()], respectively.
#' @param ... Optional parameters to [write.table()] functions.
#'
#' @return Writes a "tab"- or "csv"-formatted `alternatives`' data to a file,
#' console or clipboard. This data is meant to be subsequently imported to
#' 'DEXi' software.
#' @export
#' @seealso [export_alternatives()], [write.table()]
#'
#' @examples
#' # Load "Car.dxi"
#' CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
#' Car <- read_dexi(CarDxi)
#'
#' # Write both Car alternatives to console
#' write_alternatives(Car, file = "")
write_alternatives <- function(model,
                               alternatives = NULL,
                               file = "",
                               quote = FALSE,
                               format = c("tab",  "csv", "csv2"),
                               ...) {
  format <- match.arg(format)
  data <- export_alternatives(model, alternatives)
  if (format == "tab") {
    utils::write.table(data, file = file, quote = quote,  row.names = FALSE, sep = "\t", ...)
  }
  else if (format == "csv") {
    utils::write.csv(data, file = file, quote = quote,  row.names = FALSE, ...)
  }
  else if (format == "csv2") {
    utils::write.csv2(data, file = file, quote = quote,  row.names = FALSE, ...)
  }
}
