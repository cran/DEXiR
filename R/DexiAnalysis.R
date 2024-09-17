#' @include DexiClasses.R
#' @include DexiUtils.R
#' @include DexiAlternatives.R
NULL

#' selective_explanation
#'
#' Selective Explanation: Displays subtrees of alternatives' values in which
#' values are particularly weak (value quality is "bad") and
#' particularly strong (value quality is "good").
#' @param model A [DexiModel] object. Required.
#' @param alternatives A `data.frame` of alternatives or indices to `model$alternatives`.
#' The default value `NULL` selects `model$alternatives`.
#' @param print `logical(1)`. When `TRUE`, pretty print (add headings and left justify) the results,
#' using [print_selective_explanation()].
#' @param as_character `logical(1)`. Whether to represent alternative values
#' numerically (`FALSE`) or using text (`TRUE`).
#' @param round An integer number, argument to [value_text()].
#' @param id `character(1)`.
#' Determines the contents of the first or first two columns of the resulting `data.frame`s:
#' \describe{
#' \item{`"id"`}{Attribute IDs.}
#' \item{`"structure"`}{Attribute `$structure() + $name`.}
#' \item{anything else}{Equivalent to both `"id"` and `"structure"`.}
#' }
#' @param evaluate `logical(1)`. Whether or not to evaluate `alternatives` beforehand.
#' @param ... Optional parameters for [evaluate()].
#'
#' @return A list of lists: For each alternative contains a list of two data.frames, corresponding
#' to `"bad"` and `"good"` qualities, respectively.
#' May be pretty-printed using [print_selective_explanation()].
#' @export
#' @seealso [value_qualities()], [value_text()], [print_selective_explanation()], [evaluate()]

#' @examples
#' # Load "Car.dxi"
#' CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
#' Car <- read_dexi(CarDxi)
#'
#' # Print selective explanation of two Car$alternatives.
#' selective_explanation(Car)
#'
#' alt <- Car$alternative("MyCar",
#'        BUY.PRICE="low", MAINT.PRICE="*", X.PERS="more", X.DOORS="4", LUGGAGE=2, SAFETY=c(1, 3))
#' alte <- Car$evaluate(alt)
#'
#' # Print selective explanation of `alte`.
#' selective_explanation(Car, alte)
#'
#' # Print selective explanation of both `alt` and `alte`.
#' selective_explanation(Car, rbind.data.frame(alt, alte))
selective_explanation <- function(model,
                                  alternatives = NULL,
                                  print = TRUE,
                                  as_character = FALSE,
                                  round = NULL,
                                  id = NULL,
                                  evaluate = FALSE,
                                  ...) {

  quality_values <- function(quality, altname) {
    selected <- select_quality(model, alt, quality)
    if (ncol(selected) == 0) return(data.frame())
    attributes <- model$attrib(names(selected))
    df <- data.frame(
      id = names(selected),
      structure = sapply(attributes, function(att) paste0(att$structure(), att$name))
    )
    df[[altname]] <- alt_values(selected, attributes, as_character, round)
    if (!is.null(id)) {
      if (id == "id") df$structure <- NULL
      if (id == "struct" || id == "structure") df$id <- NULL
    }
    return(df)
  }

  stopifnot(inherits(model, DexiModelClass))
  if (is.null(alternatives)) alternatives <- model$alternatives
  altinterval <- !is.list(alternatives) && is_all_integer(alternatives)
  if (altinterval) alternatives <- model$alternatives[as.integer(alternatives),]
  stopifnot(is.data.frame(alternatives))
  if (evaluate) alternatives <- evaluate(model, alternatives, ...)
  result <- list()
  for (i in seq_increasing(1, nrow(alternatives))) {
    alt <- alternatives[i,]
    altname <- as.character(alt[1, "name"])
    if (is.null(altname) || exists(altname, where=result))
      altname <- paste0(altname, "[", i, "]")
    result[[altname]] <- list(
      bad = quality_values("bad", altname),
      good = quality_values("good", altname)
    )
  }
  if (print) {
    print_selective_explanation(result)
    invisible(result)
  } else {
    return(result)
  }
}

#' print_selective_explanation
#'
#' A helper function for [selective_explanation()]: Pretty-prints its results.
#'
#' @param explanation A list of lists,
#' containing selective explanation results produced by [selective_explanation()].
#'
#' @return `NULL`. Pretty-prints the contents of `explanation`.
#' @export
print_selective_explanation <- function(explanation) {
  stopifnot(is.list(explanation))
  for (altname in names(explanation)) {
    cat("\nSelective explanation of", altname, "\n")
    alt <- explanation[[altname]]
    data <- alt$bad
    cat("\nWeak points:\n")
    if (length(data) == 0) {
      cat("None\n")
    } else {
      print(data, row.names = FALSE, right = FALSE)
    }
    data <- alt$good
    cat("\nStrong points:\n")
    if (length(data) == 0) {
      cat("None\n")
    } else {
      print(data, row.names = FALSE, right = FALSE)
    }
  }
  NULL
}

#' plus_minus_setup
#'
#' A helper function: Initializes a data frame for [plus_minus()].
#' @param evaluated An evaluated alternative.
#' @param attributes Vector of [DexiAttribute] objects involved in `plus-minus` analysis.
#' @param minus A single integer: Maximum steps down.
#' @param plus A single integer: Maximum steps up.
#'
#' @return A data frame consisting of columns:
#' \describe{
#' \item{`"id"`}{Attribute IDs.}
#' \item{`"structure"`}{Attribute `$structure() + $name`.}
#' \item{`counts`}{Attributes' scale sizes.}
#' \item{`low_bounds`}{Low bounds of attributes' values.}
#' \item{`high_bounds`}{High bounds of attributes' values.}
#' \item{`low_diff`}{Maximum possible value decrease given `low_bound` and attribute scale.}
#' \item{`high_diff`}{Maximum possible value increase given `high_bound` and attribute scale.}
#' \item{`evals`}{Alternative evaluation for the corresponding attribute (from `evaluated`).}
#' \item{`sets`}{`evals` represented as value sets.}
#' }
#'
#' @seealso [plus_minus()]
plus_minus_setup <- function(evaluated, attributes, minus, plus) {
  evals <- lapply(attributes, function(att) evaluated[[att$id]][[1]])

  sets <- lapply(attributes, function(att) {
    eval <- evaluated[[att$id]][[1]]
    scl <- scale_of(att)
    value_to_set(eval, scl)
  })

  counts <- sapply(attributes, function(att) {
    scl <- scale_of(att)
    scl$count()
  })

  low_bounds <- sapply(sets, min)
  high_bounds <- sapply(sets, max)
  low_diff <- low_bounds - 1
  high_diff <- counts - high_bounds
  minus <- min(abs(minus), max(low_diff))
  plus <- min(abs(plus), max(high_diff))
  df <- data.frame(
    id = att_names(attributes),
    structure = sapply(attributes, function(att) paste0(att$structure(), att$name)),
    counts = counts,
    low_bounds = low_bounds,
    high_bounds = high_bounds,
    low_diff = low_diff,
    high_diff = high_diff
  )
  df$evals <- evals
  df$sets <- sets
  return(df)
}

#' plus_minus
#'
#' Plus-Minus Analysis:
#' Investigate the effects of changing single attributes values on the evaluation of `alternative`.
#' The values of discrete basic attributes ("input attributes") are changed, one attribute at a time,
#' by a particular number of steps downwards (`minus`) and upwards (`plus`),
#' while observing the changes of the `target` attribute values.
#'
#' @param model A [DexiModel] object.
#' @param alternative Either a `data.frame` representing a single alternative
#' or an index to `model$alternatives`.
#' @param target The attribute on which effects are observed. Default: `model$first()`.
#' @param minus The maximum number of downward steps to be made for each input attribute.
#' Default: `.Machine$integer.max`.
#' The actual `minus` value is further determined with respect to
#' `alternative` values and involved attributes' scales.
#' @param plus The maximum number of upward steps to be made for each input attribute.
#' Default: `.Machine$integer.max`.
#' The actual `plus` value is further determined with respect to
#' `alternative` values and involved attributes' scales.
#' @param print `logical(1)`. When `TRUE`, pretty print (left justify) the results.
#' @param as_character `logical(1)`. Whether to represent alternative values
#' numerically (`FALSE`) or using text (`TRUE`).
#' @param round An integer number, argument to [value_text()].
#' @param id `character(1)`.
#' Determines the contents of the first or first two columns of the resulting `data.frame`s:
#' \describe{
#' \item{`"id"`}{Attribute ID.}
#' \item{`"structure"`}{Attribute `$structure() + $name`.}
#' \item{anything else}{Equivalent to both `"id"` and `"structure"`.}
#' }
#' @param evaluate `logical(1)`. Whether or not to evaluate `alternative` beforehand.
#' @param ... Optional parameters for [evaluate()].
#'
#' @return A data frame consisting of columns:
#' \describe{
#' \item{`id`}{IDs of input attributes (unless excluded by the `id` argument).}
#' \item{`structure`}{Structure and names of input attributes (unless excluded by the `id` argument).}
#' \item{'For `-minus` to `-1`}{Evaluation value of `target` when decreasing the corresponding attribute value by the corresponding number of steps.}
#' \item{`target$id`}{Original `alternative` value assigned to the corresponding attribute `id`.}
#' \item{For `1` to `plus`}{Evaluation value of `target` when increasing the corresponding attribute value by the corresponding number of steps.}
#' }
#' Special values `"["` and `"]"` denote that it is not possible to decrease of increase, respectively,
#' the corresponding attributes value further.
#' @export
#' @seealso [evaluate()], [value_text()]
#'
#' @examples
#' # Load "Car.dxi"
#' CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
#' Car <- read_dexi(CarDxi)
#'
#' alt <- Car$alternative("MyCar",
#'        BUY.PRICE="low", MAINT.PRICE="*", X.PERS="more", X.DOORS="4", LUGGAGE=2, SAFETY=c(1, 3))
#' alte <- Car$evaluate(alt)
#'
#' # Default plus-minus analysis, evaluating `alt`.
#' plus_minus(Car, alt, evaluate = TRUE)
#'
#' # Plus-minus analysis of `alte`, using character strings,
#' # no pretty-printing and excluding structure info.
#' plus_minus(Car, alte, as_character=TRUE, print=FALSE, id = "id")
#'
#' # Plus-minus analysis on `target="PRICE"`, using character strings.
#' plus_minus(Car, alt, target="PRICE", as_character=TRUE, evaluate=TRUE)
plus_minus <- function(model,
                       alternative,
                       target = model$first(),
                       minus = .Machine$integer.max,
                       plus = .Machine$integer.max,
                       print = TRUE,
                       as_character = FALSE,
                       round = NULL,
                       id = NULL,
                       evaluate = FALSE,
                       ...)
{
  # Checking arguments
  stopifnot(is_class(model, DexiModelClass))
  target <- model$attrib(target)
  stopifnot(is_class(target, DexiAttributeClass))
  stopifnot(target$is_discrete())
  stopifnot(identical(model, target$model()))
  if (is_single_integer(alternative)) {
    alternative <- model$alternatives[alternative,]
  }
  stopifnot(is.data.frame(alternative))
  stopifnot("A single alternative is required" = nrow(alternative) == 1)

  # Evaluate alternative and determine target value
  evaluated <- if (evaluate) evaluate(model, alternative, root = target, ...) else alternative
  target_value <- evaluated[[target$id]][[1]]
  if (as_character) target_value <- value_text(target_value, target$scale, round)
  else target_value <- toString(target_value)

  # Determine involved attributes. Must be basic, discrete and affecting `target`.
  attributes <- model$attributes[sapply(model$attributes,
                  function(att) {
                    att$is_basic() && att$is_discrete() && att$affects(target)
                  }
                )]

  # Set up the data frame. See [plus_minus_setup()] for columns descriptions.
  data <- plus_minus_setup(evaluated, attributes, minus, plus)

  # Determine actual needed `minus` and `plus` steps.
  minus <- min(abs(minus), max(data$low_diff))
  plus <- min(abs(plus), max(data$high_diff))

  # Create plus-minus columns and add them to `data`.
  for (pm in c(seq_increasing(-minus, plus))) {
    column <- lapply(attributes, function (att) {
      if (pm == 0) {
        # Make a column of current `alternative` values.
        value <- data[data$id==att$id, "evals"][[1]]
        result <- if (as_character) value_text(value, att$scale, round) else toString(value)
      } else {
        # Make a column of `target` evaluations for the value offset of `pm`.
        bound <- data[data$id==att$id, if (pm < 0) "low_bounds" else "high_bounds"]
        #new `att` value
        value <- bound + pm
        if (value >= 1 && value <= att$scale$count()) {
          alt <- alternative
          alt[[att$id]][[1]] <- value
          eval <- evaluate(model, alt, target, ...)
          result <- eval[[target$id]][[1]]
          if (as_character) {
            result <- value_text(result, target, round)
          }
        }
        else if (value == 0) result <- "["
        else if (value == att$scale$count() + 1) result <- "]"
        else result <- ""
      }
      return(result)
      })
    if (pm == 0) data[[paste0(target$id, "=", target_value)]] <- column
    else data[[as.character(pm)]] <- column
  }

  # Data cleanup.
  for (n in c("counts", "low_bounds", "high_bounds", "low_diff", "high_diff", "evals", "sets")) {
    data[[n]] <- NULL
  }
  if (!is.null(id)) {
    if (id == "id") data$structure <- NULL
    if (id == "struct" || id == "structure") data$id <- NULL
  }
  if (print) {
    print(data, row.names = FALSE, right = FALSE)
    invisible(data)
  } else {
    return(data)
  }
}

#' compare_alternatives
#'
#' Compare Alternatives Analysis: Compare `alternative` with each of `alternatives`.
#' Display only values that differ and, optionally when `compare = TRUE`, include
#' preference-relational operators.
#'
#' @param model A [DexiModel] object. Required.
#' @param alternative Either a `data.frame` representing a single alternative or
#' an integer index to `model$alternatives`.
#' @param alternatives Either a `data.frame` representing one or more alternatives,
#' or an integer numeric vector representing indices to `model$alternatives`.
#' By default, `alternatives` are set to `model$alternatives`,
#' possibly excluding `alternative` when indexed.
#' @param root Optional [DexiAttribute] object.
#' When specified, only attributes that affect `root` are included in the analysis.
#' Otherwise, all `model$attributes` are included.
#' @param compare `logical(1)`.
#' Whether or not preference relations `"<", ">", "<=", ">="` are included in results.
#' @param deep `logical(1)`.
#' Whether of not "deep" comparison (see [compare_two_alternatives()]) is carried out.
#' @param print `logical(1)`. When `TRUE`, pretty print (left justify) the results.
#' @param as_character `logical(1)`. Whether to represent alternative values
#' numerically (`FALSE`) or using text (`TRUE`).
#' @param round An integer number, argument to [value_text()].
#' @param id `character(1)`.
#' Determines the contents of the first or first two columns of the resulting `data.frame`:
#' \describe{
#' \item{`"id"`}{Attribute ID.}
#' \item{`"structure"`}{Attribute `$structure() + $name`.}
#' \item{anything else}{Equivalent to both `"id"` and `"structure"`.}
#' }
#' @param evaluate `logical(1)`. Whether or not to evaluate `alternative` and `alternatives` beforehand.
#' @param ... Optional parameters for [evaluate()].
#'
#' @return Returns or prints a `data.frame` consisting of columns:
#' `id` (if requested), `structure` (if requested),
#' values of `alternative` and comparison results for each alternative from `alternatives`.
#' @export
#' @seealso [compare_two_alternatives()], [evaluate()]
#'
#' @examples
#' # Load "Car.dxi"
#' CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
#' Car <- read_dexi(CarDxi)
#'
#' # Extend Car$alternatives
#' car3 <- set_alternative(Car, Car$alternatives[2,], name = "Car3", LUGGAGE = 2)
#' Car$alternatives[3,] <- car3
#' car4 <- set_alternative(Car, Car$alternatives[2,], name = "Car4", LUGGAGE = 1)
#'
#' # Compare Car1 with the other two, varying some arguments
#' compare_alternatives(Car, 1, evaluate=TRUE, compare=FALSE)
#' compare_alternatives(Car, 1, evaluate=TRUE, compare=TRUE)
#' compare_alternatives(Car, 1, evaluate=TRUE, compare=TRUE, deep=FALSE)
#'
#' # Compare Car2 with Car1
#' compare_alternatives(Car, 2, 1)
#'
#' # Compare car3 with Car1 and Car2
#' compare_alternatives(Car, car3, 1:2)
#'
#' # Compare car4 with Car$alternatives
#' compare_alternatives(Car, car4)
#'
#' # Compare Car$alternatives[1,] with car3
#' compare_alternatives(Car, 1, car3)
#' compare_alternatives(Car, Car$alternatives[1,], car3)
compare_alternatives <- function(model,
                                 alternative,
                                 alternatives = NULL,
                                 root = NULL,
                                 compare = TRUE,
                                 deep = TRUE,
                                 print = TRUE,
                                 as_character = FALSE,
                                 round = NULL,
                                 id = NULL,
                                 evaluate = FALSE,
                                 ...) {
  # Checking and preparing arguments
  stopifnot(is_class(model, DexiModelClass))
  attributes <- model$attributes
  if (!is.null(root)) {
    attributes <- attributes[
      sapply(attributes, function(att) {identical(att, root) || att$affects(root)})
    ]
  }
  stopifnot("No attributes to compare at" = length(attributes) > 0)
  if (is.null(alternatives)) alternatives <- seq_increasing(1, nrow(model$alternatives))
  if (is_single_integer(alternative)) {
    altindex <- as.integer(alternative)
    alternative <- model$alternatives[altindex,]
  } else {
    altindex <- -1
  }
  altinterval <- !is.list(alternatives) && is_all_integer(alternatives)
  if (altinterval) {
    altindices <- as.integer(alternatives)
    altindices <- altindices[altindices != altindex]
    alternatives <- model$alternatives[altindices, ]
  }
  stopifnot(is.data.frame(alternatives))
  stopifnot(is.data.frame(alternative))
  stopifnot("A single alternative is required" = nrow(alternative) == 1)

  # Evaluate alternatives if requested
  if (evaluate) {
    alternative <- evaluate(model, alternative, ...)
    alternatives <- evaluate(model, alternatives, ...)
  }

  # prepare data.frame
  data <- data.frame(
    id = att_names(attributes),
    structure = sapply(attributes, function(att) paste0(att$structure(), att$name))
  )
  altname <- alternative[["name"]][[1]]
  if (is.na(altname) || is.null(altname) || length(altname) == 0) altname <- "alternative"
  data[[altname]] <- alt_values(alternative, attributes, as_character, round)

  # compare with `alternatives`
  for (altx in seq_increasing(1, nrow(alternatives))) {
    compared <- alternatives[altx,]
    compared_relations <- compare_two_alternatives(alternative, compared, attributes, deep)
    compared_values <- alt_values(compared, attributes, as_character, round)
    compared_values[compared_relations %in% c(0, -0.5, +0.5)] <- ""
    compared_column <-
      if (!compare) compared_values
      else {
        sapply(seq_increasing(1, length(attributes)), function (x) {
          comp <- compared_relations[x]
          op <-
            if (is.na(comp) || comp==0) { "" }
            else if (comp <= -1.0) { "< " }
            else if (comp <= -0.5) { "<= " }
            else if (comp >= +1.0) { "> " }
            else if (comp >= +0.5) { ">= " }
            else { "?" }
            paste0(op, compared_values[x])
        })
      }
    data[[compared[["name"]][[1]]]] <- compared_column
  }

  # finalize
  if (!is.null(id)) {
    if (id == "id") data$structure <- NULL
    if (id == "struct" || id == "structure") data$id <- NULL
  }
  if (print) {
    print(data, row.names = FALSE, right = FALSE)
    invisible(data)
  } else {
    return(data)
  }
}
