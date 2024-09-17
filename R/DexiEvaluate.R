#' @include DexiClasses.R
#' @include DexiUtils.R
#' @include DexiAttributes.R

EnumEvalMethod = c("set", "prob", "fuzzy", "fuzzynorm")

#' evaluation_order
#'
#' Determine the evaluation order of attributes. Interpreted as a sequence, the order guarantees
#' that whenever some attribute is reached as a candidate for evaluation,
#' all the previous attributes have been already evaluated.
#'
#' @param att [DexiAttribute]. The starting point of evaluation.
#' @param prune A character vector. May contain IDs of aggregate attributes at which the evaluation should
#' stop, treating them as if they were basic attributes.
#'
#' @return A character vector of attribute IDs.
#' @export
#' @seealso [evaluate()]
#'
#' @examples
#' # Load "Car.dxi"
#' CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
#' Car <- read_dexi(CarDxi)
#'
#' # Full evaluation order, starting with Car$root and without pruning
#' evaluation_order(Car$root)
#'
#' # Evaluation order, starting with the TECH.CHAR. attribute
#' evaluation_order(Car$attrib("TECH.CHAR."))
#'
#' # evaluation order, starting with Car$root and pruned at "PRICE"
#' evaluation_order(Car$root, prune = "PRICE")
#'
evaluation_order <- function (att, prune = list()) {
  stopifnot(inherits(att, DexiAttributeClass))

  order <- character()

  add_to_order <- function(att) {
    id <- att$id
    if (!(id %in% order)) {
      if (!(id %in% prune)) {
        if (att$is_link()) add_to_order(att$link)
        else if (att$is_aggregate()) sapply(att$inputs, add_to_order)
      }
      order <<- c(order, id)
    }
  }

  add_to_order(att)
  order
}

#' evaluation_parameters
#'
#' Make a list containing parameters of DEXi evaluation. The parameters determine which method
#' and normalization/aggregation functions should be used by [evaluate()].
#'
#' @inheritParams evaluate
#'
#' @return `list(method, norm, and, or)`.
#' For `NULL` `norm`, `and`, and `or` arguments, defaults are
#' taken depending on the `method`.
#' @export
#' @seealso [evaluate], [normalize_function()],
#' [norm_none()], [norm_max()], [norm_sum()], [and_function()], [or_function()].
#'
#' @examples
#' evaluation_parameters("prob", norm = norm_none)
#'
evaluation_parameters <- function (method = EnumEvalMethod, norm = NULL, and = NULL, or = NULL) {
  method <- match.arg(method)
  list(
    method = method,
    norm = normalize_function(method, norm),
    and = and_function(method, and),
    or = or_function(method, or)
  )
}

#' normalize_function
#'
#' Determine the function to be used in the normalization step of [evaluate()].
#'
#' @inheritParams evaluate
#'
#' @return Returns function `norm` if not `NULL`.
#' Otherwise, it determines the result depending on `method`:
#' \describe{
#'   \item{`"set"`:}{[norm_none()]}
#'   \item{`"prob"`:}{[norm_sum()]}
#'   \item{`"fuzzy"`:}{[norm_none()]}
#'   \item{`"fuzzynorm"`:}{[norm_max()]}
#' }
#' Fails with an error if the result is not an R function.
#' @family "normalize functions"
#' @seealso \code{\link[DEXiR]{evaluate}}, [norm_none()], [norm_max()], [norm_sum()],
#' @export
#'
normalize_function <- function(method = EnumEvalMethod, norm = NULL) {
  if (!is.function(norm)) {
    method <- match.arg(method)
    norm <-
      switch(method,
             "set" = norm_none,
             "prob" = norm_sum,
             "fuzzy" = norm_none,
             "fuzzynorm" = norm_max,
             norm_none
    )
  }
  stopifnot(is.function(norm))
  norm
}

#' and_function
#'
#' Determine the function to be used in the conjunctive aggregation step of [evaluate()].
#'
#' @inheritParams evaluate
#'
#' @return Returns the function `and` if not `NULL`.
#' Otherwise, it determines the result depending on `method`:
#' \describe{
#'   \item{`"set"`:}{\code{function(x) 0}}
#'   \item{`"prob"`:}{\code{\link[base]{prod}}}
#'   \item{`"fuzzy"`:}{\code{\link[base]{min}}}
#'   \item{`"fuzzynorm"`:}{\code{\link[base]{min}}}
#' }
#' Fails with an error if the result is not an R function.
#' @seealso \code{\link[DEXiR]{evaluate}}, [or_function()].
#' @export
#'
and_function <- function(method = EnumEvalMethod, and = NULL) {
  if (!is.function(and)) {
    method <- match.arg(method)
    and <-
      switch(method,
             "set" = function(x) 0,
             "prob" = prod,
             "fuzzy" = min,
             "fuzzynorm" = min,
             prod
      )
  }
  stopifnot(is.function(and))
  and
}

#' or_function
#'
#' Determine the function to be used in the disjunctive aggregation step of [evaluate()].
#'
#' @inheritParams evaluate
#'
#' @return Returns the function `or` if not `NULL`.
#' Otherwise, it determines the result depending on `method`:
#' \describe{
#'   \item{`"set"`:}{\code{function(x) 1}}
#'   \item{`"prob"`:}{\code{\link[base]{sum}}}
#'   \item{`"fuzzy"`:}{\code{\link[base]{max}}}
#'   \item{`"fuzzynorm"`:}{\code{\link[base]{max}}}
#' }
#' Fails with an error if the result is not an R function.
#' @export
#' @seealso \code{\link[DEXiR]{evaluate}}, [and_function()].
#'
or_function <- function(method = EnumEvalMethod, or = NULL) {
  if (!is.function(or)) {
    method <- match.arg(method)
    or <-
      switch(method,
             "set" = function(x) 1,
             "prob" = sum,
             "fuzzy" = max,
             "fuzzynorm" = max,
             sum
      )
  }
  stopifnot(is.function(or))
  or
}

evaluate_as_set <- function(fnc, inp_values) {
  grid <- expand.grid(inp_values)
  set <- c()
  for (i in seq_increasing(1, nrow(grid))) {
    args <- unlist(grid[i, ])
    eval <- fnc$evaluate(args)
    set <- c(set, eval)
  }
  if (is_empty(set) || any(is.na(set))) NA else sort(unique(set))
}

evaluate_as_distribution <- function(fnc, inp_values, eval_param) {
  inp_distrs <- lapply(inp_values, function (val) set_to_distr(val))
  mem_grid <- expand.grid(inp_distrs)
  idx_grid <- expand.grid(lapply(inp_distrs, function (distr) seq_increasing(1, length(distr))))
  ands <- apply(mem_grid, 1, eval_param$and)
  value <- rep(0, fnc$attribute$scale$count())
  for (i in seq_increasing(1, nrow(mem_grid))) {
    if (ands[[i]] == 0) next
    args <- unlist(idx_grid[i, ])
    eval <- fnc$evaluate(args)
    set <- distr_to_set(eval)
    if (is_empty(set) || any(is.na(set))) {
      return (NA)
    }
    distr <- set_to_distr(eval)
    distr <- eval_param$norm(distr)
    for (j in seq_increasing(1, length(distr))) {
      mem_val <- eval_param$and(ands[[i]], distr[[j]])
      value[[j]] <- eval_param$or(c(value[[j]], mem_val))
    }
  }
  value <- distribution(value)
}

evaluate_aggregate <- function(att, scl, alt, eval_param) {
  fnc <- att$funct
  if (!is_class(fnc, DexiFunctionClass)) return(NA)
  inputs <- att$inputs
  inp_ids <- att_names(inputs)
  inp_values <- alt[1, inp_ids]
  if (any(is.na(inp_values))) return(NA)
  classes <- sapply(inp_values, function (val) class(val[[1]]))
  if (eval_param$method == "set") {
    inp_values <- lapply(inp_values, function (val) distr_to_set(val[[1]]))
    max_len <- max(lengths(inp_values))
    value <-
      if (max_len == 1) fnc$evaluate(unlist(inp_values))
      else value <- evaluate_as_set(fnc, inp_values)
  }
  else  # distributional
  {
    value <- evaluate_as_distribution(fnc, inp_values, eval_param)
  }
  value
}

#' evaluate
#'
#' @description
#' Evaluates decision alternatives. Essentially, this is a bottom-up aggregation method:
#' starting with basic attributes (or `prune`d aggregate attributes), values of each
#' alternative are gradually aggregated towards the `root` attribute,
#' according to [evaluation_order()]. The aggregation
#' at each individual [DexiAttribute] is governed by the corresponding `DexiAttribute$funct`.
#' When alternative values are sets or distributions (see [DEXiR-package]),
#' then [evaluate()] tries all possible combinations of values of the descendant attributes.
#'
#' @details
#' [evaluate()] implements four aggregation methods:
#' `"set"`, `"prob"`, `"fuzzy"` and `"fuzzynorm"`.
#'
#' The `"set"` method interprets DEXi values as sets. The output value assigned to some `attribute` is
#' composed of the union of all `attribute$funct` evaluations for all possible combinations of values of
#' `attribute$inputs`.
#'
#' The remaining three methods interpret DEXi values as value distributions. They follow the same algorithm,
#' but use different functions (see [evaluation_parameters()]) in three algorithm steps:
#' normalization, and conjunctive and disjunctive aggregation. All values distributions involved in
#' calculations are normalized by the function `norm()`. All combinations of `attribute$input`
#' values are individually evaluated by the corresponding tabular function `attribute$funct`.
#' The value \eqn{p} of each set of `attribute$funct` arguments is determined by the conjunctive
#' aggregation function `and()` over \eqn{p}'s of individual arguments.
#' Finally, the \eqn{p} of some output value `val` is determined by the
#' disjunctive aggregation function `or()`, applied on the \eqn{p}'s of all partial evaluations that
#' map to `val`.
#'
#' For the mathematical background and more details about aggregation in DEX, please see
#' (Trdin, Bohanec, 2018). For default normalization and aggregation functions,
#' see [normalize_function()], [and_function()] and [or_function()].
#'
#' @param model [DexiModel].
#' @param alternatives A data frame containing data of one or more decision alternatives.
#' @param root [DexiAttribute]. Default: `model$root`.
#' @param method One of: `"set"` (default), `"prob"`, `"fuzzy"` or `"fuzzynorm"`.
#' @param bounding `logical(1)`. When `TRUE`, evaluation results are additionally subjected to
#' [bounded_scale_value()] to keep them in the bounds set up by the corresponding scale.
#' @param prune `character()`, containing IDs of aggregate attributes that should be treated as
#' evaluation inputs (rather than basic attributes).
#' @param norm Some normalization function of the form `function(num_vector)`, or `NULL`.
#' @param and Some conjunctive aggregation function of the form `function(num_vector)`, or `NULL`.
#' @param or Some disjunctive aggregation function of the form `function(num_vector)`, or `NULL`.
#'
#' @return A data frame containing both input and output (evaluated) values of `alternatives`.
#'
#' @seealso  [evaluation_parameters()], [normalize_function()],
#' [norm_none()], [norm_max()], [norm_sum()], [and_function()], [or_function()], [bounded_scale_value()].
#'
#' @export
#'
#' @examples
#' # Load "Car.dxi"
#' CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
#' Car <- read_dexi(CarDxi)
#'
# # Define some decision alternative (a car in this case) and evaluate it using default parameters
#' alt <- Car$alternative("MyCar_set",
#'          BUY.PRICE="low", MAINT.PRICE=2, X.PERS="more", X.DOORS="4", LUGGAGE=2, SAFETY="medium")
#' Car$evaluate(alt)
#'
#' # Try the set-based evaluation using the default "set" method
#' alt <- Car$alternative("MyCar2",
#'        BUY.PRICE="low", MAINT.PRICE="*", X.PERS="more", X.DOORS="4", LUGGAGE=2, SAFETY=2)
#' Car$evaluate(alt)
#'
#' # Use value distributions and try the methods "prob", "fuzzy" and "fuzzynorm"
#' alt <- Car$alternative("MyCar_distr",
#'        BUY.PRICE="low", MAINT.PRICE=distribution(0.1, 0.6, 0.3),
#'        X.PERS="more", X.DOORS="4", LUGGAGE=2, SAFETY=2)
#' Car$evaluate(alt, method = "prob")
#' Car$evaluate(alt, method = "fuzzy")
#' Car$evaluate(alt, method = "fuzzynorm")
#'
evaluate <- function(model,
              alternatives = model$alternatives,
              root = model$root,
              method = EnumEvalMethod,
              bounding = FALSE,
              prune = list(),
              norm = NULL,
              and = NULL,
              or = NULL) {

  # initialization
  method <- match.arg(method)
  stopifnot(is_class(model, DexiModelClass))
  stopifnot(is.data.frame(alternatives))
  if (is_single_character(root)) root <- model$attrib(root)
  stopifnot(is_class(root, DexiAttributeClass))
  eval_param <- evaluation_parameters(method, norm, and, or)

  pruning <- length(prune) > 0
  full_order <- evaluation_order(root)
  evaluation_order <- if (pruning) evaluation_order(root, prune) else full_order

  for (att_id in setdiff(full_order, names(alternatives))) {
    if (att_id != model$root$id) alternatives[att_id] <- NA
  }

  if (pruning) alternatives[ , setdiff(full_order, evaluation_order)] <- NA

  for (i in seq_increasing(1, nrow(alternatives))) {
    alt <- alternatives[i, ]
    for (att_id in evaluation_order) {
      if (att_id == model$root$id) next
      att <- model$attrib(att_id)
      if (!is_class(att, DexiAttributeClass)) next
      scl <- att$scale
      value <- NA
      if (!is_class(scl, DexiScaleClass)) {
        value <- NA
      }
      else if (att$is_link()) {
        value <- alt[[1, att$link$id]]
      }
      else if (att$is_basic() || att_id %in% prune) {
        value <- scale_value(alt[[1, att_id]], scl)
      }
      else if (att$is_aggregate()) {
        value <- evaluate_aggregate(att, scl, alt, eval_param)
      }
      if (bounding) value <- bounded_scale_value(value, scl)
      if (eval_param$method != "set" && is_class(value, DistributionClass)) value <- eval_param$norm(value)
      alt[[att_id]] <- vector("list", length = 1)
      alt[[1, att_id]] <- if (is_empty(value)) NA else value
    }

    alternatives[i, ] <- alt
  }

  alternatives
}

#' evaluate_attribute
#'
#' Evaluate `alternative` for a sequence of `attribute` values.
#'
#' @param model A [DexiModel].
#' @param attribute A [DexiAttribute] with an assigned discrete or continuous scale.
#' @param alternative A `data.frame` containing a single alternative.
#' @param seq A sequence of `attribute` numeric values for which to evaluate `alternative`.
#' For discrete scales: Must be a sequence of integers. Defaults to `attribute$scale$full_range()`.
#' For continuous scales: `seq` is required.
#' @param ... Optional parameters passed to [evaluate()].
#'
#' @return A list of evaluated alternatives for consecutive `attribute` values from `seq`.
#' @export
#' @seealso [evaluate()]
#'
#' @examples
#' # Load "Car.dxi"
#' CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
#' Car <- read_dexi(CarDxi)
#'
# # Define some decision alternative (a car in this case) and evaluate it using default parameters
#' alt <- Car$alternative("MyCar",
#'          BUY.PRICE="low", MAINT.PRICE=2, X.PERS="more", X.DOORS="4", LUGGAGE=2, SAFETY="medium")
#' safety <- Car$attrib("SAFETY")
#' # Evaluate alt for all values of att
#' evaluate_attribute(Car, safety, alt)
#' # Returns a list of three alternatives for values SAFETY=c("small", "medium", "high")
evaluate_attribute <- function(model, attribute, alternative, seq = NULL, ...) {
  stopifnot(is_class(model, DexiModelClass))
  stopifnot(is.data.frame(alternative))
  stopifnot("A single alternative is required" = nrow(alternative) == 1)
  attribute <- model$attrib(attribute)
  stopifnot(is_class(attribute, DexiAttributeClass))
  if (is.null(attributes)) attributes <- model$basic
  attributes <- model$attrib(attributes)
  scl <- scale_of(attribute)
  stopifnot("Attribute scale must be discrete or continuous" = is_class(scl, c(DexiContinuousScaleClass, DexiDiscreteScaleClass)))
  if (is.null(seq)) {
    if (scl$is_discrete()) seq <- scl$full_range()
    else stop("Value sequence 'seq' required for continuous attributes")
  }
  result <- list()
  for (value in seq) {
    alt <- alternative
    alt[[attribute$id]] <- value
    eval <- evaluate(model, alt, ...)
    if (scl$is_discrete()) index <- scl$values[value] else index <- as.character(value)
    result[[index]] <- eval
  }
  result
}

#' attribute_effect
#'
#' Given a single `alternative`, determine the effects of varying `attribute` on `target` attribute.
#'
#' @param model A [DexiModel] object. Required.
#' @param attribute A [DexiAttribute] with assigned discrete or continuous scale.
#' @param alternative A `data.frame` containing a single alternative.
#' @param target Target [DexiAttribute]. Defaults to `model$first()`.
#' @param seq A sequence of `attribute`'s numeric values for which to evaluate `alternative`.
#' For discrete scales: Must be a sequence of integers. Defaults to `attribute$scale$full_range()`.
#' For continuous scales: `seq` is required.
#' @param ... Optional parameters passed to [evaluate_attribute()].
#' @return A list of `target` evaluation results, indexed by the values of `seq`.
#' @export
#' @seealso [evaluate_attribute()]
#'
#' @examples
#' # Load "Car.dxi"
#' CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
#' Car <- read_dexi(CarDxi)
#'
# # Define some decision alternative (a car in this case) and evaluate it using default parameters
#' alt <- Car$alternative("MyCar",
#'          BUY.PRICE="low", MAINT.PRICE=2, X.PERS="more", X.DOORS="4", LUGGAGE=2, SAFETY="medium")
#' # Determine the effect of changing "SAFETY" balues on "CAR.1"
#' attribute_effect(Car, "SAFETY", alt)
#' # Returns a list of "CAR.1" values corresponding to consecutive values of "SAFETY"
#' attribute_effect(Car, "LUGGAGE", alt, "TECH.CHAR.")
#' # Returns a list of "TECH.CHAR." values corresponding to consecutive values of "LUGGAGE"
attribute_effect <- function(model, attribute, alternative, target = NULL, seq = NULL, ...) {
  stopifnot(is_class(model, DexiModelClass))
  attribute <- model$attrib(attribute)
  stopifnot(is_class(attribute, DexiAttributeClass))
  if (is.null(target)) target <- model$first()
  target <- model$attrib(target)
  stopifnot(is_class(target, DexiAttributeClass))
  eval_result <- evaluate_attribute(model, attribute, alternative, seq, ...)
  result <- list()
  for (i in seq_increasing(1, length(eval_result))) {
    eval <- eval_result[[i]]
    att_value <- eval[[attribute$id]][[1]]
    target_value <- eval[[target$id]][[1]]
    result[[att_value]] <- target_value
  }
  result
}

#' evaluate_attributes
#'
#' Apply `evaluate_attribute()` for all discrete `attributes`.
#'
#' @param model A [DexiModel] object. Required.
#' @param alternative A `data.frame` containing a single alternative.
#' @param attributes List of attributes or vector of attribute names, ID's or indices.
#' Default: All basic attributes of `model`.
#' @param ...  Optional parameters passed to [evaluate_attribute()].
#'
#' @return A list of [evaluate_attribute()] results for each `attribute`
#' @export
#' @seealso [evaluate_attribute()]
#'
#' @examples
#' # Load "Car.dxi"
#' CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
#' Car <- read_dexi(CarDxi)
#'
# # Define some decision alternative (a car in this case) and evaluate it using default parameters
#' alt <- Car$alternative("MyCar",
#'          BUY.PRICE="low", MAINT.PRICE=2, X.PERS="more", X.DOORS="4", LUGGAGE=2, SAFETY="medium")
#' safety <- Car$attrib("SAFETY")
#' # Perform evaluate_attribute() for all basic attributes of CarDxi
#' evaluate_attributes(Car, alt)
#' # Returns a list of evaluate_attribute() results corresponding to all basic attributes,
#' # indexed by attribute id
evaluate_attributes <- function(model, alternative, attributes = NULL, ...) {
  stopifnot(is_class(model, DexiModelClass))
  stopifnot(is.data.frame(alternative))
  stopifnot("A single alternative is required" = nrow(alternative) == 1)
  if (is.null(attributes)) attributes <- model$basic
  attributes <- model$attrib(attributes)
  result <- list()
  for (att in attributes) {
    if (!is.null(att$scale) && att$scale$is_discrete())
      result[[att$id]] <- evaluate_attribute(model, att, alternative, ...)
  }
  result
}
