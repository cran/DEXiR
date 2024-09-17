# Drawing DEXi charts.

#' @include DexiClasses.R
#' @include DexiUtils.R
#' @include DexiScales.R
#' @include DexiAlternatives.R
NULL

#' transparent_colors
#'
#' A helper function for making `colors` transparent.
#'
#' Requires installed package "grDevices".
#'
#' @param colors A vector of color numbers or names.
#' @param percent Required color transparency, in the range `[0:100]`.
#'
#' @return A vector of colors of the same length as `colors`.
#' @export
#'
#' @examples
#' transparent_colors(c("red", "green", "blue"), 50)
#' # c("#FF00007F", "#00FF007F", "#0000FF7F")
transparent_colors <- function(colors, percent = 50) {

  if (!requireNamespace("grDevices", quietly = TRUE)) {
    stop(
      "Package \"grDevices\" must be installed to use this function.",
      call. = FALSE
    )
  }

  rgbval <- grDevices::col2rgb(colors)
  apply(rgbval, 2, function(col) {
    grDevices::rgb(col[1], col[2], col[3], max = 255, alpha = (100 - percent) * 255 / 100)
  })
}

#' plotalt1
#'
#' Plot `alternatives` with respect to a single `attribute`.
#'
#' Standard scatterplot [base::plot] is used.
#'
#' @param model A [DexiModel] object. Required.
#' @param attribute A single [DexiAttribute] selector. It may be an [DexiAttribute] object or
#' an argument to `model$attrib()`. `attribute$scale` must be defined. Default: `model$first()`.
#' @param alternatives A `data.frame` of alternatives (normally an output of [evaluate()]) or
#' indices to `model$alternatives`. The default value `NULL` selects the whole `model$alternatives`.
#' @param colors `character(3)` representing colors corresponding to "bad", "neutral" and "good"
#' scale values, respectively. Default: `c("red", "black", "green")`.
#' @param pch Plotting character, see [graphics::points()]. Default: `20`.
#' @param size `numeric(1)`. Multiplication size factor for drawing individual points.
#' Base point size depends on `pch`.
#' @param linetype `integer()`. Line type for drawing chart grid. Default: `2`.
#' @param margins `numeric(4)`. Chart margins, passed to [graphics::par()] prior to drawing.
#' @param lm `numeric(1)`. Left chart margin. May be used to adjust the display of alternatives' names.
#' @param ... Optional parameters passed to [graphics::plot()].
#'
#' @return Draws a chart.
#' @export
#'
#' @examples
#' # Load "Car.dxi"
#' CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
#' Car <- read_dexi(CarDxi)
#'
#' # Plot all Car$alternatives with respect to "TECH.CHAR." attribute
#' plotalt1(Car, "TECH.CHAR.")
#'
#' # Plot the first Car alternative with respect to "MAINT.PRICE" attribute
#' plotalt1(Car, "MAINT.PRICE", 1)
#'
plotalt1 <- function(model,
                     attribute = model$first(),
                     alternatives = NULL,
                     colors = c("red", "black", "green"),
                     pch = 20,
                     size = 5.0,
                     linetype = 2,
                     margins = NULL,
                     lm = NULL,
                     ...
) {
  # Check & prepare arguments

  stopifnot(inherits(model, DexiModelClass))
  attribute <- model$attrib(attribute)
  stopifnot(inherits(attribute, DexiAttributeClass))
  stopif(is.null(attribute$scale))
  if (is.null(alternatives)) alternatives <- model$alternatives
  altinterval <- !is.list(alternatives) && is_all_integer(alternatives)
  if (altinterval) alternatives <- model$alternatives[as.integer(alternatives),]
  stopifnot(is.data.frame(alternatives))

  altvalues <- alternatives[[attribute$id]]
  nval <- length(altvalues)
  stopifnot(nval > 0)
  altnames <- alternatives[["name"]]
  if (is.null(altnames)) altnames <- row.names(alternatives)
  if (is.null(altnames)) atnames <- as.character(1:nval)
  nalt <- length(altnames)
  stopifnot(nalt == nval)
  qcol <- rep(colors, length = 3)

  # Construct points to be displayed

  x <- numeric()       # x coordinates of displayed points
  y <- numeric()       # y coordinates of displayed points
  colors = character() # colors of displayed points
  sizes = numeric()    # size of displayed points (considers distribution memberships)

  for (alt in 1:nalt) {
    value <- altvalues[alt]
    points <- expand_value_to_points(value, attribute, qcol)
    if (!is.data.frame(points)) next
    x <- append(x, rep(alt, nrow(points)))
    y <- append(y, points$points)
    colors <- append(colors, points$colors)
    sizes <- append(sizes, sqrt(points$sizes))
  }

  if (length(x) == 0) stop("Nothing to plot")

  # Hack to approximate width needed to display alternative names horizontally

  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op))
  if (!is.null(margins)) graphics::par(mar = margins)
  else if (!is.null(lm)) {
    mar <- graphics::par("mar")
    mar[2] <- lm
    graphics::par(mar = mar)
  }
  else {
    labelwidth <- max(sapply(altnames, nchar)) + 1
    mar <- graphics::par("mar")
    mar[2] <- labelwidth + 0.1
    graphics::par(mar=mar)
  }

  # Draw

  xlim <- if (attribute$is_discrete()) c(1 - 0.25, attribute$scale$count() + 0.25) else NULL
  xaxt <- if (attribute$is_discrete()) "n" else NULL

  plot(y = x, x = y, col = colors, cex = size * sizes,
       xlim = xlim,
       ylim = rev(c(0.5, nalt + 0.5)),
       xaxt = xaxt,
       yaxt = "n",
       pch = pch,
       panel.first = graphics::abline(h=1:nalt, lty = linetype, col ="gray"),
       xlab = attribute$id,
       ylab = "",
       ...)

  if (attribute$is_discrete()) {
    graphics::axis(1, at = 1:attribute$scale$count(), attribute$scale$values)
  }
  graphics::axis(2, at = 1:nalt, altnames, las = 2)
  graphics::mtext("Alternatives", side = 2, line = graphics::par("mar")[2] - 1.1, cex = 1.2)
}

#' plotalt2
#'
#' Draw a scatterpolot of `alternatives` with `attribute1` and `attribute2` on the
#' \eqn{x} and \eqn{y} axis, respectively.
#'
#' Standard scatterplot [graphics::plot()] is used.
#' Continuous attributes are not supported.
#'
#' @param model A [DexiModel] object. Required.
#' @param attribute1 First attribute. It may be an [DexiAttribute] object or
#' an argument to `model$attrib()`. The attribute must be discrete.
#' @param attribute2 Second attribute. It may be an [DexiAttribute] object or
#' an argument to `model$attrib()`. The attribute must be discrete.
#' @param alternatives A `data.frame` of alternatives (normally an output of [evaluate()]) or
#' indices to `model$alternatives`. The default value `NULL` selects the whole `model$alternatives`.
#' @param colors `character()`. Colors for displaying subsequent alternatives.
#' @param pch Plotting character, see [graphics::points()]. Default: `20`.
#' @param size `numeric(1)`. Multiplication size factor for drawing individual points.
#' Base point size depends on `pch`.
#' @param margins `numeric(4)`. Chart margins, passed to [graphics::par()] prior to drawing.
#' @param lm `numeric(1)`. Left chart margin. May be used to adjust the display of `attribute2`'s values.
#' @param pos A position specifier for legent text, see [graphics::text()]. Default: `4`.
#' @param offset When `pos` is specified, this value controls the distance of the text label
#' from the specified coordinate in fractions of a character width. Default: `1`.
#' @param ... Optional parameters passed to [graphics::plot()].
#'
#' @return Draws a chart.
#' @export
#'
#' @examples
#' # Load "Car.dxi"
#' CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
#' Car <- read_dexi(CarDxi)
#'
#' # Plot all Car$alternatives with respect to "PRICE" and "TECH.CHAR." attributes
#' plotalt2(Car, "PRICE", "TECH.CHAR.")
#'
#' # Plot the first Car alternative with respect to "BUY.PRICE" and "MAINT.PRICE" attributes
#' plotalt2(Car, "BUY.PRICE", "MAINT.PRICE", 1)
#'
plotalt2 <- function(model,
                     attribute1,
                     attribute2,
                     alternatives = NULL,
                     colors = NULL,
                     pch = 20,
                     size = 5.0,
                     margins = NULL,
                     lm = NULL,
                     pos = 4,
                     offset = 1,
                     ...
) {
  # Check & prepare arguments

  stopifnot(inherits(model, DexiModelClass))

  attribute1 <- model$attrib(attribute1)
  attribute2 <- model$attrib(attribute2)
  stopifnot(inherits(attribute1, DexiAttributeClass))
  stopifnot(inherits(attribute2, DexiAttributeClass))
  stopifnot(attribute1$is_discrete())
  stopifnot(attribute2$is_discrete())

  if (is.null(alternatives)) alternatives <- model$alternatives
  altinterval <- !is.list(alternatives) && is_all_integer(alternatives)
  if (altinterval) alternatives <- model$alternatives[as.integer(alternatives),]
  stopifnot(is.data.frame(alternatives))

  altvalues1 <- alternatives[[attribute1$id]]
  nval1 <- length(altvalues1)
  stopifnot(nval1 > 0)
  altvalues2 <- alternatives[[attribute2$id]]
  nval2 <- length(altvalues2)
  stopifnot(nval2 > 0)
  stopifnot(nval1 == nval2)

  altnames <- alternatives[["name"]]
  if (is.null(altnames)) altnames <- row.names(alternatives)
  if (is.null(altnames)) atnames <- as.character(1:nval1)
  nalt <- length(altnames)
  stopifnot(nalt == nval1)
  natt1 <- attribute1$scale$count()
  natt2 <- attribute2$scale$count()
  qcol <- c(colors, 1:nalt)

  # Construct points to be displayed

  x <- numeric()       # x coordinates of displayed points
  y <- numeric()       # y coordinates of displayed points
  colors = character() # colors of displayed points
  sizes = numeric()    # size of displayed points (considers distribution memberships)
  labels <- structure(rep("", natt1 * natt2), dim = c(natt1, natt2))
    # labels (alternative names) to be displayed along individual points

  for (alt in 1:nalt) {
    value1 <- altvalues1[alt]
    points1 <- expand_value_to_points(value1, attribute1)
    if (!is.list(points1)) next
    value2 <- altvalues2[alt]
    points2 <- expand_value_to_points(value2, attribute2)
    if (!is.list(points2)) next
    for (i in seq_increasing(1, length(points1$points))) {
      for (j in seq_increasing(1, length(points2$points))) {
        p1 <- points1$points[i]
        p2 <- points2$points[j]
        x <- append(x, p1)
        y <- append(y, p2)
        colors <- append(colors, qcol[alt])
        sizes <- append(sizes, sqrt(points1$sizes[i] * points2$sizes[j]))
        altname <- altnames[[alt]]
        labels[p1, p2] <- if (labels[p1, p2] == "") altname else paste0(labels[p1, p2], "\n", altname)
      }
    }
  }

  if (length(x) == 0) stop("Nothing to plot")

  # Hack to approximate width needed to display attribute2 values horizontally

  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op))

  if (!is.null(margins)) graphics::par(mar = margins)
  else if (!is.null(lm)) {
    mar <- graphics::par("mar")
    mar[2] <- lm
    graphics::par(mar = mar)
  }
  else {
    labelwidth <- max(sapply(attribute2$scale$values, nchar)) + 1
    mar <- graphics::par("mar")
    mar[2] <- labelwidth + 0.1
    graphics::par(mar=mar)
  }

  graphics::plot(x, y,
   cex = size * sizes,
   col = qcol,
   xlim = c(1 - 0.3, natt1 + 0.3),
   ylim = c(1 - 0.3, natt2 + 0.3),
   xaxt = "n",
   yaxt = "n",
   pch = pch,
   xlab = attribute1$id,
   ylab = "",
   panel.first = {
     sapply(1:natt1, function (i) graphics::abline(v = i, col = "gray", lty = 2))
     sapply(1:natt2, function (j) graphics::abline(h = j, col = "gray", lty = 2))
   },
   ...)

  graphics::axis(1, at = 1:natt1, attribute1$scale$values)
  graphics::axis(2, at = 1:natt2, attribute2$scale$values, las = 2)
  graphics::mtext(attribute2$id, side=2, line=mar[2] - 1 - 0.1)
  for (i in seq_increasing(1, natt1)) {
    for (j in seq_increasing(1, natt2)) {
      graphics::text(i, j, labels[i, j], pos = pos, offset = offset)
    }
  }
}

#' scale_alternatives
#'
#' A helper function for preparing alternatives' data for charts that involve multiple
#' attributes (such as [plotalt_parallel()]) and [plotalt_radar()]).
#' `scale_alternatives()` carries out three main operations:
#' \enumerate{
#'   \item{Aggregates DEXi values, represented by sets and distributions,
#'   into single numeric values, using one of the `aggregate` operators:
#'   `"minmax"`, `"min"`, `"max"` or `"mean"`,}
#'   \item{scales the aggregated values to the `[0,1]` interval so that they can be
#'    drawn uniformly on multiple chart axes,}
#'   \item{optionally "shifts" the values by a small amount to avoid overlapping chart lines.}
#' }
#'
#' @param model A [DexiModel] object. Required.
#' @param alternatives A `data.frame` of alternatives (normally an output of [evaluate()]) or
#' indices to `model$alternatives`. The default value `NULL` selects the whole `model$alternatives`.
#' @param attids `character()`. A character vector of [DexiAttribute] IDs to be included in the result.
#'  Default: all `model` attributes.
#' @param aggregate Determines how to aggregate DEXi values that are represented/interpreted
#' as sets in `alternatives`:
#' \describe{
#'   \item{`"min"`}{Uses the function [min()] to take the minimal set element.}
#'   \item{`"max"`}{Uses the function [max()] to take the maximal set element.}
#'   \item{`"mean"`}{Uses the function [mean()] to take the average set value.}
#'   \item{`"minmax"` (default)}{Takes both `"min"` and `"max"`, so that each alternative
#'   appears in the result twice.}
#'   \item{`"none"`}{No aggregation.}
#' }
#' Any distributions that appear in `alternatives` are interpreted as sets prior to aggregation.
#' The default operator `"minmax"` is suitable particularly for alternatives containing non-single-values
#' (sets and/or distributions). For alternatives containing only single numeric values, any of the
#' other three operators is preferred.
#' @param name `character(1)`, The name of the column in `alternatives` that contains
#' alternatives' names. Default: `"name"`.
#' @param shift `numeric(1)`. Used to "shift" numerical values by a small amount to avoid overlapping
#' lines in charts. Default: `0.01`.
#'
#' @return A list containing the elements:
#' \describe{
#'   \item{`data`}{A data frame containing the aggregated/scaled/shifted numeric values.}
#'   \item{`nalt`}{The number of alternatives. Notice that with `aggregate = "minmax"`,
#'   `data` contains twice as many rows.}
#'   \item{`groups`}{A numeric vector mapping `data` rows to `alternatives`' indices.}
#'   \item{`altnames`}{Names of alternatives.}
#'  }
#' @export
#' @seealso [plotalt_parallel()]), [plotalt_radar()])
#'
scale_alternatives <- function(model,
                                alternatives = NULL,
                                attids = NULL,
                                aggregate = c("minmax", "min", "max", "mean", "none"),
                                name = "name",
                                shift = 0.01
) {
  # Check & prepare arguments
  aggregate = match.arg(aggregate)
  stopifnot(inherits(model, DexiModelClass))
  if (is.null(alternatives)) alternatives <- model$alternatives
  altinterval <- !is.list(alternatives) && is_all_integer(alternatives)
  if (altinterval) alternatives <- model$alternatives[as.integer(alternatives),]
  stopifnot(is.data.frame(alternatives))
  stopifnot(name %in% names(alternatives))
  if (is.null(attids)) attids <- model$att_ids

  nalt <- nrow(alternatives)
  altnames <- alternatives[[name]]
  groups <- seq(1, nalt)

  if (aggregate == "minmax") {
    lb <- convert_alternatives(model, alternatives, aggregate = min)
    hb <- convert_alternatives(model, alternatives, aggregate = max)
    data <- rbind(lb, hb)
    groups <- rep(groups, 2)
  }
  else if (aggregate == "min") {
    data <- convert_alternatives(model, alternatives, aggregate = min)
  }
  else if (aggregate == "max") {
    data <- convert_alternatives(model, alternatives, aggregate = max)
  }
  else if (aggregate == "mean") {
    data <- convert_alternatives(model, alternatives, aggregate = mean)
  }
  else if (aggregate == "none") {
    # assuming that `alternatives` are already correctly scaled
    # may lead to problems later if not
    data <- alternatives
  }

  data <- data[names(data) %in% c(name, attids)]

  # shake data by `shift`
  if (!is.null(shift) && shift != 0.0) {
    shifts <- rep(seq(f = -(nalt %% 2) * shift, by = shift, length = nalt), 2)
    for (i in 1:nrow(data)) {
      if (shifts[i] != 0) data[i, 2:ncol(data)] <- data[i, 2:ncol(data)] + shifts[i]
    }
  }

  list(data = data, nalt = nalt, groups = groups, altnames = altnames)
}

#' ggplot_parallel
#'
#' Makes a basic `ggplot2` chart for displaying DEXi alternatives using parallel axes.
#' Generally, axes are uniformly scaled to the `[0,1]` interval.
#'
#' Uses [GGally::ggparcoord()] and requires package "GGally" to be installed.
#' Data presented in the chart is prepared by [scale_alternatives()].
#'
#' @param model A [DexiModel] object. Required.
#' @param alternatives A `data.frame` of alternatives (normally an output of [evaluate()]) or
#' indices to `model$alternatives`. The default value `NULL` selects the whole `model$alternatives`.
#' @param attids `character()`. A character vector of [DexiAttribute] IDs to be included in the result.
#' Default: all `model` attributes.
#' @param aggregate One of `"minmax"`, `"min"`, `"max"`, `"mean"` or "`none`". Determines how
#' to aggregate `alternatives`' values that are represented by sets or distributions.
#' @param name `character(1)`, The name of the column in `alternatives` that contains
#' alternatives' names. Default: `"name"`.
#' @param shift `numeric(1)`. Used to "shift" numeric values by a small amount to avoid overlapping
#' lines in charts. Default: `0.01`.
#' You may want to experiment with charts to determine the right value,
#'
#' @return A basic 'ggplot2' chart. Generally, this chart needs to be further enhanced
#' by graph layers, such as themes, labels, `geom_points()` and `geom_line()`.
#' See [plotalt_parallel()] that already provides some such layers.
#' @export
#' @seealso [scale_alternatives()], [plotalt_parallel()]
#'
#' @examples
#' if (requireNamespace("GGally", quietly = TRUE)) {
#'
#' # Load "Car.dxi"
#' CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
#' Car <- read_dexi(CarDxi)
#'
#' # Plot all Car$alternatives with points and lines
#' ggplot_parallel(Car) + ggplot2::geom_line(linewidth = 2) + ggplot2::geom_point(size = 3)
#' }
ggplot_parallel <- function(model,
                             alternatives = NULL,
                             attids = NULL,
                             aggregate = c("minmax", "min", "max", "mean", "none"),
                             name = "name",
                             shift = 0.01
) {

  if (!requireNamespace("GGally", quietly = TRUE)) {
    stop(
      "Package \"GGally\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }

  scaled <- scale_alternatives(model, alternatives, attids, aggregate, name, shift)
  data <- scaled$data

  GGally::ggparcoord(
    data = data,
    columns = 2:ncol(data),
    groupColumn = name,
    showPoints = TRUE,
    scale = "globalminmax"
  )
}

#' plotalt_parallel
#'
#' Makes and plots DEXi alternatives on parallel axes, corresponding to attributes.
#' Generally, axes are uniformly scaled to the `[0,1]` interval.
#'
#' Data presented in the chart is prepared by [scale_alternatives()].
#' `plotalt_parallel()` invokes [ggplot_parallel()] to make a basic chart and then
#' enhances it with graphic layers that are suitable for presenting DEXi alternatives.
#'
#' @param model A [DexiModel] object. Required.
#' @param alternatives A `data.frame` of alternatives (normally an output of [evaluate()]) or
#' indices to `model$alternatives`. The default value `NULL` selects the whole `model$alternatives`.
#' @param attids `character()`. A character vector of [DexiAttribute] IDs to be included in the result.
#'  Default: all `model` attributes.
#' @param aggregate One of `"minmax"`, `"min"`, `"max"`, `"mean"` or "`none`". Determines how
#' to aggregate `alternatives` values that are represented by sets or distributions.
#' @param name `character(1)`, The name of the column in `alternatives` that contains
#' alternatives' names. Default: `"name"`.
#' @param shift `numeric(1)`. Used to "shift" numeric results by a small amount to avoid overlapping
#' lines in charts. Default: `0.01`.
#' You may want to experiment with charts to determine the right value,
#' @param linewidth `numeric(1)`. Widths of lines drawn.
#' @param pointsize `numeric(1)`. Size of points drawn.
#' @param split One of:
#' \describe{
#' \item{`"no"`}{Draw all alternatives on the same chart.}
#' \item{`"v"`}{Split the chart vertically and draw alternatives separately.}
#' \item{`"h"`}{Split the chart horizontally and draw alternatives separately.}
#' }
#' @return A 'ggplot2' chart, enhanced with additional graph layers.
#' @export
#' @seealso [scale_alternatives()], [ggplot_parallel()]
#'
#' @examples
#' if (requireNamespace("GGally", quietly = TRUE)) {
#'
#' # Load "Car.dxi"
#' CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
#' Car <- read_dexi(CarDxi)
#'
#' # Plot all Car$alternatives with points and lines
#' plotalt_parallel(Car)
#'
#' # Show alternatives on two separate chart segments, shown one above the other.
#' plotalt_parallel(Car, split = "v")
#'
#' alts3 <- structure(
#' list(
#'   name = c("MyCar", "MyCar2", "MyCar1b"),
#'     CAR.1 = list(4L, 4L, c(1L, 4L)),
#'     PRICE = list(3L, 3L, c(1L, 3L)),
#'     BUY.PRICE = list(3L, 3L, 3L),
#'     MAINT.PRICE = list(2, 1, structure(c(0.1, 0.6, 0.3), class = "distribution")),
#'     TECH.CHAR. = list(3L, 3:4, 3L),
#'     COMFORT = list(3L, 2, 3L),
#'     X.PERS = list(3, 3, 3L),
#'     X.DOORS = list(3, 3, 3L),
#'     LUGGAGE = list(2L, 2L, 2),
#'     SAFETY = list(2, c(2, 3), 2)
#'     ),
#'     row.names = c(NA, -3L),
#'     class = "data.frame"
#'  )
#'
#' # Plot `alts2` with points and lines.
#' # Notice the "minmax" aggregation of sets and distributions.
#' plotalt_parallel(Car, alts3)
#' plotalt_parallel(Car, alts3, split = "v")
#'
#' # Now with "mean" aggregation
#' plotalt_parallel(Car, alts3, split = "v", aggregate = "mean")
#' }
plotalt_parallel <- function(model,
                             alternatives = NULL,
                             attids = NULL,
                             aggregate = c("minmax", "min", "max", "mean", "none"),
                             name = "name",
                             shift = 0.01,
                             linewidth = 2,
                             pointsize = 3,
                             split = c("no", "h", "v")
) {

  split <- match.arg(split)

  if (split %in% c("h", "v")) shift <- 0

  ggplot_parallel(model, alternatives, attids, aggregate, name, shift) +
    ggplot2::theme_bw() +
    ggplot2::geom_line(size = linewidth) +
    ggplot2::geom_point(size = pointsize) +
    ggplot2::scale_y_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1, 0.2)) +
    ggplot2::labs(x = "Attribute", y = "Scaled value", color = "Alternatives") +
    if (split == "v") ggplot2::facet_grid(name ~ .) else if (split == "h") ggplot2::facet_grid(. ~ name) else NULL
}

#' plotalt_radar
#'
#' Plots DEXi alternatives on a radar chart.
#' Generally, axes are uniformly scaled to the `[0,1]` interval.
#'
#' Uses [fmsb::radarchart()] and requires package "fmsb" to be installed.
#' Data presented in the chart is prepared by [scale_alternatives()].
#'
#' @param model A [DexiModel] object. Required.
#' @param alternatives A `data.frame` of alternatives (normally an output of [evaluate()]) or
#' indices to `model$alternatives`. The default value `NULL` selects the whole `model$alternatives`.
#' @param attids `character()`. A character vector of [DexiAttribute] IDs to be included in the result.
#'  Default: all `model` attributes.
#' @param aggregate One of `"minmax"`, `"min"`, `"max"`, `"mean"` or "`none`". Determines how
#' to aggregate `alternatives` values that are represented by sets or distributions.
#' @param name `character(1)`, The name of the column in `alternatives` that contains
#' alternatives' names. Default: `"name"`.
#' @param shift `numeric(1)`. Used to "shift" numeric values by a small amount to avoid overlapping
#' lines in charts. Default: `0.01`.
#' You may want to experiment with charts to determine the right value,
#' @param linewidth `numeric(1)`. Widths of lines drawn.
#' @param ptype A vector to specify point symbol: Default `16` (closed circle).
#' Should be 32 to not plot the points. This vector is repeatedly used for data series.
#' @param colors Colors to be used (repeatably) for data series. Default `1:8`.
#' @param unicolors A vector of one or two colors to be used for displaying the
#' minimum and maximum data series, respectively. Applies only when `split = TRUE`.
#' @param fillcolors A vector of color codes for filling polygons. Applies only when `fill = TRUE`.
#' @param transparency A number between `0` and `100` representing the transparency of
#' colors used for filling polygons.
#' @param circular `logical(1)`. Whether to make a circular (using [fmsb::radarchartcirc()])
#' or polygonal ([fmsb::radarchart()]) radar grid.
#' @param split `logical(1)`. Whether to plot all alternatives on a single chart (`FALSE`, default)
#' or make a series of plots of individual alternatives (`TRUE`).
#' @param fill `logical(1)`. Whether or not to fill polygons using `fillcolors`.
#' @param ... Optional parameters passed to [fmsb::radarchart()].
#'
#' @return Draws a chart or, when `split = TRUE` a series of charts corresponding to individual alternatives.
#' @export
#' @seealso [scale_alternatives()], [fmsb::radarchart()]
#'
#' @examples
#' if (requireNamespace("fmsb", quietly = TRUE)) {
#'
#' # Load "Car.dxi"
#' CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
#' Car <- read_dexi(CarDxi)
#'
#' # Plot all Car$alternatives with points and lines
#' plotalt_radar(Car)
#'
#' # Use different colors and fill polygons
#' plotalt_radar(Car, colors = c("blue", "brown"), fill = TRUE)
#' plotalt_radar(Car, colors = c("blue", "brown"), fillcolors = c("green", "red"), fill = TRUE)
#'
#' # Draw separate charts
#' plotalt_radar(Car, split = TRUE)
#'
#' # Draw separate charts, using the same color settings on all charts
#' plotalt_radar(Car, split = TRUE, unicolors = c("green", "red"))
#' plotalt_radar(Car, split = TRUE, unicolors = c("green", "red"), circular = TRUE)
#'
#' alts3 <- structure(
#' list(
#'   name = c("MyCar", "MyCar2", "MyCar1b"),
#'     CAR.1 = list(4L, 4L, c(1L, 4L)),
#'     PRICE = list(3L, 3L, c(1L, 3L)),
#'     BUY.PRICE = list(3L, 3L, 3L),
#'     MAINT.PRICE = list(2, 1, structure(c(0.1, 0.6, 0.3), class = "distribution")),
#'     TECH.CHAR. = list(3L, 3:4, 3L),
#'     COMFORT = list(3L, 2, 3L),
#'     X.PERS = list(3, 3, 3L),
#'     X.DOORS = list(3, 3, 3L),
#'     LUGGAGE = list(2L, 2L, 2),
#'     SAFETY = list(2, c(2, 3), 2)
#'     ),
#'     row.names = c(NA, -3L),
#'     class = "data.frame"
#'  )
#'
#' # The same chart types as above, but using more varied alternatives data
#' # Plot all Car$alternatives with points and lines
#' plotalt_radar(Car, alts3)
#'
#' # Use different colors and fill polygons
#' plotalt_radar(Car, alts3, colors = c("blue", "brown", "purple"), fill = TRUE)
#' plotalt_radar(Car, alts3, colors = c("blue", "brown", "purple"),
#'   fillcolors = c("green", "red", "yellow"), fill = TRUE)
#'
#' # Draw separate charts
#' plotalt_radar(Car, alts3, split = TRUE)
#' plotalt_radar(Car, alts3, split = TRUE, fill = TRUE)
#'
#' # Draw separate charts, using the same color settings on all charts
#' plotalt_radar(Car, alts3, split = TRUE, unicolors = c("red", "green"))
#' plotalt_radar(Car, alts3, split = TRUE, unicolors = c("green", "darkgreen"), fill = TRUE)
#' plotalt_radar(Car, alts3, split = TRUE, unicolors = c("red", "green"), circular = TRUE)
#'
#' }
plotalt_radar <- function(model,
                            alternatives = NULL,
                            attids = NULL,
                            aggregate = c("minmax", "min", "max", "mean", "none"),
                            name = "name",
                            shift = 0.01,
                            linewidth = 2,
                            ptype = 16,
                            colors = NULL,
                            unicolors = NULL,
                            fillcolors = NULL,
                            transparency = 85,
                            circular = FALSE,
                            split = FALSE,
                            fill = FALSE,
                            ...
) {
  if (!requireNamespace("fmsb", quietly = TRUE)) {
    stop(
      "Package \"fmsb\" must be installed to use this function.",
      call. = FALSE
    )
  }

  chart <- if (circular) fmsb::radarchartcirc else fmsb::radarchart

  if (split) shift <- FALSE

  scaled <- scale_alternatives(model, alternatives, attids, aggregate, name, shift)
  data <- scaled$data
  altnames <- scaled$altnames

  data <- rbind(rep(1, ncol(data)), rep(0, ncol(data)), scaled$data)
  groups <- c(0, 0, scaled$groups)
  nalt <- scaled$nalt

  colors <- rep(c(colors, 1:8)[1:nalt], 2)


  if (is.null(fillcolors)) fillcolors <- colors

  if (split) {
    for (i in 1:nalt) {

      pcol <- if (is.null(unicolors)) colors[scaled$groups == i] else unicolors
      pfcol <- if (fill) transparent_colors(pcol, transparency) else NA
      chart(data[groups %in% c(0, i), 2:ncol(data)],
            title = altnames[i],
            plwd = linewidth,
            plty = 1,
            pty = ptype,
            pcol = pcol,
            pfcol = pfcol,
            caxislabels = as.character(c(0, "", 0.5, "", 1)),
            axistype = 1,
            seg = 4,
            axislabcol = "gray40",
            ...)
    }
  }
  else
  {
    pfcol <- if (fill) transparent_colors(fillcolors, transparency) else NA
    chart(data[,2:ncol(data)],
          plwd = linewidth,
          plty = 1,
          pty = ptype,
          caxislabels = as.character(c(0, "", 0.5, "", 1)),
          axistype = 1,
          seg = 4,
          pcol = colors,
          pfcol = pfcol,
          axislabcol = "gray40",
          ...)

    graphics::legend("right", legend = altnames, col = colors, pch = ptype, lty = 1, lwd = linewidth, bty = "n")
  }

}
