#' @include DexiClasses.R
#' @include DexiScales.R
#' @include DexiFunctions.R
NULL

#' DexiAttribute
#'
#' `DexiAttribute` is a RC class representing a DEXi attribute in R.
#'
#' In a DEXi model, attributes are variables that represent observed properties of decision alternatives.
#' Attributes are structured in a tree, so each attribute may, but need not, have one or more direct
#' descendants (lower-level attributes) in the tree. Attributes without descendants are called \emph{basic}
#' and serve as model inputs. Attributes with one or more descendants are called \emph{aggregate} and
#' represent model outputs. In order to represent attribute hierarchies rather than plain trees, some attributes
#' may be \emph{linked}: two attributes of which one links to another one collectively represent,
#' in a conceptual sense, a single attribute in the hierarchy.
#'
#' When completely defined, each attribute is associated with a value scale represented by a
#' [DexiScale] object.
#' An object [DexiFunction] is also defined for each aggregate attribute, aimed at
#' defining the aggregation of the attribute's inputs to values of that attribute.
#'
#' @field name character. Name of the attribute as defined in the original DEXi model. Notice that such names
#' may not be unique and may contain characters that cannot be used for variable names in R.
#' @field id character. A unique identification of the attribute in the model. Derived from `name`
#' so that it can be used as a variable name in R.
#' @field description character. An optional textual description of the attribute.
#' @field inputs list of [DexiAttribute]s. A list of immediate descendants of this attribute in
#' the tree/hierarchy. `NULL` for basic attributes.
#' @field link [DexiAttribute]. `NULL` or a link to another [DexiAttribute]
#' @field scale [DexiScale]. Value scale associated with this attribute, or `NULL`.
#' @field funct [DexiFunction]. Aggregation function associated with this attribute, or `NULL`.
#' @field parent [DexiAttribute] or [DexiModel] (only for `DexiModel$root`). Parent
#' attribute of this attribute in the tree/hierarchy. The `DexiModel$root`'s parent
#' is the [DexiModel], which contains all those attributes.
#' @field .alternatives list. An internal field providing temporary storage for names or values of alternatives
#' while reading them from a `.dxi` file.
#'
#' @export DexiAttribute
#'
#' @examples
#' # Load "Car.dxi"
#' CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
#' Car <- read_dexi(CarDxi)
#'
#' # For example, consider attribute PRICE
#' att <- Car$attrib("PRICE")
#'
#' # Print fields and basic properties of att
#' att$verify()
#' att$name
#' att$id
#' att$description
#' att_names(att$inputs)
#' att$link
#' att$scale
#' att$funct
#' att_names(att$parent)
#' att$is_aggregate()
#' att$is_basic()
#' att$is_link()
#' att$level()
#' att$count()
#' att$ninp()
#' att$dim()
#' att$model()
#' att$structure()
#'
#' # Check if att affects attribute CAR
#' att$affects(Car$attrib("CAR"))
#'
#' # Find the index of other attributes in att's inputs
#' att$inp_index(Car$attrib("MAINT.PRICE"))
#' att$inp_index(Car$attrib("CAR"))

DexiAttribute <- setRefClass(DexiAttributeClass,
  fields = list(
   name = "character",
   id = "character",
   description = "character",
   inputs = "list", # of DexiAttribute
   link = "ANY",    # DexiAttribute
   scale = "ANY",   # DexiScaleClass,
   funct = "ANY",   # DexiFunctionClass,
   parent = "ANY",  # DexAttribute or DexiModel (for DexiModel$root)
   .alternatives = "list" # temporary storage while reading XML
  ),

  methods = list(
   initialize = function(
        name = "",
        description = "",
        inputs = list(),
        id = "",
        link = NULL,
        scale = NULL,
        funct = NULL,
        parent = NULL,
        ...)
     {
       "Initialize a \\code{DexiAttribute} object."

       name <<- name
       id <<- id
       description <<- description
       inputs <<- inputs
       link <<- link
       scale <<- scale
       funct <<- funct
       parent <<- parent
       .alternatives <<- list()
     },

   verify = function() {
     "Check the correctnes of a \\code{DexiAttribute} object and its fields. Result: \\code{error()} or \\code{TRUE}."

     stopifnot(is_single_character_or_null(name))
     stopifnot(is_single_character_or_null(description))
     stopifnot(length(inputs) == 0 || all(unlist(lapply(inputs, function(x) is_class(x, DexiAttributeClass)))))
     stopifnot(is_class_or_null(link, DexiAttributeClass))
     stopifnot(is_class_or_null(scale, DexiScaleClass))
     stopifnot(is_class_or_null(funct, DexiFunctionClass))
     stopifnot(is_class_or_null(parent, DexiAttributeClass) || is_class_or_null(parent, DexiModelClass))
     TRUE
   },

   ninp = function() {
     "Return the number of \\code{input}s of this attribute."
     length(inputs)
    },

   count = function() {
     "Return the number of \\code{input}s of this attribute."
     length(inputs)
    },

   dim = function() {
     "Dimensions of the value space determined by this attribute's \\code{inputs}.
     Result: a numeric vector of length equal to \\code{ninp()}, containing \\code{DexiScale$count()} of
     all descendant attributes, or \\code{NA} for attributes without associated scales.
     For basic attributes, \\code{dim()} returns \\code{NULL}."

     unlist(sapply(inputs,
        function (att) {
          if (is_class(att, DexiAttributeClass) && !is.null(att$scale)) att$scale$count() else NA
        }
     ))
   },

   show = function() {
     "Prints out the fields of this attribute."
     print(
       list(
         name = name,
         id = id,
         description = description,
         parent = if (is.null(parent) || !inherits(parent, DexiAttributeClass)) NULL else att_names(parent),
         inputs = att_names(inputs),
         link = if (is.null(link)) NULL else att_names(link),
         scale = scale,
         funct = funct
        )
     )
   },

   is_basic = function(include_linked = TRUE) {
     "Logical: \\code{TRUE} for basic attributes (attributes whose \\code{ninp() == 0}.
     \\code{include_linked} determines whether linked attributes are counted as basic
     (\\code{TRUE}) or not (\\code{FALSE})."

     if (ninp() > 0) FALSE
     else if (include_linked) TRUE
     else !is_link()
   },

   is_aggregate = function () {
     "Logical: \\code{TRUE} for aggregate attributes (attributes whose \\code{ninp() > 0})."
     ninp() > 0
   },

   is_link = function () {
     "Logical: Indicates whether or not this is a linked attribute."
     !is.null(link)
   },

   is_discrete = function() {
     "Logical: Indicates whether or not this is a discrete attribute."
     !is.null(scale) && scale$is_discrete()
   },

   is_continuous = function() {
     "Logical: Indicates whether or not this is a continuous attribute."
     !is.null(scale) && scale$is_continuous()
   },

   level = function() {
     "Return the level of this attribute in the hierarchy.
     The level of \\code{DexiModel$root} is 0."

     att <- parent
     lev <- 0
     while (! (is.null(att) || is_class(att, DexiModelClass))) {
       att <- att$parent
       lev <- lev + 1
     }
     lev
   },

   affects = function(ant) {
     "\\code{ant} (as \"antecedent\") is some \\code{\\link{DexiAttribute}}.
     The function returns \\code{TRUE} if \\code{ant} lies on the path leading from this attribute
     towards the root, and is therefore affected by this attribute."

     att <- parent
     while (! (is.null(att) || is_class(att, DexiModelClass))) {
       if (identical(att, ant)) return(TRUE)
       att <- att$parent
     }
     FALSE
   },

   model = function() {
     "Return the \\code{DexiModel} that contains this attribute."

     result <- parent
     while (! (is.null(result) || is_class(result, DexiModelClass))) {
       result <- result$parent
     }
     result
   },

   inp_index = function(inp) {
     "Return the index of attribute \\code{inp} in \\code{inputs} of this attribute."

     indices <- which(sapply(inputs, function (att) {identical(inp, att)}))
     if (length(indices) == 0) NA else indices[[1]]
    },

   tree_indent = function(none = " ", thru = "|", link = "*", last = "+", line = "-") {
     "Construct a string for representing the indentation of this attribute in the model structure.
     The arguments \\code{none}, \\code{thru}, \\code{link}, \\code{last} and \\code{line} are
     character strings to be used in the construction."

     result <- ""
     if (is.null(parent) || inherits(parent, DexiModelClass)) return(result)
     index <- parent$inp_index(.self)
     result <- if (index >= parent$ninp()) last else link
     att <- parent
     while (! (is.null(att$parent) || is_class(att$parent, DexiModelClass))) {
       index <- att$parent$inp_index(att)
       element <- if (index >= att$parent$ninp()) none else thru
       result <- paste0(element, result)
       att <- att$parent
     }
     result
   },

   structure = function () {
     "Make an indentation string for this attribute, used for printing it in \\code{show()}."

     tree_indent(none = "  ", thru = "| ", link = "|-", last = "+-", line = "--")
   }

  )
)

#' att_names
#'
#' Return names or IDs of [DexiAttribute] objects.
#'
#' @param atts A vector of [DexiAttribute]s.
#' @param use_id Determines whether to return attribute IDs or original DEXi names.
#'
#' @return A character vector of attribute IDs or names.
#' @export
#'
att_names <- function(atts, use_id = TRUE) {
  unname(sapply(c(atts),
    function (att)
      if (is_empty(att)) NA
      else if (!is_class(att, DexiAttributeClass)) NA
      else if (use_id) att$id
      else att$name
  ))
}

#' scale_of
#'
#' @param obj A [DexiAttribute] or [DexiScale].
#'
#' @return A [DexiScale] associated with `obj`, or `NA` for an undefined scale.
#' @export
#'
scale_of <- function(obj) {
  if (inherits(obj, DexiAttributeClass)) obj <- obj$scale
  if (inherits(obj, DexiScaleClass)) obj else NA
}
