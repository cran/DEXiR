#' DEXiR: A package for using DEXi models in R
#'
#' @name DEXiR-package
#'
#' @description DEXiR is a software package for using DEXi models in R. The main function
#' is evaluating decision alternatives using a model previously developed by DEXi software.
#'
#' @section DEXi Models:
#' DEXi models are hierarchical qualitative rule-based multi-criteria decision models developed using
#' the method DEX (Decision EXpert, \url{https://en.wikipedia.org/wiki/Decision_EXpert}),
#' using the program DEXi (\url{https://kt.ijs.si/MarkoBohanec/dexi.html})
#' or DEXiWin (\url{https://dex.ijs.si/dexisuite/dexiwin.html}).
#'
#' In general, a DEXi model consists of a hierarchy of qualitative (symbolic linguistic, discrete)
#' variables, called \emph{attributes}. Each attribute represents some observable property
#' (such as Price or Performance) of decision alternatives under study. An attribute can take
#' values from a set of words (such as "low; medium; high" or "unacc; acc; good; exc"), which is usually
#' small (up to five elements) and preferentially ordered from "bad" to "good" values.
#'
#' The \emph{hierarchy} of attributes represents a decomposition of a decision problem into
#' sub-problems, so that higher-level attributes depend on the lower-level ones. Consequently,
#' the terminal nodes represent inputs, and non-terminal attributes represent the outputs of the model.
#' Among these, the most important are one or more root attributes, which represent the final
#' evaluation(s) of the alternatives.
#'
#' The \emph{evaluation} of decision alternatives (i.e., hierarchical aggregation of values
#' from model inputs to outputs) is governed by \emph{decision rules}, defined for each
#' non-terminal attribute by the creator of the model (usually referred to as a "decision maker").
#'
#' @section Terminological remarks:
#'
#' \describe{
#'   \item{DEX}{DEX (Decision EXpert) refers to a general multi-attribute decision modeling method,
#'   characterized by using qualitative attribute hierarchies and decision tables.
#'   For further information, see (Trdin, Bohanec, 2018) and (Bohanec, 2022).}
#'   \item{DEXi}{DEXi ("DEX for instruction") refers to DEXi software.
#'   DEXi implements a subset of DEX, for instance, it is restricted to set-based evaluation methods.
#'   DEXi supports the creation and editing of \emph{DEXi models}, which are saved on `.dxi` files and
#'   subsequently read by DEXiR for processing in R. For further information on DEXi, see
#'   \url{https://kt.ijs.si/MarkoBohanec/dexi.html}.}
#'   \item{DEXiWin}{A new backward-compatible implementation of DEXi, aimed at gradually
#'   replacing it in the future. For further information on DEXiWin and
#'   related software, see \url{https://dex.ijs.si/dexisuite/dexisuite.html}.}
#'   \item{DEXiR}{DEXiR is this R package. It is capable of reading and processing DEXi models with
#'   some extensions towards the full DEX (for example, using value distributions).}
#' }
#'
#' @section DEXiR Functionality:
#'
#' Models developed using the DEXi software are stored in XML-formatted `.dxi` files. In
#' order to use DEXi models in R, DEXiR supports the following tasks:
#'
#' \enumerate{
#'  \item Reading DEXi models from `.dxi` files into the R environment, using \code{\link[DEXiR]{read_dexi}}.
#'  \item Making data frames containing data (both input and output) about considered decision alternatives,
#'    using \code{\link[DEXiR]{set_alternative}}.
#'  \item Evaluating decision alternatives, using \code{\link[DEXiR]{evaluate}}.
#'  \item Analyzing alternatives (\code{\link[DEXiR]{selective_explanation}}, \code{\link[DEXiR]{plus_minus}},
#'    \code{\link[DEXiR]{compare_alternatives}}).
#'  \item Drawing charts.
#' }
#'
#' By default, evaluation is based on sets, which is a standard evaluation procedure of DEXi. DEXiR
#' extends this by supporting:
#'
#' \itemize{
#'  \item evaluations using probabilistic and fuzzy value distributions (see \code{\link[DEXiR]{evaluate}});
#'  \item "pruned" evaluation, when the evaluation starts from selected non-terminal  attribute(s) upwards.
#' }
#'
#' @section Limitations:
#' DEXiR has been designed to facilitate \emph{using} DEXi models in R
#' produced externally by the DEXi software.
#' DEXiR does not provide any explicit means for creating and/or editing DEXi models in R.
#'
#' @section A typical DEXiR workflow:
#' This example uses a simple DEXi model for evaluating cars, which is distributed
#' together with the DEXi software (including DEXiR) and is used throughout DEX literature
#' to illustrate the methodological approach (\url{https://en.wikipedia.org/wiki/Decision_EXpert}).
#'
#' First, this model is loaded into R and printed as follows:
#'
#' \preformatted{
#' > Car <- read_dexi("data/Car.dxi")
#' > Car
#' DEXi Model:  CAR_MODEL
#' Description: Car demo
#' index id          structure          scale                     funct
#'   [1] CAR_MODEL   CAR_MODEL
#'   [2] CAR         +- CAR             unacc; acc; good; exc (+) 12 3x4
#'   [3] PRICE         |- PRICE         high; medium; low (+)     9 3x3
#'   [4] BUY.PRICE     | |- BUY.PRICE   high; medium; low (+)
#'   [5] MAINT.PRICE   | +- MAINT.PRICE high; medium; low (+)
#'   [6] TECH.CHAR.    +- TECH.CHAR.    bad; acc; good; exc (+)   9 3x3
#'   [7] COMFORT         |- COMFORT     small; medium; high (+)   36 3x4x3
#'   [8] X.PERS          | |- #PERS     to_2; 3-4; more (+)
#'   [9] X.DOORS         | |- #DOORS    2; 3; 4; more (+)
#'  [10] LUGGAGE         | +- LUGGAGE   small; medium; big (+)
#'  [11] SAFETY          +- SAFETY      small; medium; high (+)
#' }
#'
#' Rows in the table correspond to individual attributes. The columns represent the following:
#' \describe{
#'   \item{`index`}{Indices of attributes.}
#'   \item{`id`}{Unique attribute names, generated by DEXiR from original DEXi names, in order
#'    to provide syntactically correct variable names in R and allow unambiguous referencing of attributes.}
#'   \item{\code{structure}}{The hierarchical structure of attributes, named as in the original DEXi model.}
#'   \item{\code{scale}}{Value scales associated with each attribute. The symbol "(+)" indicates that the corresponding scale
#'    is ordered preferentially in increasing order.}
#'   \item{\code{funct}}{Information about the size (number of rules) and dimensions of the corresponding decision tables.}
#' }
#'
#' Looking at the structure of attributes, please notice that the attribute at index `[1]` is virtual and
#' does not actually appear in the original DEXi model. It is necessary in DEXiR to facilitate models that
#' have multiple root attributes. The "real" root of the Car model is actually `[2]` CAR. It depends on
#' two lower-level attributes, PRICE and TECH.CHAR. These are decomposed further. Overall, the model consists of
#'
#' \itemize{
#'   \item six input (\emph{basic}) attributes: BUY.PRICE, MAINT.PRICE, X.PERS, X.DOORS, LUGGAGE and SAFETY, and
#'   \item four output (\emph{aggregate}) attributes: CAR, PRICE, TECH.CHAR. and COMFORT.
#' }
#'
#' Among the latter, CAR is the most important and represents the overall evaluation of cars.
#'
#' The next step usually consists of defining a data frame representing decision alternatives
#' (i.e., cars in this case).
#' The Car model already comes with a data table about two cars:
#'
#' \preformatted{
#' > Car$alternatives
#'   name CAR PRICE BUY.PRICE MAINT.PRICE TECH.CHAR. COMFORT X.PERS X.DOORS LUGGAGE SAFETY
#' 1 Car1   4     3         2           3          4       3      3       3       3      3
#' 2 Car2   3     2         2           2          3       3      3       3       3      2
#' }
#'
#' In this data frame, attribute values are represented by ordinal numbers w.r.t. the corresponding scales.
#' A more readable output can be made using `DexiModel$as_character`:
#'
#' \preformatted{
#' > Car$as_character(Car$alternatives)
#'   name  CAR  PRICE BUY.PRICE MAINT.PRICE TECH.CHAR. COMFORT X.PERS X.DOORS LUGGAGE SAFETY
#' 1 Car1  exc    low    medium         low        exc    high   more       4     big   high
#' 2 Car2 good medium    medium      medium       good    high   more       4     big medium
#' }
#'
#' This data can be edited using common R data.frame functions. Also, DEXiR provides the method
#' `DexiModel$alternative` for defining a single decision alternative, for example:
#'
#' \preformatted{
#' > alt <- Car$alternative("MyCar1",
#'          BUY.PRICE="low", MAINT.PRICE=2, X.PERS="more", X.DOORS="4",
#'          LUGGAGE=2, SAFETY="medium")
#' > alt
#'     name CAR PRICE BUY.PRICE MAINT.PRICE TECH.CHAR. COMFORT X.PERS X.DOORS LUGGAGE SAFETY
#' 1 MyCar1  NA    NA         3           2         NA      NA      3       3       2      2
#' }
#'
#' Finally, such data tables can be evaluated using `DexiModel$evaluate`:
#'
#' \preformatted{
#' > eval <- Car$evaluate(alt)
#' > eval
#'     name CAR PRICE BUY.PRICE MAINT.PRICE TECH.CHAR. COMFORT X.PERS X.DOORS LUGGAGE SAFETY
#' 1 MyCar1   4     3         3           2          3       3      3       3       2      2
#' > Car$as_character(eval)
#'     name CAR PRICE BUY.PRICE MAINT.PRICE TECH.CHAR. COMFORT X.PERS X.DOORS LUGGAGE SAFETY
#' 1 MyCar1 exc   low       low      medium       good    high   more       4  medium medium
#' }
#'
#' @section Analysis of alternatives:
#' Once defined and evaluated, alternatives can be analysed further.
#' DEXiR provides three analysis methods:
#' \describe{
#' \item{\code{\link[DEXiR]{selective_explanation}}}{Exposing particular weak and strong points of alternatives.}
#' \item{\code{\link[DEXiR]{plus_minus}} analysis}{Exploring effects of changing individual attributes to evaluation results.}
#' \item{\code{\link[DEXiR]{compare_alternatives}}}{Comparison of an alternative with other alternatives.}
#' }
#'
#' Examples:
#'
#' \preformatted{
#' > Car$selective_explanation(1)
#'
#' Selective explanation of Car1
#'
#' Weak points:
#' None
#'
#' Strong points:
#'  id          structure         Car1
#'  CAR.1       +-CAR             4
#'  PRICE         |-PRICE         3
#'  MAINT.PRICE   | +-MAINT.PRICE 3
#'  TECH.CHAR.    +-TECH.CHAR.    4
#'  COMFORT         |-COMFORT     3
#'  X.PERS          | |-#PERS     3
#'  LUGGAGE         | +-LUGGAGE   3
#'  SAFETY          +-SAFETY      3
#'
#' > Car$plus_minus(1, as_character = TRUE)
#'  id          structure         -2    -1    CAR.1=exc 1
#'  BUY.PRICE     | |-BUY.PRICE   [     unacc medium    exc
#'  MAINT.PRICE   | +-MAINT.PRICE unacc exc   low       ]
#'  X.PERS          | |-#PERS     unacc exc   more      ]
#'  X.DOORS         | |-#DOORS    unacc exc   4         exc
#'  LUGGAGE         | +-LUGGAGE   unacc exc   big       ]
#'  SAFETY          +-SAFETY      unacc exc   high      ]
#'
#' > Car$compare_alternatives(1, as_character = TRUE)
#'  id          structure         Car1   Car2
#'  CAR         CAR               NULL   NULL
#'  CAR.1       +-CAR             exc    > good
#'  PRICE         |-PRICE         low    > medium
#'  BUY.PRICE     | |-BUY.PRICE   medium
#'  MAINT.PRICE   | +-MAINT.PRICE low    > medium
#'  TECH.CHAR.    +-TECH.CHAR.    exc    > good
#'  COMFORT         |-COMFORT     high
#'  X.PERS          | |-#PERS     more
#'  X.DOORS         | |-#DOORS    4
#'  LUGGAGE         | +-LUGGAGE   big
#'  SAFETY          +-SAFETY      high   > medium
#'}
#'
#' @section Charts:
#' Evaluation results can be drawn on charts. DEXiR provides four charts that display multiple alternatives:
#' \describe{
#' \item{\code{\link[DEXiR]{plotalt1}}}{with respect to a single attribute, drawing a scatterplot "alternatives by attribute-values"}
#' \item{\code{\link[DEXiR]{plotalt2}}}{with respect to two attributes, drawing a scatterplot "attribute1 by attribute2"}
#' \item{\code{\link[DEXiR]{plotalt_parallel}}}{with respect to multiple attributes, drawing evaluation results  using parallel axes}
#' \item{\code{\link[DEXiR]{plotalt_radar}}}{with respect to multiple attributes, drawing evaluation results on a radar chart}
#' }
#' The latter two plots scale evaluation results to the `[0:1]` interval.
#' Evaluation values represented by sets or distributions are plotted either as intervals
#' (`aggregate = "minmax"`) or are aggregated to a single value
#' (`aggregate = "min"`, `"max"` or `"mean"`).
#'
#' Examples:
#'
#' \preformatted{
#' Plot all Car alternatives with respect to Car$first() ("CAR.1"))
#' > plotalt1(Car)
#'
#' Plot evaluation results of all Car alternatives with respect to attribute "PRICE"
#' > plotalt1(Car, "PRICE")
#'
#' Draw "TECH.CHAR." by "PRICE" scatterplot of all Car alternatives
#' > plotalt2(Car, "TECH.CHAR.", "PRICE")
#'
#' Draw a "TECH.CHAR." by "PRICE" scatterplot of the second Car alternative
#' > plotalt2(Car, "TECH.CHAR.", "PRICE", 2)
#'
#' Draw all Car alternatives on parallel axes
#' > plotalt_parallel(Car)
#'
#' Draw all Car alternatives on a radar chart
#' > plotalt_radar(Car)
#'}
#'
#' @section On the use of values in DEXi models:
#'
#' \emph{DEXi values} are used throughout DEXi models.
#' They provide input values and carry results of evaluations in data frames that contain data
#' about decision alternatives.
#' Values are also used in definitions of [DexiFunction]s and are returned by
#' `DexiFunction$evaluate` when evaluating some function for a given set of arguments.
#'
#' In DEXi, values are always bound to the context provided by a [DexiScale]. Since each
#' fully defined [DexiAttribute] is associated with some scale, we can generalize the
#' scale context to attributes and speak about "assigning some value to an attribute".
#'
#' The scale type determines the type and possible range of values that can be assigned to an attribute.
#' DEXiR implements two scale types: [DexiContinuousScale] and [DexiDiscreteScale].
#' Regarding the values, the former is really simple: it allows assigning any single real number to the
#' corresponding attribute. In other words, continuous DEXi values are of type numeric(1).
#'
#' [DexiDiscreteScale] is the main scale type used throughout DEXi models and supports
#' a wider range of value types.
#'
#' The "normal" and most common discrete value is a "single qualitative value".
#' For illustration, let us use the scale composed of four qualitative values:
#' `"unacc"`, `"acc"`, `"good"`, `"exc"`. Then, "a single qualitative value" denotes
#' one of these words. Internally in DEXiR, such values are not represented by character strings, but rather
#' by ordinal numbers, so that ord(`"unacc"`) = 1, ord(`"acc"`) = 2, etc. Some DEXiR functions
#' can convert between the two representations, see `DexiModel$as_character`
#' and [set_alternative()].
#'
#' In order to cope with missing, incomplete or uncertain data, DEX extends the concept of single values
#' to value \emph{sets} and \emph{distributions}. In DEXiR, wherever it is possible to use a single qualitative
#' value, it is also possible to use a value set or distribution. This is the main reason that all DEXiR
#' data structures related to DEXi values are represented by lists rather than plain vectors.
#' This includes all data frames that represent decision alternatives and all functions that return
#' qualitative values. Also note that while sets are fully implemented in the current DEXi software,
#' distributions are not and are thus considered extensions towards the full DEX method.
#'
#' A \emph{DEXi value set} is a subset of the full range of a [DexiDiscreteScale] values.
#' For the above example, the full range of ordinal values is `1:4`, and some possible subsets are
#' `c(2)`, `c(2, 4)`, `c(1, 2, 3)` and `1:4`. Internally, sets are represented by
#' plain integer vectors or plain numeric vectors containing integer numbers.
#'
#' A \emph{DEXi value distribution} associates each [DexiDiscreteScale] value with some
#' number, generally denoted \eqn{p} and normally expected to be in the \[0,1\] interval.
#' Depending on the context and used evaluation method (see [evaluate()]), \eqn{p} can be
#' interpreted as \emph{probability} or \emph{fuzzy set membership}. In DEXiR, value distributions are
#' represented using the S3 class "distribution" (see [distribution]).
#' For example, `distribution(0.5, 0, 0.2, 0.3)` represents a value distribution over the
#' above scale example, assigning
#' \eqn{p = 0.5} to `"unacc"`,
#' \eqn{p = 0.0} to `"acc"`,
#' \eqn{p = 0.2} to `"good"` and
#' \eqn{p = 0.3} to `"exc"`.
#'
#' Remarks:
#' \itemize{
#'   \item The value `distribution(0.5, 0, 0.2, 0.3)` is internally represented as `c(0.5, 0, 0.2, 0.3)`,
#'     whose `class()` is `"distribution"`.
#'   \item Using a special class for distributions is necessary to distinguish them from sets. For instance, the
#'     notation `c(1, 1)` is ambiguous and would be interpreted differently as a set or distribution.
#'   \item Some DEXiR functions (see `DexiModel$as_character` and [set_alternative()])
#'    support the formulation of distributions in the form of named vectors or lists, for instance
#'     `list(unacc=0.5, good=0.2, exc=0.3)`.
#'   \item In data frames that contain data about decision alternatives, numeric vectors that contain
#'     non-integer values are implicitly interpreted as distributions rather than sets.
#'}
#'
#' @section Examples of using value sets and distributions:
#'
#' First, let us consider a car for which we have no evidence about its possible maintenance costs.
#' For the value of `MAINT.PRICE`, we may use `"*"`, which denotes the full range of
#' the corresponding attribute values (equivalent to `1:3` or `c(1, 2, 3)` in this case).
#' Notice how the evaluation method considers all the possible values of `MAINT.PRICE`
#' and propagates them upwards.
#'
#' \preformatted{
#' alt <- Car$alternative("MyCar1a",
#'        BUY.PRICE="low", MAINT.PRICE="*", X.PERS="more", X.DOORS="4", LUGGAGE=2, SAFETY=2)
#' Car$evaluate(alt)
#'      name  CAR PRICE BUY.PRICE MAINT.PRICE TECH.CHAR. COMFORT X.PERS X.DOORS LUGGAGE SAFETY
#' 1 MyCar1a 1, 4  1, 3         3     1, 2, 3          3       3      3       3       2      2
#' }
#'
#' The above evaluation result is not really useful, as the car turns out to be `c(1, 4)`, that is,
#' either `"unacc"` or `"exc"`, depending on maintenance costs.
#' Thus, let us try using value distribution for MAINT.PRICE, telling DEXiR that low maintenance costs
#' are somewhat unexpected (\eqn{p = 0.1}) and that medium costs (\eqn{p = 0.6}) are more likely than
#' high (\eqn{p = 0.3}). Using the evaluation method `"prob"` (where \eqn{p}'s are interpreted
#' as probabilities) gives the following results:
#'
#' \preformatted{
#' alt <- Car$alternative("MyCar1b",
#'        BUY.PRICE="low", MAINT.PRICE=distribution(0.1, 0.6, 0.3),
#'        X.PERS="more", X.DOORS="4", LUGGAGE=2, SAFETY=2)
#' Car$evaluate(alt, method = "prob")
#'      name                CAR         PRICE BUY.PRICE   MAINT.PRICE TECH.CHAR. COMFORT X.PERS X.DOORS LUGGAGE SAFETY
#' 1 MyCar1b 0.1, 0.0, 0.0, 0.9 0.1, 0.0, 0.9         3 0.1, 0.6, 0.3 0, 0, 1, 0 0, 0, 1      3       3       2      2
#' }
#'
#' In this case, the final evaluation of `CAR` is `distribution(0.1, 0.0, 0.0, 0.9)`, that is,
#' `list(unacc=0.1, exc=0.9)`. It is much more likely that `MyCar1b` is `"exc"` than `"unacc"`.

#' @section References:
#' \itemize{
#'   \item \emph{Decision EXpert}. Wikipedia, \url{https://en.wikipedia.org/wiki/Decision_EXpert}.
#'   \item Trdin, N., Bohanec, M.: Extending the multi-criteria decision making method DEX
#'         with numeric attributes, value distributions and relational models.
#'         \emph{Central European Journal of Operations Research}, 1-24, 2018
#'         \doi{10.1007/s10100-017-0468-9}.
#'   \item Bohanec, M.: DEX (Decision EXpert): A Qualitative Hierarchical Multi-criteria Method.
#'         In: Kulkarni, A.J. (ed.):
#'         \emph{Multiple Criteria Decision Making: Techniques, Analysis and Applications}.
#'         Singapore: Springer, 39-78, 2022
#'         \doi{10.1007/978-981-16-7414-3_3}.
#'   \item \emph{DEXi: A Program for Multi-Attribute Decision Making}.
#'         \url{https://kt.ijs.si/MarkoBohanec/dexi.html}.
#'   \item Bohanec, M.: \emph{DEXi: Program for Multi-Attribute Decision Making, User's Manual, Version 5.04}.
#'         IJS Report DP-13100, Jožef Stefan Institute, Ljubljana, 2020.
#'         \url{https://kt.ijs.si/MarkoBohanec/pub/DEXiManual504.pdf}.
#'   \item Bohanec, M.: \emph{ DEXiWin: DEX Decision Modeling Software, User’s Manual, Version 1.2}.
#'         IJS Report DP-14741, Jožef Stefan Institute, Ljubljana, 2024.
#'         \url{https://kt.ijs.si/MarkoBohanec/pub/2024_DP14747_DEXiWin.pdf}.
#'   \item \emph{DEX Software}. \url{https://dex.ijs.si}.
#' }
#'
"_PACKAGE"
NULL
#> NULL
