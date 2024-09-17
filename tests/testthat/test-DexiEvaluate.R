test_that("evaluation_order() works", {
  evalord <- evaluation_order(CarDxi$root)
  expect_equal(evalord,
    c("BUY.PRICE", "MAINT.PRICE", "PRICE", "X.PERS", "X.DOORS", "LUGGAGE", "COMFORT", "SAFETY", "TECH.CHAR.", "CAR", "CAR_MODEL"))
  evalord <- evaluation_order(CarDxi$root, prune = "COMFORT")
  expect_equal(evalord,
    c("BUY.PRICE", "MAINT.PRICE", "PRICE", "COMFORT", "SAFETY", "TECH.CHAR.", "CAR", "CAR_MODEL"))
  evalord <- evaluation_order(CarDxi$root, prune = c("TECH.CHAR.", "PRICE"))
  expect_equal(evalord, c("PRICE", "TECH.CHAR.", "CAR", "CAR_MODEL"))
})

test_that("evaluate_as_set() works on CarDxi/CAR", {
  att <- CarDxi$attrib("CAR")
  fnc <- att$funct

  inps <- list(1, 1)
  eval <- evaluate_as_set(fnc, inps)
  expect_equal(eval, 1)

  inps <- list(2, 2)
  eval <- evaluate_as_set(fnc, inps)
  expect_equal(eval, 2)

  inps <- list(c(2, 1), 2)
  eval <- evaluate_as_set(fnc, inps)
  expect_equal(eval, c(1, 2))

  inps <- list(c(2, 1), c(2, 3))
  eval <- evaluate_as_set(fnc, inps)
  expect_equal(eval, c(1, 2, 3))
})

test_that("evaluate_as_set() works on set-expanded CarDxi/CAR", {
  att <- CarDxi$attrib("CAR")
  fnc <- DexiTabularFunction(att, values = att$funct$values)
  fnc$values[[1]] <- c(1,2)

  inps <- list(1, 1)
  eval <- evaluate_as_set(fnc, inps)
  expect_equal(eval, c(1, 2))

  inps <- list(2, 2)
  eval <- evaluate_as_set(fnc, inps)
  expect_equal(eval, 2)

  inps <- list(c(3, 1), c(4, 1))
  eval <- evaluate_as_set(fnc, inps)
  expect_equal(eval, c(1, 2, 4))
})

test_that("evaluate_as_distribution(method = 'prob') works on CarDxi/CAR", {
  att <- CarDxi$attrib("CAR")
  fnc <- att$funct
  eval_param <- evaluation_parameters("prob")

  inps <- list(distribution(1), c(1))
  eval <- evaluate_as_distribution(fnc, inps, eval_param)
  expect_equal(eval, distribution(1, 0, 0, 0))

  inps <- list(distribution(1, 0, 1), c(1))
  eval <- evaluate_as_distribution(fnc, inps, eval_param)
  expect_equal(eval, distribution(2, 0, 0, 0))

  inps <- list(distribution(0.1, 0.3, 0.6), c(0.1, 0.2, 0.3, 0.4))
  eval <- evaluate_as_distribution(fnc, inps, eval_param)
  expect_equal(eval, distribution(0.19, 0.06, 0.21, 0.54))
})

test_that("evaluate_as_distribution(method = 'fuzzy') works on CarDxi/CAR", {
  att <- CarDxi$attrib("CAR")
  fnc <- att$funct
  eval_param <- evaluation_parameters("fuzzy")

  inps <- list(distribution(1), c(1))
  eval <- evaluate_as_distribution(fnc, inps, eval_param)
  expect_equal(eval, distribution(1, 0, 0, 0))

  inps <- list(distribution(1, 0, 1), c(1))
  eval <- evaluate_as_distribution(fnc, inps, eval_param)
  expect_equal(eval, distribution(1, 0, 0, 0))

  inps <- list(distribution(0.1, 0.3, 0.6), c(0.1, 0.2, 0.3, 0.4))
  eval <- evaluate_as_distribution(fnc, inps, eval_param)
  expect_equal(eval, distribution(0.1, 0.2, 0.3, 0.4))

  inps <- list(distribution(0.2, 0.4, 1), c(0.1, 0.5, 0.7, 1))
  eval <- evaluate_as_distribution(fnc, inps, eval_param)
  expect_equal(eval, distribution(0.2, 0.4, 0.5, 1))
})

test_that("evaluate_as_distribution(method = 'prob') works on set-expanded CarDxi/CAR", {
  att <- CarDxi$attrib("CAR")
  fnc <- DexiTabularFunction(att, values = att$funct$values)
  fnc$values[[1]] <- c(1, 2)
  fnc$values[[2]] <- c(2, 3)

  eval_param <- evaluation_parameters("prob")

  inps <- list(distribution(1), c(1))
  eval <- evaluate_as_distribution(fnc, inps, eval_param)
  expect_equal(eval, distribution(0.5, 0.5, 0, 0))

  inps <- list(distribution(1, 0, 1), c(1))
  eval <- evaluate_as_distribution(fnc, inps, eval_param)
  expect_equal(eval, distribution(1.5, 0.5, 0, 0))

  inps <- list(distribution(0.1, 0.3, 0.6), c(0.1, 0.2, 0.3, 0.4))
  eval <- evaluate_as_distribution(fnc, inps, eval_param)
  expect_equal(eval, distribution(0.155, 0.08, 0.225, 0.54))
})

test_that("Plain evaluation of CarDxi", {
  alts0 <- CarDxi$alternatives
  alts <- alts0
  alts[, CarDxi$aggregate_ids] <- NA
  eval <- evaluate(CarDxi, alts)
  expect_equal(unlist(alts0[1,]), unlist(eval[1,]))
  expect_equal(unlist(alts0[2,]), unlist(eval[2,]))
  expect_true(all(unlist(alts0[1,]) == unlist(eval[1,])))
  expect_false(all(unlist(alts0[1,]) == unlist(eval[2,])))
})

test_that("Plain pruned evaluation of CarDxi", {
  alts0 <- CarDxi$alternatives
  alts <- alts0
  eval <- evaluate(CarDxi, alts, prune = "PRICE")
  unchanged <- c("name", "CAR", "PRICE", "TECH.CHAR.", "COMFORT", "X.PERS", "X.DOORS", "LUGGAGE", "SAFETY")
  expect_equal(unlist(alts0[1, unchanged]), unlist(eval[1, unchanged]))
  expect_equal(unlist(alts0[2, unchanged]), unlist(eval[2, unchanged]))
  expect_true(all(is.na(eval[(c("BUY.PRICE","MAINT.PRICE"))])))
})

test_that("Set evaluation of CarDxi", {
  alt0 <- CarDxi$alternatives[1,]
  alt <- set_alternative(CarDxi, alt0, BUY.PRICE="*")
  alt[, CarDxi$aggregate_ids] <- NA
  eval <- evaluate(CarDxi, alt)
  unchanged <- c("MAINT.PRICE", "TECH.CHAR.", "COMFORT", "X.PERS", "X.DOORS", "LUGGAGE", "SAFETY")
  expect_equal(unlist(alt0[1, unchanged]), unlist(eval[1, unchanged]))
  expect_equal(eval[1, "BUY.PRICE"], list(c(1,2,3)))
  expect_equal(unname(unlist(eval[1, "PRICE"])), c(1,3))
  expect_equal(unname(unlist(eval[1, "CAR"])), c(1,4))

  alt0 <- CarDxi$alternatives[2,]
  alt <- set_alternative(CarDxi, alt0, SAFETY="*")
  alt[, CarDxi$aggregate_ids] <- NA
  eval <- evaluate(CarDxi, alt)
  unchanged <- c("PRICE", "BUY.PRICE", "MAINT.PRICE", "COMFORT", "X.PERS", "X.DOORS", "LUGGAGE")
  expect_equal(unlist(alt0[1, unchanged]), unlist(eval[1, unchanged]))
  expect_equal(eval[1, "SAFETY"], list(c(1,2,3)))
  expect_equal(unname(unlist(eval[1, "TECH.CHAR."])), c(1,3,4))
  expect_equal(unname(unlist(eval[1, "CAR"])), c(1,3,4))

  alt0 <- CarDxi$alternatives[2,]
  alt <- set_alternative(CarDxi, alt0, SAFETY=c(2,3))
  alt[, CarDxi$aggregate_ids] <- NA
  eval <- evaluate(CarDxi, alt)
  unchanged <- c("PRICE", "BUY.PRICE", "MAINT.PRICE", "COMFORT", "X.PERS", "X.DOORS", "LUGGAGE")
  expect_equal(unlist(alt0[1, unchanged]), unlist(eval[1, unchanged]))
  expect_equal(eval[1, "SAFETY"], list(c(2,3)))
  expect_equal(unname(unlist(eval[1, "TECH.CHAR."])), c(3,4))
  expect_equal(unname(unlist(eval[1, "CAR"])), c(3,4))
})

test_that("Prob evaluation of CarDxi", {
  alt0 <- CarDxi$alternatives[1,]
  alt <- set_alternative(CarDxi, alt0, BUY.PRICE="*")
  alt[, CarDxi$aggregate_ids] <- NA
  eval <- evaluate(CarDxi, alt, method = "prob")
  unchanged <- c("MAINT.PRICE", "X.PERS", "X.DOORS", "LUGGAGE", "SAFETY")
  expect_equal(unlist(alt0[1, unchanged]), unlist(eval[1, unchanged]))
  expect_equal(eval[[1, "BUY.PRICE"]], c(1,2,3))
  expect_equal(eval[[1, "TECH.CHAR."]], distribution(0, 0, 0, 1))
  expect_equal(eval[[1, "COMFORT"]], distribution(0, 0, 1))
  expect_equal(eval[[1, "PRICE"]], distribution(1/3, 0, 2/3))
  expect_equal(eval[[1, "CAR"]], distribution(1/3, 0, 0, 2/3))

  alt0 <- CarDxi$alternatives[2,]
  alt <- set_alternative(CarDxi, alt0, SAFETY="*")
  alt[, CarDxi$aggregate_ids] <- NA
  eval <- evaluate(CarDxi, alt, method = "prob")
  unchanged <- c("BUY.PRICE", "MAINT.PRICE", "X.PERS", "X.DOORS", "LUGGAGE")
  expect_equal(unlist(alt0[1, unchanged]), unlist(eval[1, unchanged]))
  expect_equal(eval[[1, "SAFETY"]], c(1,2,3))
  expect_equal(eval[[1, "COMFORT"]], distribution(0, 0, 1))
  expect_equal(eval[[1, "TECH.CHAR."]], distribution(1/3, 0, 1/3, 1/3))
  expect_equal(eval[[1, "CAR"]], distribution(1/3, 0, 1/3, 1/3))

  alt0 <- CarDxi$alternatives[2,]
  alt <- set_alternative(CarDxi, alt0, SAFETY=c(2,3))
  alt[, CarDxi$aggregate_ids] <- NA
  eval <- evaluate(CarDxi, alt, method = "prob")
  unchanged <- c("BUY.PRICE", "MAINT.PRICE", "X.PERS", "X.DOORS", "LUGGAGE")
  expect_equal(unlist(alt0[1, unchanged]), unlist(eval[1, unchanged]))
  expect_equal(eval[[1, "SAFETY"]], c(2,3))
  expect_equal(eval[[1, "COMFORT"]], distribution(0, 0, 1))
  expect_equal(eval[[1, "TECH.CHAR."]], distribution(0, 0, 1/2, 1/2))
  expect_equal(eval[[1, "CAR"]], distribution(0, 0, 1/2, 1/2))
})

test_that("Fuzzy~norm evaluation of CarDxi", {
  alt0 <- CarDxi$alternatives[1,]
  alt <- set_alternative(CarDxi, alt0, BUY.PRICE="*")
  alt[, CarDxi$aggregate_ids] <- NA
  eval <- evaluate(CarDxi, alt, method = "fuzzy")
  unchanged <- c("MAINT.PRICE", "X.PERS", "X.DOORS", "LUGGAGE", "SAFETY")
  expect_equal(unlist(alt0[1, unchanged]), unlist(eval[1, unchanged]))
  expect_equal(eval[[1, "BUY.PRICE"]], c(1,2,3))
  expect_equal(eval[[1, "TECH.CHAR."]], distribution(0, 0, 0, 1))
  expect_equal(eval[[1, "COMFORT"]], distribution(0, 0, 1))
  expect_equal(eval[[1, "PRICE"]], distribution(1, 0, 1))
  expect_equal(eval[[1, "CAR"]], distribution(1, 0, 0, 1))

  alt0 <- CarDxi$alternatives[2,]
  alt <- set_alternative(CarDxi, alt0, SAFETY="*")
  alt[, CarDxi$aggregate_ids] <- NA
  eval <- evaluate(CarDxi, alt, method = "fuzzy")
  unchanged <- c("BUY.PRICE", "MAINT.PRICE", "X.PERS", "X.DOORS", "LUGGAGE")
  expect_equal(unlist(alt0[1, unchanged]), unlist(eval[1, unchanged]))
  expect_equal(eval[[1, "SAFETY"]], c(1,2,3))
  expect_equal(eval[[1, "COMFORT"]], distribution(0, 0, 1))
  expect_equal(eval[[1, "TECH.CHAR."]], distribution(1, 0, 1, 1))
  expect_equal(eval[[1, "CAR"]], distribution(1, 0, 1, 1))

  alt0 <- CarDxi$alternatives[2,]
  alt <- set_alternative(CarDxi, alt0, SAFETY=c(2,3))
  alt[, CarDxi$aggregate_ids] <- NA
  eval <- evaluate(CarDxi, alt, method = "fuzzy")
  unchanged <- c("BUY.PRICE", "MAINT.PRICE", "X.PERS", "X.DOORS", "LUGGAGE")
  expect_equal(unlist(alt0[1, unchanged]), unlist(eval[1, unchanged]))
  expect_equal(eval[[1, "SAFETY"]], c(2,3))
  expect_equal(eval[[1, "COMFORT"]], distribution(0, 0, 1))
  expect_equal(eval[[1, "TECH.CHAR."]], distribution(0, 0, 1, 1))
  expect_equal(eval[[1, "CAR"]], distribution(0, 0, 1, 1))

  alt0 <- CarDxi$alternatives[2,]
  alt <- set_alternative(CarDxi, alt0, SAFETY=distribution(0, 0.2, 0.5))
  alt[, CarDxi$aggregate_ids] <- NA
  eval <- evaluate(CarDxi, alt, method = "fuzzy")
  unchanged <- c("BUY.PRICE", "MAINT.PRICE", "X.PERS", "X.DOORS", "LUGGAGE")
  expect_equal(unlist(alt0[1, unchanged]), unlist(eval[1, unchanged]))
  expect_equal(eval[[1, "SAFETY"]], distribution(0, 0.2, 0.5))
  expect_equal(eval[[1, "COMFORT"]], distribution(0, 0, 1))
  expect_equal(eval[[1, "TECH.CHAR."]], distribution(0, 0, 0.2, 0.5))
  expect_equal(eval[[1, "CAR"]], distribution(0, 0, 0.2, 0.5))

  alt0 <- CarDxi$alternatives[2,]
  alt <- set_alternative(CarDxi, alt0, SAFETY=distribution(0, 0.2, 0.5))
  alt[, CarDxi$aggregate_ids] <- NA
  eval <- evaluate(CarDxi, alt, method = "fuzzynorm")
  unchanged <- c("BUY.PRICE", "MAINT.PRICE", "X.PERS", "X.DOORS", "LUGGAGE")
  expect_equal(unlist(alt0[1, unchanged]), unlist(eval[1, unchanged]))
  expect_equal(eval[[1, "SAFETY"]], distribution(0, 0.4, 1))
  expect_equal(eval[[1, "COMFORT"]], distribution(0, 0, 1))
  expect_equal(eval[[1, "TECH.CHAR."]], distribution(0, 0, 0.4, 1))
  expect_equal(eval[[1, "CAR"]], distribution(0, 0, 0.4, 1))
})

test_that("evaluate_attribute works on discrete scales", {
  alt <- CarDxi$alternative("MyCar", BUY.PRICE="low", MAINT.PRICE=2, X.PERS="more", X.DOORS="4", LUGGAGE=2, SAFETY="medium")
  rootid <- CarDxi$attrib("CAR")$id
  safety <- CarDxi$attrib("SAFETY")
  eval <- evaluate_attribute(CarDxi, safety, alt)
  expect_equal(length(eval), 3)
  expect_equal(names(eval), c("small", "medium", "high"))
  expect_equal(eval[[1]][[1, rootid]], 1)
  expect_equal(eval[[2]][[1, rootid]], 4)
  expect_equal(eval[[3]][[1, rootid]], 4)

  maint <- CarDxi$attrib("MAINT.PRICE")
  eval <- evaluate_attribute(CarDxi, maint, alt)
  expect_equal(length(eval), 3)
  expect_equal(names(eval), c("high", "medium", "low"))
  expect_equal(eval[[1]][[1, rootid]], 1)
  expect_equal(eval[[2]][[1, rootid]], 4)
  expect_equal(eval[[3]][[1, rootid]], 4)
})

test_that("evaluate_attribute works on continuous scales", {
  alt <- ContinuousNewDxi$alternatives[ContinuousNewDxi$alternatives$name == "Test3",]
  cat("Alt\n")
  rootid <- ContinuousNewDxi$attrib("OneLevel")$id
  n2 <- ContinuousNewDxi$attrib("N2")
  eval <- evaluate_attribute(ContinuousNewDxi, n2, alt, seq=seq(-2, 2, 0.5))
  expect_equal(length(eval), 9)
  expect_equal(names(eval), c("-2", "-1.5", "-1", "-0.5", "0", "0.5", "1", "1.5", "2"))
  expect_equal(eval[[1]][[1, rootid]], 1)
  expect_equal(eval[[2]][[1, rootid]], 1)
  expect_equal(eval[[3]][[1, rootid]], c(1,2))
  expect_equal(eval[[4]][[1, rootid]], c(1,2))
  expect_equal(eval[[5]][[1, rootid]], c(1,2))
  expect_equal(eval[[6]][[1, rootid]], c(1,2))
  expect_equal(eval[[7]][[1, rootid]], c(1,2))
  expect_equal(eval[[8]][[1, rootid]], 3)
  expect_equal(eval[[9]][[1, rootid]], 3)
})

test_that("attribute_effect works", {
  alt <- CarDxi$alternative("MyCar", BUY.PRICE="low", MAINT.PRICE=2, X.PERS="more", X.DOORS="4", LUGGAGE=2, SAFETY=c("medium", "high"))
  eval <- attribute_effect(CarDxi, "LUGGAGE", alt)
  expect_equal(length(eval), 3)
  expect_equal(eval[[1]], 1)
  expect_equal(eval[[2]], 4)
  expect_equal(eval[[3]], 4)

  eval <- attribute_effect(CarDxi, "LUGGAGE", alt, "TECH.CHAR.")
  expect_equal(length(eval), 3)
  expect_equal(eval[[1]], 1)
  expect_equal(eval[[2]], c(3,4))
  expect_equal(eval[[3]], c(3,4))
})

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

test_that("evaluate_attributes works", {
  alt <- CarDxi$alternative("MyCar", BUY.PRICE="low", MAINT.PRICE=2, X.PERS="more", X.DOORS="4", LUGGAGE=2, SAFETY="medium")
  eval <- evaluate_attributes(CarDxi, alt)
  expect_equal(length(eval), length(CarDxi$basic))
})

test_that("Plain evaluation of LinkedDxi", {
  alts0 <- LinkedDxi$alternatives
  alts <- alts0
  alts[, LinkedDxi$aggregate_ids] <- NA
  eval <- evaluate(LinkedDxi, alts)
  for (i in 1:nrow(alts0)) {
    expect_equal(unlist(alts0[i,]), unlist(eval[i,]))
  }
})

test_that("Plain evaluation of ContinuousNewDxi", {
  alts0 <- ContinuousNewDxi$alternatives
  alts <- alts0
  alts[, ContinuousNewDxi$aggregate_ids] <- NA
  eval <- evaluate(ContinuousNewDxi, alts)
  expect_identical(eval,
    structure(list(name = list("Null/Null", "Null/All", "Test1",
      "Test2", "Test3", "Test4", "Test5", "Test6", "Test7"),
      OneLevel = list(NA, NA, 1L, 1:2, 3L, 2:3, 3L, 3L, 1:2),
      X1 = list(NA, NA,  1, 1, 1, 2, 2, 2, 1),
      N1 = list(NA, NA, -2, -2, -2, 2, 2, 2, 0),
      X2 = list(NA, NA, 1, 2, 3, 1, 2, 3, 2),
      N2 = list(NA, NA, -2, 0, 2, -2, 0, 2, 0)),
      row.names = c(NA, -9L), class = "data.frame"))
})

test_that("Plain evaluation of DozenDxi", {
  alts0 <- DozenDxi$alternatives
  alts <- alts0
  alts[, DozenDxi$aggregate_ids] <- NA
  alts[is.na(alts)] <- "*"
  eval <- evaluate(DozenDxi, alts)
  expect_equal(alts0[, DozenDxi$aggregate_ids], eval[, DozenDxi$aggregate_ids])
})

