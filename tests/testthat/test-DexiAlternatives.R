test_that("set_alternative() creates a new alternative with NA values", {
  a1 <- CarDxi$alternative()
  expect_true(is.data.frame(a1))
  expect_equal(ncol(a1), 11)
  expect_equal(nrow(a1), 1)
  expect_equal(unname(unlist(a1[1,])), c("NewAlternative", rep(NA, length(names(CarDxi$alternatives)) - 1 )))
  a2 <- set_alternative(CarDxi, "NewAlternative")
  expect_true(identical(a1, a2))
})

test_that("set_alternative() sets alternative values", {
  a0 <- set_alternative(CarDxi, "A0")
  a1 <- set_alternative(CarDxi, a0)
  expect_identical(a0, a1)

  a1 <- set_alternative(CarDxi, "My", SAFETY=2, BUY.PRICE="low", list(COMFORT=2, MAINT.PRICE=c("low"=0.2, high=0.8)))
  expect_true(all(is.na(a1[c("CAR", "PRICE","TECH.CHAR.","X.PERS","X.DOORS","LUGGAGE")])))
  expect_equal(a1[[1, "BUY.PRICE"]], 3)
  expect_equal(a1[[1, "MAINT.PRICE"]], distribution(0.8,0,0.2))
  expect_equal(a1[[1, "COMFORT"]], 2)
  expect_equal(a1[[1, "SAFETY"]], 2)

  a2 <- set_alternative(CarDxi, a1)
  expect_identical(a1, a2)
})

test_that("alt_values() makes alternative value strings", {
  expect_equal(
    unlist(alt_values(CarDxi$alternatives[1,], CarDxi$attributes)),
    c("NULL", "exc", "low", "medium", "low", "exc", "high", "more", "4", "big", "high")
  )
  expect_equal(
    unlist(alt_values(CarDxi$alternatives[1,], CarDxi$attributes, as_character = FALSE)),
    c("", "4", "3", "2", "3", "4", "3", "3", "3", "3", "3")
  )
})

test_that("compare_two_alternatives() compares alternatives", {
  c12 <- compare_two_alternatives(CarDxi$alternatives[1,], CarDxi$alternatives[2,], CarDxi$attributes)
  expect_equal(c12, c(NA, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1))

  a3 <- set_alternative(CarDxi, CarDxi$alternatives[2,], name = "new", LUGGAGE = 2)
  c23 <- compare_two_alternatives(CarDxi$alternatives[2,], a3, CarDxi$attributes)
  expect_equal(c23, c(NA, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5, 0.0, 0.0, 1.0, 0.0))
})

test_that("convert_data_continuous() works", {
  scl <- DexiContinuousScale()
  expect_equal(convert_data_continuous(c(1, 2, 5), scl), c(0, 0.25, 1))
  expect_equal(
    convert_data_continuous(c(1, 2, 5), scl, imin = 0, imax = 10, omin = 0, omax = 100),
    c(10, 20, 50)
  )
  expect_equal(convert_data_continuous(list(1, 2, 5), scl, map_values = FALSE), c(1, 2, 5))
  expect_equal(convert_data_continuous(list(1, 2, NA), scl), c(0, 1, NA))
})

test_that("convert_data_discrete() works", {
  scla <- DexiDiscreteScale(values = c("L", "M", "H"))
  scld <- DexiDiscreteScale(values = c("L", "M", "H"), order = "descending")
  expect_equal(convert_data_discrete(c(1, 2, 3), scla), c(0.0, 0.5, 1.0))
  expect_equal(convert_data_discrete(c(1, 2, 3), scld), c(1.0, 0.5, 0.0))
  expect_equal(convert_data_discrete(list(1, 2, 3), scla), c(0.0, 0.5, 1.0))
  expect_equal(convert_data_discrete(list(1, 2, 3), scld), c(1.0, 0.5, 0.0))
  expect_equal(convert_data_discrete(list(1, 2, 3), scld, omax=10), c(10, 5, 0))
  data <- list(1, c(1,2), distribution(0.2, 0, 0.8), NA)
  expect_equal(convert_data_discrete(data, scla, omax=10),  c(0, 0, 0, NA))
  expect_equal(convert_data_discrete(data, scld, omax=10), c(10, 10, 10, NA))
  expect_equal(convert_data_discrete(data, scla, aggregate=max, omax=10), c(0, 5, 10, NA))
  expect_equal(convert_data_discrete(data, scla, aggregate=mean, omax=10), c(0.0, 2.5, 5.0, NA))
  expect_equal(convert_data_discrete(data, scla, interpret = "distr", aggregate=mean), c(1/3, 2/3, 1/3, NA))
  expect_equal(convert_data_discrete(data, scld, interpret = "distr", aggregate=mean), c(1/3, 2/3, 1/3, NA))
  expect_equal(convert_data_discrete(data, scla, interpret = "distr", aggregate=min), c(0, 0, 0, NA))
  expect_equal(convert_data_discrete(data, scla, interpret = "distr", aggregate=max), c(1, 1, 0.8, NA))
})


test_that("convert_alternatives() works", {

  ca <- convert_alternatives(ContinuousNewDxi)
  s <- toString(ca)
  expect_equal(s, "c(\"Null/Null\", \"Null/All\", \"Test1\", \"Test2\", \"Test3\", \"Test4\", \"Test5\", \"Test6\", \"Test7\"), c(NA, 0, 0, 0.5, 1, 0.5, 1, 1, 0.5), c(NA, 0, 0, 0, 0, 1, 1, 1, 0), c(NA, NA, 0, 0, 0, 1, 1, 1, 0.5), c(NA, 0, 0, 0.5, 1, 0, 0.5, 1, 0.5), c(NA, NA, 0, 0.5, 1, 0, 0.5, 1, 0.5)")

  attnames <- c("name", "PRICE", "BUY.PRICE", "MAINT.PRICE", "TECH.CHAR.", "COMFORT", "X.PERS", "X.DOORS", "LUGGAGE", "SAFETY")
  data <- CarDxi$alternatives[, attnames]

  ca <- convert_alternatives(CarDxi, data)
  s <- toString(ca)
  expect_equal(s, "c(\"Car1\", \"Car2\"), c(1, 0.5), c(0.5, 0.5), c(1, 0.5), c(1, 0.666666666666667), c(1, 1), c(1, 1), c(0.666666666666667, 0.666666666666667), c(1, 1), c(1, 0.5)")

  alts3 <- structure(
    list(
      name = c("MyCar", "MyCar2", "MyCar1b"),
      CAR.1 = list(4L, 4L, c(1L, 4L)),
      PRICE = list(3L, 3L, c(1L, 3L)),
      BUY.PRICE = list(3L, 3L, 3L),
      MAINT.PRICE = list(2, 1, structure(c(0.1, 0.6, 0.3), class = "distribution")),
      TECH.CHAR. = list(3L, 3:4, 3L),
      COMFORT = list(3L, 2, 3L),
      X.PERS = list(3, 3, 3L),
      X.DOORS = list(3, 3, 3L),
      LUGGAGE = list(2L, 2L, 2),
      SAFETY = list(2, c(2, 3), 2)
    ),
    row.names = c(NA, -3L),
    class = "data.frame"
  )

  expect_equal(toString(alts3), "c(\"MyCar\", \"MyCar2\", \"MyCar1b\"), list(4, 4, c(1, 4)), list(3, 3, c(1, 3)), list(3, 3, 3), list(2, 1, c(0.1, 0.6, 0.3)), list(3, 3:4, 3), list(3, 2, 3), list(3, 3, 3), list(3, 3, 3), list(2, 2, 2), list(2, c(2, 3), 2)")

  alts3 <- alts3[, attnames]

  ca <- convert_alternatives(CarDxi, alts3)
  s <- toString(ca)
  expect_equal(s, "c(\"MyCar\", \"MyCar2\", \"MyCar1b\"), c(1, 1, 0), c(1, 1, 1), c(0.5, 0, 0), c(0.666666666666667, 0.666666666666667, 0.666666666666667), c(1, 0.5, 1), c(1, 1, 1), c(0.666666666666667, 0.666666666666667, 0.666666666666667), c(0.5, 0.5, 0.5), c(0.5, 0.5, 0.5)")

  ca <- convert_alternatives(CarDxi, alts3, omax=100)
  s <- toString(ca)
  expect_equal(s, "c(\"MyCar\", \"MyCar2\", \"MyCar1b\"), c(100, 100, 0), c(100, 100, 100), c(50, 0, 0), c(66.6666666666667, 66.6666666666667, 66.6666666666667), c(100, 50, 100), c(100, 100, 100), c(66.6666666666667, 66.6666666666667, 66.6666666666667), c(50, 50, 50), c(50, 50, 50)")
})

