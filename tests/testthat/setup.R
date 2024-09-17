test_that("Reading DEXi models from Xml", {
  expect_silent(CarDxi <<- read_dexi(CarXml))
  expect_silent(Car2Dxi <<- read_dexi(Car2Xml))
  expect_silent(LinkedDxi <<- read_dexi(LinkedXml))
  expect_silent(ContinuousOldDxi <<- read_dexi(ContinuousOldXml))
  expect_silent(ContinuousNewDxi <<- read_dexi(ContinuousNewXml))
  expect_silent(ContinuousNewDxiNoAlt <<- read_dexi(ContinuousNewXmlNoAlt))
  expect_silent(DozenDxi <<- read_dexi(DozenXml))
})

