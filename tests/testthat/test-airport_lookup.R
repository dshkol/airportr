# test_that("airport lookup checks that input codes are valid", {
#   expect_
# })

test_that("airport lookup checks format correctly", {
  expect_gt(nchar(airport_lookup("YVR")),0)
})

test_that("Output type needs to be specified if airport name is supplied", {
  expect_error(airport_lookup("Vancouver"))
})

# Add many more
