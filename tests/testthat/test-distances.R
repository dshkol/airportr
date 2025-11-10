## Tests for distance calculation functions

test_that("airport_distance calculates distance correctly", {
  # Distance between Vancouver (YVR) and Toronto (YYZ) is approximately 3360 km
  distance <- airport_distance("YVR", "YYZ")
  expect_true(is.numeric(distance))
  expect_gt(distance, 3300)
  expect_lt(distance, 3400)
})

test_that("airport_distance is symmetric", {
  dist1 <- airport_distance("YVR", "YYZ")
  dist2 <- airport_distance("YYZ", "YVR")
  expect_equal(dist1, dist2)
})

test_that("airport_distance validates IATA codes", {
  expect_error(airport_distance("XXX", "YYZ"), "invalid")
  expect_error(airport_distance("YVR", "XXX"), "invalid")
  expect_error(airport_distance("XXX", "XXX"), "invalid")
})

test_that("airport_distance returns zero for same airport", {
  distance <- airport_distance("YVR", "YVR")
  expect_equal(distance, 0, tolerance = 0.1)
})

## Tests for airports_near_airport function

test_that("airports_near_airport finds nearby airports", {
  result <- airports_near_airport("YVR", distance = 100)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  # Should include YVR itself
  expect_true("YVR" %in% result$IATA)
})

test_that("airports_near_airport accepts different distances", {
  result_50 <- airports_near_airport("YVR", distance = 50)
  result_200 <- airports_near_airport("YVR", distance = 200)
  expect_true(nrow(result_200) >= nrow(result_50))
})

test_that("airports_near_airport handles invalid input", {
  expect_error(airports_near_airport("XXX"), "Invalid IATA code")
})

## Tests for airports_around function

test_that("airports_around finds airports near coordinates", {
  # Coordinates near Vancouver
  result <- airports_around(49.2, -123.0, distance = 100)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("airports_around validates coordinates", {
  expect_error(airports_around(100, -123), "Latitude must be between -90 and 90")
  expect_error(airports_around(-100, -123), "Latitude must be between -90 and 90")
  expect_error(airports_around(49, 200), "Longitude must be between -180 and 180")
  expect_error(airports_around(49, -200), "Longitude must be between -180 and 180")
})

test_that("airports_around validates input types", {
  expect_error(airports_around("abc", -123), "must be numeric")
  expect_error(airports_around(49, "abc"), "must be numeric")
})

test_that("airports_around accepts different distances", {
  result_50 <- airports_around(49.2, -123.0, distance = 50)
  result_200 <- airports_around(49.2, -123.0, distance = 200)
  expect_true(nrow(result_200) >= nrow(result_50))
})
