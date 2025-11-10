## Tests for airport_lookup function

test_that("airport_lookup works with IATA codes", {
  expect_equal(airport_lookup("YVR"), "Vancouver International Airport")
  expect_equal(airport_lookup("YVR", output_type = "city"), "Vancouver")
  expect_equal(airport_lookup("YVR", output_type = "ICAO"), "CYVR")
})

test_that("airport_lookup works with ICAO codes", {
  expect_equal(airport_lookup("CYVR", input_type = "ICAO"), "Vancouver International Airport")
  expect_equal(airport_lookup("CYVR", input_type = "ICAO", output_type = "IATA"), "YVR")
  expect_equal(airport_lookup("CYVR", input_type = "ICAO", output_type = "city"), "Vancouver")
})

test_that("airport_lookup works with airport names", {
  expect_equal(airport_lookup("Vancouver International Airport", input_type = "name", output_type = "IATA"), "YVR")
  expect_equal(airport_lookup("Vancouver International Airport", input_type = "name", output_type = "ICAO"), "CYVR")
})

test_that("airport_lookup requires output_type for name input", {
  expect_error(airport_lookup("Vancouver"), "Output type needs to be specified")
  expect_error(airport_lookup("Vancouver International Airport"), "Output type needs to be specified")
})

test_that("airport_lookup detects invalid codes", {
  expect_error(airport_lookup("XXX"), "Invalid IATA code")
  expect_error(airport_lookup("ZZZZ"), "Invalid ICAO code")
})

test_that("airport_lookup validates input and output types", {
  expect_error(airport_lookup("YVR", input_type = "invalid"), "Input type must be one of")
  expect_error(airport_lookup("YVR", output_type = "invalid"), "Output type must be one of")
})

## Tests for airport_detail function

test_that("airport_detail returns correct structure", {
  result <- airport_detail("YVR")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true("Name" %in% names(result))
  expect_true("IATA" %in% names(result))
  expect_true("ICAO" %in% names(result))
})

test_that("airport_detail works with different input types", {
  result_iata <- airport_detail("YVR")
  result_icao <- airport_detail("CYVR", input_type = "ICAO")
  expect_equal(result_iata$Name, result_icao$Name)
})

test_that("airport_detail handles invalid input", {
  expect_error(airport_detail("XXX"), "Invalid IATA code")
  expect_error(airport_detail("ZZZZ"), "Invalid ICAO code")
})

## Tests for city_airports function

test_that("city_airports finds airports by city", {
  result <- city_airports("Vancouver")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(all(result$City == "Vancouver"))
})

test_that("city_airports works with country filter", {
  result <- city_airports("London", "Canada")
  expect_s3_class(result, "data.frame")
  expect_true(all(result$City == "London"))
  expect_true(all(result$Country == "Canada"))
})

test_that("city_airports warns for multiple countries", {
  expect_warning(city_airports("London"), "multiple countries")
})

test_that("city_airports handles invalid country codes", {
  expect_error(city_airports("London", "ZZZ"), "Invalid country name or code")
})

## Tests for airport_location function

test_that("airport_location returns coordinates", {
  result <- airport_location("YVR")
  expect_s3_class(result, "data.frame")
  expect_true("Latitude" %in% names(result))
  expect_true("Longitude" %in% names(result))
  expect_equal(nrow(result), 1)
})

test_that("airport_location works with different input types", {
  result_iata <- airport_location("YVR", "IATA")
  result_icao <- airport_location("CYVR", "ICAO")
  expect_equal(result_iata$Latitude, result_icao$Latitude)
  expect_equal(result_iata$Longitude, result_icao$Longitude)
})

test_that("airport_location handles invalid input", {
  expect_error(airport_location("XXX"), "Invalid IATA code")
})

test_that("airport_location works with airport names", {
  result <- airport_location("Vancouver International Airport", input_type = "name")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(result$Latitude > 49 && result$Latitude < 50)
  expect_true(result$Longitude < -122 && result$Longitude > -124)
})

## Tests for fuzzy matching and warning paths

test_that("airport_lookup suggests similar names when no exact match", {
  expect_warning(result <- airport_lookup("Vancover", input_type = "name", output_type = "IATA"),
                 "No exact matches but some similar names")
  expect_null(result)
})

test_that("airport_detail suggests similar names when no exact match", {
  expect_warning(result <- airport_detail("Heathow Airport", input_type = "name"),
                 "No exact matches but some similar names")
  expect_null(result)
})

test_that("airport_location suggests similar names when no exact match", {
  expect_warning(result <- airport_location("Vancover International", input_type = "name"),
                 "No exact matches but some similar names")
  expect_null(result)
})

test_that("city_airports suggests similar city names when no match", {
  expect_warning(result <- city_airports("Vancouvr"),
                 "No exact matches but some similarly named cities")
  expect_null(result)
})

test_that("city_airports handles non-existent cities", {
  expect_error(city_airports("Nonexistentcityname123"),
               "Unable to find matching or similar names")
})

test_that("airport_lookup handles multiple exact name matches", {
  # Bathurst Airport exists in both Canada and Australia
  expect_warning(result <- airport_lookup("Bathurst Airport", input_type = "name", output_type = "IATA"),
                 "Multiple airport names are exact matches")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 1)
  expect_true("Bathurst" %in% result$City)
})

test_that("city_airports works with different country code formats", {
  # Test Alpha-2 code
  result_alpha2 <- city_airports("London", "CA")
  expect_true(all(result_alpha2$Country == "Canada"))

  # Test Alpha-3 code
  result_alpha3 <- city_airports("London", "CAN")
  expect_true(all(result_alpha3$Country == "Canada"))

  # Test numeric code
  result_numeric <- city_airports("London", "124")
  expect_true(all(result_numeric$Country == "Canada"))
})

test_that("city_airports validates country input type", {
  expect_error(city_airports("London", 124),
               "Country names or codes have to be in character format")
})

test_that("airport_lookup auto-detects IATA codes", {
  # Should detect 3-character uppercase as IATA
  expect_equal(airport_lookup("YYZ", output_type = "city"), "Toronto")
})

test_that("airport_lookup auto-detects ICAO codes", {
  # Should detect 4-character uppercase as ICAO
  expect_equal(airport_lookup("CYYZ", output_type = "city"), "Toronto")
})

test_that("airport_detail auto-detects codes", {
  result_iata <- airport_detail("YYZ")
  result_icao <- airport_detail("CYYZ")
  expect_equal(result_iata$Name, result_icao$Name)
})

test_that("airport_location auto-detects codes", {
  result_iata <- airport_location("YYZ")
  result_icao <- airport_location("CYYZ")
  expect_equal(result_iata$Latitude, result_icao$Latitude)
})
