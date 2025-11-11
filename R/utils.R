utils::globalVariables(c("airports","IATA","ICAO","Name","Latitude","Longitude","City","Country","Country Code", "Country Code (Alpha-2)", "Country Code (Alpha-3)"))

# Constants for code matching
IATA_CODE_LENGTH <- 3
ICAO_CODE_LENGTH <- 4
FUZZY_MATCH_DISTANCE <- 0.15

# Constants for distance calculations
# Earth radius in kilometres (spherical approximation)
EARTH_RADIUS_KM <- 6373
# Kilometres per degree latitude (approximately constant)
KM_PER_DEGREE_LAT <- 110.574
# Kilometres per degree longitude at equator
KM_PER_DEGREE_LON_EQUATOR <- 111.320
# Degrees to radians conversion factor
DEG_TO_RAD <- pi / 180

# Helper function to detect input type based on format
detect_input_type <- function(input) {
  data("airports", envir = environment())

  input_length <- nchar(input)
  is_uppercase <- input == toupper(input)

  if (input_length == IATA_CODE_LENGTH && is_uppercase) {
    if (!is.na(match(input, airports$IATA))) {
      return("IATA")
    } else {
      stop("Invalid IATA code", call. = FALSE)
    }
  }

  if (input_length == ICAO_CODE_LENGTH && is_uppercase) {
    if (!is.na(match(input, airports$ICAO))) {
      return("ICAO")
    } else {
      stop("Invalid ICAO code", call. = FALSE)
    }
  }

  if (input_length > ICAO_CODE_LENGTH && !is_uppercase) {
    return("name")
  }

  stop("Unable to detect input type. Please specify input_type explicitly.", call. = FALSE)
}

# Helper function to validate input and output types
validate_types <- function(input_type, output_type = NULL) {
  valid_types <- c("IATA", "ICAO", "name", "city")

  if (!input_type %in% valid_types[1:3]) {
    stop("Input type must be one of \"IATA\", \"ICAO\", or \"name\"", call. = FALSE)
  }

  if (!is.null(output_type) && !output_type %in% valid_types) {
    stop("Output type must be one of \"IATA\", \"ICAO\", \"name\", or \"city\"", call. = FALSE)
  }

  if (!is.null(output_type) && input_type == output_type && input_type != "name") {
    stop("Input type matches output type. Nothing to do.", call. = FALSE)
  }
}

# Helper function to find similar matches
find_similar_names <- function(input, max_distance = FUZZY_MATCH_DISTANCE) {
  data("airports", envir = environment())

  agrep(input, airports$Name,
        ignore.case = TRUE,
        max.distance = max_distance,
        value = TRUE)
}

# Environment to store package-level data
.airportr_env <- new.env(parent = emptyenv())

# Create country lookup table lazily
get_country_lookup <- function() {
  if (is.null(.airportr_env$country_lookup)) {
    .airportr_env$country_lookup <- airports %>%
      dplyr::distinct(Country, `Country Code`, `Country Code (Alpha-2)`, `Country Code (Alpha-3)`)
  }
  .airportr_env$country_lookup
}
