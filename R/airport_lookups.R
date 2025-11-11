#' Translate airport codes or names into other standard airport formats
#'
#' Return city name, airport name, IATA code, or ICAO code given an input IATA code, ICAO code, or airport name.
#'
#' @param input An airport name, IATA code, or ICAO code. Input type will be guessed unless explicitly defined
#' @param input_type One of "name", "IATA", or "ICAO". Function will attempt to guess type
#' if not supplied
#' @param output_type One of "name", "city", "IATA", or "ICAO". Defaults to "name" if
#' otherwise not specified
#' @return The appropriate city, airport name, IATA code, or ICAO code for that airport
#'
#' @export
#'
#' @examples
#' airport_lookup("CYVR")
#' airport_lookup("YVR", output_type = "city")
#' airport_lookup("Vancouver International Airport", input_type="name",output_type = "IATA")
#' airport_lookup("YVR",input_type = "IATA", output_type = "city")
#'
#' # Produces a list of similar named airports
#' airport_lookup("Vancoover","name","city")
airport_lookup <- function(input, input_type = "IATA", output_type = "name") {
  # Load airports data
  data("airports", envir = environment())

  # Auto-detect input type if not specified
  if(missing(input_type)) {
    input_type <- detect_input_type(input)
    # Require explicit output type when input is a name
    if(input_type == "name" && (missing(output_type) || output_type == "name")) {
      stop("Output type needs to be specified if airport name is supplied where code is expected.\nOutput type should be one of \"IATA\", \"ICAO\", \"name\", or \"city\"", call. = FALSE)
    }
  }

  # Validate input and output types
  validate_types(input_type, output_type)
  # Filter airports based on input type
  if(input_type == "IATA") {
    match <- airports %>% dplyr::filter(IATA == input)
    if(nrow(match) == 0) {
      stop("Unable to find matching IATA code.", call. = FALSE)
    }
  } else if(input_type == "ICAO") {
    match <- airports %>% dplyr::filter(ICAO == input)
    if(nrow(match) == 0) {
      stop("Unable to find matching ICAO code.", call. = FALSE)
    }
  } else if(input_type == "name") {
    match <- airports %>% dplyr::filter(Name == input)

    if(nrow(match) > 1) {
      warning("Multiple airport names are exact matches.", immediate. = TRUE)
      return(match %>% dplyr::select(Name, City, Country, IATA, ICAO))
    }

    if(nrow(match) == 0) {
      similar <- find_similar_names(input)
      if(length(similar) > 0) {
        warning("No exact matches but some similar names in the database include:",
                immediate. = TRUE)
        cat(similar, sep = "\n")
      } else {
        warning("Unable to find matching name in database.")
      }
      return(NULL)
    }
  }

  # Extract the requested output field
  result <- switch(output_type,
    "name" = match %>% dplyr::pull(Name),
    "city" = match %>% dplyr::pull(City),
    "IATA" = match %>% dplyr::pull(IATA),
    "ICAO" = match %>% dplyr::pull(ICAO)
  )

  return(result)
}

#' Lookup full airport details based of a standard airport input
#'
#' Return all airport details given an input IATA code, ICAO code, or airport name.
#'
#' @param input An airport name, IATA code, or ICAO code. Input type will be guessed unless
#' explicitly defined
#' @param input_type One of "name", "IATA", or "ICAO". Function will attempt to guess type if
#' not supplied
#' @return A 1x17 tibble with airport details
#'
#' @export
#'
#' @examples
#' airport_detail("YVR")
#' airport_detail("London Heathrow Airport")
airport_detail <- function(input, input_type) {
  # Load airports data
  data("airports", envir = environment())

  # Auto-detect input type if not specified
  if(missing(input_type)) {
    input_type <- detect_input_type(input)
  }

  # Validate input type
  validate_types(input_type)

  # Filter airports based on input type
  if(input_type == "IATA") {
    match <- airports %>% dplyr::filter(IATA == input)
    if(nrow(match) == 0) {
      stop("Unable to find matching IATA code.", call. = FALSE)
    }
    return(match)
  }

  if(input_type == "ICAO") {
    match <- airports %>% dplyr::filter(ICAO == input)
    if(nrow(match) == 0) {
      stop("Unable to find matching ICAO code.", call. = FALSE)
    }
    return(match)
  }

  if(input_type == "name") {
    match <- airports %>% dplyr::filter(Name == input)
    if(nrow(match) == 0) {
      similar <- find_similar_names(input)
      if(length(similar) > 0) {
        warning("No exact matches but some similar names in the database include:",
                immediate. = TRUE)
        cat(similar, sep = "\n")
      } else {
        warning("Unable to find matching name in database.", immediate. = TRUE)
      }
      return(NULL)
    }
    return(match)
  }
}

#' Return all airports serving an input city.
#'
#' This function takes a city normal city name as an input argument and returns all airports associated with that city. Airports are typically associated with their local metropolitan area but some exceptions may be present in the data. If there are no matching results in the data for the city argument, a list of closely named alternatives will be suggested with a warning.
#'
#' @param city A city name. If no exact match will attempt to prompt user with suggested alternatives
#' @param country (Optional) A country name or ISO country code in either numeric, alpha-2, or alpha 3 format. Case insensitive.
#' @return A \code{Nx17} tibble with airport details where \code{n} is the number of airports serving that city
#'
#' @export
#'
#' @examples
#' city_airports("Vancouver")
#' city_airports("London")
#' city_airports("London","Canada")
#' city_airports("London","CA")
#' city_airports("London","CAN")
#' city_airports("London","124")
city_airports <- function(city, country) {
  # Load airports data
  data("airports", envir = environment())

  # Filter by city
  if(missing(country)) {
    match <- airports %>% dplyr::filter(City == city)
    if(nrow(match) > 0 && length(unique(match$Country)) > 1) {
      warning("Input city matches cities in multiple countries. Do you want to specify a country argument? See documentation ?city_airports for details.")
    }
  } else {
    # Validate country input
    if(!is.character(country)) {
      stop("Country names or codes have to be in character format. If using a numeric country code, wrap it as a string. For example: '036' for Australia", call. = FALSE)
    }

    # Lookup country name from code
    lookup_table <- get_country_lookup()
    input_country <- lookup_table[apply(lookup_table, 1, function(x) any(grep(paste0("^", country, "$"), x, ignore.case = TRUE))), ]$Country

    if(length(input_country) == 0) {
      stop("Invalid country name or code. Please enter a country name or country code in either numeric, Alpha-2, or Alpha-3 format.", call. = FALSE)
    }

    match <- airports %>%
      dplyr::filter(City == city, Country == input_country)
  }

  # Handle no matches
  if(nrow(match) == 0) {
    similar <- unique(agrep(city, airports$City,
                           ignore.case = TRUE,
                           value = TRUE,
                           max.distance = FUZZY_MATCH_DISTANCE))
    if(length(similar) > 0) {
      warning("No exact matches but some similarly named cities in the database include:",
              immediate. = TRUE)
      cat(similar, sep = "\n")
      return(NULL)
    } else {
      stop("Unable to find matching or similar names in database.", call. = FALSE)
    }
  }

  return(match)
}

#' Lookup airport location coordinates given a standard airport input.
#'
#' Returns airport location in longitude and latitude coordinates given an input IATA code, ICAO code, or airport name.
#'
#' @param input An airport name, IATA code, or ICAO code. Input type will be guessed unless #' explicitly defined
#' @param input_type One of "name", "IATA", or "ICAO". Function will attempt to guess type
#' if not supplied
#' @return List of longitude and latitude coordinates
#'
#' @export
#'
#' @examples
#' airport_location("YVR","IATA")
#' #' airport_location("Vancouver International Airport","name")
airport_location <- function(input, input_type) {
  # Load airports data
  data("airports", envir = environment())

  # Auto-detect input type if not specified
  if(missing(input_type)) {
    input_type <- detect_input_type(input)
  }

  # Validate input type
  validate_types(input_type)

  # Filter and extract location
  if(input_type == "IATA") {
    match <- airports %>%
      dplyr::filter(IATA == input) %>%
      dplyr::select(Latitude, Longitude)
    if(nrow(match) == 0) {
      stop("Unable to find matching IATA code.", call. = FALSE)
    }
    return(match)
  }

  if(input_type == "ICAO") {
    match <- airports %>%
      dplyr::filter(ICAO == input) %>%
      dplyr::select(Latitude, Longitude)
    if(nrow(match) == 0) {
      stop("Unable to find matching ICAO code.", call. = FALSE)
    }
    return(match)
  }

  if(input_type == "name") {
    match <- airports %>%
      dplyr::filter(Name == input) %>%
      dplyr::select(Latitude, Longitude)

    if(nrow(match) == 0) {
      similar <- find_similar_names(input)
      if(length(similar) > 0) {
        warning("No exact matches but some similar names in the database include:",
                immediate. = TRUE)
        cat(similar, sep = "\n")
      } else {
        warning("Unable to find matching name in database.")
      }
      return(NULL)
    }
    return(match)
  }
}
