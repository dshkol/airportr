#' Translate airport codes or names into other standard airport formats
#'
#' Return city name, airport name, IATA code, or IACO code given an input IATA code, ICAO code, or airport name.
#'
#' @param input An airport name, IATA code, or ICAO code. Input type will be guessed unless #' explicitly defined
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
  data("airports", envir=environment())
  if(missing(input_type)) {
    if(nchar(input)==3 & input == toupper(input)) {
      if(!is.na(match(input,airports$IATA))) {input_type <- "IATA"}
      else {stop("Invalid IATA code")}
      }
    if(nchar(input)==4 & input == toupper(input)) {
      if(!is.na(match(input,airports$ICAO))) {input_type <- "ICAO"}
      else {stop("Invalid ICAO code")}
      }
    if(nchar(input)>4 & input != toupper(input)) {
      input_type <- "name"
      if(missing(output_type) | output_type == "name") {
        stop("Output type needs to be specified if airport name is supplied.\nOutput type should be one of \"IATA\", \"ICAO\", \"name\", or \"city\"")
        }
      }
  }
  if(!missing(input_type) & !input_type %in% c("IATA","ICAO","name")) {
    stop("Input type must be one of \"IATA\", \"ICAO\", or \"name\"")}
  if(!output_type %in% c("IATA","ICAO","name","city")) {
    stop("Output type must be one of \"IATA\", \"ICAO\", \"name\", or \"city\"")}
  if(!missing(input_type) & (input_type == output_type)) {
    stop("Input type matches output type. Nothing to do.")
  }
  match <- airports %>% dplyr::filter()
  if(!missing(input) & input_type == "IATA") {
    match<- airports %>% dplyr::filter(IATA == input)
    if(sum(lengths(match))==0) {stop("Unable to find matching IATA code.")}
    if(output_type == "name") {match %>% dplyr::pull(Name) -> result}
    if(output_type == "ICAO") {match %>% dplyr::pull(ICAO) -> result}
    if(output_type == "city") {match %>% dplyr::pull(City) -> result}
    return(result)
  }
  if(input_type == "ICAO") {
    match<- airports %>% dplyr::filter(ICAO == input)
    if(sum(lengths(match))==0) {stop("Unable to find matching ICAO code.")}
    if(output_type == "name") {match %>% dplyr::pull(Name) -> result}
    if(output_type == "IATA") {match %>% dplyr::pull(IATA) -> result}
    if(output_type == "city") {match %>% dplyr::pull(City) -> result}
    return(result)
  }
  if(input_type == "name") {
    match<- airports %>% dplyr::filter(Name == input)
    if(length(match$Name) == 1) {
      if(output_type == "IATA") {match %>% dplyr::pull(IATA) -> result}
      if(output_type == "ICAO") {match %>% dplyr::pull(ICAO) -> result}
      if(output_type == "city") {match %>% dplyr::pull(City) -> result}
      return(result)
    }
    if(length(match$Name) > 1) {
      warning("Multiple airport names are exact matches.", immediate. = TRUE)
      match %>% dplyr::select(Name, City, Country, IATA, ICAO) -> result
      return(result)
    }
    if(length(match$Name) == 0) {
      similar <- agrep(input, airports$Name,
                       ignore.case = TRUE,
                       max.distance = 0.15,
                       value = TRUE)
      if(length(similar)>0) {
      warning("No exact matches but some similar names in the database include:",
              immediate. = TRUE)
      cat(similar, sep = "\n")
      } else {
        warning("Unable to find matching name in database.")
        }
    }
  }
}

#' Lookup full airport details based of a standard airport input
#'
#' Return all airport details given an input IATA code, ICAO code, or airport name.
#'
#' @param input An airport name, IATA code, or ICAO code. Input type will be guessed unless
#' explicitly defined
#' @param input_type One of "name", "IATA", or "ICAO". Function will attempt to guess type if
#' not supplied
#' @return A 1x14 tibble with airport details
#'
#' @export
#'
#' @examples
#' airport_detail("YVR")
#' airport_detail("London Heathrow Airport")
airport_detail <- function(input, input_type) {
  data("airports", envir=environment())
  if(missing(input_type)) {
    if(nchar(input)==3 & input == toupper(input)) {
      if(!is.na(match(input,airports$IATA))) {input_type <- "IATA"}
      else {stop("Invalid IATA code")}
    }
    if(nchar(input)==4 & input == toupper(input)) {
      if(!is.na(match(input,airports$ICAO))) {input_type <- "ICAO"}
      else {stop("Invalid ICAO code")}
    }
    if(nchar(input)>4 & input != toupper(input)) {
      input_type <- "name"
    }
  }
  if(!input_type %in% c("IATA","ICAO","name")) {
    stop("Input type must be one of \"IATA\",\"ICAO\", or \"name\"")}
  if(input_type == "IATA") {
    match<- airports %>% dplyr::filter(IATA == input)
    if(sum(lengths(match))==0) {stop("Unable to find matching IATA code.")}
    return(match)
  }
  if(input_type == "ICAO") {
    match<- airports %>% dplyr::filter(ICAO == input)
    if(sum(lengths(match))==0) {stop("Unable to find matching ICAO code.")}
    return(match)
  }
  if(input_type == "name") {
    match<- airports %>% dplyr::filter(Name == input)
    if(sum(lengths(match))==0) {

      similar <- agrep(input, airports$Name,
                       ignore.case = TRUE,
                       value = TRUE,
                       max.distance = 0.15)
      if(length(similar)>0) {
        warning("No exact matches but some similar names in the database include:",
                immediate. = TRUE)
        cat(similar, sep = "\n")} else {warning("Unable to find matching name in database.",
                                                immediate. = TRUE)
        }
    } else {return(match)}
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
  data("airports", envir = environment())
  if(missing(country)) {
    match<- airports %>% dplyr::filter(City == city)
    if(length(unique(match$Country)) > 1) {
      warning("Input city matches cities in multiple countries. Do you want to specify a country argument? See documentation ?city_airports for details.")
    }
  } else {
    if(!is.character(country)) stop("Country names or codes have to be in character format. If using a numeric country code, wrap it as a string. For example: '036' for Australia")
    else {
      input_country <- country_lookup[apply(country_lookup, 1, function(x)  any(grep(paste0("^",country,"$"), x, ignore.case = TRUE))),]$Country
      if(nchar(input_country) == 0) stop("Invalid country name or code. Please enter a country name or country code in either numeric, Alpha-2, or Alpha-3 format.")
      match <- airports %>%
        dplyr::filter(City == city, Country == input_country)
    }
  }
  if(length(rownames(match)) == 0) {
    similar <- unique(agrep(city, airports$City, ignore.case = TRUE, value = TRUE,
                     max.distance = 0.15))
    match <- NULL
    if(length(similar)>0) {
      match <- NULL
      warning("No exact matches but some similarly named cities in the database include:",
              immediate. = TRUE)
      cat(similar, sep = "\n")} else {
        match <- NULL
        stop("Unable to find matching or similar names in database.")
      }
  }
  if(!is.null(match)) return(match)
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
  data("airports", envir=environment())
  if(missing(input_type)) {
    if(nchar(input)==3 & input == toupper(input)) {
      if(!is.na(match(input,airports$IATA))) {input_type <- "IATA"}
      else {stop("Invalid IATA code")}
    }
    if(nchar(input) == 4 & input == toupper(input)) {
      if(!is.na(match(input,airports$ICAO))) {input_type <- "ICAO"}
      else {stop("Invalid ICAO code")}
    }
    if(nchar(input) > 4 & input != toupper(input)) {
      input_type <- "name"
    }
  }
  if(!input_type %in% c("IATA","ICAO","name")) {
    stop("Input type must be one of \"IATA\",\"ICAO\", or \"name\"")}
  if(input_type == "IATA") {
    match<- airports %>% dplyr::filter(IATA == input) %>% dplyr::select(Latitude, Longitude)
    if(sum(lengths(match))==0) {stop("Unable to find matching IATA code.")}
    return(match)
  }
  if(input_type == "ICAO") {
    match<- airports %>% dplyr::filter(ICAO == input) %>% dplyr::select(Latitude, Longitude)
    if(sum(lengths(match))==0) {stop("Unable to find matching ICAO code.")}
    return(match)
    }

  if(input_type == "name") {
    match<- airports %>% dplyr::filter(Name == input) %>% dplyr::select(Latitude, Longitude)
    if(length(match$Latitude) == 0) {
      similar <- agrep(input, airports$Name,
                       ignore.case = TRUE,
                       value = TRUE,
                       max.distance = 0.15)
      if(sum(lengths(match))==0) {
        warning("No exact matches but some similar names in the database include:",
                immediate. = TRUE)
        cat(similar, sep = "\n")} else {warning("Unable to find matching name in database.")
        }
    } else {return(match)}
  }
}
