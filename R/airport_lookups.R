#' Return city name, airport name, IATA code, or IACO code given an input IATA code, ICAO code, or airport name.
#'
#' @param input An airport name, IATA code, or ICAO code
#' @param input_type One of "name", "IATA", or "ICAO"
#' @param output_type One of "name", "city", "IATA", or "ICAO"
#' @return The appropriate city, airport name, IATA code, or ICAO code for that airport
#' @examples
#' airport_lookup("Vancouver International Airport","name","IATA")
#' airport_lookup("YVR","IATA","city")
#'
#' Produces a list of similar named airports
#' airport_lookup("Vancoover","IATA","city")
airport_lookup <- function(input, input_type, output_type) {
  if(!input_type %in% c("IATA","ICAO","name")) {
    stop("Input type must be one of \"IATA\", \"ICAO\", or \"name\"")}
  if(!output_type %in% c("IATA","ICAO","name","city")) {
    stop("Input type must be one of \"IATA\", \"ICAO\", \"name\", or \"city\"")}
  data("airports")
  if(input_type == output_type) {
    warning("Input type matches output type. Nothing to do.")
    }
  if(input_type == "IATA") {
    match<- airports %>% dplyr::filter(IATA == input)
    if(sum(lengths(match))==0) {stop("Unable to find matching IATA code.")}
    if(output_type == "name") {match %>% pull(Name) -> result}
    if(output_type == "ICAO") {match %>% pull(ICAO) -> result}
    if(output_type == "city") {match %>% pull(City) -> result}
    return(result)
  }
  if(input_type == "ICAO") {
    match<- airports %>% dplyr::filter(ICAO == input)
    if(sum(lengths(match))==0) {stop("Unable to find matching ICAO code.")}
    if(output_type == "name") {match %>% pull(Name) -> result}
    if(output_type == "IATA") {match %>% pull(IATA) -> result}
    if(output_type == "city") {match %>% pull(City) -> result}
    return(result)
  }
  if(input_type == "name") {
    match<- airports %>% dplyr::filter(Name == input)
    if(length(match$Name) == 1) {
      if(output_type == "IATA") {match %>% pull(IATA) -> result}
      if(output_type == "ICAO") {match %>% pull(ICAO) -> result}
      if(output_type == "city") {match %>% pull(City) -> result}
      return(result)
    }
    if(length(match$Name) > 1) {
      warning("Multiple airport names are exact matches.", immediate. = TRUE)
      match %>% select(Name, City, Country, IATA, ICAO) -> result
      return(result)
    }
    if(length(match$Name) == 0) {
      similar <- agrep(input, airports$Name, ignore.case = TRUE, value = TRUE)
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

#' Return all airport details given an input IATA code, ICAO code, or airport name
#'
#' @param input An airport name, IATA code, or ICAO code
#' @param input_type One of "name", "IATA", or "ICAO"
#' @return A 1x14 tibble with airport details
#' @examples
#' airport_detail("YVR","IATA")
#' airport_detail("London Heathrow Airport","name")
airport_detail <- function(input, input_type) {
  if(!input_type %in% c("IATA","ICAO","name")) {
    stop("Input type must be one of \"IATA\",\"ICAO\", or \"name\"")}
  data("airports")
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
    return(match)
    if(sum(lengths(match))==0) {
      similar <- agrep(input, airports$Name, ignore.case = TRUE, value = TRUE)
      if(length(similar)>0) {
        warning("No exact matches but some similar names in the database include:",
                immediate. = TRUE)
        cat(similar, sep = "\n")} else {warning("Unable to find matching name in database.",
                                                immediate. = TRUE)
        }
    }
  }
}

#' Return all airports matching a city input
#'
#' @param city A city name
#' @param country (Optional) A country name
#' @return A nx14 tibble with airport details where n is the number of airports serving that city
#' @examples
#' city_airports("Vancouver")
#' city_airports("London")
#' city_airports("London","Canada")
city_airports <- function(city, country) {
  data("airports")
  if(missing(country)) {
    match<- airports %>% dplyr::filter(City == city)
    if(length(unique(match$Country)) > 1) {
      warning("City matches in cities in multiple countries. Do you want to specify a country   argument? See documentation ?city_airports for details.")
      return(match)
    }
  } else {
    match <- airports %>% dplyr::filter(City == city, Country == country)
    return(match)
  }
  if(sum(lengths(match))==0) {
    similar <- agrep(input, airports$Name, ignore.case = TRUE, value = TRUE)
    if(length(similar)>0) {
      warning("No exact matches but some similarly named cities in the database include:",
              immediate. = TRUE)
      cat(similar, sep = "\n")} else {warning("Unable to find matching name in database.",
                                              immediate. = TRUE)
      }
  }
}

#' Return airport location in lon/lat given an input IATA code, ICAO code, or airport name.
#'
#' @param input An airport name, IATA code, or ICAO code
#' @param input_type One of "name", "IATA", or "ICAO"
#' @return List of longitude and latitude coordinates
#' @examples
#' airport_location("YVR","IATA")
#' #' airport_location("Vancouver International Airport","name")
airport_location <- function(input, input_type) {
  if(!input_type %in% c("IATA","ICAO","name")) {
    stop("Input type must be one of \"IATA\",\"ICAO\", or \"name\"")}
  data("airports")
  if(input_type == "IATA") {
    match<- airports %>% dplyr::filter(IATA == input) %>% select(Latitude, Longitude)
    if(sum(lengths(match))==0) {stop("Unable to find matching IATA code.")}
    return(match)
  }
  if(input_type == "ICAO") {
    match<- airports %>% dplyr::filter(ICAO == input) %>% select(Latitude, Longitude)
    if(sum(lengths(match))==0) {stop("Unable to find matching ICAO code.")}
    return(match)
    }

  if(input_type == "name") {
    match<- airports %>% dplyr::filter(Name == input) %>% select(Latitude, Longitude)
    return(match)
    if(length(match$Latitude) == 0) {
      similar <- agrep(input, airports$Name, ignore.case = TRUE, value = TRUE)
      if(sum(lengths(match))==0) {
        warning("No exact matches but some similar names in the database include:",
                immediate. = TRUE)
        cat(similar, sep = "\n")} else {warning("Unable to find matching name in database.")
        }
    }
  }
}

