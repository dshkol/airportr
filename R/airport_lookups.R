#' Return city given an input IATA code, ICAO code, or airport name
#'
#' @param input An airport name, IATA code, or ICAO code.
#' @param input_type One of "name", "IATA", or "ICAO".
#' @param output_type One of "name", "city", "IATA", or "ICAO".
#' @return The city served by that airport.
#' @examples
#' add(1, 1)
#' add(10, 1)
airport_lookup <- function(input, input_type, output_type) {
  data("airports")
  if(input_type == output_type) {
    warning("Input type matches output type. Nothing to do.")
    }
  if(input_type == "IATA") {
    match<- airports %>% dplyr::filter(IATA == input)
    if(output_type == "name") {match %>% pull(Name) -> return}
    if(output_type == "ICAO") {match %>% pull(ICAO) -> return}
    if(output_type == "city") {match %>% pull(City) -> return}
  }
  if(input_type == "ICAO") {
    match<- airports %>% dplyr::filter(ICAO == input)
    if(output_type == "name") {match %>% pull(Name) -> return}
    if(output_type == "IATA") {match %>% pull(IATA) -> return}
    if(output_type == "city") {match %>% pull(City) -> return}
  }
  if(input_type == "name") {
    match<- airports %>% dplyr::filter(Name == input)
    if(length(match$Name) == 0) {
      similar <- agrep(input, airports$Name, ignore.case = TRUE, value = TRUE)
      if(length(similar)>0) {
      warning("No exact matches but some similar names in the database include:")
      cat(similar, sep = "\n")} else {warning("No such name in database.")}
    }
    if(output_type == "IATA") {match %>% pull(IATA) -> return}
    if(output_type == "ICAO") {match %>% pull(ICAO) -> return}
    if(output_type == "city") {match %>% pull(City) -> return}
  }
}
