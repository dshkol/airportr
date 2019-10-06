utils::globalVariables(c("airports","IATA","ICAO","Name","Latitude","Longitude","City","Country","country_lookup"))

# Creating a quick lookup table for country lookups
data("airports", envir=environment())
country_lookup <- airports %>%
  dplyr::distinct(Country, `Country Code`, `Country Code (Alpha-2)`, `Country Code (Alpha-3)`)
