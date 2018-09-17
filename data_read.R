airport_cols <- c("OpenFlights ID",
                  "Name",
                  "City",
                  "Country",
                  "IATA",
                  "ICAO",
                  "Latitude",
                  "Longitude",
                  "Altitude",
                  "UTC",
                  "DST",
                  "Timezone",
                  "Type",
                  "Source")

airports <- readr::read_csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat", col_names = airport_cols)



