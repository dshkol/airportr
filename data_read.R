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

iso2 <- readr::read_csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/slim-2/slim-2.csv") %>%
  select(country = name, alpha2 = `alpha-2`, country_code = `country-code`)

iso3 <- readr::read_csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/slim-3/slim-3.csv") %>%
  select(country = name, alpha3 = `alpha-3`)

iso_codes <- full_join(iso2, iso3, by = "country") %>% select(country, country_code, alpha2, alpha3)

airports <- left_join(
  airports %>%
    mutate(
      Country2 = Country,
      Country = case_when(
        Country == "Cote d'Ivoire" ~ "Côte d'Ivoire",
        Country == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland" ,
        Country == "Congo (Brazzaville)" ~ "Congo",
        Country == "Congo (Kinshasa)" ~ "Congo, Democratic Republic of the",
        Country == "Swaziland" ~ "Eswatini" ,
        Country == "Reunion" ~ "Réunion",
        Country == "Cape Verde" ~ "Cabo Verde",
        Country == "Tanzania" ~ "Tanzania, United Republic of" ,
        Country == "Czech Republic" ~ "Czechia",
        Country == "Moldova" ~ "Moldova, Republic of",
        Country == "Macedonia" ~ "North Macedonia",
        Country == "Iran" ~ "Iran (Islamic Republic of)",
        Country == "West Bank" ~ "Palestine, State of" ,
        Country == "Syria" ~ "Syrian Arab Republic",
        Country == "Midway Islands" ~ "United States Minor Outlying Islands",
        Country == "Wake Island" ~ "United States Minor Outlying Islands",
        Country == "Johnston Atoll" ~ "United States Minor Outlying Islands",
        Country == "Micronesia" ~ "Micronesia (Federated States of)" ,
        Country == "Taiwan" ~ "Taiwan, Province of China" ,
        Country == "South Korea" ~ "Korea, Republic of",
        Country == "Bolivia" ~ "Bolivia (Plurinational State of)",
        Country == "Venezuela" ~ "Venezuela (Bolivarian Republic of)",
        Country == "Virgin Islands" ~ "Virgin Islands (U.S.)",
        Country == "Netherlands Antilles" &
          City == "Kralendijk" ~ "Bonaire, Sint Eustatius and Saba",
        Country == "Netherlands Antilles" &
          City == "Willemstad" ~ "Curaçao",
        Country == "Netherlands Antilles" &
          City == "Oranjestad" ~ "Bonaire, Sint Eustatius and Saba",
        Country == "Netherlands Antilles" &
          City == "Philipsburg" ~ "Sint Maarten (Dutch part)",
        Country == "Netherlands Antilles" &
          City == "Saba" ~ "Bonaire, Sint Eustatius and Saba",
        Country == "British Virgin Islands" ~ "Virgin Islands (British)" ,
        Country == "Saint Helena" ~ "Saint Helena, Ascension and Tristan da Cunha" ,
        Country == "Russia" ~ "Russian Federation",
        Country == "Laos" ~ "Lao People's Democratic Republic",
        Country == "Macau" ~ "Macao",
        Country == "Vietnam" ~ "Viet Nam",
        Country == "Burma" ~ "Myanmar",
        Country == "Brunei" ~ "Brunei Darussalam",
        Country == "East Timor" ~ "Timor-Leste",
        Country == "North Korea" ~ "Korea (Democratic People's Republic of) ",
        Country == "United States" ~ "United States of America",
        Country == "Palestine" ~ "Palestine, State of",
        Country == "Svalbard" ~ "Svalbard and Jan Mayen",
        Country == "Falkland Islands" ~ "Falkland Islands (Malvinas)",
        TRUE ~ Country
      )
    ),
  iso_codes,
  by = c("Country" = "country")
) %>%
  select(
    `OpenFlights ID`,
    Name,
    City,
    IATA,
    ICAO,
    Country = Country2,
    `Country Code` = country_code,
    `Country Code (Alpha-2)` = alpha2,
    `Country Code (Alpha-3)` = alpha3,
    Latitude,
    Longitude,
    Altitude,
    UTC,
    DST,
    Timezone,
    Type,
    Source
  )

save(airports, file = "data/airports.rda")
#save(iso_codes, file = "data/iso_codes.rda")

# Last revised 6 OCT 2019
