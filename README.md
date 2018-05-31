# airportr

This package takes open source airport data and provides tools to look up information, translate names into codes and vice-verse, as well as some basic calculation functions for measuring distances.

## Example Usage

``` r
# Return an airport name given an IATA code
airport_lookup("YVR", input_type = "IATA", output_type = "name")

# Return an airport IATA code given an airport name
airport_lookup("Vancouver International Airport", input_type = "name", output_type ="IATA")

# Return all available details for an aiport given an airport name
airport_detail("YVR", input_type = "name")

# Return location of an airport given an ICAO code
airport_location("CYVR", input_type = "ICAO")
```

# To-do

- [x] Build lookup functions with meaningful messages
- [x] Debug lookup functions
- [ ] Add generic input for functions that can handle different types of inputs
- [ ] Add unit tests
- [ ] Add distance calculation functions
- [ ] Add nearest neighbour search and geographic lookup functions
