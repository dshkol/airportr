# airportr

This package uses open source airport data to provide tools to look up information, translate names and codes into other formats, as well as some basic calculation functions for measuring distances.

## Installation

```r
# install.packages("devtools")
devtools::install_github("dshkol/airportr")
```

## Example Usage: Lookup functions

This package is primarily to facilitate working with and translating structured airport data from one format into another. 

``` r
# Return an airport name given an IATA code
airport_lookup("YVR", input_type = "IATA", output_type = "name")
[1] "Vancouver International Airport"

# Return an airport IATA code given an airport name
airport_lookup("Vancouver International Airport", input_type = "name", output_type ="IATA")
[1] "YVR"

# If input type is not specified will try to guess
airport_lookup("YVR", output_type = "city")
[1] "Vancouver"

# Return all available details for an aiport given an airport name, IATA code, or ICAO code
airport_detail("YVR")
# A tibble: 1 x 14
  `OpenFlights ID` Name      City  Country IATA  ICAO  Latitude Longitude Altitude   UTC DST  
             <int> <chr>     <chr> <chr>   <chr> <chr>    <dbl>     <dbl>    <int> <dbl> <chr>
1              156 Vancouve… Vanc… Canada  YVR   CYVR      49.2     -123.       14    -8 A    
# ... with 3 more variables: Timezone <chr>, Type <chr>, Source <chr>

airport_detail("CYVR")
# A tibble: 1 x 14
  `OpenFlights ID` Name      City  Country IATA  ICAO  Latitude Longitude Altitude   UTC DST  
             <int> <chr>     <chr> <chr>   <chr> <chr>    <dbl>     <dbl>    <int> <dbl> <chr>
1              156 Vancouve… Vanc… Canada  YVR   CYVR      49.2     -123.       14    -8 A    
# ... with 3 more variables: Timezone <chr>, Type <chr>, Source <chr>

# Return location of an airport given an ICAO code
airport_location("CYVR", input_type = "ICAO")
# A tibble: 1 x 2
  Latitude Longitude
     <dbl>     <dbl>
1     49.2     -123.

# Display all airports serving a given city
city_airports("Vancouver")
# A tibble: 3 x 14
  `OpenFlights ID` Name      City  Country IATA  ICAO  Latitude Longitude Altitude   UTC DST  
             <int> <chr>     <chr> <chr>   <chr> <chr>    <dbl>     <dbl>    <int> <dbl> <chr>
1              156 Vancouve… Vanc… Canada  YVR   CYVR      49.2     -123.       14    -8 A    
2             4107 Coal Har… Vanc… Canada  "\\N" CAQ3      50.6     -128.        0    -8 A    
3             5500 Vancouve… Vanc… Canada  CXH   CYHC      49.3     -123.        0    -8 A    
# ... with 3 more variables: Timezone <chr>, Type <chr>, Source <chr>
```
## Example Usage: Distance functions

This package also includes some convenience functions for working with distances between airports. 

```r
# Calculate distance between two airports in km
airport_distance("YVR","LHR")
[1] 7580.963
```
Another common airport-related task is to locate airports in the vicinity of a city or specified location. This is easy to implement wit h the `airports_near()` and `airports_around()` functions: 

```r
#' airports_near("YEG")
# A tibble: 3 x 14
  `OpenFlights ID` Name            City      Country IATA  ICAO  Latitude Longitude Altitude   UTC DST   Timezone   Type  Source 
             <int> <chr>           <chr>     <chr>   <chr> <chr>    <dbl>     <dbl>    <int> <dbl> <chr> <chr>      <chr> <chr>  
1               49 Edmonton Inter… Edmonton  Canada  YEG   CYEG      53.3     -114.     2373    -7 A     America/E… airp… OurAir…
2              131 Rocky Mountain… Rocky Mo… Canada  YRM   CYRM      52.4     -115.     3244    -7 A     America/E… airp… OurAir…
3              165 Edmonton City … Edmonton  Canada  YXD   CYXD      53.6     -114.     2202    -7 A     America/E… airp… OurAir…

#' airports_around(49.2,-123, distance = 20)
# A tibble: 3 x 14
  `OpenFlights ID` Name            City     Country IATA  ICAO  Latitude Longitude Altitude   UTC DST   Timezone    Type  Source 
             <int> <chr>           <chr>    <chr>   <chr> <chr>    <dbl>     <dbl>    <int> <dbl> <chr> <chr>       <chr> <chr>  
1              156 Vancouver Inte… Vancouv… Canada  YVR   CYVR      49.2     -123.       14    -8 A     America/Va… airp… OurAir…
2             5500 Vancouver Harb… Vancouv… Canada  CXH   CYHC      49.3     -123.        0    -8 A     America/Va… airp… OurAir…
3             7273 Boundary Bay A… Boundar… Canada  YDT   CZBB      49.1     -123.        6    -8 A     America/Va… airp… OurAir…
```

## Data

Airport data is from the [OpenFlights Airport Database](https://openflights.org/data.html) made available under the [Open Database License](https://opendatacommons.org/licenses/odbl/1.0/). 

Disclaimer on the data from OpenFlights:

> This data is not suitable for navigation. OpenFlights does not assume any responsibility whatsoever for its accuracy, and consequently assumes no liability whatsoever for results obtained or loss or damage incurred as a result of application of the data. OpenFlights expressly disclaims all warranties, expressed or implied, including but not limited to implied warranties of merchantability and fitness for any particular purpose.

## To-do

- [x] Build lookup functions with meaningful messages
- [x] Debug lookup functions
- [x] Add generic input for functions that can handle different types of inputs
- [x] Add distance calculation functions
- [x] Add nearest neighbour search and geographic lookup functions
- [ ] Test for edge cases and unusual input/output
- [ ] Submit to CRAN

Suggestions for additional functionality are welcome.
