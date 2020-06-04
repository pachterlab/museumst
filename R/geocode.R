geocode_city <- function(sheet) {
  city <- `state/province` <- country <- city2 <- geometry <- NULL
  # Geocode cities
  cities <- sheet %>%
    select(city, `state/province`, country) %>%
    distinct() %>%
    filter(!is.na(city)) %>%
    unite(col = "city2", city, `state/province`, country, remove = FALSE, sep = ", ") %>%
    mutate(city2 = str_remove(city2, "NA, "))
  cities_gc <- tmaptools::geocode_OSM(cities$city2)
  cities_gc <- cbind(cities, cities_gc)
  cities_gc <- sf::st_as_sf(cities_gc, coords = c("lon", "lat"), crs = sf::st_crs(4326)) %>%
    select(city, city2, `state/province`, country, geometry)
  cities_gc
}

geocode_first_time <- function(sheet, cache = TRUE, cache_location = ".") {
  if (cache) {
    cache_location <- normalizePath(cache_location, mustWork = FALSE)
    fn <- paste0(cache_location, "/geocode_cache.rds")
  }
  city_gc <- geocode_city(sheet)
  if (cache) saveRDS(city_gc, file = fn, version = 2)
  city_gc
}

#' Geocode institutions and cities
#'
#' Get longitude and latitude of institutions and cities. If cache is used and
#' there're some institutions or cities not already in the cache, those will be
#' added.
#'
#' @param sheet The tibble read from Google Sheets, which has columns country,
#' state/province, city, and institution.
#' @param cache Logial, whether to cache.
#' @param cache_location Where to save the cache
#' @return A sf data frame for longitude and latitude of cities
#' @importFrom dplyr distinct
#' @importFrom tidyr unite
#' @importFrom stringr str_remove
#' @export
geocode_inst_city <- function(sheet, cache = TRUE, cache_location = ".") {
  .pkg_check("tmaptools")
  city <- NULL
  if (cache) {
    cache_location <- normalizePath(cache_location, mustWork = FALSE)
    fn <- paste0(cache_location, "/geocode_cache.rds")
    first_time <- !dir.exists(cache_location) | !file.exists(fn)
    if (!dir.exists(cache_location)) dir.create(cache_location)
    fn_inst <- system.file("geocode_cache.rds", package = "museumst")
    if (first_time || file.mtime(fn_inst) > file.mtime(fn)) {
      file.copy(fn_inst, fn)
      return(readRDS(fn))
    } else {
      # Get existing cache
      city_gc <- readRDS(fn)
      sheet <- sheet %>%
        filter(!is.na(city))
      # Check if there's new city
      sheet_city <- sheet %>%
        anti_join(city_gc, by = c("country", "state/province", "city"))
      if (nrow(sheet_city) > 0) {
        city_gc2 <- geocode_city(sheet_city)
        city_gc <- rbind(city_gc, city_gc2)
        message("Added ", nrow(sheet_city), " new cities to cache.")
      }
      return(city_gc)
    }
  } else {
    geocode_first_time(sheet, cache = FALSE)
  }
}
