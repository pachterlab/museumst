geocode_inst <- function(sheet) {
  country <- city <- institution <- NULL
  # Geocode institutions
  institution2 <- sheet %>%
    select(country, city, institution) %>%
    distinct() %>%
    filter(!is.na(institution)) %>%
    unite(col = "institution2", institution, city, country, remove = FALSE, sep = ", ") %>%
    mutate(institution2 = str_remove(institution2, "NA, "))
  institution_gc <- ggmap::geocode(institution2$institution2)
  institution_gc <- cbind(institution2, institution_gc)
  institution_gc <- sf::st_as_sf(institution_gc, coords = c("lon", "lat"), crs = sf::st_crs(4326))
  institution_gc
}
geocode_city <- function(sheet) {
  city <- `state/province` <- country <- city2 <- NULL
  # Geocode cities
  cities <- sheet %>%
    select(city, `state/province`, country) %>%
    distinct() %>%
    filter(!is.na(city)) %>%
    unite(col = "city2", city, `state/province`, country, remove = FALSE, sep = ", ") %>%
    mutate(city2 = str_remove(city2, "NA, "))
  cities_gc <- ggmap::geocode(cities$city2)
  cities_gc <- cbind(cities, cities_gc)
  cities_gc <- sf::st_as_sf(cities_gc, coords = c("lon", "lat"), crs = sf::st_crs(4326))
  cities_gc
}

geocode_first_time <- function(sheet, cache = TRUE, cache_location = ".") {
  if (cache) {
    cache_location <- normalizePath(cache_location, mustWork = FALSE)
    fn <- paste0(cache_location, "/geocode_cache.rds")
  }
  inst_gc <- geocode_inst(sheet)
  city_gc <- geocode_city(sheet)
  out <- list(inst_gc = inst_gc,
              city_gc = city_gc)
  if (cache) saveRDS(out, file = fn, version = 2)
  out
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
#' @return A list of two sf data frames:
#' \describe{
#'   \item{inst_gc}{A sf data frame with columns city, institution, and geometry.}
#'   \item{city_gc}{A sf data frame with columns country, state/province, city,
#'   and geometry.}
#' }
#' @importFrom dplyr distinct
#' @importFrom tidyr unite
#' @importFrom stringr str_remove
#' @importFrom zeallot %<-%
#' @export
geocode_inst_city <- function(sheet, cache = TRUE, cache_location = ".") {
  .pkg_check("ggmap")
  city <- institution <- NULL
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
      c(inst_gc, city_gc) %<-% readRDS(fn)
      sheet <- sheet %>%
        filter(!is.na(city), !is.na(institution))
      # Check if there's new institution
      sheet_inst <- sheet %>%
        anti_join(inst_gc, by = c("country", "city", "institution"))
      if (nrow(sheet_inst) > 0) {
        inst_gc2 <- geocode_inst(sheet_inst)
        inst_gc <- rbind(inst_gc, inst_gc2)
        message("Added ", nrow(sheet_inst), " new institutions to cache.")
        # Check if there's new city
        sheet_city <- sheet %>%
          anti_join(city_gc, by = c("country", "state/province", "city"))
        if (nrow(sheet_city) > 0) {
          city_gc2 <- geocode_city(sheet_city)
          city_gc <- rbind(city_gc, city_gc2)
          message("Added ", nrow(sheet_city), " new cities to cache.")
        }
        out <- list(inst_gc = inst_gc,
                    city_gc = city_gc)
        saveRDS(out, file = fn, version = 2)
        return(out)
      } else {
        out <- list(inst_gc = inst_gc,
                    city_gc = city_gc)
        return(out)
      }
    }
  } else {
    geocode_first_time(sheet, cache = FALSE)
  }
}
