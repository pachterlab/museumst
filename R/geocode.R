# Geocodes a location using OpenStreetMap Nominatim
#
# Geocodes a location (based on a search query) to coordinates and a bounding box. Similar to geocode from the ggmap package. It uses OpenStreetMap Nominatim. For processing large amount of queries, please read the usage policy (\url{http://wiki.openstreetmap.org/wiki/Nominatim_usage_policy}).
#
# I copied this function from the package \code{tmaptools} since this is the only function from
# that package that I use here and I don't want to deal with the other dependencies
# like \code{lwgeom}. I also stripped the functionalities for bbox and option
# as.sf since I'm not using those here.
#
# @param q a character (vector) that specifies a search query. For instance \code{"India"} or \code{"CBS Weg 11, Heerlen, Netherlands"}.
# @param projection projection in which the coordinates and bounding box are returned. Either a \code{\link[sp:CRS]{CRS}} object or a character value. If it is a character, it can either be a \code{PROJ.4} character string or a shortcut. See \code{\link{get_proj4}} for a list of shortcut values. By default latitude longitude coordinates.
# @param return.first.only Only return the first result
# @param server OpenStreetMap Nominatim server name. Could also be a local OSM Nominatim server.
#
.geocode_OSM <- function(q, projection=NULL, return.first.only=TRUE, server="http://nominatim.openstreetmap.org") {
  n <- length(q)
  q2 <- gsub(" ", "+", enc2utf8(q), fixed = TRUE)
  addr <- paste0(server, "/search?q=", q2, "&format=xml&polygon=0&addressdetails=0")

  project <- !missing(projection)

  output2 <- lapply(1:n, function(k) {
    tmpfile <- tempfile()
    suppressWarnings(download.file(addr[k], destfile = tmpfile, mode= "wb", quiet = TRUE))

    doc <- XML::xmlTreeParse(tmpfile, encoding="UTF-8")
    unlink(tmpfile)

    res <- XML::xmlChildren(XML::xmlRoot(doc))

    if (length(res)==0) {
      message(paste("No results found for \"", q[k], "\".", sep="")) #if (n==1)
      return(NULL)
    }

    idx <- if (return.first.only) 1 else 1:length(res)

    sn_names <- c("place_id", "osm_type", "osm_id", "place_rank", "display_name", "class", "type", "importance", "icon")
    output <- lapply(idx, function(i) {
      search_result <- XML::xmlAttrs(res[[i]])

      search_result_id <- search_result[sn_names]
      names(search_result_id) <- sn_names # in case of missings
      Encoding(search_result_id) <- "UTF-8"

      search_result_loc <- as.numeric(search_result[c("lat", "lon")])
      names(search_result_loc) <- c("lat", "lon")

      if (!project) {
        coords <- search_result_loc[c("lon", "lat")]
        names(coords) <- c("x", "y")
      } else {
        .crs_longlat <- sf::st_crs(4326)
        p <- sf::st_sf(sf::st_sfc(sf::st_point(search_result_loc[2:1]), crs = .crs_longlat))

        p <- sf::st_transform(p, crs=projection)

        coords <- as.vector(sf::st_coordinates(p))
        names(coords) <- c("x", "y")

        search_result_loc <- as.list(coords)
        names(search_result_loc) <- c("x", "y")
      }

      res <- c(list(query=q[k]), search_result_loc)
      res <- as.data.frame(res, stringsAsFactors=FALSE)
      res
    })
  })

  output3 <- do.call(c, output2)

  if (is.null(output3)) return(NULL)

  df <- do.call(rbind, output3)
  df
}


geocode_city <- function(sheet) {
  city <- `state/province` <- country <- city2 <- geometry <- NULL
  # Geocode cities
  cities <- sheet %>%
    select(city, `state/province`, country) %>%
    distinct() %>%
    filter(!is.na(city)) %>%
    unite(col = "city2", city, `state/province`, country, remove = FALSE, sep = ", ") %>%
    mutate(city2 = str_remove(city2, "NA, "))
  cities_gc <- .geocode_OSM(cities$city2)
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
#' @importFrom utils download.file
#' @export
geocode_inst_city <- function(sheet, cache = TRUE, cache_location = ".") {
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
