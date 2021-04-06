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
      warning(paste("No results found for \"", q[k], "\".", sep="")) #if (n==1)
      return(c(lat = NA, lon = NA))
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

#' Get longitude and latitude of cities
#'
#' @param sheet A data frame with columns "city", "state/province", and "country".
#' @param existing A data frame with earlier output of this function, so only
#' cities not already in the collection are geocoded.
#' @param geocode_method Which geocoding service to use. Must be either "OSM"
#' (OpenStreetMap) or "Google". If "Google", then an API key must be provided in
#' environment variable `GGMAP_GOOGLE_API_KEY`.
#' @return A data frame like the internal data `lcm_city_gc`, with columns
#' "city", "city2" (city name is concatenated to state and country in case
#' multiple cities around the world have the same name), "state/province",
#' "country", and "geometry" (sfc_POINT for the longitudes and latitudes).
#' @export
geocode_city <- function(sheet, existing = NULL,
                         geocode_method = c("OSM", "Google")) {
  city <- `state/province` <- country <- city2 <- geometry <- NULL
  geocode_method <- match.arg(geocode_method)
  # Check the existing collection
  names_expect <- c("city", "city2", "state/province", "country", "geometry")
  if (is.data.frame(existing)) {
    cond <- all.equal(sort(names(existing)), sort(names_expect))
    if (!isTRUE(cond)) {
      stop("Data frame 'existing' is not correctly formatted. See data('lcm_city_gc').")
    }
  }
  # Geocode cities
  cities <- sheet %>%
    select(city, `state/province`, country) %>%
    distinct() %>%
    filter(!is.na(city)) %>%
    unite(col = "city2", city, `state/province`, country, remove = FALSE, sep = ", ") %>%
    mutate(city2 = str_remove(city2, "NA, "))
  if (is.data.frame(existing)) {
    cities <- cities %>%
      anti_join(existing, by = "city2")
  }
  geocode_fun <- if (geocode_method == "OSM") .geocode_OSM else ggmap::geocode
  cities_gc <- geocode_fun(cities$city2)
  cities_gc <- cbind(cities, cities_gc)
  cities_gc <- sf::st_as_sf(cities_gc, coords = c("lon", "lat"), crs = sf::st_crs(4326)) %>%
    select(city, city2, `state/province`, country, geometry)
  if (is.data.frame(existing)) {
    cities_gc <- rbind(cities_gc, existing)
  }
  cities_gc
}

geocode_first_time <- function(sheet, cache = TRUE, cache_location = ".", ...) {
  if (cache) {
    cache_location <- normalizePath(cache_location, mustWork = FALSE)
    fn <- paste0(cache_location, "/geocode_cache.rds")
  }
  city_gc <- geocode_city(sheet, ...)
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
geocode_inst_city <- function(sheet, cache = TRUE, cache_location = ".", ...) {
  city <- NULL
  if (cache) {
    cache_location <- normalizePath(cache_location, mustWork = FALSE)
    fn <- paste0(cache_location, "/geocode_cache.rds")
    first_time <- !dir.exists(cache_location) | !file.exists(fn)
    if (!dir.exists(cache_location)) dir.create(cache_location)
    fn_inst <- system.file("geocode_cache.rds", package = "museumst")
    if (first_time || file.mtime(fn_inst) > file.mtime(fn)) {
      file.copy(fn_inst, fn)
    }
    # Get existing cache
    city_gc <- readRDS(fn)
    sheet <- sheet %>%
      filter(!is.na(city))
    # Check if there's new city
    sheet_city <- sheet %>%
      anti_join(city_gc, by = c("country", "state/province", "city")) %>%
      select(country, `state/province`, city) %>%
      distinct()
    if (nrow(sheet_city) > 0) {
      city_gc2 <- geocode_city(sheet_city, ...)
      city_gc <- rbind(city_gc, city_gc2)
      message("Added ", nrow(sheet_city), " new cities to cache.")
    }
    return(city_gc)
  } else {
    geocode_first_time(sheet, cache = FALSE)
  }
}

#' Geocode addresses and institutions with Google's API
#'
#' I'm using Google's API even though it's not free because it works better than
#' OpenStreetMap when it comes to addresses and institution names.
#'
#' @param df A data frame that has a column for addresses.
#' @param address_col Column for addresses. Tidyeval is used here so it doesn't
#' need to be quoted.
#' @return A data frame with all columns of df, but with city, state, and country
#' added. Only rows with valid geocoding results are returned.
#' @importFrom stringr str_trim
#' @export
geocode_address <- function(df, address_col) {
  address_col <- enquo(address_col)
  addresses <- pull(df, !!address_col)
  addresses <- addresses[!is.na(addresses)]
  df2 <- data.frame(address = unique(addresses))
  df_gc <- df2 %>%
    mutate(geodata = purrr::map(address, ggmap::geocode, output = "all"))
  df_gc <- df_gc %>%
    mutate(address_components = purrr::map(geodata, list("results", 1, "address_components")),
           to_rm = map_lgl(address_components, is.null)) %>%
    filter(!to_rm) %>%
    select(-to_rm)
  df_gc <- df_gc %>%
    mutate(address_components = purrr::map(address_components,
                                           function(.x) {
                                             as_tibble(transpose(.x)) %>%
                                             unnest(c("long_name", "short_name")) %>%
                                             unnest("types") %>%
                                             unnest("types")
                                             }),
           country = map_chr(address_components, ~ .x$long_name[.x$types == "country"]),
           `state/province` = map_chr(address_components,
                                      function(.x) {
                                        out <- .x$long_name[.x$types == "administrative_area_level_1"]
                                        if (length(out) == 0) NA_character_ else out
                                      }))
  df_gc <- df_gc %>%
    mutate(city = map_chr(address_components,
                          function(.x) {
                            out <- .x$long_name[.x$types == "postal_town"]
                            if (length(out) == 0)
                              out <- .x$long_name[.x$types == "locality"]
                            if (length(out) == 0)
                              out <- .x$long_name[.x$types == "administrative_area_level_2"]
                            if (length(out) == 0)
                              out <- .x$long_name[.x$types == "administrative_area_level_3"]
                            if (length(out) == 0)
                              out <- .x$long_name[.x$types == "administrative_area_level_1"]
                            if (length(out) == 0) out <- NA_character_
                            if (length(out) > 1L) out <- out[1]
                            out
                          }))
  df_gc <- df_gc %>%
    mutate(`state/province` = str_remove(`state/province`, "\\s(Sheng)|(Shi)$") %>%
             str_trim(),
           city = str_remove(city, "\\s(City)|(Shi)$") %>%
             str_trim()) %>%
    ungroup()
  by_vec <- "address"
  names(by_vec) <- quo_name(address_col)
  out <- df %>%
    inner_join(df_gc, by = by_vec)
  df_gc
}
