#' Construct HTML image labels for time lines
#'
#' Internal, called by `plot_timeline`.
#'
#' @param image_path Character vector, paths to the images of interest. Should be NA for
#' items that don't have an image.
#' @param width Width of the images on the time line in pixels to be shown on the
#' plot.
#' @param description The text for the items to be shown beneath the images.
#' Should be a character vector of the same length as `image_path`.
#' @param description_width Width of the description text in characters to wrap
#' the text in the labels.
#' @return A character vector of HTML code to plot the images and descriptions
#' with `geom_richtext`.
make_image_labs <- function(image_path, width, description, description_width) {
  descr_wrap <- strwrap(description, width = description_width)
  descr_wrap <- paste(descr_wrap, collapse = "<br>")
  if (is.na(image_path)) {
    descr_wrap
  } else {
    paste0("<img src='", image_path, "' width='", width, "' /><br>", descr_wrap)
  }
}

#' Plot time line with images and descriptions of each event
#'
#' The time line is plotted horizontally with ticks for years. The labels with
#' images and descriptions are above and/or below that line.
#'
#' These are the sources of images that are not stated to be under public domain
#' found online with filter "free to share and use".
#' No changes were made to the images unless indicated. The author and license,
#' if found, are listed here as well.
#'
#' \describe{
#'   \item{drosophila.jpg}{http://gompel.org/images-2/drosophilidae}
#'   \item{zebrafish.jpg}{https://thumbs.dreamstime.com/m/zebrafish-zebra-barb-danio-rerio-freshwater-aquarium-fish-isolated-white-background-50201849.jpg}
#'   \item{ciona.jpg}{http://www.habitas.org.uk/marinelife/tunicata/cioints.jpg}
#'   \item{xenopus.jpg}{https://en.wikipedia.org/wiki/African_clawed_frog#/media/File:Xenopus_laevis_02.jpg
#'     by Brian Gratwicke. License: https://creativecommons.org/licenses/by/2.0/
#'   }
#'   \item{celegans.jpg}{https://en.wikipedia.org/wiki/Caenorhabditis_elegans#/media/File:Adult_Caenorhabditis_elegans.jpg
#'     by Kbradnam at English Wikipedia. License: https://creativecommons.org/licenses/by-sa/2.5/
#'     A smaller version of the original is used here.
#'   }
#'   \item{arabidopsis.jpg}{http://parts.igem.org/wiki/images/b/bd/Plants_Arabidopsis_thaliana_400px.jpg}
#'   \item{skull.jpg}{http://pngimg.com/download/42558 License: https://creativecommons.org/licenses/by-nc/4.0/
#'     The original was compressed and converted to jpg here.
#'   }
#'   \item{platynereis.jpg}{https://en.wikipedia.org/wiki/Epitoky#/media/File:PlatynereisDumeriliiFemaleEpitoke.tif
#'     By Martin Gühmann. A smaller jpg version of the original is used here.
#'     License: https://creativecommons.org/licenses/by-sa/4.0/
#'   }
#'   \item{yeast.jpg}{https://en.wikipedia.org/wiki/Shmoo#/media/File:Shmoos_s_cerevisiae.jpg
#'     By Pilarbini. This is a smaller version of the original.
#'     License: https://creativecommons.org/licenses/by-sa/4.0/deed.en
#'   }
#' }
#'
#' @param events_df A data frame with at least these columns:
#' \describe{
#'   \item{image}{Paths to the images to use in the label. NA for items without
#'   an image.}
#'   \item{date_published}{A vector of Date objects for dates when the event of
#'   interest was published. Note that the actual time when those events occurred
#'   is most likely earlier, sometimes much earlier than the publication date,
#'   and the events might have become quite influential before their publication.
#'   But the publication date is the easiest way to get an actual date.}
#'   \item{description}{Short descriptions of the events. The plot won't look
#'   good if the descriptions are too long.}
#' }
#' @param ys A numeric vector of the y coordinates of the items. Since I don't
#' know how to implement the ggrepel algorithm to make sure that the labels don't
#' overlap for geom_richtext, I have to manually set the y coordinates to make
#' sure that the labels don't overlap and look good.
#' @param width Width of the labels in pixels. How the labels will look depends
#' on the size of the plot. Would be cool if this can be specified in terms of
#' units within the plot.
#' @param description_width Width of the description text in characters to wrap
#' the text in the labels.
#' @param expand_x A numeric vector of length 2 of the proportion to expand the
#' x axis on the left and the right. This is a way to manually make sure that the
#' labels are not cropped off at the edge of the plot.
#' @param expand_y Same as expand_x, but for the y axis.
#' @return A ggplot2 object for the plot.
#' @importFrom dplyr case_when arrange between
#' @importFrom purrr map2_chr
#' @importFrom lubridate floor_date ceiling_date
#' @importFrom ggplot2 ggplot geom_point aes geom_hline geom_segment scale_x_date
#' expansion scale_y_continuous theme_void annotate
#' @export
plot_timeline <- function(events_df, ys, width = 100, description_width = 20,
                          expand_x = c(0.1, 0.1), expand_y = c(0.05, 0.05)) {
  .pkg_check("ggtext")
  image <- date_published <- description <- lab <- vjusts <- NULL
  events_df <- events_df %>%
    mutate(image = case_when(is.na(image) ~ NA_character_,
                             TRUE ~ image),
           description = paste(year(date_published), description),
           lab = map2_chr(image, description,
                          ~ make_image_labs(.x, width, .y, description_width))) %>%
    arrange(date_published) %>%
    mutate(vjusts = case_when(ys >= 0 ~ 1,
                              TRUE ~ 0),
           ys = ys)
  yrs_range <- max(year(events_df$date_published)) - min(year(events_df$date_published))
  date_brks <- case_when(yrs_range <= 20 ~ "1 year",
                           between(yrs_range, 20, 50) ~ "2 years",
                           between(yrs_range, 50, 100) ~ "5 years",
                           TRUE ~ "10 years")
  axis <- seq(floor_date(min(events_df$date_published), "year"),
              ceiling_date(max(events_df$date_published), "year"),
              by = date_brks)
  p <- ggplot(events_df) +
    geom_point(aes(x = date_published), y = 0) +
    geom_hline(yintercept = 0) +
    geom_segment(aes(x = date_published, y = 0, xend = date_published, yend = ys)) +
    ggtext::geom_richtext(aes(x = date_published, y = ys, label = lab, vjust = vjusts),
                  color = "blue", fill = "white") +
    scale_x_date(expand = expansion(expand_x)) +
    scale_y_continuous(expand = expansion(expand_y)) +
    theme_void() +
    annotate("point", x = axis, y = 0, shape = 3) +
    annotate("text", x = axis, y = 0, label = format(axis, "%Y"),
             color = "gray50", vjust = 1.4)
  p
}

#' Number of publications per year
#'
#' Plot bar plot of the number of publications per year. Preprints are excluded
#' as the dates are incoherent with those for published papers. I find facetting
#' makes the plot easier to read than filling with different colors.
#'
#' @param pubs A data frame with at least these columns:
#' \describe{
#'   \item{journal}{Name of the journal of the paper.}
#'   \item{year}{Year when the paper was published.}
#' }
#' There must be one row per publication or per method or species for each title
#' if faceting by those. If facetting, then a column whose name is the value in
#' `facet_by` must be present.
#' @param facet_by Name of a column for facetting.
#' @param binwidth Width of bins for the histogram in days.
#' @param preprints Logical, whether preprints should be included.
#' @param n_top Number of categories with the most publications to plot in facets;
#' the other categories are lumped into "other".
#' @param n_top_fill Number of categories with the most publications to be
#' differentiated by color.
#' @param sort_by How to sort the facets. first_appeared means the category that
#' appeared earlier will be nearer to the top. count means the category with more
#' count (number of publications) will be nearer to the top. Ignored if not
#' facetting.
#' @return A ggplot2 object.
#' @importFrom dplyr filter select
#' @importFrom forcats fct_reorder fct_lump_n
#' @importFrom rlang !! sym
#' @importFrom ggplot2 geom_bar scale_x_continuous labs theme facet_wrap
#' @importFrom scales breaks_pretty
#' @export
pubs_per_year <- function(pubs, facet_by = NULL, fill_by = NULL, binwidth = 365,
                          preprints = FALSE, n_top = Inf, n_top_fill = Inf,
                          sort_by = c("first_appeared", "count", "recent_count")) {
  journal <- date_published <- facets <- NULL
  sort_by <- match.arg(sort_by)
  if (!preprints) {
    pubs <- pubs %>%
      filter(!journal %in% c("bioRxiv", "arXiv"))
  }
  if (!is.null(facet_by)) {
    pubs <- pubs %>%
      mutate(facets = !!sym(facet_by))
    if (sort_by == "first_appeared") {
      pubs <- pubs %>%
        mutate(facets = fct_reorder(facets, date_published, .fun = "min"),
               w = 1)
    } else if (sort_by == "count") {
      pubs <- pubs %>%
        mutate(facets = fct_infreq(facets),
               w = 1)
    } else {
      date_thresh <- lubridate::as_date(max(pubs$date_published) - lubridate::ddays(binwidth) * 2)
      pubs <- pubs %>%
        mutate(w = as.numeric(date_published > date_thresh),
               facets = fct_reorder(facets, w, .fun = "sum", .desc = TRUE))
    }
    pubs <- pubs %>%
      mutate(facets = fct_lump_n(facets, n = n_top, ties.method = "first", w = w))
  }
  if (!is.null(fill_by)) {
    if (fill_by == "species") {
      # Use fixed colors for species
      pubs <- pubs %>%
        mutate(fill = case_when(species %in% names(species_cols) ~ species,
                                TRUE ~ "Other"),
               fill = fct_infreq(fill) %>% fct_relevel("Other", after = Inf))
    } else {
      if (n_top_fill > 11) {
        warning("Maximum of 12 colors are supported for colorblind friend palette, ",
                "less common categories are lumped into Other.")
        n_top_fill <- 11
      }
      pubs <- pubs %>%
        mutate(fill = fct_infreq(!!sym(fill_by)),
               fill = fct_lump_n(fill, n = n_top_fill, ties.method = "first"),
               fill = fct_infreq(fill) %>% fct_relevel("Other", after = Inf))
    }
  }
  p <- ggplot(pubs, aes(date_published))
  if (!is.null(facet_by)) {
    p <- p +
      geom_histogram(aes(fill = "all"), binwidth = binwidth,
                     data = select(pubs, -facets),
                     fill = "gray70", alpha = 0.5, show.legend = FALSE)
  }
  if (!is.null(fill_by)) {
    p <- p +
      geom_histogram(aes(fill = fill), binwidth = binwidth)
    if (fill_by != "species") {
      pal_use <- ifelse(n_top_fill > 7, "Paired", "Set2")
      p <- p +
        scale_fill_brewer(palette = pal_use, name = "")
    } else {
      p <- p +
        scale_fill_manual(values = species_cols, name = "") +
        theme(legend.text = element_text(face = "italic"))
    }
  } else {
    p <- p +
      geom_histogram(binwidth = binwidth)
  }
  p <- p +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       breaks = breaks_pretty()) +
    scale_x_date(breaks = breaks_pretty(10)) +
    labs(y = "Number of publications", x = "Date published") +
    theme(panel.grid.minor = element_blank())
  if (!is.null(facet_by)) {
    p <- p +
      facet_wrap(~ facets, ncol = 1)
  }
  p
}

#' Number of publications per category
#'
#' I think it looks better when the bars are horizontal to make the category
#' names easier to read, as the names can be quite long. This will plot a bar
#' chart for the number of publications per category, sorted according to the
#' number of publications.
#'
#' @param pubs A data frame at least with a column for the category of interest.
#' @param category Column name to plot. Tidyeval is supported. If it's species
#' or language, then img_df does not have to be supplied for isotype plot since
#' the images are supplied internally.
#' @param n_top Number of top entries to plot. Especially useful for isotype.
#' @param isotype Logical, whether to make isotype plot, like one icon stands for
#' a certain number of publications.
#' @param img_df A data frame with one column with the same name as `category`
#' and another column called `image_paths` for path to the images. Relative path
#' is fine since it will be internally converted to absolute path. This argument
#' can be left as NULL if category is species or language, since in these two
#' cases, the `img_df` is provided internally.
#' @param img_unit Integer, how many publications for one icon.
#' @return A ggplot2 object.
#' @importFrom rlang enquo as_name
#' @importFrom forcats fct_infreq fct_rev
#' @importFrom grid unit
#' @importFrom ggplot2 coord_flip theme element_blank
#' @importFrom purrr map_chr map
#' @importFrom dplyr row_number desc inner_join
#' @export
pubs_per_cat <- function(pubs, category, n_top = NULL, isotype = FALSE, img_df = NULL,
                         img_unit = NULL) {
  n <- reordered <- image <- NULL
  category <- enquo(category)
  if (!is.null(n_top)) {
    top <- pubs %>%
      count(!!category) %>%
      filter(row_number(desc(n)) <= n_top) %>%
      pull(!!category)
    pubs <- pubs %>%
      filter(!!category %in% top)
  }
  if (isotype) {
    .pkg_check("magick")
    .pkg_check("ggtextures")
    image_paths <- NULL
    if (quo_name(category) == "species") {
      img_df <- species_img %>%
        mutate(image_paths = map_chr(image_paths, system.file, package = "museumst"))
    } else if (quo_name(category) == "language") {
      img_df <- lang_img %>%
        mutate(image_paths = map_chr(image_paths, system.file, package = "museumst"))
    }
    img_df <- img_df %>%
      mutate(image_paths = map_chr(image_paths, normalizePath, mustWork = TRUE),
             image = map(image_paths, magick::image_read))
    pubs <- pubs %>%
      inner_join(img_df, by = as_name(category))
    if (is.null(img_unit)) {
      img_unit <- round(nrow(pubs)/20)
      message("img_unit not supplied. Using heuristic value ", img_unit)
    }
    pubs <- pubs %>%
      mutate(reordered = fct_infreq(!!category) %>% fct_rev())
    p <- ggplot(pubs, aes(reordered)) +
      ggtextures::geom_isotype_bar(aes(image = image),
                       img_width = grid::unit(img_unit, "native"),
                       img_height = NULL,
                       nrow = 1, ncol = NA,
                       hjust = 0, vjust = 0.5)
  } else {
    pubs <- pubs %>%
      mutate(reordered = fct_infreq(!!category) %>% fct_rev())
    p <- ggplot(pubs, aes(reordered)) + geom_bar()
  }
  p <- p +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       breaks = breaks_pretty()) +
    labs(y = "Number of publications", x = quo_name(category)) +
    coord_flip() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())
  p
}

#' Plot number of publications at each location
#'
#' Plots points on a map, and the areas of the points are proportional to the
#' number of publications at the location. Can facet by some category like
#' method or species.
#'
#' World map will use the Robinson projection. European map uses the LAEA Europe
#' projection (EPSG:3035), and the American map uses the US National Atlas Equal Area
#' projection (EPSG:2163) and Alaska and Hawaii are moved and included. The option
#' to zoom in on Europe and the US is available because those are the two regions
#' of the world with the most publications and it's hard to see the data when
#' plotted on a world map.
#'
#' @inheritParams pubs_per_year
#' @param city_gc From geocode_inst_city
#' @param zoom Whether to plot the world map or only Europe (centered on Western
#' Europe and some Eastern European countries are partially cropped off) or only
#' the US.
#' @param ncol Number of columns in facetted plot.
#' @param label_cities Logical, whether to label cities. In facetted plots, cities
#' are not labeled to reduce clutter.
#' @param n_label Number of top cities to label, so the labels won't clutter the
#' plot.
#' @param per_year Logical, whether to do the count for each year separately.
#' This is for making animations with gganimate.
#' @param plot Whether to plot points, rectangular bins (bin2d), or hexagonal
#' bins (hexbin). The binned options are useful when there's overplotting.
#' @param bins Numeric vector of length 2, the number of bins for bin2d or hex
#' in the x and y directions. Ignored if plotting points.
#' @return A ggplot2 object
#' @importFrom rlang !!!
#' @importFrom dplyr left_join count semi_join vars
#' @importFrom ggplot2 geom_sf scale_size_area scale_color_viridis_c coord_sf
#' @importFrom scales breaks_width
#' @importFrom ggrepel geom_label_repel
#' @export
pubs_on_map <- function(pubs, city_gc,
                        zoom = c("world", "europe", "usa"),
                        plot = c("point", "bin2d", "hexbin"),
                        facet_by = "none", n_top = Inf,
                        ncol = 3, label_cities = TRUE, n_label = 10,
                        per_year = FALSE, bins = c(70, 70)) {
  zoom <- match.arg(zoom)
  plot <- match.arg(plot)
  .pkg_check("sf")
  .pkg_check("rnaturalearth")
  .pkg_check("rnaturalearthdata")
  .pkg_check("rgeos")
  if (zoom == "usa") .pkg_check("urbnmapr")
  if (plot == "hexbin") {
    .pkg_check("hexbin")
  }
  if (per_year) {
    .pkg_check("gganimate")
    vars_count <- c("country", "state/province", "city", "year")
    label_cities <- FALSE
  } else {
    vars_count <- c("country", "state/province", "city")
  }
  if (facet_by == "none") {
    inst_count <- pubs %>%
      count(!!!syms(vars_count))
  } else {
    inst_count <- pubs %>%
      count(!!!syms(c(vars_count, facet_by)))
  }
  inst_count <- inst_count %>%
    left_join(city_gc, by = c("country", "state/province", "city"))
  country <- geometry <- NULL
  if (zoom == "world") {
    map_use <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")
    # use Robinson projection
    robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"
    map_use <- sf::st_transform(map_use, robin)
    inst_count <- inst_count %>%
      mutate(geometry = sf::st_transform(geometry, robin))
  } else if (zoom == "europe") {
    map_use <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    crs_europe <- 3035
    inst_count <- inst_count %>%
      filter(country %in% europe_countries) %>%
      mutate(geometry = sf::st_transform(geometry, crs = crs_europe))
    # project on European transformation
    map_use <- sf::st_transform(map_use, crs = crs_europe)
  } else if (zoom == "usa") {
    map_use <- usa_w_pop
    crs_usa <- 2163
    inst_count <- inst_count %>%
      filter(country %in% c("USA", "US", "United States", "United States of America")) %>%
      mutate(geometry = sf::st_transform(geometry, crs = crs_usa))
    # Temporary solution for Hawaii and Alaska
    if (any(inst_count$`state/province` %in% c("HI", "AK", "Hawaii", "Alaska"))) {
      state_labs <- urbnmapr::get_urbn_labels(sf = TRUE)
      inst_count$geometry[inst_count$`state/province` %in% c("HI", "Hawaii")] <-
        state_labs$geometry[state_labs$state_abbv == "HI"]
      inst_count$geometry[inst_count$`state/province` %in% c("AK", "Alaska")] <-
        state_labs$geometry[state_labs$state_abbv == "AK"]
    }
  }
  if (max(inst_count$n, na.rm = TRUE) < 4) {
    size_break_width <- 1
  } else {
    size_break_width <- ceiling((max(inst_count$n, na.rm = TRUE) -
                                   min(inst_count$n, na.rm = TRUE))/4)
  }
  n <- NULL
  if (plot == "point") {
    p <- ggplot() +
      geom_sf(data = map_use) +
      scale_size_area(name = "Number of\npublications",
                      breaks = breaks_width(size_break_width)) +
      theme(panel.border = element_blank(), axis.title = element_blank())
    city2 <- city <- NULL
    if (facet_by == "none") {
      if (per_year) {
        p <- p +
          geom_sf(data = inst_count, aes(geometry = geometry, size = n, color = n,
                                         group = city2),
                  alpha = 0.7, show.legend = "point")
      } else {
        p <- p +
          geom_sf(data = inst_count, aes(geometry = geometry, size = n, color = n),
                  alpha = 0.7, show.legend = "point")
      }
      p <- p +
        scale_color_viridis_c(name = "", breaks_width(size_break_width))
    } else {
      inst_count <- inst_count %>%
        mutate(facets = fct_lump_n(!!sym(facet_by), n = n_top))
      if (per_year) {
        p <- p +
          geom_sf(data = inst_count, aes(geometry = geometry, size = n,
                                         group = city2,
                                         color = facets),
                  alpha = 0.7, show.legend = "point")
      } else {
        p <- p +
          geom_sf(data = inst_count, aes(geometry = geometry, size = n,
                                         color = facets),
                  alpha = 0.7, show.legend = "point")
      }
      p <- p +
        facet_wrap(vars(facets), ncol = ncol) +
        theme(legend.position = "none")
    }
    if (zoom == "europe") {
      # Limit to that box
      p <- p +
        coord_sf(xlim = xylims[c("xmin", "xmax")], ylim = xylims[c("ymin", "ymax")],
                 crs = crs_europe)
    }
    if (per_year) {
      year <- NULL
      p <- p +
        gganimate::transition_states(year, state_length = 5, transition_length = 1) +
        labs(title = "{closest_state}") +
        gganimate::enter_fade() +
        gganimate::exit_fade()
    }
  } else {
    inst_count2 <- uncount(inst_count, n)
    coords <- sf::st_coordinates(inst_count2$geometry)
    colnames(coords) <- c("lon", "lat")
    coords <- as_tibble(coords)
    inst_count2 <- cbind(inst_count2, coords)
    if (facet_by != "none") {
      inst_count <- inst_count %>%
        mutate(facets = fct_lump_n(!!sym(facet_by), n = n_top))
    }
    p <- ggplot() +
      geom_sf(data = map_use) +
      scale_fill_distiller(palette = "Blues", direction = 1) +
      theme(panel.border = element_blank(), axis.title = element_blank())
    if (zoom == "europe") {
      # Limit to that box
      p <- p +
        coord_sf(xlim = xylims[c("xmin", "xmax")], ylim = xylims[c("ymin", "ymax")],
                 crs = crs_europe)
    }
    if (plot == "hexbin") {
      p <- p +
        geom_hex(data = inst_count2, aes(lon, lat), bins = bins)
    } else if (plot == "bin2d") {
      p <- p +
        geom_bin2d(data = inst_count2, aes(lon, lat), bins = bins)
    }
    if (facet_by != "none") {
      p <- p +
        facet_wrap(vars(facets), ncol = ncol)
    }
  }
  if (label_cities && !per_year) {
    inst_count <- inst_count %>%
      mutate(city_rank = row_number(desc(n)),
             city_label = case_when(city_rank <= n_label ~ city,
                                    TRUE ~ ""))
    p <- p +
      geom_label_repel(data = inst_count, aes(geometry = geometry, label = city_label),
                       alpha = 0.7, stat = "sf_coordinates")
  }
  p
}

#' Plot per capita data as choropleth or bar plot
#'
#' For the entire world, Europe (for European countries tend to be smaller) and
#' states within the US.
#'
#' @param pubs A data frame with one row per publication and columns country and
#' for the US, also a column "state/province".
#' @param zoom Whether to plot the world map or only Europe (centered on Western
#' Europe and some Eastern European countries are partially cropped off) or only
#' the US.
#' @param plot Whether to plot choropleth or bar plot.
#' @param label_states If plotting the US, whether to label the states.
#' @return A ggplot2 object.
#' @importFrom ggplot2 scale_fill_distiller geom_col geom_sf_text
#' @importFrom stringr str_length
#' @export
pubs_per_capita <- function(pubs, zoom = c("world", "europe", "usa"),
                            plot = c("choropleth", "bar"),
                            label_states = TRUE) {
  .pkg_check("sf")
  .pkg_check("rnaturalearth")
  .pkg_check("rnaturalearthdata")
  .pkg_check("rgeos")
  zoom <- match.arg(zoom)
  plot <- match.arg(plot)
  if (zoom != "usa") {
    if (zoom == "world") {
      map_use <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")
      if (plot == "choropleth") {
        robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"
        map_use <- sf::st_transform(map_use, robin)
      }
    } else {
      map_use <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
      if (plot == "choropleth") {
        map_use <- sf::st_transform(map_use, 3035)
      }
    }
    country <- per_capita <- pop_est <- country_full <- n <- `state/province` <-
      `2019` <- state_name <- area <- NULL
    pubs_count <- pubs %>%
      mutate(country_full = case_when(country == "USA" ~ "United States",
                                      country == "UK" ~ "United Kingdom",
                                      TRUE ~ country)) %>%
      count(country_full, country)
    if (zoom == "europe") {
      pubs_count <- pubs_count %>%
        filter(country %in% europe_countries)
    }
    map_use <- map_use %>%
      left_join(pubs_count, by = c("name" = "country_full")) %>%
      mutate(per_capita = n/pop_est)
    if (plot == "bar") {
      map_use <- map_use %>%
        filter(!is.na(per_capita)) %>%
        mutate(area = fct_reorder(country, per_capita))
    }
  } else {
    map_use <- usa_w_pop
    pubs_count <- pubs %>%
      filter(country %in% c("USA", "US", "United States", "United States of America")) %>%
      count(`state/province`)
    # Convert state names to abbreviations
    pubs_count <- pubs_count %>%
      mutate(`state/province` = case_when(
        `state/province` %in% map_use$state_abbv ~ `state/province`,
        TRUE ~ map_use$state_abbv[match(`state/province`, map_use$state_name)]
      ))
    map_use <- map_use %>%
      left_join(pubs_count,
                by = c("state_abbv" = "state/province")) %>%
      mutate(per_capita = n/`2019`)
    if (plot == "bar") {
      map_use <- map_use %>%
        filter(!is.na(per_capita)) %>%
        mutate(area = fct_reorder(state_name, per_capita))
    }
  }
  if (plot == "choropleth") {
    p <- ggplot(map_use) +
      geom_sf(aes(fill = log10(per_capita))) +
      scale_fill_distiller(palette = "Blues", na.value = "white", direction = 1,
                           name = "# pub.\nper capita\n(log10)") +
      theme(panel.border = element_blank(), axis.title = element_blank())
    if (zoom == "europe") {
      p <- p +
        coord_sf(xlim = xylims[c("xmin", "xmax")], ylim = xylims[c("ymin", "ymax")],
                 crs = 3035)
    }
    if (zoom == "usa" && label_states) {
      # Label the states
      state_labels <- urbnmapr::get_urbn_labels("states", sf = TRUE)
      p <- p +
        geom_sf_text(data = state_labels, aes(geometry = geometry, label = state_abbv))
    }
  } else {
    area_lab <- if (zoom == "usa") "state" else "country"
    if (zoom == "europe") {
      map_use <- map_use %>%
        filter(country %in% europe_countries)
    }
    p <- ggplot(map_use, aes(per_capita, area)) +
      geom_col() +
      scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
      labs(y = area_lab)
  }
  p
}

#' Plot heatmap to show relationship between two categorical variables
#'
#' For instance, are papers for certain techniques more likely to be in certain
#' journals? Is there an association between species and journal? This is just
#' for visualization. Use `fisher.test` to see if it's significant. I still wonder
#' if I should rewrite this with ggplot2, which is more work than base R in this
#' case.
#'
#' @inheritParams pubs_per_year
#' @param row_var Variable for rows of the heatmap. Tidyeval is supported.
#' @param col_var Variable for columns of the heatmap.
#' @param ... Extra arguments to pass to `heatmap`
#' @return A base R heatmap is plotted to the current device.
#' @importFrom dplyr pull
#' @importFrom tidyr pivot_wider
#' @importFrom stats heatmap
#' @export
cat_heatmap <- function(pubs, row_var, col_var, ...) {
  rv <- enquo(row_var)
  cv <- enquo(col_var)
  mat1 <- pubs %>%
    count(!!rv, !!cv) %>%
    pivot_wider(names_from = !!cv, values_from = "n")
  method_mat <- as.matrix(mat1[,-1])
  rownames(method_mat) <- pull(mat1, !!rv)
  method_mat[is.na(method_mat)] <- 0
  heatmap(method_mat, ...)
}

#' Plot histogram for each value of a logical variable
#'
#' Plots 3 histograms showing the number of publications per year for TRUE, FALSE,
#' and NA, with the histogram overlaid on top of a translucent one for all
#' values. There's one facet per row so it's easy to compare how things change
#' with time.
#'
#' @inheritParams pubs_per_year
#' @param col_use Which logical variable to plot. Tidyeval is supported.
#' @return A ggplot2 object
#' @importFrom ggplot2 geom_histogram facet_grid scale_fill_brewer
#' @export
hist_bool <- function(pubs, col_use, binwidth = 365, preprints = FALSE) {
  date_published <- journal <- v <- NULL
  col_use <- enquo(col_use)
  if (!preprints) {
    pubs <- pubs %>%
      filter(!journal %in% c("bioRxiv", "arXiv"))
  }
  pubs <- pubs %>%
    mutate(v = !!col_use)
  ggplot(pubs, aes(date_published)) +
    geom_histogram(aes(fill = 'all'), alpha = 0.7, fill = "gray70",
                   data = select(pubs, -v), binwidth = binwidth) +
    geom_histogram(aes(fill = v), binwidth = binwidth) +
    facet_grid(rows = vars(v)) +
    scale_y_continuous(breaks = breaks_pretty(), expand = expansion(c(0, 0.05))) +
    scale_x_date(breaks = breaks_pretty(10)) +
    scale_fill_brewer(palette = "Set1", na.value = "gray50") +
    theme(panel.grid.minor = element_blank(), legend.position = "none") +
    labs(x = "date published")
}

#' Plot outlines of histogram for a logical variable
#'
#' Kind of like `hist_bool`, but instead of plotting TRUE, FALSE, and NA in 3
#' separate facets, it plots them as an outline overlaid on a translucent
#' histogram for all values. This is useful when facetting with another categorical
#' variable, such as programming language.
#'
#' @inheritParams hist_bool
#' @inheritParams pubs_per_year
#' @inheritParams pubs_on_map
#' @importFrom tidyr complete
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom rlang quo_name
#' @export
hist_bool_line <- function(pubs, col_use, facet_by = NULL, ncol = 3, n_top = Inf,
                           binwidth = 365, preprints = FALSE) {
  date_published <- journal <- v <- NULL
  col_use <- enquo(col_use)
  if (!preprints) {
    pubs <- pubs %>%
      filter(!journal %in% c("bioRxiv", "arXiv"))
  }
  pubs <- pubs %>%
    mutate(v = !!col_use)
  if (!is.null(facet_by)) {
    pubs <- pubs %>%
      mutate(facets = fct_lump_n(!!sym(facet_by), n = n_top))
  }
  p <- ggplot(pubs, aes(date_published)) +
    geom_histogram(aes(fill = 'all'), alpha = 0.7, fill = "gray90",
                   data = select(pubs, -v), binwidth = binwidth) +
    geom_freqpoly(aes(color = v), binwidth = binwidth) +
    scale_y_continuous(breaks = breaks_pretty(), expand = expansion(c(0, 0.05))) +
    scale_x_date(breaks = breaks_pretty(10)) +
    scale_color_brewer(name = quo_name(col_use), palette = "Set1", na.value = "gray50") +
    theme(panel.grid.minor = element_blank(), legend.position = "top") +
    labs(y = "count", x = "date published")
  if (!is.null(facet_by)) {
    p <- p + facet_wrap(vars(!!sym(facet_by)), ncol = ncol)
  }
  p
}

#' Test whether something is associated with time
#'
#' Fits a logistic regression model with glm to use year to predict proportion
#' of a logical variable is TRUE, and tests whether beta is 0.
#'
#' @inheritParams hist_bool
#' @return A glm object is returned invisibly. The summary is printed to screen
#' @importFrom dplyr group_by summarize
#' @importFrom stats glm
#' @export
test_year_bool <- function(pubs, col_use, preprints = FALSE) {
  journal <- NULL
  col_use <- enquo(col_use)
  if (!preprints) {
    pubs <- pubs %>%
      filter(!journal %in% c("bioRxiv", "arXiv"))
  }
  pubs <- pubs %>%
    mutate(bool_use = !!col_use)
  out <- glm(bool_use ~ date_published, data = pubs, family = "binomial")
  print(summary(out))
  invisible(out)
}

#' Prequel vs current binned over time
#'
#' Plots freqpoly for prequel vs current, with an option to start both at the
#' date when the first publication in the category appeared. The point here is
#' not to compare to the distribution of everything, like in hist_bool, or to
#' compare when things started, like when I plotted a histogram of different
#' methods over time, but to compare the height of the histograms and how steeply
#' they rise and fall. So I think freqpoly may be better than blocky histograms
#' for this purposes.
#'
#' @inheritParams pubs_per_year
#' @inheritParams hist_bool
#' @param since_first Logical. Whether to plot days after the first publication
#' appeared.
#' @return A ggplot2 object
#' @importFrom ggplot2 scale_color_brewer geom_freqpoly scale_fill_discrete
#' @export
era_freqpoly <- function(pubs, col_use, since_first = FALSE, binwidth = 365,
                         preprints = FALSE) {
  journal <- date_published <- days_since_first <- NULL
  col_use <- enquo(col_use)
  if (!preprints) {
    pubs <- pubs %>%
      filter(!journal %in% c("bioRxiv", "arXiv"))
  }
  if (since_first) {
    days_from_start <- pubs %>%
      group_by(!!col_use) %>%
      mutate(days_since_first = as.numeric(date_published - min(date_published)))
    p <- ggplot(days_from_start, aes(days_since_first)) +
      labs(x = "Days since the first publication")
  } else {
    p <- ggplot(pubs, aes(date_published)) +
      labs(x = "Date published")
  }
  p <- p +
    geom_freqpoly(binwidth = binwidth, aes(color = !!col_use)) +
    scale_color_brewer(name = quo_name(col_use), palette = "Set1", na.value = "gray50") +
    labs(y = "Number of publications")
  p
}
