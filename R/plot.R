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
#' @importFrom ggtext geom_richtext
#' @export
plot_timeline <- function(events_df, ys, width = 100, description_width = 20,
                          expand_x = c(0.1, 0.1), expand_y = c(0.05, 0.05)) {
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
    geom_richtext(aes(x = date_published, y = ys, label = lab, vjust = vjusts),
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
#' @return A ggplot2 object.
#' @importFrom dplyr filter select
#' @importFrom forcats fct_reorder
#' @importFrom rlang !! sym
#' @importFrom ggplot2 geom_bar scale_x_continuous labs theme facet_wrap
#' @importFrom scales breaks_pretty
#' @export
pubs_per_year <- function(pubs, facet_by = NULL, binwidth = 365) {
  pubs <- pubs %>%
    filter(!journal %in% c("bioRxiv", "arXiv"))
  if (!is.null(facet_by)) {
    pubs <- pubs %>%
      mutate(facets = fct_reorder(!!sym(facet_by), date_published, .fun = "min"))
  }
  p <- ggplot(pubs, aes(date_published))
  if (!is.null(facet_by)) {
    p <- p +
      geom_histogram(aes(fill = "all"), binwidth = binwidth,
                     data = select(pubs, -facets),
                     fill = "gray70", alpha = 0.5)
  }
  p <- p +
    geom_histogram(binwidth = binwidth) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       breaks = breaks_pretty()) +
    scale_x_date(breaks = breaks_pretty(10)) +
    labs(y = "Number of publication") +
    theme(panel.grid.minor = element_blank(), legend.position = "none")
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
#' or language, then
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
#' @importFrom ggtextures geom_isotype_bar
#' @importFrom grid unit
#' @importFrom ggplot2 coord_flip theme element_blank
#' @importFrom magick image_read
#' @importFrom purrr map_chr map
#' @importFrom dplyr row_number desc inner_join
#' @export
pubs_per_cat <- function(pubs, category, n_top = NULL, isotype = FALSE, img_df = NULL,
                         img_unit = NULL) {
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
    if (as_name(category) == "species") {
      img_df <- species_img %>%
        mutate(image_paths = map_chr(image_paths, system.file, package = "museumst"))
    } else if (as_name(category) == "language") {
      img_df <- lang_img %>%
        mutate(image_paths = system.file(image_paths, package = "museumst"))
    }
    img_df <- img_df %>%
      mutate(image_paths = map_chr(image_paths, normalizePath, mustWork = TRUE),
             image = map(image_paths, image_read))
    pubs <- pubs %>%
      inner_join(img_df, by = as_name(category))
    if (is.null(img_unit)) {
      img_unit <- round(nrow(pubs)/20)
      message("img_unit not supplied. Using heuristic value ", img_unit)
    }
    pubs <- pubs %>%
      mutate(reordered = fct_infreq(!!category) %>% fct_rev())
    p <- ggplot(pubs, aes(reordered)) +
      geom_isotype_bar(aes(image = image),
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
    labs(y = "Number of publications", x = as_name(category)) +
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
#' @param inst_gc From geocode_inst_city
#' @param city_gc From geocode_inst_city
#' @param zoom Whether to plot the world map or only Europe (centered on Western
#' Europe and some Eastern European countries are partially cropped off) or only
#' the US.
#' @param ncol Number of columns in facetted plot.
#' @param label_cities Logical, whether to label cities. In facetted plots, cities
#' are not labeled to reduce clutter.
#' @param per_year Logical, whether to do the count for each year separately.
#' This is for making animations with gganimate.
#' @return A ggplot2 object
#' @importFrom rlang !!!
#' @importFrom dplyr left_join count semi_join vars
#' @importFrom rnaturalearth ne_countries
#' @importFrom ggplot2 geom_sf scale_size_area scale_color_viridis_c coord_sf
#' @importFrom gganimate transition_states enter_fade exit_fade
#' @importFrom scales breaks_width
#' @importFrom ggrepel geom_label_repel
#' @export
pubs_on_map <- function(pubs, inst_gc, city_gc,
                        zoom = c("world", "europe", "usa"),
                        facet_by = "none",
                        ncol = 3, label_cities = TRUE,
                        per_year = FALSE) {
  zoom <- match.arg(zoom)
  if (per_year) {
    vars_count <- c("country", "city", "institution", "year")
    label_cities <- FALSE
  } else {
    vars_count <- c("country", "city", "institution")
  }
  if (facet_by == "none") {
    inst_count <- pubs %>%
      count(!!!syms(vars_count))
  } else {
    inst_count <- pubs %>%
      count(!!!syms(c(vars_count, facet_by)))
  }
  inst_count <- inst_count %>%
    left_join(inst_gc, by = c("country", "city", "institution"))
  city_gc <- city_gc %>%
    semi_join(inst_count, by = c("country", "city"))
  if (zoom == "world") {
    map_use <- ne_countries(scale = "small", returnclass = "sf")
    # use Robinson projection
    map_use <- st_transform(map_use, 54030)
    inst_count <- inst_count %>%
      mutate(geometry = st_transform(geometry, 54030))
  } else if (zoom == "europe") {
    map_use <- ne_countries(scale = "medium", returnclass = "sf")
    crs_europe <- 3035
    inst_count <- inst_count %>%
      filter(country %in% europe_countries) %>%
      mutate(geometry = st_transform(geometry, crs = crs_europe))
    city_gc <- city_gc %>%
      filter(country %in% europe_countries) %>%
      mutate(geometry = st_transform(geometry, crs = crs_europe))
    # project on European transformation
    map_use <- st_transform(map_use, crs = crs_europe)
  } else if (zoom == "usa") {
    map_use <- usa_w_pop
    crs_usa <- 2163
    inst_count <- inst_count %>%
      filter(country == "USA") %>%
      mutate(geometry = st_transform(geometry, crs = crs_usa))
    city_gc <- city_gc %>%
      filter(country == "USA") %>%
      mutate(geometry = st_transform(geometry, crs = crs_usa))
  }
  if (max(inst_count$n, na.rm = TRUE) < 4) {
    size_break_width <- 1
  } else {
    size_break_width <- ceiling((max(inst_count$n, na.rm = TRUE) -
                                   min(inst_count$n, na.rm = TRUE))/4)
  }
  p <- ggplot() +
    geom_sf(data = map_use) +
    scale_size_area(name = "Number of\npublications",
                    breaks = breaks_width(size_break_width)) +
    theme(panel.border = element_blank(), axis.title = element_blank())
  if (facet_by == "none") {
    if (per_year) {
      p <- p +
        geom_sf(data = inst_count, aes(geometry = geometry, size = n, color = n,
                                       group = institution2),
                alpha = 0.7, show.legend = "point")
    } else {
      p <- p +
        geom_sf(data = inst_count, aes(geometry = geometry, size = n, color = n),
                alpha = 0.7, show.legend = "point")
    }
    p <- p +
      scale_color_viridis_c(name = "", breaks_width(size_break_width))
    if (zoom != "world" && label_cities) {
      p <- p +
        geom_label_repel(data = city_gc, aes(geometry = geometry, label = city),
                         alpha = 0.7, stat = "sf_coordinates")
    }
  } else {
    if (per_year) {
      p <- p +
        geom_sf(data = inst_count, aes(geometry = geometry, size = n,
                                        group = institution2,
                                        color = !!sym(facet_by)),
                alpha = 0.7, show.legend = "point")
    } else {
      p <- p +
        geom_sf(data = inst_count, aes(geometry = geometry, size = n,
                    color = !!sym(facet_by)),
                alpha = 0.7, show.legend = "point")
    }
    p <- p +
      facet_wrap(vars(!!sym(facet_by)), ncol = ncol) +
      theme(legend.position = "none")
  }
  if (zoom == "europe") {
    # Limit to that box
    p <- p +
      coord_sf(xlim = xylims[c("xmin", "xmax")], ylim = xylims[c("ymin", "ymax")],
               crs = crs_europe)
  }
  if (per_year) {
    p <- p +
      transition_states(year, state_length = 5, transition_length = 1) +
      labs(title = "{closest_state}") +
      enter_fade() +
      exit_fade()
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
#' @return A ggplot2 object.
#' @importFrom ggplot2 scale_fill_distiller
#' @export
pubs_per_capita <- function(pubs, zoom = c("world", "europe", "usa"),
                            plot = c("choropleth", "bar")) {
  zoom <- match.arg(zoom)
  plot <- match.arg(plot)
  if (zoom != "usa") {
    if (zoom == "world") {
      map_use <- ne_countries(scale = "small", returnclass = "sf")
      if (plot == "choropleth") {
        map_use <- st_transform(map_use, 54030)
      }
    } else {
      map_use <- ne_countries(scale = "medium", returnclass = "sf")
      if (plot == "choropleth") {
        map_use <- st_transform(map_use, 3035)
      }
    }
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
      filter(country == "USA") %>%
      count(`state/province`)
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

#' Plot word cloud for a column
#'
#' Plots a word cloud for columns in the metadata that don't have a controlled
#' vocabulary. I guess maybe I should start using a controlled vocabulary.
#'
#' @param sheet The sheet read in from Google Sheets as a tibble.
#' @param col_use Which column to break down into words for word cloud. Tidyeval
#' is supported.
#' @param species_use Which species to filter by.
#' @param year_min Minimum year. The range of years will be >= year_min and
#' < year_max.
#' @param year_max Maximum year.
#' @param other_stop_words A character vector for other stop words besides the
#' stop_words data frame that comes with tidytext.
#' @return A html widget.
#' @importFrom dplyr distinct anti_join
#' @importFrom tidytext unnest_tokens
#' @importFrom wordcloud2 wordcloud2
#' @export
plot_wordcloud <- function(sheet, col_use = title, species_use = "all",
                           year_min = NA, year_max = NA,
                           other_stop_words = NULL) {
  col_use <- enquo(col_use)
  df <- sheet %>%
    select(title, year, species, !!col_use) %>%
    filter(!is.na(!!col_use))
  if (species_use != "all") {
    df <- df %>%
      filter(species == species_use)
  }
  if (!is.na(year_min) && !is.na(year_max)) {
    df <- df %>%
      filter(year >= year_min, year < year_max)
  }
  df <- df %>%
    select(-species, -year) %>%
    distinct() %>%
    unnest_tokens(output = "word", input = !!col_use) %>%
    anti_join(tidytext::stop_words, by = "word") %>%
    count(word, name = "freq")
  if (!is.null(other_stop_words)) {
    df <- df %>%
      filter(!word %in% other_stop_words)
  }
  wordcloud2(df, minRotation = -pi/2, maxRotation = -pi/2, shuffle = FALSE)
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
#' @importFrom ggplot2 geom_histogram facet_grid
#' @export
hist_bool <- function(pubs, col_use, binwidth = 365) {
  col_use <- enquo(col_use)
  pubs <- pubs %>%
    filter(!journal %in% c("bioRxiv", "arXiv")) %>%
    mutate(v = !!col_use)
  ggplot(pubs, aes(date_published)) +
    geom_histogram(aes(fill = 'all'), alpha = 0.7, fill = "gray70",
                   data = select(pubs, -v), binwidth = binwidth) +
    geom_histogram(aes(fill = v), binwidth = binwidth) +
    facet_grid(rows = vars(v)) +
    scale_y_continuous(breaks = breaks_pretty(), expand = expansion(c(0, 0.05))) +
    scale_x_date(breaks = breaks_pretty(10)) +
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
#' @importFrom tidyr complete
#' @importFrom ggplot2 scale_x_continuous
#' @export
hist_bool_line <- function(pubs, col_use, facet_by = NULL, ncol = 3, binwidth = 365) {
  col_use <- enquo(col_use)
  pubs <- pubs %>%
    filter(!journal %in% c("bioRxiv", "arXiv")) %>%
    mutate(v = !!col_use)
  p <- ggplot(pubs, aes(date_published)) +
    geom_histogram(aes(fill = 'all'), alpha = 0.7, fill = "gray90",
                   data = select(pubs, -v), binwidth = binwidth) +
    geom_freqpoly(aes(color = v), binwidth = binwidth) +
    scale_y_continuous(breaks = breaks_pretty(), expand = expansion(c(0, 0.05))) +
    scale_x_date(breaks = breaks_pretty(10)) +
    scale_color_discrete(name = as_name(col_use)) +
    theme(panel.grid.minor = element_blank(), legend.position = "top") +
    labs(y = "count")
  if (!is.null(facet_by)) {
    p <- p + facet_wrap(vars(!!sym(facet_by)), ncol = ncol)
  }
  p
}

#' Test whether something is associated with time
#'
#' Fits a linear model with lm to use year to predict proportion of a logical
#' variable is TRUE, and tests whether beta is 0.
#'
#' @inheritParams hist_bool
#' @return A lm object is returned invisibly. The summary is printed to screen
#' @importFrom dplyr group_by summarize
#' @export
test_year_bool <- function(pubs, col_use) {
  col_use <- enquo(col_use)
  df <- pubs %>%
    filter(!journal %in% c("bioRxiv", "arXiv")) %>%
    mutate(bool_use = !!col_use)
  out <- lm(bool_use ~ date_published, data = df)
  print(summary(out))
  invisible(out)
}
