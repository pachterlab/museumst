#' Plot word cloud for a column
#'
#' Plots a word cloud for columns in the metadata that don't have a controlled
#' vocabulary. I guess maybe I should start using a controlled vocabulary.
#'
#' @inheritParams wordcloud::wordcloud
#' @param sheet The sheet read in from Google Sheets as a tibble.
#' @param col_use Which column to break down into words for word cloud. Tidyeval
#' is supported.
#' @param species_use Which species to filter by.
#' @param year_min Minimum year. The range of years will be >= year_min and
#' < year_max.
#' @param year_max Maximum year.
#' @param other_stop_words A character vector for other stop words besides the
#' stop_words data frame that comes with tidytext.
#' @param seed Random seed to use.
#' @param ... Other arguments passed to \code{\link{wordcloud}}.
#' @return A html widget.
#' @importFrom dplyr distinct anti_join
#' @importFrom tidytext unnest_tokens
#' @importFrom wordcloud wordcloud
#' @importFrom scales brewer_pal
#' @export
plot_wordcloud <- function(sheet, col_use = title, species_use = "all",
                           year_min = NA, year_max = NA,
                           other_stop_words = NULL, seed = 1,
                           scale = c(6, 0.1), min.freq = 1, ...) {
  title <- year <- species <- word <- NULL
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
  set.seed(seed)
  wordcloud(words = df$word, freq = df$freq, scale = scale, min.freq = min.freq,
            colors = brewer_pal(palette = "Dark2")(8),
            random.color = TRUE, ...)
}

#' Compare word frequencies between current era and prequel
#'
#' @inheritParams plot_wordcloud
#' @inheritParams pubs_per_cat
#' @param era Which column indicates whether the item is from prequel or current era.
#' Can be character, factor, or logical. Tidyeval is supported.
#' @return A ggplot2 object.
#' @importFrom dplyr ungroup
#' @importFrom ggplot2 geom_abline coord_equal
#' @importFrom ggrepel geom_text_repel
#' @export
word_prop_scatter <- function(pubs, col_use = title, era = sheet, n_top = 20,
                              other_stop_words = NULL) {
  date_published <- word <- n <- title <- sheet <- prop <- label <- NULL
  col_use <- enquo(col_use)
  era <- enquo(era)
  eras <- pubs %>% pull(!!era) %>% unique()
  if (length(eras) != 2L) stop("Column ", quo_name(era), " should only have 2 unique values.")
  eras <- sort(as.character(eras))
  freqs <- pubs %>%
    filter(!is.na(!!col_use)) %>%
    select(date_published, !!col_use, !!era) %>%
    unnest_tokens("word", input = !!col_use) %>%
    anti_join(tidytext::stop_words, by = "word") %>%
    count(!!era, word) %>%
    # Calculate the proportion
    group_by(!!era) %>%
    mutate(prop = n/sum(n)) %>%
    ungroup() %>%
    select(word, !!era, prop) %>%
    pivot_wider(names_from = quo_name(era), values_from = "prop", values_fill = list(prop = 0)) %>%
    mutate(dif = !!sym(eras[1]) - !!sym(eras[2]),
           label = case_when(row_number(desc(dif)) <= n_top/2 ~ word,
                             row_number(dif) <= n_top/2 ~ word,
                             TRUE ~ ""))
  if (!is.null(other_stop_words)) {
    freqs <- freqs %>%
      filter(!word %in% other_stop_words)
  }
  ggplot(freqs, aes(!!sym(eras[1]), !!sym(eras[2]))) +
    geom_point(alpha = 0.3) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    geom_text_repel(aes(label = label)) +
    coord_equal()
}
