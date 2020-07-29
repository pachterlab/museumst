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
#' @param ... Other arguments passed to \code{wordcloud::wordcloud}.
#' @return A html widget.
#' @importFrom dplyr distinct anti_join
#' @importFrom scales brewer_pal
#' @export
plot_wordcloud <- function(sheet, col_use = title, species_use = "all",
                           year_min = NA, year_max = NA,
                           other_stop_words = NULL, seed = 1,
                           scale = c(6, 0.1), min.freq = 1, ...) {
  .pkg_check("tidytext")
  .pkg_check("wordcloud")
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
    tidytext::unnest_tokens(output = "word", input = !!col_use) %>%
    anti_join(tidytext::stop_words, by = "word") %>%
    count(word, name = "freq")
  if (!is.null(other_stop_words)) {
    df <- df %>%
      filter(!word %in% other_stop_words)
  }
  set.seed(seed)
  wordcloud::wordcloud(words = df$word, freq = df$freq, scale = scale,
                       min.freq = min.freq,
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
  .pkg_check("tidytext")
  date_published <- word <- n <- title <- sheet <- prop <- label <- NULL
  col_use <- enquo(col_use)
  era <- enquo(era)
  eras <- pubs %>% pull(!!era) %>% unique()
  if (length(eras) != 2L) stop("Column ", quo_name(era), " should only have 2 unique values.")
  eras <- sort(as.character(eras))
  freqs <- pubs %>%
    filter(!is.na(!!col_use)) %>%
    select(date_published, !!col_use, !!era) %>%
    tidytext::unnest_tokens("word", input = !!col_use) %>%
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

#' Find the appropriate number of topics for stm
#'
#' This function is just like stm's searchK, but it uses tidyverse and allows a
#' formula for prevalence. Adapted from Julia Silge's blog.
#'
#' @param documents Quanteda dfm object.
#' @param K Numeric vector of numbers of topics to try.
#' @param prevalence Formula of topic prevalence.
#' @param seed Random seed to use.
#' @param ... Other arguments to pass to \code{stm::stm}.
#' @return A tibble with the metrics output by searchK, plus a column for the
#' fitted model for each value of K tried.
#' @export

my_searchK <- function(documents, K, prevalence = ~ 1, seed = 19, ...) {
  .pkg_check("quanteda")
  .pkg_check("stm")
  heldout <- stm::make.heldout(documents, seed = seed)
  dc <- heldout$documents
  vocab <- heldout$vocab
  meta <- quanteda::docvars(documents)
  fun <- function(.x, seed, prevalence, ...) {
    stm::stm(dc, vocab = vocab, K = .x, seed = seed,
             prevalence = prevalence, data = meta, ...)
  }
  many_models <- tibble(K = sample(K, length(K), replace = FALSE)) %>%
    mutate(topic_model = furrr::future_map(K, fun, seed = seed, prevalence = prevalence, ...))
  k_result <- many_models %>%
    mutate(exclusivity = map(topic_model, stm::exclusivity),
           semantic_coherence = map(topic_model, stm::semanticCoherence, dc),
           eval_heldout = map(topic_model, stm::eval.heldout, heldout$missing),
           residual = map(topic_model, stm::checkResiduals, dc),
           bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
           lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
           lbound = bound + lfact,
           iterations = map_dbl(topic_model, function(x) length(x$convergence$bound))) %>%
    arrange(K)
  k_result
}

#' Plot the metrics for each K tried
#'
#' Adapted from Julia Silge's blog.
#'
#' @param k_result output of my_searchK
#' @return A ggplot object.
#' @export

plot_k_result <- function(k_result) {
  k_result %>%
    transmute(K,
              `Lower bound` = lbound,
              Residuals = map_dbl(residual, "dispersion"),
              `Semantic coherence` = map_dbl(semantic_coherence, mean),
              `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
    gather(Metric, Value, -K) %>%
    ggplot(aes(K, Value, color = Metric)) +
    geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
    facet_wrap(~Metric, scales = "free_y") +
    labs(x = "K (number of topics)",
         y = NULL,
         title = "Model diagnostics by number of topics")
}

#' Plot exclusivity vs. semantic coherence for different K's
#'
#' Adapted from Julia Silge's blog.
#'
#' @inheritParams plot_k_result
#' @param Ks Numeric vector of number of topics to plot.
#' @return A ggplot object
#' @export
plot_ec <- function(k_result, Ks) {
  k_result %>%
    select(K, exclusivity, semantic_coherence) %>%
    filter(K %in% Ks) %>%
    unnest(cols = c("exclusivity", "semantic_coherence")) %>%
    mutate(K = as.factor(K)) %>%
    ggplot(aes(semantic_coherence, exclusivity, color = K, shape = K)) +
    geom_point(size = 2, alpha = 0.7) +
    labs(x = "Semantic coherence",
         y = "Exclusivity",
         title = "Comparing exclusivity and semantic coherence",
         subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")
}
