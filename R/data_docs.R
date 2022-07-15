#' List of institution words
#'
#' @format A character vector
#' @source Some words I often find in department names, and in addition, country
#' and city names.
"inst_words"

#' Color palette for species
#'
#' To give each common species a fixed color for a unified look among figures.
#'
#' @format A named vector of colors; the names are the species, including "Other".
#' @source dittoSeq
"species_cols"

#' Color palette for sheets
#'
#' To give each sheet, i.e. each category of technologies, a fixed color for a
#' unified look among figures.
#'
#' @format A named vector of colors; the names are the sheets for data collection
#' @source Brewer
"sheet_fill"

#' LCM abstracts from PubMed and bioRxiv search
#'
#' Abstracts of LCM literautre from PubMed API in a data frame, last updated on
#' 2021-03-27. See `data-raw/lcm_abstracts.Rmd`, `data-raw/update_lcm.Rmd`, and
#' `data-raw/lcm_biorxiv.Rmd` in the GitHub repo of this package.
#'
#' @format Data frame
#' @source The search term was "((laser capture microdissection) OR (laser
#'   microdissection)) AND ((microarray) OR (transcriptome) OR (RNA-seq))" on
#'   PubMed, and "laser microdissection" on bioRxiv.
"lcm_abstracts"

#' City geocodes of LCM literature
#'
#' Longitude and latitude of institutions of first authors of LCM literature as
#' of publication, for plotting on maps. See `data-raw/lcm_abstracts.Rmd`,
#' `data-raw/update_lcm.Rmd`, and `data-raw/lcm_biorxiv.Rmd` in the GitHub repo
#' of this package.
#'
#' @format sf data frame with longitude and latitude in the geometry column.
#' @source Address was pulled from the PubMed or bioRxiv API, and geocoded with
#'   the Google geocoding API with my own API key.
"lcm_city_gc"

#' Document term matrix of LCM abstracts
#'
#' Each term is a unigram, i.e. a word, except for common phrases. Stop words and
#' rare words were removed.
#'
#' @format A `quanteda` `dfm` object
#' @source See the `lcm_text_mining.Rmd` file in the `data-raw` in the [GitHub
#' repo of this package](https://www.github.com/pachterlab/museumst) for how
#' this matrix was generated, including what the phrases and stopwords are and
#' what counts as a rare term.
"lcm_dfm2"

#' stm model of LCM abstracts
#'
#' The R package `stm` was used for topic modeling of LCM abstracts, with date
#' published and city as covariates for topic prevalence. Thirty five topics
#' were used.
#'
#' @format A `stm` object
#' @source See the `lcm_text_mining.Rmd` file in the `data-raw` in the [GitHub
#' repo of this package](https://www.github.com/pachterlab/museumst) for how
#' this model was fitted.
"stm_res"

#' quanteda tokens for LCM abstracts
#'
#' Common phrases and unigrams, without stop words, and stemmed.
#'
#' @format A quanteda tokens object
#' @source See the `lcm_text_mining.Rmd` file in the `data-raw` in the [GitHub
#' repo of this package](https://www.github.com/pachterlab/museumst) for how
#' tokenization was performed.
"lcm_tokens"
