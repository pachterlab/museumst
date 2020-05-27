library(tidyverse)
library(magick)
library(urbnmapr)
library(readxl)
library(rnaturalearth)
library(googlesheets4)
species_img <- tibble(species = c("Mus musculus", "Drosophila melanogaster",
                                  "Danio rerio", "Ciona intestinalis", "Xenopus laevis",
                                  "Caenorhabditis elegans", "Arabidopsis thaliana",
                                  "Homo sapiens", "Platynereis dumereilii",
                                  "Saccharomyces cerevisiae", "Gallus gallus"),
                      image_paths = paste0("images/", c("mouse", "drosophila",
                                                        "zebrafish", "ciona2",
                                                        "xenopus", "celegans",
                                                        "arabidopsis", "skull",
                                                        "platynereis", "yeast",
                                                        "chicken"),
                                           ".jpg"))
saveRDS(species_img, "output/species_img.rds")

lang_img <- tibble(language = c("C", "C++", "CUDA", "Java", "Mathematica",
                                "MATLAB", "Python", "R", "Rust", "Scala"),
                   image_paths = paste0("images/",
                                        c("c.png", "CPlusPlus.jpg", "cuda.png",
                                          "java.png", "mathematica.png",
                                          "matlab.jpg", "python.png", "Rlogo.png",
                                          "rust.png", "scala.png")))
saveRDS(lang_img, "output/lang_img.rds")

# American map with data for population per state
usa <- get_urbn_map(sf = TRUE)
state_pop <- read_excel("data/state_populations.xlsx", skip = 3) %>%
  rename(state_name = ...1) %>%
  select(state_name, `2010`:`2019`) %>%
  mutate(state_name = str_remove(state_name, "\\."))
usa <- usa %>%
  left_join(state_pop, by = "state_name")
saveRDS(usa, "output/usa_w_pop.rds")

# Words to remove when plotting word cloud for department names
institution_words <- c("institute", "institution", "school", "college",
                       "department", "division", "faculty", "center", "centre",
                       "laboratory", "institutes", "graduate", "advanced",
                       "academic", "program", "unit", "lab", "national",
                       "research", "international")
world <- ne_countries(scale = "small", returnclass = "sf")
institution_words <- c(institution_words, tolower(unique(world$name)),
                       tolower(usa$state_name), "us", "uk", "usa", "british",
                       "britain", "american")
city_pop <- read_csv("data/city_population_2014.csv", n_max = 17060)
institution_words <- c(institution_words, tolower(unique(city_pop$City)))
saveRDS(institution_words, "output/inst_words.rds")

# Get geocode of all institutions present
sheets <- read_metadata(c("Prequel", "smFISH", "Array", "ISS",
                          "Microdissection", "No imaging",
                          "Analysis", "Prequel analysis"))
gcs <- geocode_first_time(sheets, cache = TRUE, cache_location = "inst")
