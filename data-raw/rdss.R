library(tidyverse)
library(magick)
library(readxl)
library(rnaturalearth)
library(googlesheets4)
library(sf)
library(rgeos)
species_img <- tibble(species = c("Mus musculus", "Drosophila melanogaster",
                                  "Danio rerio", "Ciona intestinalis", "Xenopus laevis",
                                  "Caenorhabditis elegans", "Arabidopsis thaliana",
                                  "Homo sapiens", "Platynereis dumereilii",
                                  "Saccharomyces cerevisiae", "Gallus gallus",
                                  "Drosophila virilis", "Xenopus tropicalis"),
                      image_paths = paste0("images/", c("mouse", "drosophila",
                                                        "zebrafish", "ciona",
                                                        "xenopus", "celegans",
                                                        "arabidopsis", "skull",
                                                        "platynereis", "yeast",
                                                        "chicken", "virilis",
                                                        "xenopus"),
                                           ".jpg"))

lang_img <- tibble(language = c("C", "C++", "CUDA", "Java", "Mathematica",
                                "MATLAB", "Python", "R", "Rust", "Scala",
                                "JavaScript"),
                   image_paths = paste0("images/",
                                        c("c.png", "CPlusPlus.jpg", "cuda.png",
                                          "java.png", "mathematica.jpg",
                                          "matlab.jpg", "python.png", "Rlogo.png",
                                          "rust.png", "scala.png", "js.png")))

# American map with data for population per state
na <- ne_states(c("United States of America", "Canada", "Mexico"), returnclass = "sf")
# From https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/totals/nst-est2019-01.xlsx
state_pop <- read_excel("data-raw/state_populations.xlsx", skip = 3) %>%
  rename(name = ...1) %>%
  select(name, `2010`:`2019`) %>%
  mutate(name = str_remove(name, "\\."))
na <- na %>%
  left_join(state_pop, by = "name")
na <- na %>%
  rename(state_name = name)
na_w_pop <- na %>%
  mutate(geometry = sf::st_transform(geometry, crs = 2163))

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

# Get geocode of all institutions present
sheets <- read_metadata(c("Prequel", "smFISH", "Array", "ISS",
                          "Microdissection", "No imaging",
                          "Analysis", "Prequel analysis"), update = TRUE)
gcs <- geocode_first_time(sheets, cache = TRUE, cache_location = "inst")

# Europe limits
europe_poly <- matrix(c(30, 68, 30, 35, -7, 35, -7, 68, 30, 68),
                      ncol = 2, byrow = TRUE)
europe_limits <- st_polygon(list(europe_poly)) %>% st_sfc()
europe_limits <- st_as_sf(europe_limits, crs = 4326)
europe_limits <- st_transform(europe_limits, 3035)
xylims <- st_bbox(europe_limits)

# Bounding box around continental America
xylims_us <- na_w_pop %>%
  filter(iso_a2 == "US", !state_name %in% c("Alaska", "Hawaii")) %>%
  st_bbox()

# Cache sheets
sheet_use <- c("Prequel", "smFISH", "Array", "ISS",
               "Microdissection", "No imaging",
               "Analysis", "Prequel analysis")
sheets <- read_metadata_fresh(sheet_use = sheet_use)
fns <- str_replace(sheet_use, "\\s", "_")
map2(sheets, fns, ~ saveRDS(.x, file = paste0("inst/sheets_cache/", .y, ".rds"),
                            version = 2))

# Get the internal copy of major events sheet
url_use <- "https://docs.google.com/spreadsheets/d/1sJDb9B7AtYmfKv4-m8XR7uc3XXw_k4kGSout8cqZ8bY/edit#gid=566523154"
events <- read_sheet(url_use, "major events")
saveRDS(events, "inst/major_events.rds", version = 2)

# For plotting European data only
europe_countries <- c("Albania", "Andorra", "Austria", "Belgium", "Bosnia and Herzegovina",
                      "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia",
                      "Faroe Islands", "Finland", "France", "Germany", "Gibraltar",
                      "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kosovo",
                      "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta",
                      "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway",
                      "Poland", "Portugal", "Romania", "San Marino", "Serbia", "Slovakia",
                      "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom",
                      "UK", "Vatican", "Russia")

# When coloring by species, give each species a fixed color.
# Also, when there are too many species, the colors are hard to tell from each other
# So colors are only for the most commonly used species.
species <- c("Mus musculus", "Drosophila melanogaster",
             "Danio rerio", "Caenorhabditis elegans", "Xenopus laevis",
             "Ciona intestinalis", "Gallus gallus", "Arabidopsis thaliana")
species_cols <- scales::brewer_pal(palette = "Set2")(8)
species_cols <- c(species_cols, scales::brewer_pal(palette = "Paired")(1), "gray50")
names(species_cols) <- c(species, "Homo sapiens", "Other")
usethis::use_data(species_cols)
usethis::use_data(lang_img, species_img, na_w_pop,
                  europe_countries, xylims, xylims_us, internal = TRUE,
                  overwrite = TRUE)

# Colors for sheets for plotting
sheets <- c("Prequel", "Microdissection", "Array", "smFISH", "ISS",
            "No imaging")
sheet_fill <- scales::brewer_pal(palette = "Pastel2")(length(sheets))
names(sheet_fill) <- sheets
usethis::use_data(sheet_fill)

# World map without borders, just coastal lines
# For now store in this package, but will move to native territory package
map_small <- ne_countries(scale = "small", returnclass = "sp")
one_world_small <- st_as_sf(gUnaryUnion(map_small))
map_medium <- ne_countries(scale = "medium", returnclass = "sp")
one_world_medium <- st_as_sf(gUnaryUnion(map_medium))
usethis::use_data(one_world_small)
usethis::use_data(one_world_medium)
