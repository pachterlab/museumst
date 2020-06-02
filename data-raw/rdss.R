library(tidyverse)
library(magick)
library(urbnmapr)
library(readxl)
library(rnaturalearth)
library(googlesheets4)
library(sf)
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
                                "MATLAB", "Python", "R", "Rust", "Scala"),
                   image_paths = paste0("images/",
                                        c("c.png", "CPlusPlus.jpg", "cuda.png",
                                          "java.png", "mathematica.jpg",
                                          "matlab.jpg", "python.png", "Rlogo.png",
                                          "rust.png", "scala.png")))

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
gcs <- geocode_inst_city(sheets, cache = TRUE, cache_location = "inst")

# Europe limits
europe_poly <- matrix(c(30, 68, 30, 35, -7, 35, -7, 68, 30, 68),
                      ncol = 2, byrow = TRUE)
europe_limits <- st_polygon(list(europe_poly)) %>% st_sfc()
europe_limits <- st_as_sf(europe_limits, crs = 4326)
europe_limits <- st_transform(europe_limits, 3035)
xylims <- st_bbox(europe_limits)

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
