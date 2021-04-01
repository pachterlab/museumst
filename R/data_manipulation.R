# Update this manually when the copy of metadata within the package is updated
.pkg_update_time <- "2021-03-31 18:02:50 PDT"

read_metadata_fresh <- function(sheet_use) {
  date_published <- NULL
  gs4_deauth()
  url_use <- "https://docs.google.com/spreadsheets/d/1sJDb9B7AtYmfKv4-m8XR7uc3XXw_k4kGSout8cqZ8bY/edit#gid=566523154"
  sheets <- map(sheet_use, read_sheet, ss = url_use)
  sheets <- map(sheets, ~ .x %>%
                  mutate(year = year(date_published),
                         date_published = as_date(date_published)))
  return(sheets)
}

#' Read the metadata from Google Sheets
#'
#' To do: Cache and add an argument to update cache. Unlike the geocodes, this
#' won't be stored in the data of this package since it's much faster to download
#' the sheet than to geocode.
#'
#' @inheritParams geocode_inst_city
#' @param sheet_use Name of the sheet(s) to read.
#' @param update Logical, whether to update cache. If not, then the cache is read,
#' and if the cache is absent, then a file contained within
#' this package is read. This file within the package is updated with each
#' version of this package. If TRUE, then the data is directly downloaded from
#' Google Sheets, which is guaranteed to be up to date.
#' @return A tibble for the sheet of interest.
#' @importFrom googlesheets4 gs4_deauth read_sheet
#' @importFrom googledrive drive_deauth drive_get
#' @importFrom dplyr mutate group_split
#' @importFrom magrittr %>%
#' @importFrom lubridate year as_date
#' @importFrom purrr map reduce map2_dfr
#' @importFrom stringr str_replace
#' @export
read_metadata <- function(sheet_use = c("Prequel", "smFISH", "Array", "ISS",
                                        "Microdissection", "No imaging",
                                        "Analysis", "Prequel analysis"),
                          cache = TRUE, cache_location = "./sheets_cache",
                          update = FALSE) {
  sheet_use <- match.arg(sheet_use, several.ok = TRUE)
  sheet_use2 <- str_replace(sheet_use, "\\s", "_")
  if (cache) {
    cache_location <- normalizePath(cache_location, mustWork = FALSE)
    if (!dir.exists(cache_location)) dir.create(cache_location)
    fn <- paste0(cache_location, "/", sheet_use2, ".rds")
    fn_inst <- system.file(paste0("sheets_cache/", sheet_use2, ".rds"),
                           package = "museumst")
    inds <- !file.exists(fn)
    if (!update) {
      if (any(inds)) {
        file.copy(fn_inst[inds], fn[inds])
      }
      out <- map(fn, readRDS)
    } else {
      # Check if the sheet is newer than the version that comes with this package
      url_use <- "https://docs.google.com/spreadsheets/d/1sJDb9B7AtYmfKv4-m8XR7uc3XXw_k4kGSout8cqZ8bY/edit#gid=566523154"
      drive_deauth()
      metas <- drive_get(url_use)
      updated <- metas$drive_resource[[1]]$modifiedTime
      cache_updated <- file.mtime(fn)
      cache_updated[inds] <- .pkg_update_time
      need_update <- cache_updated < updated
      if (any(need_update)) {
        out <- read_metadata_fresh(sheet_use[need_update])
        fn_save <- paste0(cache_location, "/", sheet_use2[need_update], ".rds")
        for (i in seq_along(out)) {
          saveRDS(out[[i]], fn_save[i])
        }
      }
      need_cp <- !need_update & inds
      if (any(!need_update)) {
        if (any(need_cp)) {
          file.copy(fn_inst[need_cp], fn[need_cp])
        }
        out2 <- map(fn[!need_update], readRDS)
      }
      if (any(need_update) && !all(need_update)) {
        out <- c(out, out2)
        # reorder sheet names
        sheet_use <- c(sheet_use[need_update], sheet_use[!need_update])
      }
      if (all(!need_update)) {
        out <- out2
      }
    }
  } else {
    out <- read_metadata_fresh(sheet_use)
  }
  if (length(out) > 1) {
    colnames_use <- map(out, names) %>% reduce(intersect)
    out <- map2_dfr(out, sheet_use, ~ .x %>% mutate(sheet = .y) %>%
                         select(!!!syms(c(colnames_use, "sheet"))))
  } else {
    out <- out[[1]]
  }
  out
}

#' Read the major events sheet
#'
#' There's a column for image file names. Absence of file name means that I can't
#' legally use the image for free. The ones present in the image column can be
#' reused in this package for free. PNAS's website says permission is not required
#' for reuse of figures for non-commercial purposes in the Requesting Permissions
#' section: https://www.pnas.org/page/about/rights-permissions The other images
#' used are under some form of Creative Commons license, and a corresponding link
#' to the license is in the license column.
#'
#' @inheritParams read_metadata
#' @return A tibble. In the returned tibble, the image column will be the paths
#' to the images of interest on your computer for plotting time lines.
#' @export
read_major_events <- function(update = FALSE) {
  image <- date_published <-NULL
  fn_inst <- system.file("major_events.rds", package = "museumst")
  if (update) {
    url_use <- "https://docs.google.com/spreadsheets/d/1sJDb9B7AtYmfKv4-m8XR7uc3XXw_k4kGSout8cqZ8bY/edit#gid=566523154"
    drive_deauth()
    metas <- drive_get(url_use)
    if (.pkg_update_time < metas$drive_resource[[1]]$modifiedTime) {
      gs4_deauth()
      out <- read_sheet(url_use, sheet = "major events")
    } else {
      out <- readRDS(fn_inst)
    }
  } else {
    out <- readRDS(fn_inst)
  }
  out <- out %>%
    mutate(image = map_chr(paste0("images/", image), system.file, package = "museumst"),
           image = case_when(image == "" ~ NA_character_,
                             TRUE ~ image),
           date_published = as_date(date_published))
  out
}

#' Transform the sheet to have one species, method, or language per row
#'
#' When entering species, method, and programming languages into the spreadsheet,
#' sometimes one paper or dataset has multiple of those, and those are separated
#' by a semicolon. This function unnests that and returns a data frame with one
#' row per category of interest.
#'
#' @param sheet The tibble for the spreadsheet.
#' @param col_use Column to unnest. Tidyeval is supported.
#' @param other_cols Character vector of other columns to include.
#' @return A data frame with one row per category of interest. NAs are removed.
#' @importFrom stringr str_split
#' @importFrom tidyr unnest
#' @export
unnest_cat <- function(sheet, col_use, other_cols = NULL) {
  col_new <- NULL
  col_use <- enquo(col_use)
  out <- sheet %>%
    filter(!is.na(!!col_use)) %>%
    mutate(col_new = str_split(!!col_use, pattern = "(;|,)\\s"))
  date_published <- year <- title <- journal <- country <- city <- institution <- NULL
  if (!is.null(other_cols)) {
    out <- out %>%
      select(date_published, year, title, journal, col_new, country, `state/province`, city,
             institution, !!!syms(other_cols))
  } else {
    out <- out %>%
      select(date_published, year, title, journal, col_new, country, `state/province`, city,
             institution)
  }
  out <- out %>%
    unnest(cols = "col_new") %>%
    distinct()
  names(out)[names(out) == "col_new"] <- as_name(col_use)
  # Check that the combination of title and category of interest is not duplicated
  check <- out %>%
    filter(!is.na(title)) %>%
    select(title, !!col_use)
  if (anyDuplicated(check)) {
    warning("Combination of title and ", as_name(col_use), " is duplicated. ",
            "Consider changing other_cols.")
  }
  out
}

#' Get data frame with one publication per row
#'
#' For current era papers, one dataset occupies one row. This function only keeps
#' information about publications so each row is a distinct publication.
#'
#' @inheritParams unnest_cat
#' @return A data frame with one row per publication.
#' @importFrom rlang syms
#' @export
get_pubs_df <- function(sheet, other_cols = NULL) {
  date_published <- year <- title <- journal <- language <- department <- NULL
  if (!is.null(other_cols)) {
    out <- sheet %>%
      select(date_published, year, title, journal, language:department,
             !!!syms(other_cols))
  } else {
    out <- sheet %>%
      select(date_published, year, title, journal, language:department)
  }
  out <- out %>%
    filter(!is.na(title)) %>%
    distinct()
  if (anyDuplicated(out$title[!is.na(out$title)])) {
    warning("Titles are duplicated. Consider changing other_cols.")
  }
  out
}

