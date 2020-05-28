#' List of European countries
#'
#' @format A character vector
#' @source foobar
"europe_countries"

#' List of institution words
#'
#' @format A character vector
#' @source Some words I often find in department names, and in addition, country
#' and city names.
"inst_words"

#' Programming language logos
#'
#' @format A data frame with programming language and path to image of the logos.
#' @source online
"lang_img"

#' Species images for isotype plot
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
#' @format A data frame with species Latin names and path to images.
#' @source online
"species_img"

#' American map with Alaska and Hawaii moved and with state population estimates
#'
#' @format sf
#' @source urbnmapr; state population data is from census.gov
"usa_w_pop"

#' Bounds of longitude and latitude for European maps
#'
#' This is to make contributions of Europe easier to see. Since the most crowded
#' area is Western Europe, this box centers on Western Europe while some countries
#' in Eastern Europe are cropped out.
#'
#' @format A named numeric vector
#' @source I just tried until I got something that looks good.
"xylims"
