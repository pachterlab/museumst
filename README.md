# museumst: Museum of Spatial Transcriptomics

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build status](https://github.com/pachterlab/museumst/workflows/R-CMD-check/badge.svg)](https://github.com/pachterlab/museumst/actions)
<!-- badges: end -->

This package contains functions to make plots in analysis of manually curated metadata about publications throughout the history of spatial transcriptomics. The vignettes are the analysis notebooks.

These plots are made for all sheets unless specified:
  * Time line of major events. While all sheets will have it, it's made in the sheet specific section.
  * Number of publications
    + Per year (excluding preprints)
    + Per journal
    + Per species
    + Per method
    + Make isotype plot for top 5 species, just a fun and memorable visualization
    + Per institution
    + Per country
    + Per city, plotted on a world map. In the sheet specific section, if there are many cities in one country represented in that sheet, then another map just for that country.
    + Per species per city, since I noted that some institutions focus on a certain species
    + Per method per city, similar reason to why per species per city. 
    + Which method has data for the most species
  * Number of datasets (for those sheets with dataset information)
    + Per publication
    + Per species
    + Number of genes and cells/bins per dataset, separated by method; this will be in the sheet specific section as different sheets need to be treated differently here
  * Word cloud
    + All titles
    + Titles for top species (sheet specific)
    + Titles for top methods (sheet specific)
    + Titles over time will be in the sheet specific section as different sheets have different time scales.
    + Department names
    + For dataset sheets, downstream analyses (not standardized, don't take it too seriously as it might depend on my mood when I wrote it)
    + For data analysis sheets, core principles and summaries
    + Tissues for each species
  * For data analysis sheets, code availability status
  * For non-prequel sheets, programming languages used

Instead of using a child document for the shared part, I still decided to copy the notebook so I can fine tune the plots for visual appeal and write comments. I can also feel free to use `gganimate`. Shall I turn this spreadsheet and these notebooks into a website for people to explore? That sounds like a good idea, though the only way I know how to do it are `shiny` and `blogdown`. 

To do: 

  * More defensive programming
  * Move dependencies for isotype plot, timeline, and maps to Suggests (maybe)
  * Polish user interface
  * Build shiny app for exploring this metadata
  * Put the vignettes on a RStudio Cloud project for those who do code to play around with. The entire package repo will be there so the user can also change the code and make other plots.
  * Get this package to CRAN
  * The code to produce final figures for the review papers about this metadata will be in a separate repo, but this package will make it a lot easier to make those figures.
