# For the N P flows project
#
# Load libraries and query GCAM output
# Jillian Sturtevant, August 2026

# Change
#  INSTALL_PACKAGES to TRUE to install required packages
#  QUERY_GCAM to TRUE to re-query GCAM output
#  PATH_TO_GCAM to change the path to GCAM output
#  FIGS_DIR to change figures sub-directory e.g., figures/v1, v2, etc.

# set bools
INSTALL_PACKAGES <- FALSE # set to TRUE to install required packages
QUERY_GCAM <- FALSE # set to TRUE to re-query GCAM output

# install packages ----

if(INSTALL_PACKAGES){
  install.packages('ggplot2')
  install.packages('dplyr')
  install.packages('tidyr')
  install.packages('readr')
  install.packages('grid')
  install.packages('maps')
  install.packages("patchwork")
  install.packages("purrr")
  install.packages("ggtext")
  install.packages("egg")
  install.packages("scales")
  install.packages("ggrepel")

  install.packages("devtools")

  # install JGCRI packages from GitHub
  devtools::install_github("JGCRI/rgcam", build_vignettes = TRUE)
  devtools::install_github("JGCRI/rmap", build_vignettes = TRUE)
}

# load libraries ----
library(rgcam)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(grid)
library(maps)
library(patchwork)
library(rmap)
library(purrr)
library(ggtext)
library(egg)
library(scales)
library(ggrepel)

# paths ----
DATA_DIR <- "data/"
FIGS_DIR <- "figures/v1/" # change the subfolder here if desired

# create figs dir if it doesn't exist
if (!dir.exists(FIGS_DIR)) {
  dir.create(FIGS_DIR)
  paste0("Created directory: ", FIGS_DIR)
  }

# constants ----
CONV_USD_1975_2020 <- 3.8
CONV_KG_T <- 1000
H2_GJ_kg <- 0.1202
DAYS_PER_YEAR <- 365.25
CONV_PCAL_MCAL <- 1e9
CONV_NH3_N <- 14/17    # convert mass of NH3 to mass of N

HIST_YEARS <- c(1975, 1990, 2005, 2010, 2015, 2020, 2025)

# query GCAM ----
OUTPUTFILE <- "NPflows.proj"

if (QUERY_GCAM) {
  # query variables
  PATH_TO_GCAM <- "output"

  SCENARIOS <- c("Reference")

  QUERYFILE <- "BatchQueries_OTAQ_nutrients.xml"

  # query
  conn <- localDBConn(PATH_TO_GCAM, "database_basexdb")
  NPflows_proj <- addScenario(conn, OUTPUTFILE, SCENARIOS, QUERYFILE, clobber = TRUE)
}

# load project ----
NPflows_proj <- loadProject(OUTPUTFILE)


print(paste0("Queries: ", listQueries(NPflows_proj)))

