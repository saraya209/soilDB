#' ---
#' title: "Import Additional Tables from NASIS Database"
#' author: "Samuel Araya"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output:
#'    html_document:
#'      toc: true
#'      keep_md: true
#' ---
# /*
# This Section is NOT Executed. Use only to RENDER REPORTS:
rm(list = ls())
library(knitr)
opts_chunk$set(
  tidy = TRUE,
  warning = FALSE,
  cache = FALSE,
  message = FALSE
)
rmarkdown::render(input = "nasis_access_2.R", output_dir = "Report")
#
# */
#' This script accesses local NASIS database using the `soilDB` package.
#' Setting up ODBC Connection to local NASIS is required to be able to access
#' local NASIS from R. Instructions are to set ODBC connection are 
#' found [here](http://ncss-tech.github.io/AQP/soilDB/setup_local_nasis.html).
#' 
#' **Import Packages**
#+ import_libraries, warning=FALSE, message=FALSE
# Data manipulation
#library(gt)
#library(gluedown)
library(tidyverse)
library(janitor)
library(collapse)
library(foreign) # read dbf files
## GIS
#library(terra)
#library(sf) #newer, better with `tidyverse`
#library(soilDB)
# Database packages (replacement for RODBC)
library(DBI)
library(odbc)
library( RSQLite)
## Custom Functions
source("set_paths.R")
source("functions_araya_ssurgo.R")
fips = read_csv(file = "texas_areasymbol_county_fips_lookup.csv")

# Test package

channel <- soilDB::dbConnectNASIS(dsn=NULL)
db_tables = DBI::dbListTables(channel)
#' List NASIS tables

mu = get_mapunit_from_NASIS() 

x = metadata %>% 
  dplyr::filter(ColumnPhysicalName == "desgnsuffix")

hz = channel %>% 
  dplyr::tbl("component") %>% 
  dplyr::select(hzname, desgndisc, desgnmaster, desgnmasterprime,desgnvert, coiidref )
hz

#' 
#' ## Load NASIS Data
#' 
#' Fetch tables from the local NASIS database selected set.
#' Horizon Designation Suffix Table `chdesgnsuffix`.
#' 
ndes = a_get_component_horizon_data_from_NASIS_db() 
nsuf = a_get_component_horizon_suffix_data_from_NASIS_db()



nmap = soilDB::get_mapunit_from_NASIS() %>% as_tibble()
ncomp = soilDB::get_component_data_from_NASIS_db() %>% as_tibble()
nhor = soilDB::get_component_horizon_data_from_NASIS_db() %>% as_tibble()
#' ### Load SSURGO Data
smappoly = readr::read_rds(file = file.path(data_dir, "mupolygontable.rds")) %>% 
  dplyr::left_join(fips, by = "areasymbol")
scomp = read_rds(file = file.path(data_dir, "component.rds"))
shor = read_rds(file = file.path(data_dir, "chorizon.rds"))


ndes2 = ndes %>% 
  dplyr::distinct( .keep_all = FALSE)

#'
x = ndes %>% 
  filter(chiid == 9319584)

z = ndes2 %>% 
  filter(chiid == 9319584)

y = nhor %>% 
  filter(chiid == 9161417)

#'
#' 
#' NASIS filter should have kept only 'correlated' mapunits but it included some
#' non correlated mapunits. 
nmap %>% 
  janitor::tabyl(mustatus) %>% 
  gt() %>% 
  gt_theme_538() %>% 
  fmt_percent(3)
#' Remove mapunits with status NOT equal to 'correlated'. 
#' *The NASIS query should have removed those.*
#' Also convert table keys into character format.
nmukey_keep = nmap %>% 
  dplyr::filter(mustatus == "correlated") %>% 
  dplyr::distinct(lmapunitiid) %>% 
  dplyr::pull(lmapunitiid)

nmap = nmap %>% 
  dplyr::filter(lmapunitiid %in% nmukey_keep) %>% 
  dplyr::mutate(lmapunitiid = as.character(lmapunitiid),
                dmuiid = as.character(dmuiid)) %>% 
  dplyr::left_join(fips, by = "areasymbol")