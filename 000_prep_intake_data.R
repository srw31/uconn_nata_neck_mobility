## ---------------------------
## Script: 
##
## Purpose:
##
## Author: Stuart Wallace [stuart.wallace@uconn.edu]
##
## Date Created: 2023-03-07
## Date(s) Revised: 
## ---------------------------
## Notes:
##   
##
## ---------------------------
# SET UP WORKSPACE, LOAD PACKAGES & DATA ----------------------------------------------------
# if (!require("pacman")) install.packages("pacman", INSTALL_opts = "--no-multiarch")

pacman::p_load(here, lubridate, tidyverse, readr, corrr, data.table)
here()
wd <- here()

# # create folder structure
# for(df in c('code', 'data_files', 'output')){
# if(!exists(df))
# dir.create(path = paste0(wd, "/", df))
# }

se_raw <- read_excel("data_files/nata_neck_mobility_testing_SE_raw.xlsx")
ne_raw <- read_excel("data_files/nata_neck_mobility_testing_NE_raw.xlsx")

# se_data <- saveRDS(se_raw, here("data_files/se_data_raw.Rds"))
# ne_data <- saveRDS(ne_raw, here("data_files/ne_data_raw.Rds"))

colnames(se_raw)
colnames(ne_raw)

ne_df <- ne_raw
se_df <- se_raw %>% select(-First_Name, Last_Name)

str(ne_raw$Neck_Flex_2)

all_sites_df <- bind_rows(ne_df, se_df)

# CATEGORIZE NAMES
fast_cols <- colnames(select(se_raw, starts_with("FAST")))
fast_p_cols <- colnames(select(se_raw, starts_with("FAST_P")))
lat_cols <- colnames(select(se_raw, starts_with("LAT")))
sane_cols <- lat_cols <- colnames(select(se_raw, starts_with("SANE")))
                                  