## ---------------------------
## Script: 000_prep_intake_data.R
##
## Purpose: Script to upload, clean up, and save assessment data for analysis
##
## Author: Stuart Wallace [stuart.wallace@uconn.edu]
##
## Date Created: 2023-03-07
## Date(s) Revised: 2023-03-17 - updated NE data in order to merge with SE data
##                  2023-03-22 - cleaned up raw xlsx files, finalized R-side cleanup of column names/values
##                               NE - update study IDs | SE clean up values coded as dates in column N
##                  2023-03-31 - added school name cleanup at bottom of code before saving to rds/csv
##                  2023-07-29 - added back in recoding of FAST_1 for SE cohort
## ---------------------------
## Notes:
##   
##
## ---------------------------
# SET UP WORKSPACE, LOAD PACKAGES & DATA ----------------------------------------------------
# if (!require("pacman")) install.packages("pacman", INSTALL_opts = "--no-multiarch")

pacman::p_load(here, janitor, lubridate, tidyverse, readr, corrr, data.table, inspectdf)
here()
wd <- here()

# # create folder structure
# for(df in c('code', 'data_files', 'output')){
# if(!exists(df))
# dir.create(path = paste0(wd, "/", df))
# }

# se_raw <- readxl::read_excel("data_files/rawAssessment/nata_neck_mobility_testing_SE_raw_n4142_correction.xlsx")
# ne_raw <- readxl::read_excel("data_files/rawAssessment/nata_neck_mobility_testing_NE_raw_updated_studyID.xlsx")
# ne_raw <- ne_raw[1:47,] # GET RID OF EXTRA ROWS


# se_data <- saveRDS(se_raw, here("data_files/se_data_raw.Rds"))
# ne_data <- saveRDS(ne_raw, here("data_files/ne_data_raw.Rds"))


se_data <- readRDS(here("data_files/se_data_raw.Rds"))
ne_data <- readRDS(here("data_files/ne_data_raw.Rds"))


# CLEAN DATA --------------------------------------------------------------

# FIND COLUMN NAMES THAT ARE MISMATCHED
# janitor::compare_df_cols(se_data, ne_data, return = "all")

# First_Name, Last_Name missing from NE -- remove
# Shoulder_ADD_ -- SE
# Shoulder_HADD_ -- NE

# RENAME/REMOVE RELEVANT COLUMNS
names(ne_data) <- str_replace_all(names(ne_data), "Shoulder_HADD", "Shoulder_ADD")
se_data <- se_data %>% select(-ends_with("Name"))
ne_data <- ne_data[1:47,] # GET RID OF EXTRA ROWS 

# NON-PRO DATA
ne_data_non_pro <- ne_data[,1:75]
se_data_non_pro <- se_data[,1:75]


# CLEAN PRO DATA ----------------------------------------------------------
# ne_data <- ne_data %>% mutate_at(vars(FAST_1), recode,
#                           "COMPLETELY" = 1, "EXTREMELY" = 2, "MODERATELY" = 3, "SLIGHTLY" = 4, "NOT SATISFIED" = 5,
#                           "DID NOT ANSWER" = NA)

ne_data_pro <- ne_data %>% select(ID, all_of(starts_with("FAST")), all_of(starts_with("LAT"))) %>% 
  mutate(across(FAST_1:FAST_P_9, ~str_replace_na(.x)))

se_data_pro <- se_data %>% select(ID, all_of(starts_with("FAST")), all_of(starts_with("LAT"))) %>% 
  mutate(across(FAST_1:FAST_P_9, ~str_replace_na(.x)))

ne_data_pro <- ne_data_pro %>% mutate(across(FAST_2:FAST_P_9, ~recode(.x,
                                 "1" = "None", "2" = "Mild", 
                                 "3" = "Moderate", "4" = "Severe", 
                                 "5" = "Extreme", "NA" = "No Answer")))

se_data_pro <- se_data_pro %>% mutate(across(FAST_2:FAST_P_9, ~recode(.x,
                                                              "1" = "None", "2" = "Mild", 
                                                              "3" = "Moderate", "4" = "Severe", 
                                                              "5" = "Extreme", "NA" = "No Answer")))


# CLEAN ROM DATA ----------------------------------------------------------
# FIND COLUMNS THAT NEED SPLITTING INTO 2 AND PUT INTO NEW COLUMNS
# janitor::compare_df_cols(se_data, ne_data, return = "all")

# MERGE COLUMNS WHERE A THIRD MEASUREMENT WAS DONE AND THEN CONVERT TO NUMERIC
ne_data_non_pro_2 <- reduce(seq_along(ne_data_non_pro), 
                    .init = ne_data_non_pro, 
                    ~ .x %>% separate(names(ne_data_non_pro)[.y], 
                                      sep = ',', 
                                      into = paste0(names(ne_data_non_pro)[.y], '_col_' , seq(1 + max(str_count(ne_data_non_pro[[.y]], ','), na.rm  = TRUE))),
                                      extra = "merge", 
                                      fill = 'right'
                    )
)
se_data_non_pro_2 <- reduce(seq_along(se_data_non_pro), 
                    .init = se_data_non_pro, 
                    ~ .x %>% separate(names(se_data_non_pro)[.y], 
                                      sep = ',', 
                                      into = paste0(names(se_data_non_pro)[.y], '_col_' , seq(1 + max(str_count(se_data_non_pro[[.y]], ','), na.rm  = TRUE))),
                                      extra = "merge", 
                                      fill = 'right'
                    )
)


merge_fxn <- function(x) sub(".*, ", "", x)

ne_data_non_pro_2 <- ne_data_non_pro_2 %>% mutate_at(vars(ends_with("_2")), merge_fxn)
#ne_data_non_pro_2 <- ne_data_non_pro_2 %>% mutate_at(vars(ends_with("_2")), as.numeric)


se_data_non_pro_2 <- se_data_non_pro_2 %>% mutate_at(vars(ends_with("_2")), merge_fxn)
#se_data_non_pro_2 <- se_data_non_pro_2 %>% mutate_at(vars(ends_with("_2")), as.numeric)

# CATEGORIZE NAMES
fast_cols <- colnames(select(se_data, starts_with("FAST")))
fast_p_cols <- colnames(select(se_data, starts_with("FAST_P")))
lat_cols <- colnames(select(se_data, starts_with("LAT")))
sane_cols <- lat_cols <- colnames(select(se_data, starts_with("SANE")))

# MERGE ALL DATA

all_sites_non_pro <- bind_rows(ne_data_non_pro_2, se_data_non_pro_2)

# COLS 29-92 FOR REPORTS - ALL NECK AND SHOULDER METRICS
# CREATE NEW AVERAGES -----------------------------------------------------

gen_vars <- function(df, x) {
  mutate(df,
         "{x}_mean" := mean(c(!! sym(paste0(x, "_1")) , !! sym(paste0(x, "_2"))), na.rm = TRUE))
}

cfrt1 <- c("CFRT_Left", "CFRT_Right")
neck1 <- c("Neck_Flex", "Neck_Ext")
neck2 <- c("Neck_Rot_", "Neck_Side_")
shoulder1 <- c("IR", "ER", "Flex", "ADD")
side <- c("Right", "Left")

neck_pre <- c()
for (i in neck2){
  for (j in side){
    neck_pre <- c(neck_pre, paste0(i, j))
  }
}

shoulder_pre <- c()
for (i in shoulder1){
  for (j in side){
    shoulder_pre <- c(shoulder_pre, paste0("Shoulder_", i, "_", j))
  }
}

all_vars <- c(neck_pre, shoulder_pre, cfrt1, neck1)

for (i in all_vars){
all_sites_non_pro <- all_sites_non_pro %>% rowwise() %>% 
  reduce(i, gen_vars, .init = .)
}

# FINAL CLEANUP

all_sites_with_pro <- all_sites_with_pro %>% mutate_at(vars(School), recode, "YALE" = "Yale", "UCONN" = "UConn", "SACRED HEART" = "SHU", "WF" = "WFU")

all_sites_with_pro <- all_sites_with_pro %>% select(!ends_with("Avg"))
saveRDS(all_sites_with_pro, file = here("data_files/data_for_reports_with_pro.rds"))
write.csv(all_sites_with_pro, paste0("data_for_reports_with_pro", Sys.Date(), ".csv"), row.names = F)

x <- ne_data %>% select(all_of(starts_with("FAST"))) %>% inspectdf::inspect_cat()
inspectdf::show_plot(x)
