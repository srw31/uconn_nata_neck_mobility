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
##                  2023-07-29 - cleaned up FAST_1 recoding, commented out recoding of FAST_1 for SE cohort
## ---------------------------
## Notes:
##   
##
## ---------------------------
# SET UP WORKSPACE, LOAD PACKAGES & DATA ----------------------------------------------------
# if (!require("pacman")) install.packages("pacman", INSTALL_opts = "--no-multiarch")

pacman::p_load(here, janitor, lubridate, tidyverse, readr, corrr, data.table)
here()
wd <- here()

# # create folder structure
# for(df in c('code', 'data_files', 'output')){
# if(!exists(df))
# dir.create(path = paste0(wd, "/", df))
# }

# se_raw <- readxl::read_excel("data_files/rawAssessment/nata_neck_mobility_testing_SE_raw_n4142_correction.xlsx")
# ne_raw <- readxl::read_excel("data_files/rawAssessment/nata_neck_mobility_testing_NE_raw_updated_studyID.xlsx")

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


# RECODE FAST_1 QUESTION
ne_data <- ne_data %>% mutate_at(vars(FAST_1), recode, "COMPLETELY" = "1", "EXTREMELY" = "2", "MODERATELY" = "3", "SLIGHTLY" = "4", "NOT SATISFIED" = "5", "DID NOT ANSWER" = "NA")
#se_data <- se_data %>% mutate_at(vars(FAST_1), recode, "COMPLETELY" = 1, "EXTREMELY" = 2, "MODERATELY" = 3, "SLIGHTLY" = 4, "NOT SATISFIED" = 5)


# FIND COLUMNS THAT NEED SPLITTING INTO 2 AND PUT INTO NEW COLUMNS
# janitor::compare_df_cols(se_data, ne_data, return = "all")

# MERGE COLUMNS WHERE A THIRD MEASUREMENT WAS DONE AND THEN CONVERT TO NUMERIC
ne_data_2 <- reduce(seq_along(ne_data), 
                    .init = ne_data, 
                    ~ .x %>% separate(names(ne_data)[.y], 
                                      sep = ',', 
                                      into = paste0(names(ne_data)[.y], '_col_' , seq(1 + max(str_count(ne_data[[.y]], ','), na.rm  = TRUE))),
                                      extra = "merge", 
                                      fill = 'right'
                    )
)
se_data_2 <- reduce(seq_along(se_data), 
                    .init = se_data, 
                    ~ .x %>% separate(names(se_data)[.y], 
                                      sep = ',', 
                                      into = paste0(names(se_data)[.y], '_col_' , seq(1 + max(str_count(se_data[[.y]], ','), na.rm  = TRUE))),
                                      extra = "merge", 
                                      fill = 'right'
                    )
)


merge_fxn <- function(x) sub(".*, ", "", x)
ne_data_2 <- ne_data %>% mutate_at(vars(ends_with("_2")), merge_fxn)
ne_data_2 <- ne_data_2 %>% mutate(across(ends_with("_2"), ~ as.numeric(.x)))

# CATEGORIZE NAMES
fast_cols <- colnames(select(se_data, starts_with("FAST")))
fast_p_cols <- colnames(select(se_data, starts_with("FAST_P")))
lat_cols <- colnames(select(se_data, starts_with("LAT")))
sane_cols <- lat_cols <- colnames(select(se_data, starts_with("SANE")))

# MERGE ALL DATA
ne_data_no_pro <- ne_data_2 %>% select(-all_of(starts_with("FAST"))) %>% 
  select(-all_of(starts_with("LAT"))) %>% 
  select(-all_of(starts_with("SANE")))

se_data_no_pro <- se_data %>% select(-all_of(starts_with("FAST"))) %>% 
  select(-all_of(starts_with("LAT"))) %>% 
  select(-all_of(starts_with("SANE")))

all_sites_no_pro <- bind_rows(ne_data_no_pro, se_data_no_pro)

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
all_sites_no_pro <- all_sites_no_pro %>% rowwise() %>% 
  reduce(i, gen_vars, .init = .)
}

# FINAL CLEANUP

all_sites_no_pro <- all_sites_no_pro %>% mutate_at(vars(School), recode, "YALE" = "Yale", "UCONN" = "UConn", "SACRED HEART" = "SHU", "WF" = "WFU")

all_sites_no_pro <- all_sites_no_pro %>% select(!ends_with("Avg"))
saveRDS(all_sites_no_pro, file = here("data_files/data_for_reports.rds"))
write.csv(all_sites_no_pro, paste0("data_for_reports_", Sys.Date(), ".csv"), row.names = F)


                                  