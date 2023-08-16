## ---------------------------
## Author: Stuart Wallace [stuart.rankin.wallace@gmail.com]
## Date Created: 2023-07-30
## R Version: R version 4.0.3 (2020-10-10)
## ---------------------------
## Notes: fixes 000a_prep_intake_data_with_pro.R file to use for APTA analysis 
##  - cleans up FAST_1 and LAT_2 columns that weren't cleaned/recoded properly
##  - recodes LAT variables to numeric and creates new mean score variable 
## ---------------------------
# SET UP WORKSPACE, LOAD PACKAGES & DATA ----------------------------------------------------
# set working directory
here()
wd <- here()


# LOAD PRO DATA -----------------------------------------------------------
# SCRATCH TO PULL IN LATERALITY DATA
ne_data <- ne_data[1:47, ] # GET RID OF EXTRA ROWS
ne_data_pro <-
  ne_data %>% select(ID, all_of(starts_with("FAST")), all_of(starts_with("LAT"))) %>%
  mutate(across(FAST_1:FAST_P_9, ~ str_replace_na(.x)))


# VARIABLE CLEANUP AND RECODING --------------------------------------------------
se_data_pro <-
  se_data %>% select(ID, all_of(starts_with("FAST")), all_of(starts_with("LAT"))) %>%
  mutate(across(FAST_1:FAST_P_9, ~ str_replace_na(.x)))

ne_data_pro <-
  ne_data_pro %>% mutate(across(
    FAST_2:FAST_P_9,
    ~ recode(
      .x,
      "1" = "None",
      "2" = "Mild",
      "3" = "Moderate",
      "4" = "Severe",
      "5" = "Extreme",
      "NA" = "No Answer"
    )
  ))

se_data_pro <-
  se_data_pro %>% mutate(across(
    FAST_2:FAST_P_9,
    ~ recode(
      .x,
      "1" = "None",
      "2" = "Mild",
      "3" = "Moderate",
      "4" = "Severe",
      "5" = "Extreme",
      "NA" = "No Answer"
    )
  ))

all_sites_pro <- bind_rows(ne_data_pro, se_data_pro)
all_sites_pro <- all_sites_pro %>%
  mutate_at(
    vars(FAST_1),
    recode,
    "1" = "Completely",
    "2" = "Extremely",
    "3" = "Moderately",
    "4" = "Slightly",
    "5" = "Not Satisfied",
    "No Answer" = "NA"
  ) %>%
  mutate(across(LAT_1:LAT_15, ~ case_when(.x == "R" ~ 4,
                                          .x == "L" ~ -4,
                                          .x == "E" ~ 0))) %>%
  mutate(lat_mean = rowMeans(select(., starts_with("LAT")), na.rm = TRUE))


# SAVE RECODED PRO DATA TO RDS --------------------------------------------
all_sites_pro_corrected <-
  saveRDS(all_sites_pro, here("data_files/all_sites_pro.Rds"))

# LOAD AND MERGE CLEAN PRO DATA  ------------------------------------------
pro_data_corrected <-
  readRDS(here("data_files/data_for_reports_with_pro.rds")) %>%
  select(-starts_with("FAST"),-starts_with("LAT"))

# PULL IN CORRECTED PRO DF
all_sites_fast_lat <- readRDS(here("data_files/all_sites_pro.Rds"))

all_sites_with_pro_corrected <-
  left_join(pro_data_corrected, all_sites_fast_lat, by = "ID")


# UPDATE INJURY VALUES ----------------------------------------------------
all_sites_with_pro_corrected$Injury_History_Neck[which(all_sites_with_pro_corrected$ID == "S082")] <- "NO"
all_sites_with_pro_corrected$Injury_History_Neck[which(is.na(all_sites_with_pro_corrected$ID))] <- "NO"



# SAVE CLEANED UP DF TO RDS -----------------------------------------------
all_sites_with_pro_corrected <-
  saveRDS(
    all_sites_with_pro_corrected,
    here("data_files/all_sites_with_pro_corrected.Rds")
  )
