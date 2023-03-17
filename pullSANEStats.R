pacman::p_load(here, lubridate, tidyverse, readr, corrr, data.table)
here()
wd <- here()

nata_neck_mobility_player_contact_list_rev3 <- read_csv("data_files/nata_neck_mobility_player_contact_list_rev3.csv")
sane_results <- read_csv("data_files/sane_results_03162023.csv")
sane_results <- sane_results[-c(1:2),]
sane_results <- sane_results %>% select(-c(RecipientLastName, RecipientFirstName))

# MERGE SCHOOL DATA TO SANE OUTPUT
sane_all <- sane_results %>% left_join(nata_neck_mobility_player_contact_list_rev3, 
                                                by = c("RecipientEmail" = "email"))
# QUICK TALLY BY SCHOOL
sane_all %>% group_by(school) %>% tally()

nrow(nata_neck_mobility_player_contact_list_rev3) # 126
nrow(sane_all)
# 473+126 = 599 total surveys sent as of 3/15
126/599 #21%

tm <- unique(sane_all$school)

for(i in tm){

write.csv(sane_all %>% select(school, studyID, nameLast, nameFirst, StartDate) %>% 
filter(school == i) %>%
  group_by(school, studyID, nameLast, nameFirst) %>% 
  summarise_all(list(minDate = min, maxDate = max, n_total = length)), 
paste0(i, "_survey_stats_", Sys.Date(), ".csv"), row.names = F)
}

for(i in tm){
x <- bind_rows((sane_all %>% select(school, studyID, nameLast, nameFirst, StartDate) %>% 
                  filter(school == i) %>%
                  group_by(school, studyID, nameLast, nameFirst) %>% 
                  summarise_all(list(minDate = min, maxDate = max, n_total = length))),
               
               (nata_neck_mobility_player_contact_list_rev3 %>% select(school, studyID, nameLast, nameFirst) %>% 
                  filter(school == i)))

x2 <- x %>% group_by(school, studyID, nameLast, nameFirst) %>% 
  summarise_each(funs(max(., na.rm = TRUE))) %>% 
  mutate(across(.cols = everything(), ~ ifelse(is.infinite(.x), 0, .x))) %>% 
  arrange(., desc(n_total))

write.csv(x2, paste0(i, "_survey_stats_", Sys.Date(), ".csv"), row.names = F)
}
                                                