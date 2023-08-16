# USE THIS TO PULL WEEKLY SURVEY COMPLIANCE STATS

pacman::p_load(here, lubridate, tidyverse, readr, janitor, data.table)
here()
wd <- here()

# playerInfo <- read_csv("data_files/nata_neck_mobility_player_contact_list_rev3.csv")
# saveRDS(playerInfo, here("data_files/playerInfo.rds"))

playerInfo <- readRDS(here("data_files/playerInfo.rds"))

sane_results <- read_csv(list.files(path = here("data_files/"), pattern = "*2023.csv", full.names = TRUE))

sane_results <- sane_results[-c(1:2),]
# sane_results <- sane_results %>% select(-c(RecipientLastName, RecipientFirstName))

# MERGE SCHOOL DATA TO SANE OUTPUT
sane_all <- sane_results %>% left_join(playerInfo, by = c("RecipientEmail" = "email"))


# sane_all %>% group_by(school) %>% tally()
sane_all <- sane_all %>% mutate(date2 = as.Date(StartDate)) 
sane_all <- sane_all %>% mutate(distribution_no = case_when(date2 > '2023-02-27' & date2 <= '2023-03-05' ~ '1',
                          date2 <= '2023-03-12' & date2 >= '2023-03-06' ~ '2',
                          date2 <= '2023-03-19' & date2 >= '2023-03-13' ~ '3',
                          date2 <= '2023-03-26' & date2 >= '2023-03-20' ~ '4',
                          date2 <= '2023-04-02' & date2 >= '2023-03-27' ~ '5',
                          date2 <= '2023-04-09' & date2 >= '2023-04-03' ~ '6',
                          date2 <= '2023-04-16' & date2 >= '2023-04-10' ~ '7',
                          date2 <= '2023-04-23' & date2 >= '2023-04-17' ~ '8',
                          date2 <= '2023-04-30' & date2 >= '2023-04-24' ~ '9',
                          date2 <= '2023-05-07' & date2 >= '2023-05-01' ~ '10',
                          date2 <= '2023-05-14' & date2 >= '2023-05-08' ~ '11',
                          date2 <= '2023-05-21' & date2 >= '2023-05-15' ~ '12',
                          date2 <= '2023-05-28' & date2 >= '2023-05-22' ~ '13',
                          TRUE ~ '0'))

tm <- unique(sane_all$school)

# RESULTS BY TEAM ---------------------------------------------------------
sane_team <- sane_all %>% complete(school, distribution_no)

sane_team <- sane_team %>% select(school, date2, distribution_no) %>% 
            group_by(school, distribution_no) %>% 
            summarise_all(list(minDate = min, maxDate = max,
                               n_total = length))

sane_team <- sane_team %>% mutate(n_total = ifelse(
  is.na(minDate) == TRUE & n_total == 1, 0, n_total))


sane_team <- sane_team %>% mutate(n_roster = case_when(school == "HPU" ~ 22,
                                      school == "NGU" ~ 23,
                                      school == "SHU" & maxDate < '2023-03-19' ~ 13,
                                      school == "SHU" & maxDate > '2023-03-18' ~ 12, # UPDATED 3/21 TO REMOVE LONGO (ORIG: 13)
                                      school == "UConn" ~ 23,
                                      school == "WFU" ~ 17, 
                                      school == "Winthrop" ~ 13, # UPDATED 4/20 TO REMOVE SHEPHERD/KEUP (TJ) (ORIG: 15)
                                      TRUE ~ 11))

n_dist <- max(as.numeric(sane_all$distribution_no))

sane_team_rates <- sane_team %>% 
  filter(distribution_no >=1) %>% 
  mutate(completion_rate = round(100*(n_total/n_roster), 2))

sane_team_rates <- sane_team_rates %>% 
  mutate(completion_rate = ifelse(completion_rate > 100, 100, completion_rate)) %>% 
  group_by(school) %>% 
  arrange(maxDate) %>% 
  mutate(completion_rate_rolling = case_when(distribution_no != 0 ~
                                   round((100*cumsum(n_total))/cumsum(n_roster),2))) %>% 
                                   ungroup() %>% 
                                   adorn_totals("row",,,,-completion_rate_rolling) 

# sane_team_rates <- sane_team_rates %>%
#   mutate(across(ends_with("_rate"),
#                   ~ row_number() == n(), ~ round(100*(n_total/n_roster), 2), .))
                                  
write.csv(sane_team_rates, here("output/weekly_stats/", paste0("all_sites_survey_stats_", Sys.Date(), ".csv")), row.names = F)

write.csv(sane_all %>% select(school, studyID, nameLast, nameFirst, date2) %>% 
  group_by(school, studyID, nameLast, nameFirst) %>% 
  summarise_all(list(minDate = min, maxDate = max, n_total = length)), 
here("output/weekly_stats/", paste0("all_player_survey_stats_", Sys.Date(), ".csv")), row.names = F)


# RESULTS - ALL TEAMS -----------------------------------------------------
sane_all_teams <- sane_team_rates %>% ungroup() %>% select(distribution_no, n_total, n_roster) %>% 
  group_by(distribution_no) %>% 
  summarise_all(list(sum))
  
sane_all_teams <- sane_all_teams %>%  mutate(completion_rate = round(100*(n_total/n_roster), 2)) %>% 
  #group_by(school) %>% 
  mutate(completion_rate_rolling = case_when(distribution_no > 0 ~
                                               round((100*cumsum(n_total))/cumsum(n_roster),2))) %>% 
  ungroup() %>% 
  adorn_totals("row",,,,-completion_rate_rolling) %>% 
  mutate_at(vars(ends_with("_rate")), funs(ifelse(row_number() == n(), round(100*(n_total/n_roster), 2), .)))

sane_all_teams <- sane_all_teams %>% filter(distribution_no != "-")

# CSV RESULTS TO TEAMS ----------------------------------------------------
# TEAM-LEVEL
for(i in tm){
  t1 <- sane_team %>% filter(school == i) %>% 
    mutate(completion_rate = round(100*(n_total/n_roster), 2)) %>%
    mutate(completion_rate_rolling = case_when(distribution_no > 0 ~
    round((100*cumsum(n_total))/cumsum(n_roster),2))) %>%
    ungroup() %>%
    adorn_totals("row",,,,-completion_rate_rolling) %>% 
    mutate_at(vars(ends_with("_rate")), funs(ifelse(row_number() == n(), round(100*(n_total/n_roster), 2), .)))
 write.csv(t1, here("output/weekly_stats/",paste0(i, "_survey_stats_team_", Sys.Date(), ".csv")), row.names = F)
}


#PLAYER-LEVEL
for(i in tm){
x <- bind_rows((sane_all %>% select(school, studyID, nameLast, nameFirst, date2) %>% 
                  filter(school == i) %>%
                  group_by(school, studyID, nameLast, nameFirst) %>% 
                  summarise_all(list(minDate = min, maxDate = max, n_total = length))),
               
               (playerInfo %>% select(school, studyID, nameLast, nameFirst) %>% 
                  filter(school == i)))

x2 <- x %>% group_by(school, studyID, nameLast, nameFirst) %>% 
  summarise_each(funs(max(., na.rm = TRUE))) %>% 
  mutate(across(.cols = n_total, ~ ifelse(is.infinite(.x), 0, .x))) %>% 
  arrange(., desc(n_total))

write.csv(x2, here("output/weekly_stats/",paste0(i, "_survey_stats_player_", Sys.Date(), ".csv")), row.names = F)
}
                                                