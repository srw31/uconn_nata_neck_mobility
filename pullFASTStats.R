

pacman::p_load(here, lubridate, tidyverse, readr, janitor, data.table)
here()
wd <- here()

# playerInfo <- read_csv("data_files/nata_neck_mobility_player_contact_list_rev3.csv")
# saveRDS(playerInfo, here("data_files/playerInfo.rds"))

playerInfo <- readRDS(here("data_files/playerInfo.rds"))

fast_results <- read_csv("C:/Users/vegas/Desktop/R Projects/uconn_nata_neck_mobility/data_files/fast_results.csv")

fast_results <- fast_results[-c(1:2),]
# fast_results <- fast_results %>% select(-c(RecipientLastName, RecipientFirstName))

# MERGE SCHOOL DATA TO fast OUTPUT
fast_all <- fast_results %>% left_join(playerInfo, by = c("RecipientEmail" = "email"))

fast_all <- fast_all %>% mutate(date2 = as.Date(StartDate)) %>% 
  filter(Status == "IP Address")

# fast_all %>% group_by(school) %>% tally()

tm <- unique(fast_all$school)

# RESULTS BY TEAM ---------------------------------------------------------
fast_team <- fast_all %>% select(school, date2) %>% 
            group_by(school) %>% 
            summarise_all(list(minDate = min, maxDate = max,
                               n_total = length))


fast_team <- fast_team %>% mutate(n_roster = case_when(school == "HPU" ~ 22,
                                      school == "NGU" ~ 23,
                                      school == "SHU" ~ 12, # UPDATED 3/21 TO REMOVE LONGO (ORIG: 13)
                                      school == "UConn" ~ 23,
                                      school == "WFU" ~ 17, 
                                      school == "Winthrop" ~ 13, # UPDATED 4/20 TO REMOVE SHEPHERD/KEUP (TJ) (ORIG: 15)
                                      TRUE ~ 11))

fast_team_rates <- fast_team %>% 
  mutate(completion_rate = round(100*(n_total/n_roster), 2)) %>% adorn_totals("row",,,,-completion_rate) %>% 
  mutate_at(vars(ends_with("_rate")), funs(ifelse(row_number() == n(), round(100*(n_total/n_roster), 2), .))) %>% 
  select(c(school, n_total, completion_rate))
  
                                  
write.csv(fast_team_rates, here("output/weekly_stats/", paste0("all_sites_fast_stats_", Sys.Date(), ".csv")), row.names = F)



# PULL NAMES FOR NONRESPONSE ----------------------------------------------

fast_followup <- read_csv("C:/Users/vegas/Desktop/R Projects/uconn_nata_neck_mobility/data_files/fast_to_text_04202023.csv") %>% 
  select(`Last Name`, `First Name`,`Email Address`)

players_no_fast <- fast_followup %>% left_join(playerInfo, by = c("Last Name" = "nameLast", "First Name" = "nameFirst", "Email Address" = "email")) %>% 
  select(`Last Name`, `First Name`, `Email Address`, school)

# players_no_fast %>% group_by(school) %>% tally()

tm <- unique(players_no_fast$school)

for (i in tm){
  
  x <- players_no_fast %>%filter(school == i)
  write.csv(x, here("output/",paste0(i, "_fast_missing_", Sys.Date(), ".csv")), row.names = F)
}
