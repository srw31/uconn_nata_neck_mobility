# PULL CONTACT INFO OVER TO TEXT NON-RESPONDERS

pacman::p_load(here, lubridate, tidyverse, readr, janitor)

wd <- here()
texts_to_send_04202023 <- read_csv("data_files/text_followup/sane_to_text_04202023.csv") %>% clean_names()
playerInfo <- readRDS("C:/Users/vegas/Desktop/R Projects/uconn_nata_neck_mobility/data_files/playerInfo.rds") 

email_links_texts_04202023 <- read_csv("data_files/text_followup/sane_email_links_texts_04202023.csv") %>% clean_names()
email_links_texts_04202023 <- email_links_texts_04202023 %>% select(last_name, first_name, link, email)

text_ids <- texts_to_send_04202023 %>% left_join(email_links_texts_04202023, by = c("last_name", "first_name", "email_address" = "email", "link"))

text_ids <- text_ids %>% left_join(playerInfo, by = c("last_name" = "nameLast", "first_name" = "nameFirst", "email_address" = "email")) %>% 
  select(c(last_name, first_name, link, phone)) %>% filter(!is.na(phone))

write_csv(text_ids, file = paste0(wd,"/data_files/player_texts_for_", Sys.Date(), ".csv"))
