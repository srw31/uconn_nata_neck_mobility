# players <- c("S106")

teams <- readRDS("C:/Users/vegas/Desktop/R Projects/uconn_nata_neck_mobility/data_files/playerInfo.rds")
for (i in unique(teams$school)){
  assign(paste0("ids_", i), as.vector(t(teams %>% filter(school == i) %>% distinct(studyID))))
}

for (p in ids_SHU) {
  rmarkdown::render(input = "code/generateReports_r_l_avg_only.Rmd",
                    params = list(ID = p),
                    output_file= paste0("../output/player_reports/shu/dom_nondom/report_",p, ".pdf"))
}


