## ---------------------------
## Script: a001_apta_analysis.r
##
## Purpose: EDA of neck and shoulder ROM data for APTA abstract
##
## Author: Stuart Wallace [stuart.wallace@uconn.edu]
##
## Date Created: 2023-07-25
## Date(s) Revised: 2023-07-30 - cleaned up LAT variables, converted to numeric, added mean LAT variable
## ---------------------------
## Notes:
## ---------------------------
# SET UP WORKSPACE, LOAD PACKAGES & DATA ----------------------------------------------------
# if (!require("pacman")) install.packages("pacman", INSTALL_opts = "--no-multiarch")

pacman::p_load(here,
               janitor,
               tidyverse,
               readr,
               data.table,
               inspectdf,
               ggpubr,
               GGally,
               gtsummary)
here()
wd <- here()


# LOAD DATA ---------------------------------------------------------------
# aptaData <-
#   readRDS(here("data_files/all_sites_with_pro_corrected.Rds"))

aptaData_short <- readRDS(here("data_files/aptaData_short.Rds"))

# # CLEAN UP DATA -----------------------------------------------------------
# colnames(aptaData)
#
# # limit dataset to just what columns we need
# # create cohort variable (NE vs. SE)
# aptaData_short <-
#   aptaData %>% select(
#     ID,
#     Hand_Dominance,
#     Age,
#     Primary_Role,
#     starts_with("Injury"),
#     starts_with("Current"),
#     ends_with("_mean"),
#     starts_with("LAT")
#   ) %>%
#   mutate(cohort = case_when(str_detect(ID, pattern = "^N") ~ "NE", TRUE ~ "SE"))

aptaData_short <- aptaData_short %>% janitor::clean_names()
aptaData_short <- aptaData_short %>% 
  mutate_at(vars(hand_dominance, primary_role, starts_with("injury"),
                 ends_with("injury")), as.factor)

cat_vars <- colnames(select(aptaData_short, hand_dominance, primary_role, starts_with("injury"),
                    ends_with("injury")))

func_refactor <- function(x) forcats::fct_collapse(x, Y = c("1+", "21+", "7+", "Y", "y", "yes", "Yes", "YES"),
                                                   N = c("N", "No", "n", "NO", "no"),
                                                   R = c("r", "right", "Right", "RIGHT"),
                                                   L = c("l", "left", "Left", "LEFT"),
                                                   RP = c("RELIEVER", "Reliever", "reliever"),
                                                   SP = c("STARTER","Starter", "starter"))

aptaData_short <- aptaData_short %>%
           mutate_at(vars(cat_vars), ~ func_refactor(.))

lat_vars <- aptaData_short %>% select(all_of(starts_with("lat")), -lat_mean) %>% colnames()

aptaData_short <- aptaData_short %>%  
  mutate(lat_sum = sum(c_across(lat_1:lat_15)))

# save to RDS
saveRDS(aptaData_short, here(paste("data_files/aptaData_short.Rds")))

# EDA ---------------------------------------------------------------------
eda_cat <- inspectdf::inspect_cat(aptaData_short[,-1])
 inspectdf::show_plot(eda_cat)

eda_num_cohort <-
  aptaData_short %>% group_by(cohort) %>% inspectdf::inspect_num() %>%
  dplyr::mutate(across(where(is.numeric), round, 1))

eda_num_cohort %>% select(-pcnt_na, -hist) %>% kableExtra::kbl() %>%
  kableExtra::kable_classic(full_width = F, html_font = "Cambria")


# left
eda_num_elb_inj_l <-
  aptaData_short %>% select(!starts_with("shoulder")) %>% 
                              filter(hand_dominance == "L") %>% group_by(injury_history_elbow) %>% inspectdf::inspect_num() %>%
  dplyr::mutate(across(where(is.numeric), round, 1))

eda_num_elb_inj_l %>% select(-pcnt_na, -hist) %>% kableExtra::kbl() %>%
  kableExtra::kable_classic(full_width = F, html_font = "Cambria")

# right

eda_num_elb_inj_r <-
  aptaData_short %>% select(!starts_with("shoulder")) %>% 
  filter(hand_dominance == "R") %>% group_by(injury_history_elbow) %>% inspectdf::inspect_num() %>%
  dplyr::mutate(across(where(is.numeric), round, 1))

eda_num_elb_inj_r %>% select(-pcnt_na, -hist) %>% kableExtra::kbl() %>%
  kableExtra::kable_classic(full_width = F, html_font = "Cambria")

eda_num_gh_inj <-
  aptaData_short %>% group_by(injury_history_shoulder) %>% inspectdf::inspect_num() %>%
  dplyr::mutate(across(where(is.numeric), round, 1))

eda_num_gh_inj %>% select(-pcnt_na, -hist) %>% kableExtra::kbl() %>%
  kableExtra::kable_classic(full_width = F, html_font = "Cambria")


# create histograms of ROM values by cohort
# ne <-
#   aptaData_short %>% select(cohort, ends_with("_mean")) %>% filter(cohort == "NE") %>% inspectdf::inspect_num()
# inspectdf::show_plot(ne) + ggtitle("NE Cohort")

# correlations
# ne_corr <-
#   aptaData_short %>% select(cohort, ends_with("_mean")) %>% filter(cohort == "NE") %>% inspectdf::inspect_cor()
# inspectdf::show_plot(ne_corr)


# PLOTS FOR EDA -----------------------------------------------------------
plot_vars <- colnames(aptaData_short %>% select(ends_with("mean")))

for (i in plot_vars) {
  p <- ggpubr::ggboxplot(
    aptaData_short,
    "cohort",
    i,
    color = "cohort",
    palette = "aaas",
    shape = "cohort",
    orientation = "horizontal"
  ) +
    ggtitle(paste(i, "by Cohort"))
  
  png(here("output/apta_plots", paste0("hbox_cohort_", i, ".png")))
  print(p)
  dev.off()
}

# correlations
# Injury History - Shoulder
aptaData_short %>% na.omit() %>% filter(injury_history_shoulder == "Y") %>% 
  select(
  #cohort,
  hand_dominance,
  starts_with("neck"),
  starts_with("cf"),
  lat_sum 
  #shoulder_ir_right_mean,
  #shoulder_ir_left_mean
) %>% 
  GGally::ggpairs(.,             # Data frame
                  upper = list(continuous = wrap("cor", method = "spearman")),
                  columns = 2:10,       # Columns
                     aes(color = hand_dominance,  # Color by group (cat. variable)
                         alpha = 0.5)
                  )

# Injury History - Elbow | LHP
aptaData_short %>% na.omit() %>% filter(hand_dominance == "L") %>% 
  select(
    #cohort,
    injury_history_elbow,
    starts_with("neck"),
    starts_with("cf"),
    lat_sum 
    #shoulder_ir_right_mean,
    #shoulder_ir_left_mean
  ) %>% 
  GGally::ggpairs(.,             # Data frame
                  upper = list(continuous = wrap("cor", method = "spearman")),
                  columns = 2:10,       # Columns
                  aes(color = injury_history_elbow,  # Color by group (cat. variable)
                      alpha = 0.5)
  )

# Injury History - Elbow | RHP
aptaData_short %>% na.omit() %>% filter(hand_dominance == "R") %>% 
  select(
    #cohort,
    injury_history_elbow,
    starts_with("neck"),
    starts_with("cf"),
    lat_sum 
    #shoulder_ir_right_mean,
    #shoulder_ir_left_mean
  ) %>% 
  GGally::ggpairs(.,             # Data frame
                  upper = list(continuous = wrap("cor", method = "spearman")),
                  columns = 2:10,       # Columns
                  aes(color = injury_history_elbow,  # Color by group (cat. variable)
                      alpha = 0.5)
  )

# CREATE TABLE 1 ----------------------------------------------------------
t <- aptaData_short %>% select(cohort, ends_with("_mean")) %>%
  
  # tbl_strata(
  #   strata = cohort,
  #   ~.x %>%
  tbl_summary(
    by = cohort,
    type = all_continuous() ~ "continuous2",
    #statistic = list(all_continuous() ~ "{mean} ({sd}), {median} ({p25}, {p75})",
    statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
    #"{median} ({p25}, {p75})",
    all_categorical() ~ "{n} / {N} ({p}%)",
    digits = all_continuous() ~ 2,
    missing_text = "(Missing)"
  ) %>%
  
  #as_gt() %>%
  modify_caption("Neck and Shoulder ROM By Cohort") %>%
  #gt::tab_options(table.font.size = "small")
  gtsummary::as_flex_table() %>%
  flextable::save_as_docx(t, path = file.path(here(
    "output/apta_plots/table1_by_cohort.docx"
  )))
#gt::gtsave(t, file = file.path(here("output/apta_plots"), "table1_by_cohort.docx"))



# CREATE LIKERT BAR PLOTS -----------------------------------------------------
# Pull only the FAST data
se_data <- readRDS(here("data_files/se_data_raw.Rds"))
ne_data <- readRDS(here("data_files/ne_data_raw.Rds"))

ne_data_fast <-
  ne_data %>% select(ID, all_of(starts_with("FAST"))) %>%
  mutate(across(FAST_1:FAST_P_9, ~ str_replace_na(.x)))

se_data_fast <-
  se_data %>% select(ID, all_of(starts_with("FAST"))) %>%
  mutate(across(FAST_1:FAST_P_9, ~ str_replace_na(.x)))

ne_data_fast <-
  ne_data_fast %>% mutate(across(
    FAST_1:FAST_P_9,
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

se_data_fast <-
  se_data_fast %>% mutate(across(
    FAST_1:FAST_P_9,
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


ne_data_fast$cohort <- "NE"
se_data_fast$cohort <- "SE"

all_sites_fast <- bind_rows(ne_data_fast, se_data_fast)

fast_p <- all_sites_fast %>% select(starts_with("FAST_P")) %>%
  mutate_at(vars(starts_with("FAST")), as.factor)


x <- inspectdf::inspect_cat(fast_p)
x %>% inspectdf::show_plot()

###
fast_p <- aptaData %>% select(starts_with("FAST_P")) %>%
  mutate_at(vars(starts_with("FAST")), as.factor)
f_short <- fast_p[, c(2, 9)]

f_short$FAST_P_2 <- factor(f_short$FAST_P_2,
                           levels = c("1", "2", "3", "4", "5"),
                           ordered = TRUE)
f_short$FAST_P_9 <- factor(f_short$FAST_P_9,
                           levels = c("1", "2", "3", "4", "5"),
                           ordered = TRUE)

f_short <- as.data.frame(f_short)

likert_results <- likert::likert(fast_p)
plot(likert_results, type = "bar")
