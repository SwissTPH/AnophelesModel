#################################
# Example script for downstream analysis using the postprocessed simulation outputs
#
# 10.11.2022
# monica.golumbeanu@unibas.ch
#################################
library(RSQLite)
library(magrittr)
library(dplyr)
library(ggplot2)

# Define database file path
db_exp_KEN = "/scicore/home/pothin/golmon00/OpenMalaria/AnophelesModel/KEN.sqlite"
db_exp_PNG = "/scicore/home/pothin/golmon00/OpenMalaria/AnophelesModel/PNG.sqlite"
scens_KEN = readRDS("/scicore/home/pothin/golmon00/OpenMalaria/AnophelesModel/KEN/cache/scenarios.rds")
scens_PNG = readRDS("/scicore/home/pothin/golmon00/OpenMalaria/AnophelesModel/PNG/cache/scenarios.rds")

# Read the results table from the database:
# open database connection:
conn = DBI::dbConnect(RSQLite::SQLite(), db_exp_KEN)
# extract the results table, this should have the same name as provided to slurmPrepareResults
all_simul_KEN = dbReadTable(conn, "om_results")
# always disconnect from the database
DBI::dbDisconnect(conn)

# open database connection:
conn = DBI::dbConnect(RSQLite::SQLite(), db_exp_PNG)
# extract the results table, this should have the same name as provided to slurmPrepareResults
all_simul_PNG = dbReadTable(conn, "om_results")
# always disconnect from the database
DBI::dbDisconnect(conn)

# Merge the processed results with the scenario metadata
scens_KEN$scenario_id = scens_KEN$ID
all_simul_merged_KEN = merge(all_simul_KEN, scens_KEN, by = c("scenario_id"))
all_simul_merged_KEN$year = format(as.Date(all_simul_KEN$date), format = "%Y")
all_simul_merged_KEN$month = format(as.Date(all_simul_KEN$date, format="%Y-%m-%d"),"%m")

scens_PNG$scenario_id = scens_PNG$ID
all_simul_merged_PNG = merge(all_simul_PNG, scens_PNG, by = c("scenario_id"))
all_simul_merged_PNG$year = format(as.Date(all_simul_PNG$date), format = "%Y")
all_simul_merged_PNG$month = format(as.Date(all_simul_PNG$date, format="%Y-%m-%d"),"%m")

# Aggregate results by setting, EIR, age group and year
# calculate prevalence
all_simul_aggr_KEN = all_simul_merged_KEN %>%
                  group_by(setting, EIR, age_group, date) %>%
                  summarise(mean_prev = mean(prevalenceRate), sd_prev = sd(prevalenceRate))

all_simul_aggr_PNG = all_simul_merged_PNG %>%
  group_by(setting, EIR, age_group, date) %>%
  summarise(mean_prev = mean(prevalenceRate), sd_prev = sd(prevalenceRate))

# Plot the prevalence for one age group and EIR
plot_df_KEN = all_simul_aggr_KEN[which(all_simul_aggr_KEN$age_group == "0-100"),]
plot_df_PNG = all_simul_aggr_PNG[which(all_simul_aggr_PNG$age_group == "0-100"),]

plot_df = rbind.data.frame(plot_df_KEN, plot_df_PNG)
levels(plot_df$setting) <- c(levels(plot_df$setting), "Kenya", "Papua New Guinea")
plot_df[which(plot_df$setting == "KEN"), "setting"] = "Kenya"
plot_df[which(plot_df$setting == "PNG"), "setting"] = "Papua New Guinea"

plot_df$setting = factor(plot_df$setting, levels = c("Papua New Guinea", "Kenya"))

ggplot(plot_df, aes(x = as.Date(date), y = mean_prev*100, color = setting)) +
  theme_light() + theme_linedraw() + theme_bw(base_size = 16) +
  geom_ribbon(aes(ymin = (mean_prev - sd_prev)*100, ymax = (mean_prev - sd_prev)*100 + 1,
                  fill = setting, alpha = 0.5, color = NULL), show.legend = FALSE) +
    geom_vline(xintercept = as.Date("2000-01-01"), color = "black", linetype = "dashed") +
  labs(x = "Time", y="Prevalence (%)") +
  scale_color_manual(values = c("#74c476", "#c51b8a")) +
    scale_fill_manual(values = c("#74c476", "#c51b8a")) +
  labs(color = "Setting") + theme(legend.position = "top") +
  geom_line()

ggsave("~/paper_AnophelesModel/Figures/Fig4.pdf", width = 8, height = 4)

