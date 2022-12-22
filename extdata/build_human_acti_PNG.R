####################################################
# Create human activity table for Anopheles Farauti
#
# 25.11.2022
# monica.golumbeanu@unibas.ch
####################################################

human_act_tab = NULL
human_act_tab$species = rep("Homo sapiens", 32)
human_act_tab$country = rep("Papua New Guineea", 32)
human_act_tab$site = rep("Mugil", 32)
human_act_tab$hour = rep(c("16:00", "17:00", "18:00", "19:00", "20:00", "21:00", 
                      "22:00", "23:00", "00:00", "01:00", "02:00", "03:00",
                      "04:00", "05:00", "06:00", "07:00"), 2)


humans_ind = c(0, 0, 0.15, 0.47, 0.83, 0.96, 1, 1, 1, 1, 1, 1, 1, 0.53, 0.03, 0)
humans_bed = c(0, 0, 0.15, 0.47, 0.83, 0.92, 0.96, 0.96, 0.96, 0.96, 0.96, 0.96, 0.96, 0.88, 0, 0)

human_act_tab$sampling = c(rep("IND", 16), rep("BED", 16))

human_act_tab$value = c(humans_ind, humans_bed)

human_act_tab = as.data.frame(human_act_tab)

write.csv(human_act_tab, "~/GitRepos/anophelesmodel/paper_scripts/PNG_human_patterns.csv", row.names = FALSE, quote = FALSE)



