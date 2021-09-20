## HQ Checks
rm(list = ls())

library(tidyverse)
library(openxlsx)
library(cleaninginspectoR)
library(compareDF)
library(readxl)
library(cluster)
library(plyr)

# source
source("./R/check_time.R")
source("./R/data_falsification.R")
source("./R/item_boxplots.R")
source("./R/minimum_standards.R")
source("./R/check_log.R")

# load all sheets
data <-"./input/REACH_RCA_Base_de_données_Suivi_des_marchés_Aoüt_2021.xlsx"

sheets <- openxlsx::getSheetNames(data)
SheetList <- lapply(sheets,openxlsx::read.xlsx,xlsxFile=data)
names(SheetList) <- sheets

clean <- SheetList[[2]]
clean_log <- SheetList[[3]]
del <- SheetList[[4]]
tool <- SheetList[[13]]

# check falsification
false.data <- calculateDifferences(clean, tool) %>% filter(number.different.columns < 5)
write.xlsx(false.data, paste0("./output/similar survyes_",Sys.Date(),".xlsx"))

# check min standards
min.stand.price <- minium_standards_prices(clean, "q0_7_localite", ends_with("_prix")) %>% filter(minimum_standards == "requirement not met")
write.xlsx(min.stand.price, paste0("./output/minimum standards_",Sys.Date(),".xlsx"))

# issues
issues <- inspect_all(clean)

# %>% filter(!is.na(index))

clean_red <- select(clean, c("uuid", "index"))
issues <- left_join(issues, clean_red, "index")

there <- anti_join(issues, clean_log, "uuid")
write.xlsx(there, paste0("./output/CAR1903_JMMI_Outliers Check_",Sys.Date(),".xlsx"))

# check deletion log
check <- semi_join(clean, del, "uuid")

del$uuid %in% clean$uuid

# check log application
clean_log <- clean_log %>% filter(question.name %in% names(clean))
clean_log <- clean_log %>% filter(uuid %in% clean$uuid)

log.c <- check_log(clean, clean_log, variable = "question.name", old_log_var = "old.value", new_log_var = "new.value") %>%
                    mutate(., check = ifelse(new.value == value_extracted, "Log applied correctly", "Please check log")) %>%
                    filter(check == "Please check log")

write.xlsx(log.c, paste0("./output/check cleaning log_",Sys.Date(),".xlsx"))






### analysis
meds <- clean %>% select(q0_7_localite, mais_350g_prix, manioc_500g_prix, riz_500g_prix, haricot_500g_prix, arachide_150g_prix,
                         viande_prix, huile_vegetale_prix, savon_200g_prix, moustiquaire_prix, bidon_20l_prix,
                         drap_prix, pagne_6yards_prix, natte_prix, bache_prix, marmite_5l_prix, cuvette_30l_prix,
                         sucre_200g_prix, sel_150g_prix, theiere_prix, seau_20l_prix, bois_chauffage_prix, essence_prix, eau_20l_prix)

sapply(meds, class)


maiz_med <- meds %>% group_by(q0_7_localite) %>% summarise_all(~median(., na.rm = TRUE))
nat_meds <- maiz_med %>% summarise_all(~median(., na.rm = TRUE))
nat_meds[1,1] <- "national"

meds_final <- rbind(maiz_med, nat_meds)

write.xlsx(meds_final, paste0("./output/Medians Check Localite_",Sys.Date(),".xlsx"))

## check boxplot
plot <- item_boxplots_formatted(meds, "q0_7_localite", ends_with("_prix"))
plot
