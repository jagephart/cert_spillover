# TITLE: case_data_pull
# DATE: 31-Mar-20
# AUTHOR: Jessica
#____________________________________________________________________________________________________________________#
# Load packages
#____________________________________________________________________________________________________________________#
library(tidyverse)

#____________________________________________________________________________________________________________________#
# Load and clean data
#____________________________________________________________________________________________________________________#
df <- read.csv("/nfs/jgephart-data/all_annual_liveweight.csv")
df$Commodity.Code <- as.character(df$Commodity.Code)

df <- df %>%
  select(-X) %>%
  filter(Exporter != "World") %>%
  filter(Importer != "World")
#____________________________________________________________________________________________________________________#
# Set case study HS codes
#____________________________________________________________________________________________________________________#
Barents <- c("030250", "030251", "030352", "030360", "030363", "030471", "030551", "030562")
RossSea <- c("030268", "030283", "030362", "030383", "030422", "030446", "030455", "030485", "030412")
Bahamas <- c("030611", "030621", "030631", "030691", "160530")
Guyana <- c("030613", "030617", "030623", "030627", "030636", "030695", "160520", "160521", "160529")
Ghana <- c("030233", "030343", "030487", "160414")
Peru <- c("030242", "030554", "160416", "030563")

case_HS <- data.frame(case = c(rep("Barents", length(Barents)), rep("RossSea", length(RossSea)),
                               rep("Bahamas", length(Bahamas)), rep("Guyana", length(Guyana)),
                               rep("Ghana", length(Ghana)), rep("Peru", length(Peru))),
                      Commodity.Code = as.numeric(c(Barents, RossSea, Bahamas, Guyana, Ghana, Peru)))
case_HS$Commodity.Code <- as.character(case_HS$Commodity.Code)

#____________________________________________________________________________________________________________________#
# Pull codes for each case
#____________________________________________________________________________________________________________________#
df_out <- left_join(case_HS, df, by = "Commodity.Code")

# Check codes for each group
sum(!((df_out %>% filter(case == "Barents"))$Commodity.Code %in% as.character(as.numeric(Barents))))
sum(!(as.character(as.numeric(Barents)) %in% (df_out %>% filter(case == "Barents"))$Commodity.Code))

sum(!((df_out %>% filter(case == "RossSea"))$Commodity.Code %in% as.character(as.numeric(RossSea))))
sum(!(as.character(as.numeric(RossSea)) %in% (df_out %>% filter(case == "RossSea"))$Commodity.Code))

sum(!((df_out %>% filter(case == "Bahamas"))$Commodity.Code %in% as.character(as.numeric(Bahamas))))
sum(!(as.character(as.numeric(Bahamas)) %in% (df_out %>% filter(case == "Bahamas"))$Commodity.Code))

sum(!((df_out %>% filter(case == "Guyana"))$Commodity.Code %in% as.character(as.numeric(Guyana))))
sum(!(as.character(as.numeric(Guyana)) %in% (df_out %>% filter(case == "Guyana"))$Commodity.Code))

sum(!((df_out %>% filter(case == "Ghana"))$Commodity.Code %in% as.character(as.numeric(Ghana))))
sum(!(as.character(as.numeric(Ghana)) %in% (df_out %>% filter(case == "Ghana"))$Commodity.Code))

sum(!((df_out %>% filter(case == "Peru"))$Commodity.Code %in% as.character(as.numeric(Peru))))
sum(!(as.character(as.numeric(Peru)) %in% (df_out %>% filter(case == "Peru"))$Commodity.Code))

#____________________________________________________________________________________________________________________#
# Save dataframe in csv
#____________________________________________________________________________________________________________________#
write.csv(df_out, "case_trade_31Mar20.csv")

#____________________________________________________________________________________________________________________#
# Misc. code
#____________________________________________________________________________________________________________________#
df_US <- df %>%
  filter(Importer.ISO == "USA") %>%
  filter(Year %in% c(2010:2018))

US_summary <- df_US %>% 
  group_by(Exporter) %>%
  summarise(Total = sum(Max.Value))
