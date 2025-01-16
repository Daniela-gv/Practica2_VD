library(dplyr)
library(tidyr)
library(unheadr)
library(tibble)
library(purrr)
library(janitor)
library(openxlsx)
library(xlsx)
library(hablar)
library(readr)
library(naniar)
library(r2d3)
library(countrycode)
library(stringr)



setwd("C:/Users/Dana/Documents/UOC/Dades/VD/datasets")

# Gestió dataset mortalitat 

morts <- read.csv("MORT_100.csv",header = F)

morts_merge <- unbreak_rows(morts, "^20", V2, sep = "_")
morts_merge <- morts_merge %>%
  row_to_names(row_number = 1)
morts_pivot <- morts_merge %>%
  pivot_longer(cols = -1, 
               names_to = c("Year",".value"), 
               names_sep = "_")
morts_pivot <- morts_pivot %>%
  mutate("Country" = `_Countries, territories and areas`) %>%
  select(-`_Countries, territories and areas`)
morts_pivot <- morts_pivot[, c(5,c(1:4))]
colnames(morts_pivot) <- c("Country", "Year","deaths(0-27d)", "deaths(1-59m)", "deaths(0-4y)" )
class(morts_pivot$`deaths(0-27d)`)
morts_pivot <- morts_pivot %>%
  convert(hablar::num("Year","deaths(0-27d)", "deaths(1-59m)", "deaths(0-4y)")) 
morts_pivot <- morts_pivot %>%
  mutate("deaths(<4y)" = rowSums(morts_pivot[,3:5]))

# Gestió dataset ART

aditional_ds <- read.csv("HIV_0000000011,HIV_0000000023,HIV_0000000024.csv", encoding = "UTF-8")
aditional_ds <- aditional_ds %>%
  filter(Year >= 2000 & Year <= 2021) %>%
  replace_with_na_all(condition = ~.x == "No data") 
colnames(aditional_ds) <- c("Country", "Year", "est_ART_coverage_ped", "est_needing_ART_ped", "rep_receiving_ART")
aditional_ds <- aditional_ds %>%
  mutate("Year" = as.numeric(Year))

 # Unió datasets mort-ART

m_art <- full_join(morts_pivot, aditional_ds, by = c("Country", "Year"))

m_art <- m_art %>%
  separate_wider_delim(est_ART_coverage_ped, names = c("est_ART_coverage_ped", "est_ART_coverage_ped[range]"), delim = " ")

# Gestió dataset fussionat (WHO-UNICEF)

epi_HIV <- openxlsx::read.xlsx("HIV_Epidemiology_Children_Adolescents_2024.xlsx", sheet = 2)
epi_HIV <- epi_HIV %>%
  row_to_names(row_number = 1)

table(epi_HIV$Indicator)

# Seleccio de indicadors:
EIR_HIV <- epi_HIV %>%
  filter(Indicator == "Estimated incidence rate (new HIV infection per 1,000 uninfected population)" & 
           Age == "Age 0-14") %>%
  select(ISO3, "Country" = "Country/Region", "Region" = "UNICEF Region", Year, Sex, "EIR" = "Value", Lower, Upper)
table(EIR_HIV$Sex)

EMCTR_HIV <- epi_HIV %>%
  filter(Indicator == "Estimated mother-to-child transmission rate (%)") %>%
  select(ISO3, "Country" = "Country/Region", "Region" = "UNICEF Region", Year, "mTOcTR" = "Value", Lower, Upper)
  
  
living_est <- epi_HIV %>%
  filter(Indicator == "Estimated number of adolescents and/or young people living with HIV") %>%
  select(ISO3, "Country" = "Country/Region", "Region" = "UNICEF Region", Year, Sex, Age, "living_HIV" = "Value", Lower, Upper)


nRelDeaths <- epi_HIV %>%
  filter(Indicator == "Estimated number of annual AIDS-related deaths")%>%
  select(ISO3, "Country" = "Country/Region", "Region" = "UNICEF Region", Year, Sex, Age, "death_HIV" = "Value", Lower, Upper)

  
pLiv <- epi_HIV %>%
  filter(Indicator == "Estimated number of people living with HIV" &
           Age == "Age 0-4" & 
           Sex == "Both") %>%
  select(ISO3, "Country" = "Country/Region", "Region" = "UNICEF Region", Year,"livingHIV04" = "Value", Lower, Upper)
pLv14 <- epi_HIV %>%
  filter(Indicator == "Estimated number of people living with HIV" &
           Age == "Age 0-4" & 
           Sex == "Both") %>%
  select(ISO3, "Country" = "Country/Region", "Region" = "UNICEF Region", Year,"livingHIV014" = "Value", Lower, Upper)

newInf <- epi_HIV %>%
  filter(Indicator == "Estimated number of annual new HIV infections" & Age == "Age 0-4" & Sex == "Both") %>%
  select(ISO3, "Country" = "Country/Region", "Region" = "UNICEF Region", Year,"infec_HIV" = "Value", Lower, Upper)

Death_rate <- epi_HIV %>% 
  filter(Indicator == "Estimated rate of annual AIDS-related deaths (per 100,000 population)" & 
         Age == "Age 0-14" &
          Sex == "Both") %>%
  select(ISO3, "Country" = "Country/Region", "Region" = "UNICEF Region", Year, "death_rate" = "Value",  Lower, Upper)


# Unió datasets: 

pl4_nI <- full_join(pLiv[,0:5],newInf[,0:5], by = c("ISO3", "Country", "Region", "Year")) 
pl4nI_MC <- full_join(pl4_nI, EMCTR_HIV[,0:5], by = c("ISO3", "Country", "Region", "Year"))
pl4nI_MC <- pl4nI_MC %>%
  mutate("Year" = as.numeric(Year))

dimpa <- full_join(pl4nI_MC, m_art[,c(1,2,6,10)], by = c("Country", "Year"))

# Adaptació del dataset final:

dimpa <- map_df(dimpa, ~ gsub("<","", .x))

dimpa <- dimpa %>%
  mutate(ISO3 = ifelse(is.na(ISO3),countrycode(Country, origin = 'country.name', destination = 'iso3c'), ISO3))

dimpa2 <- map_df(dimpa, ~ gsub(",","", .x))

dimpa2 <- dimpa2 %>%
  convert(hablar::num("livingHIV04","infec_HIV", "mTOcTR", "deaths(<4y)", "rep_receiving_ART"))

dimpa3 <- dimpa2 %>%
  filter(!grepl("UNICEF ",Country))

dimpa3$Country <- gsub(r"{\s*\([^\)]+\)}","",dimpa3$Country)
dim(table(dimpa2[is.na(dimpa2$Region),]$Country))
print(dimpa3[which(grepl("[(]", dimpa3$Country)),], n= 119)

# Unifico regions amb dataset WHO Regions (https://ourworldindata.org/grapher/who-regions)

regions_name <- read.csv("C:/Users/Dana/Documents/UOC/Dades/VD/datasets/who-regions.csv")
regions_name <- map_df(regions_name, ~ gsub(r"{\s*\([^\)]+\)}","", .x))
colnames(regions_name)
regions_name <- regions_name %>%
  select("ISO3" = "Code", "Region2" = "World.regions.according.to.WHO")


dimpa4 <- full_join(dimpa3, regions_name, by = "ISO3")

# Genero el datasset semi-final
write.xlsx(dimpa4, "dataset_complertR.xlsx")

# Elimino manualment àreas fussionades 
dimpa4.5 <- read.xlsx("dataset_complertRr.xlsx", sheetIndex = 1)
dimpa5 <- dimpa4[,3:9] %>%
  filter(!is.na(Region2))

dimpa6<- dimpa5 %>% 
  pivot_wider(names_from ='Region2',
              names_glue = "{Region2}_{.value}",
                          values_from = c(3:7),
                          values_fn = function(x) sum(x, na.rm = TRUE))
dimpa6 <- dimpa6 %>% filter(Year %in% c(2000:2021))

# Taxa de transmissio en % [valors més grans de 100?] - considero error i elimino digits sobrants al davant coerencia amb la resta de dades
dimpa4.5 <- dimpa4.5 %>%
  mutate(mTOcTR = ifelse(mTOcTR > 1000, str_sub(mTOcTR, 3, 6), ifelse(mTOcTR > 100, str_sub(mTOcTR, 2, 5), mTOcTR)))
dimpa4.5$mTOcTR <- as.numeric(dimpa4.5$mTOcTR)
dimpa7 <- dimpa4.5 %>%
  group_by(Region2, Year)%>%
  mutate(avg = mean(na.omit(mTOcTR)))
dimpa7$avg <- round(dimpa7$avg, 1)

# Dataset final 
xlsx::write.xlsx(as.data.frame(dimpa7), "dataset_complertRr12.xlsx", row.names = F)





