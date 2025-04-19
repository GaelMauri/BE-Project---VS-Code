
#Library Loading
.libPaths("C:/Users/Usuario/AppData/Local/R/win-library/4.2")
library(wooldridge) 
library(fixest)
library(tidyverse)
library(here)
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(writexl)
library(plotly)
library(htmlwidgets)
library(languageserver)
library(ViewPipeSteps)

#Dataset Loading
Sales <- read_excel("Datasets/EUSales.xlsx")
Pop <- read_excel("Datasets/DKDEPopulation.xlsx")
CPI <- read_excel("Datasets/DKDECPI.xlsx")
Firms <- read_excel("Datasets/DKDEFirms.xlsx")
Size <- read_excel("Datasets/DKDESize.xlsx")
Employees <- read_excel("Datasets/DKDEEmployees.xlsx")
Wage <- read_excel("Datasets/DKDEWage.xlsx")

#Data Name Simplification
names(Sales)[names(Sales) == "time_period"] <- "Year"
names(Sales)[names(Sales) == "value"] <- "CapitaTurnover"
names(Sales)[names(Sales) == "country"] <- "Country"
names(Pop)[names(Pop) == "...1"] <- "Country"
names(CPI)[names(CPI) == "...1"] <- "Country"
names(Firms)[names(Firms) == "TIME"] <- "Country"
names(Employees)[names(Employees) == "...1"] <- "Country"
names(Wage)[names(Wage) == "TIME"] <- "Country"

#Data Cleaning and Correction
Sales <- Sales %>%
  filter(Year != 2022,
         Country %in% c("Germany","Denmark"),
         drug_class == "Total", 
         unit_of_measure == "US dollars per person, PPP converted") %>%
         select(-measure, -unit_of_measure, -drug_class)

Pop <- Pop %>%
  pivot_longer(cols = -Country, 
               names_to = "Year", 
               values_to = "Population")

CPI <- CPI %>%
  pivot_longer(cols = -Country, 
               names_to = "Year", 
               values_to = "CPI")
CPI$CPI[CPI$Country == "Germany(2020)"] <- round(CPI$CPI[CPI$Country == "Germany(2020)"] * 100/94.5, 2)
CPI <- CPI %>%
  mutate(Country = str_remove(Country, "\\(.*\\)"))

Employees <- Employees %>%
  pivot_longer(cols = -Country, 
               names_to = "Year", 
               values_to = "Employees")
Employees <- Employees %>%
  mutate(Year = ifelse(Year == 20180, 2018, Year))

Firms <- Firms %>%
  pivot_longer(cols = -Country, 
               names_to = "Year", 
               values_to = "Firms")

Wage <- Wage %>% 
  mutate_all(as.character) %>%
  pivot_longer(cols = -Country, 
               names_to = "Year", 
               values_to = "Wage")
Wage$Wage <- as.numeric(Wage$Wage)

Turnover <- merge(Sales, Pop, by = c("Country", "Year"), all= TRUE)
Turnover <- merge(Turnover, CPI, by = c("Country", "Year"), all= TRUE)
Turnover$Turnover <- round((Turnover$CapitaTurnover*100/Turnover$CPI * Turnover$Population)/1000000000, 2) 
Turnover$CapitaTurnover <- round(Turnover$CapitaTurnover*100/Turnover$CPI, 2)

Data <- merge(Turnover, Employees, by = c("Country", "Year"), all= TRUE)
Data <- merge(Data, Firms, by = c("Country", "Year"), all= TRUE)
Data <- merge(Data, Wage, by = c("Country", "Year"), all= TRUE)
rm(Sales, Pop, CPI, Employees, Firms, Turnover, Wage)
Data <- Data %>%
  group_by(Country) %>%
  mutate(Inflation = CPI - lag(CPI)) %>%
  ungroup()

Data$Wage <- round((Data$Wage*1000000/Data$Employees)*100/Data$CPI, 2)


#Data Vector Classification
Data <- Data %>%
  mutate(across(c(Year, Country), 
         as.character),
         across(c(CapitaTurnover, Population, CPI, Turnover, Employees, Firms, Wage, Inflation),
         as.numeric))

Size <- Size %>%
  mutate(across(c(Year, Country, Size), 
         as.character),
         across(c(FirmsBySize), 
         as.numeric))

#Number to Index Value
DataChange <- Data %>%
  group_by(Country) %>%
  mutate(ChangeinCapitaTurnover = round((CapitaTurnover-lag(CapitaTurnover))*100/lag(CapitaTurnover),2),
         ChangeinPopulation = round((Population-lag(Population))*100/lag(Population),2),
         ChangeinTurnover = round((Turnover-lag(Turnover))*100/lag(Turnover),2),
         ChangeinEmployees = round((Employees-lag(Employees))*100/lag(Employees),2),
         UnitChangeinFirms = Firms-lag(Firms),
         ChangeinFirms = round((Firms-lag(Firms))*100/lag(Firms),2),
         ChangeinWage = round((Wage-lag(Wage))*100/lag(Wage),2)) %>%
  ungroup() %>%
  select(-CPI)

SizeChange <- Size %>%
  arrange(Size)%>%
  group_by(Country) %>%
  mutate(ChangeinFirmsBySize = round((FirmsBySize-lag(FirmsBySize))*100/lag(FirmsBySize),2),
         ChangeinFirmsBySize = ifelse(is.infinite(ChangeinFirmsBySize), NA, ChangeinFirmsBySize))

ls()

View(Data)
