This repository serves to showcase my capabilities in R coding and small to medium code management. The code originated from a project where some colleagues and I analyzed data from the pharmaceutical industry from Denmark and Germany, between 2009 and 2021, reporting a comparison and evaluation of their performance. While my colleagues provided me the data, I was in charge of gathering, cleaning, analyzing and visualizing the data, controlling the quality of the output that would be used in said project.
The data with which we assessed both industries contains information about their respective CPI's, number of employees, number of firms and their sizes, population, and wages, all within the previously mentioned years. 
The cleaning, correction and visualization of the data was done in R Studio, but for the purpose of this repository, I will be using this R markdown to demonstrate my abilities in R coding and data management:

These libraries were used in the project:
```{r}
# Load necessary libraries
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
```

To load the data and correct the variable names, eliminating inconsistencies for an easier mergeing:
```{r}
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
```	
