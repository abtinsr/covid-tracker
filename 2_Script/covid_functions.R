######################################################
##### PACKAGES
######################################################
# Data fetching:
library(httr)
library(jsonlite)
library(RCurl)

#Data organizing:
library(magrittr)
library(stringr)
library(tidyr)
library(dplyr)
library(data.table)
library(reshape2) # TURN COLUMNS/FIELDS INTO ROWS - SWITCH POSITION WITH RESHAPE2
library(lubridate) # FIX THE RUBBISH DATES INTO PROPER ONES WITH LUBRIDATE

######################################################
##### IMPORTING AND EXPORTING DATA
######################################################
importCovidData <- function(url) {
  rawdata <- getURL(url)
  csvdata <- read.csv(text = rawdata)
  csvdata %<>% 
    mutate_each(funs(as.character), c(Province.State, 
                                      Country.Region))
  return(csvdata)
}


exportCovidData <- function() {
  write.csv(confirmed_table, "Corona_ConfirmedCases_PerDay.csv")
  write.csv(deaths_table, "Corona_Deaths_PerDay.csv")
}

######################################################
##### GET REGIONAL DATA
######################################################
getRegionalData <- function() {
  regions <- 
    getURL("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")
  
  table_regions <- read.csv(text = regions)
  
  colnames(table_regions)[1] <- "Country" 
  
  table_regions %<>% 
    mutate_each(funs(as.character), c(Country, 
                                      alpha.2,
                                      alpha.3,
                                      iso_3166.2,
                                      region,
                                      sub.region,
                                      intermediate.region))
  return(table_regions)
}

######################################################
##### GET POPULATION DATA
######################################################
getPopulationData <- function() {
  table_populations <- 
    read.csv("1_Data/worldBank_population.csv", 
             fill = TRUE, 
             header = TRUE,
             stringsAsFactors = TRUE,
             sep = ";",
             comment.char = "")
  
  table_populations %<>% 
    select(c(Country.Name, Country.Code, X2018))
  
  colnames(table_populations)[1] <- "Country" 
  colnames(table_populations)[2] <- "alpha.3" 
  colnames(table_populations)[3] <- "Population_2018" 
  
  table_populations %<>% 
    mutate_each(funs(as.character), c(Country, 
                                      alpha.3))
  
  return(table_populations)
}

######################################################
##### GET DENSITY DATA
######################################################
getDensityData <- function() {
  table_density <- 
    read.csv("1_Data/worldBank_popDensity.csv", 
             fill = TRUE, 
             header = TRUE,
             stringsAsFactors = TRUE,
             sep = ";",
             comment.char = "")
  
  table_density %<>% 
    select(c(Country.Name, Country.Code, X2018))
  
  colnames(table_density)[1] <- "Country" 
  colnames(table_density)[2] <- "alpha.3" 
  colnames(table_density)[3] <- "populationDensity_2018" 
  
  lapply(table_density, class)
  
  table_density %<>% 
    mutate_each(funs(as.character), c(Country, 
                                      alpha.3)) %>% 
    mutate_each(funs(as.integer), c(populationDensity_2018))
  
  return(table_density)
  
}

######################################################
##### GET DEMOGRAPHICS DATA
######################################################
getDemographicsData <- function() {
  table_demographics <-
    read.csv("1_Data/worldBank_pop65.csv", 
           fill = TRUE, 
           header = TRUE,
           stringsAsFactors = TRUE,
           sep = ";",
           comment.char = "")
  
  table_demographics %<>% 
    select(c(Country.Name, Country.Code, X2018))
  
  colnames(table_demographics)[1] <- "Country" 
  colnames(table_demographics)[2] <- "alpha.3" 
  colnames(table_demographics)[3] <- "percentageAbove65_2018" 
  
  table_demographics %<>% 
    mutate_each(funs(as.character), c(Country, 
                                      alpha.3)) %>% 
    mutate(percentageAbove65_2018 = round(percentageAbove65_2018, 2))
  
  return(table_demographics)
}


######################################################
##### CLEAN ORIGINAL DATASET
######################################################
fixColumnNames <- function(df) {
  
  data_table <- df
  
  names(data_table) %<>% 
    str_replace_all("[.]", "-") %>% 
    str_replace_all("X", "")
  
  colnames(data_table)[1] <- "Country" 
  
  return(data_table)
  
}

cleanData <- function(data) {
  
  # Remove unwanted columns
  data %<>% 
    select(-c(Province.State, Lat, Long))
  
  # Merge split rows (in this case, country data spread across provinces)
  data %<>% 
    group_by(Country.Region) %>% 
    summarise_all(.funs = c(sum))
  
  # Fix columns names (especially dates)
  data %<>% 
    fixColumnNames()
  
  return(data)
  
}

######################################################
##### CORRECT FAULTY DATA POINTS
######################################################
# Dates are written on American structure... 

# FAULTS IN CONFIRMED CASES DATA

confirmedCasesCorrections <- function(data) {
  
  tempData <- data %>% 
    filter(Country == "Italy") %>% 
    mutate(`3-12-20` = 15113)
  data[data$Country == "Italy",] <- tempData
  
  tempData <- data %>% 
    filter(Country == "Spain") %>% 
    mutate(`3-12-20` = 2965)
  data[data$Country == "Spain",] <- tempData
  
  return(data)
}

deathsCorrections <- function(data) {
  
  # Fel i Islandsdata - justerat så att 15 mars har 0 dödsfall och 20 mars har 1 dödsfall.
  tempData <- data %>% 
    filter(Country == "Iceland") %>% 
    mutate(`3-15-20` = 0,
           `3-20-20` = 1)
  data[data$Country == "Iceland",] <- tempData
  
  # Fel i Italiendata - justerat så att 12 mars har 1016 dödsfall och 15113 bekräftade fall (WHO-data)
  tempData <- data %>% 
    filter(Country == "Italy") %>% 
    mutate(`3-12-20` = 1016)
  data[data$Country == "Italy",] <- tempData
  
  # Fel i Spaniendata - justerat så att 12 mars har 84 dödsfall och 2965 bekräftade fall (WHO-data)
  tempData <- data %>% 
    filter(Country == "Spain") %>% 
    mutate(`3-12-20` = 84)
  data[data$Country == "Spain",] <- tempData
  
  return(data)
}

######################################################
##### CORRECT THE COUNTRY CODES
######################################################
changeCode <- function(data = codesJoined,
                       country, 
                       newCode) {
  
  tempData <- data %>% 
    filter(Country == country) %>% 
    mutate(alpha.3 = newCode)
  
  data[data$Country == country,] <- tempData
  
  return(data)
} 

gitHubCorrections <- function(wrong_codes, 
                              correct_codes) {
  
  codesJoined <- merge(wrong_codes, correct_codes, by = c("Country"), all = TRUE)
  
  codesJoined %<>% 
    changeCode(country = "Brunei", 
               newCode = "BRN") %>% 
    changeCode(country = "Burma",
               newCode = "MMR") %>% 
    changeCode(country = "Syria",
               newCode = "SYR") %>% 
    changeCode(country = "Laos",
               newCode = "LAO") %>% 
    changeCode(country = "Kyrgyzstan",
               newCode = "KGZ") %>% 
    changeCode(country = "Congo (Kinshasa)",
               newCode = "COD") %>% 
    changeCode(country = "Congo (Brazzaville)",
               newCode = "COG") %>% 
    changeCode(country = "Saint Kitts and Nevis",
               newCode = "KNA") %>% 
    changeCode(country = "Saint Lucia",
               newCode = "LCA") %>% 
    changeCode(country = "Saint Vincent and the Grenadines",
               newCode = "VCT") %>% 
    changeCode(country = "Western Sahara",
               newCode = "ESH") %>% 
    changeCode(country = "Taiwan*",
               newCode = "TWN") %>% 
    changeCode(country = "Holy See",
               newCode = "VAT")
  
  return(codesJoined)
  
}


######################################################
##### TRANSPOSE ORIGINAL DATA
######################################################
transposeData <- function(data, value_name) {
  data %<>% 
    melt(value.name = value_name, variable.name = "Date")
  
  # Fix rubbish dates into proper ones
  data$Date %<>% 
    mdy()
  
  return(data)
}

######################################################
##### ADD ADD-ON DATA
######################################################
addExtraDataToDeaths <- function(covid_data, 
                                 population_data, 
                                 density_data, 
                                 regional_data, 
                                 demographic_data) {
  # Add total population data. 
  covid_data %<>%  
    merge(population_data[, c("alpha.3", 
                                "Population_2018")], 
          by = c("alpha.3"), 
          all.x = TRUE)
  
  covid_data %<>% 
    mutate(deathPerCapita = round(((confirmedDeaths / Population_2018) * 100000),2))
  
  # Add population density data. 
  covid_data %<>%  
    merge(density_data[, c("alpha.3",
                            "populationDensity_2018")], 
          by = c("alpha.3"), 
          all.x = TRUE)
  
  # We create the density groups based on categories made by the US Department of Agriculture (Wikipedia).
  covid_data %<>% 
    mutate(populationDensityGroup = ifelse(populationDensity_2018 < 10, "<10/km2",
                                           ifelse(populationDensity_2018 < 40, "10-40/km2",
                                                  ifelse(populationDensity_2018 < 100, "40-100/km2",
                                                         ifelse(populationDensity_2018 < 500, "100-500/km2",
                                                                ">500/km2")))))
  
  # Add regional category data. 
  covid_data %<>%  
    merge(regional_data[, c("alpha.3", 
                            "region", 
                            "sub.region", 
                            "intermediate.region")], 
          by = c("alpha.3"), 
          all.x = TRUE)
  
  covid_data %<>% 
    rename(Region = "region",
           subRegion = "sub.region",
           intermediateRegion = "intermediate.region")
  
  # Add demographic (age) data. 
  covid_data %<>%  
    merge(demographic_data[, c("alpha.3",
                                 "percentageAbove65_2018")], 
          by = c("alpha.3"), 
          all.x = TRUE)
  
  # Many countries have about 2-3% 65+ year-olds. If we do five categories, it could look like this... 
  covid_data %<>% 
    mutate(ageAbove65Group = ifelse(percentageAbove65_2018 < 1, "<1%",
                                    ifelse(percentageAbove65_2018 < 3, "1-3%",
                                           ifelse(percentageAbove65_2018 < 10, "3-10%",
                                                  ifelse(percentageAbove65_2018 < 20, "10-20%",
                                                         ">20%")))))
  
  return(covid_data)
  
}






