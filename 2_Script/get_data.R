######################################################
##### PACKAGES
######################################################
# My functions
source("2_Script/standard_packages.R") # All standard packages.
source("2_Script/covid_functions.R") # Covid-specific data cleaning functions.

######################################################
##### IMPORT, CLEAN AND MERGE DATA BASED ON FUNCTIONS
######################################################
getData <- function() {
  
  ######################################################
  ##### GET THE DAILY UPDATED CSV FILES FROM GITHUB 
  ######################################################
  confirmed_table <- 
    importCovidData(url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  
  deaths_table <- 
    importCovidData(url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  
  ######################################################
  ##### GET OTHER DATA: REGIONS, POPULATION, DENSITY, DEMOGRAPHICS
  ######################################################
  # Regions
  table_regions <- getRegionalData()
  
  # Populations
  table_populations <- getPopulationData()
  
  # Density
  table_density <- getDensityData()
  
  # Demographics (% of population 65+)
  table_demographics <- getDemographicsData()
  
  ######################################################
  ###### CLEAN ORIGINAL DATASET
  ######################################################
  confirmed_table %<>% 
    cleanData() %>% 
    confirmedCasesCorrections()
  
  deaths_table %<>% 
    cleanData() %>% 
    deathsCorrections()
  
  ######################################################
  ###### FIX WRONG DATA POINTS
  ######################################################
  #confirmed_table <- confirmedCasesCorrections()
  
  #deaths_table <- deathsCorrections()
  
  ######################################################
  ##### CREATE PROPER COUNTRY CODES
  ######################################################
  codesCorrect <- table_populations[c("Country", "alpha.3")]
  codesWrong <- confirmed_table["Country"]
  
  codesJoined <- gitHubCorrections(wrong_codes = codesWrong, correct_codes = codesCorrect)
  
  # Join the alpha.3 country codes to our working dataset
  
  confirmed_table %<>% 
    merge(codesJoined, by = "Country", all.x = TRUE)
  
  deaths_table %<>% 
    merge(codesJoined, by = "Country", all.x = TRUE)
  
  ######################################################
  ###### TRANSPOSE DATA
  ######################################################
  confirmed_table %<>%  
    transposeData(value_name = "confirmedCases")
  
  deaths_table %<>% 
    transposeData(value_name = "confirmedDeaths")
  
  ######################################################
  ###### ADD RELEVANT METRICS TO THE CORE DATA
  ######################################################
  # From here on, we focus on the reported deaths. It is more reliable data at this stage.
  deaths_table %<>% 
    addExtraDataToDeaths(population_data = table_populations, 
                         density_data = table_density, 
                         regional_data = table_regions, 
                         demographic_data = table_demographics)  
  
  ######################################################
  ###### RETURN TABLES
  ######################################################
  df = list()
  
  df$confirmed_table <- confirmed_table
  df$deaths_table <- deaths_table
  
  return(df)
  
}