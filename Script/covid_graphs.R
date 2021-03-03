######################################################
##### PACKAGES
######################################################
# My functions
source("Script/standard_packages.R") # All standard packages.

######################################################
##### GRAPH PREP
######################################################
graphPreparationDeaths <- function() {
  # We want to filter countries with less than 5 confirmed cases OR less than 1 death.
  # We also want to collect only data that is seven days old (since it takes time for it to be correctly updated).
  death1 <- deaths_table %>% 
    group_by(Country) %>% 
    filter(confirmedDeaths >= 1) %>% 
    filter(max(Date) - Date > 7)
  
  # We want to count the number of days passed since the first filtered date.
  death1 %<>% 
    group_by(Country) %>% 
    mutate(daysPassed = Date - min(Date))
  
  # Relevel factors to get a nicer graph when you add other factors to it.
  death1$populationDensityGroup %<>% 
    fct_relevel( 
      ">500/km2", 
      "100-500/km2",
      "40-100/km2",
      "10-40/km2",
      "<10/km2")
  
  death1$ageAbove65Group %<>% 
    fct_relevel( 
      ">20%", 
      "10-20%",
      "3-10%",
      "1-3%")
  
  return(death1)
}

graphPreparationConfirmedCases <- function() {
  # We want to filter countries with less than 5 confirmed cases OR less than 1 death.
  # We also want to collect only data that is seven days old (since it takes time for it to be correctly updated).
  confirmed1 <- confirmed_table %>% 
    group_by(Country) %>% 
    filter(confirmedCases >= 5) %>% 
    filter(max(Date) - Date > 7)
  
  # We want to count the number of days passed since the first filtered date.
  confirmed1 %<>% 
    group_by(Country) %>% 
    mutate(daysPassed = Date - min(Date))
  
  return(confirmed1)
}

######################################################
##### COUNTRY FILTERING
######################################################
listWorstCases <- function(nbr_per_subregion) {
  
  latest_deaths <- 
    deaths_table %>% 
    filter(Date == max(Date))
  
  listCountries <- latest_deaths %>% 
    filter(Population_2018 >= 1000000) %>% # Filter countries with populations less than 1,000,000
    group_by(subRegion) %>% # Group by subregion
    top_n(nbr_per_subregion, deathPerCapita) # Select x countries worst-hit per subregion
  
  listCountries <- as.vector(unique(listCountries[['Country']])) # Transform into vector
  
  return(listCountries)
}

listByRegion <- function(region_name) {
  
  latest_deaths <- 
    deaths_table %>% 
    filter(Date == max(Date))
  
  listCountries <- latest_deaths %>% 
    filter(Population_2018 >= 1000000 & Region == region_name) # Filter countries with populations less than 1,000,000 within certain region
  
  listCountries <- as.vector(unique(listCountries[['Country']])) # Transform into vector
  
  return(listCountries)
}

######################################################
##### LINE GRAPHS
######################################################
deaths_per_time <- function(df, time, deathtype, headtitle, ytitle, xtitle) {
  ggplot(data=df, 
         aes(x={{time}},
             y={{deathtype}})) +
    geom_line(aes(colour = Country)) +
    scale_color_discrete(name = "Countries") +
    geom_dl(aes(label = Country), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8, last.bumpup)) +
    labs(title = "Deaths attributed to covid-19 by country",
         caption = "Source: John Hopkins Coronavirus Resource Center, World Bank Population Data (2018)", 
         x = xtitle,
         y = ytitle) +
    scale_y_continuous(labels = comma) +
    theme_clean()
}
  