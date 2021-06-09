######################################################
##### PACKAGES
######################################################
# My functions
source("2_Script/standard_packages.R") # All standard packages.

######################################################
##### GRAPH PREP
######################################################
graphPreparationDeaths <- function(data) {
  # We want to filter countries with less than 5 confirmed cases OR less than 1 death.
  # We want to collect only the data that is at least one day old (to reduce risk of incorrect uploads of data).
  df_deaths <- data %>% 
    group_by(Country) %>% 
    filter(confirmedDeaths >= 1) %>% 
    filter(max(Date) - Date > 1)
  
  # We want to count the number of days passed since the first filtered date.
  df_deaths %<>% 
    group_by(Country) %>% 
    mutate(daysPassed = Date - min(Date))
  
  # Relevel factors to get a nicer graph when you add other factors to it.
  df_deaths$populationDensityGroup %<>% 
    fct_relevel( 
      ">500/km2", 
      "100-500/km2",
      "40-100/km2",
      "10-40/km2",
      "<10/km2")
  
  df_deaths$ageAbove65Group %<>% 
    fct_relevel( 
      ">20%", 
      "10-20%",
      "3-10%",
      "1-3%")
  
  return(df_deaths)
}

graphPreparationConfirmedCases <- function(data) {
  # We want to filter countries with less than 5 confirmed cases OR less than 1 death.
  # We want to collect only the data that is at least one day old (to reduce risk of incorrect uploads of data).
  df_confirmed <- data %>% 
    group_by(Country) %>% 
    filter(confirmedCases >= 5) %>% 
    filter(max(Date) - Date > 1)
  
  # We want to count the number of days passed since the first filtered date.
  df_confirmed %<>% 
    group_by(Country) %>% 
    mutate(daysPassed = Date - min(Date))
  
  return(df_confirmed)
}

######################################################
##### COUNTRY FILTERING
######################################################
listWorstCases <- function(data, nbr_per_subregion) {
  
  latest_deaths <- 
    data %>% 
    filter(Date == max(Date))
  
  listCountries <- latest_deaths %>% 
    filter(Population_2018 >= 1000000) %>% # Filter countries with populations less than 1,000,000
    group_by(subRegion) %>% # Group by subregion
    top_n(nbr_per_subregion, deathPerCapita) # Select x countries worst-hit per subregion
  
  listCountries <- as.vector(unique(listCountries[['Country']])) # Transform into vector
  
  return(listCountries)
}

listByRegion <- function(data, region_name) {
  
  latest_deaths <- 
    data %>% 
    filter(Date == max(Date))
  
  listCountries <- latest_deaths %>% 
    filter(Population_2018 >= 1000000 & Region == region_name) # Filter countries with populations less than 1,000,000 within certain region
  
  listCountries <- as.vector(unique(listCountries[['Country']])) # Transform into vector
  
  return(listCountries)
}

filterData <- function(data, method, country_list = NULL, cases_per_subregion = NULL, continent = NULL) {
  
  if(method == "Manual") {
    if(is.null(country_list)) {
      stop('Please enter a country list.')
    }
    
    listCountries <- country_list
  }
  
  else if(method == "Worst Cases Per Sub-Region") {
    if(is.null(cases_per_subregion)) {
      stop('Please enter the number of worst-performers you wish to highlight per subregion.')
    }
    
    # If you wish to look at top x countries with populations over 1,000,000 and most per-capita deaths per subregion.
    listCountries <- listWorstCases(data = data, nbr_per_subregion = cases_per_subregion)
  }
  
  else if(method == "Continental") {
    if(is.null(continent)) {
      stop('Please enter the continent you wish to examine.')
    }
    
    # If you wish to look at countries with populations over 1,000,000 within a certain region.
    listCountries <- listByRegion(data = data, region_name = continent)
  }
  
  else {
    stop('Select one of three methods: (1) "Manual", (2) "Worst Cases Per Sub-Region", or (3) "Continental".')
  }
  
  filtered_data <- data %>% 
    filter(Country %in% listCountries)
  
  return(filtered_data)
  
}

######################################################
##### LINE GRAPHS - DEATHS OVER TIME
######################################################
deathsOverTime <- function(df, time, deathtype, headtitle, ytitle, xtitle) {
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

######################################################
##### STACKED BARS - DEATH DISTRIBUTIONS PER REGION
######################################################  
fn_roll_mean <- function (x) { # https://stackoverflow.com/questions/39419376/r-dplyr-rollmean-using-group-by-columns
  if (length(x) < 7) {
    rep(NA,length(x)) 
  } else {
    roll_mean(x, n = 7, align = "right", fill = NA)
  }
}

dailyDeathsAndRollingAvgs <- function(data, segment) {
  new_data <- data %>% 
    select({{segment}}, Date, confirmedDeaths) %>% 
    group_by({{segment}}, Date) %>% 
    summarise(totalDeaths = sum(confirmedDeaths)) %>% # Summarise deaths per date and segment
    mutate(newConfirmedDeaths = totalDeaths - lag(totalDeaths)) %>% # Count the new confirmed deaths per day
    mutate(newConfirmedDeaths = ifelse(newConfirmedDeaths < 0, 0, newConfirmedDeaths)) # Replace negative values with zero (which are due to revisions in dataset)
  
  new_data %<>% 
    group_by({{segment}}) %>% 
    mutate(n = 1) %>% # Create a "day counter"
    mutate(SMA = cumsum(ifelse(is.na(newConfirmedDeaths), 0, newConfirmedDeaths)) / cumsum(n)) %>% # Add a Simple Moving Average (SMA)
    select(-n) %>% 
    mutate(SMA7 = fn_roll_mean(newConfirmedDeaths)) # Add a 7-Day Simple Moving Average (SMA7)
  
  return(new_data)
}

deathsByGeo <- function(data, segment) {
  
  segment_string <- deparse(substitute(segment))
  
  if(segment_string == "Region") {
    
    # Calculate daily NEW deaths per region
    df <- dailyDeathsAndRollingAvgs(data = data, segment = {{segment}})
    
    # Relevel the regions to get a nicer graph
    df$Region %<>% 
      fct_relevel( 
        "Oceania", 
        "Africa",
        "Asia",
        "Americas",
        "Europe")
    
    color_list = c("Asia" = "seagreen3", 
                   "Africa" = "goldenrod3", 
                   "Europe" = "steelblue4",
                   "Americas" = "coral2",
                   "Oceania" = "palevioletred3")
    
  } 
  else if(segment_string == "subRegion") {
    
    listGeos <- 
      c("Western Europe",
        "Southern Europe", 
        "Northern Europe",
        "Northern America",
        "South-eastern Asia",
        "Southern Asia",
        "Western Asia",
        "Eastern Asia")
    
    # Caculate daily NEW deaths per region
    df <- dailyDeathsAndRollingAvgs(data = data, segment = {{segment}})
    df %<>% 
      filter(subRegion %in% listGeos)
    
    # Relevel the regions to get a nicer graph
    df$subRegion %<>% 
      fct_relevel( 
        "Eastern Asia",
        "Western Asia",
        "South-eastern Asia", 
        "Southern Asia", 
        "Northern America",
        "Northern Europe",
        "Western Europe",
        "Southern Europe")
    
    color_list = c("Eastern Asia" = "seagreen3", 
                   "Western Asia" = "goldenrod3", 
                   "South-eastern Asia" = "steelblue4",
                   "Southern Asia" = "coral2",
                   "Northern America" = "steelblue2",
                   "Northern Europe" = "palevioletred3", 
                   "Western Europe" = "goldenrod1", 
                   "Southern Europe" = "lightsalmon2")
    
  } 
  else if(segment_string == "Country") {
    
    listGeos <- 
      c("Sweden",
        "Denmark", 
        "Norway",
        "Finland")
    
    # Caculate daily NEW deaths per region
    df <- dailyDeathsAndRollingAvgs(data = data, segment = {{segment}})
    df %<>% 
      filter(Country %in% listGeos)
    
    # Relevel the regions to get a nicer graph
    df$Country %<>% 
      fct_relevel( 
        "Sweden",
        "Denmark", 
        "Norway",
        "Finland")
    
    color_list = c("Norway" = "seagreen3", 
                   "Finland" = "goldenrod3", 
                   "Sweden" = "steelblue4",
                   "Denmark" = "coral2")
    
  }
  else {
    stop("Unacceptable [segment] input. Change to 'Region', 'subRegion' or 'Country'.")
  }
  
  par(mfrow=c(1,2))
  
  # And plot
  A <- ggplot(data=df, 
              aes(x=Date, 
                  y=SMA7)) + 
    geom_col(aes(fill = {{segment}})) +
    labs(title = "# new deaths per region (SMA7)",
         y = "# new confirmed deaths") +
    scale_fill_manual(name = segment_string, values = color_list) +
    theme_clean()
  
  B <- ggplot(data=df, 
              aes(x=Date, 
                  y=SMA7)) + 
    geom_col(position = "fill", stat = "identity", aes(fill = {{segment}})) +
    labs(title = "% new deaths per region (SMA7)",
         x = "Date",
         y = "% share new confirmed deaths",
         caption = "Source: John Hopkins Coronavirus Resource Center") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(name = segment_string, values = color_list) +
    theme_clean()
  
  ggarrange(A, B, 
            labels = c("A", "B"),
            ncol = 1, nrow = 2)
  
}



