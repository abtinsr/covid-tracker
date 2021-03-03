######################################################
##### PACKAGES
######################################################
# My functions
source("Script/standard_packages.R") # All standard packages.
source("Script/covid_functions.R") # Covid-specific functions.
source("Script/covid_graphs.R") # Covid-specific graphs.

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
  cleanData()

deaths_table %<>% 
  cleanData()

######################################################
###### FIX WRONG DATA POINTS
######################################################
confirmed_table <- confirmedCasesCorrections()

deaths_table <- deathsCorrections()

######################################################
###### EXPORT FILES AS CSV
######################################################
exportCovidData()

######################################################
##### CREATE PROPER COUNTRY CODES
######################################################
codesCorrect <- table_populations[c("Country", "alpha.3")]
codesWrong <- confirmed_table["Country"]

codesJoined <- gitHubCorrections()

rm(codesCorrect)
rm(codesWrong)


# Join the alpha.3 country codes to our working dataset

confirmed_table %<>% 
  merge(codesJoined, by = "Country", all.x = TRUE)

deaths_table %<>% 
  merge(codesJoined, by = "Country", all.x = TRUE)

rm(codesJoined)

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
  addExtraDataToDeaths()

rm(table_demographics)
rm(table_density)
rm(table_populations)
rm(table_regions)

######################################################
###### SELECT COUNTRIES
######################################################
death1 <- graphPreparationDeaths() # RENAME deaths
confirmed1 <- graphPreparationConfirmedCases() # RENAME confirmed

# We don't want to study all countries. Let's select a couple based on intuition AND deaths. 
# We should try to select countries with reliable data. 
latest_deaths <- 
  deaths_table %>% 
  filter(Date == max(Date))

# If you wish to manually list countries to examine. 
listCountries <- 
  c("Sweden", 
    "Denmark", 
    "Norway", 
    "Japan", 
    "Italy",
    "US",
    "Germany", 
    "Korea, South",
    "Singapore", 
    "Finland", 
    "France",
    "United Kingdom", 
    "Spain",
    "Indonesia",
    "Belgium",
    "Netherlands",
    "Switzerland", 
    "Canada",
    "Greece",
    "Portugal",
    "Ukraine",
    "Lithuania",
    "Estonia",
    "Latvia",
    "Slovakia",
    "Croatia", 
    "Peru", 
    "Chile",
    "Brazil", 
    "Mexico", 
    "Bolivia")

# If you wish to look at top x countries with populations over 1,000,000 and most per-capita deaths per subregion.
listCountries <- listWorstCases(nbr_per_subregion = 2)

# If you wish to look at countries with populations over 1,000,000 within a certain region.
listCountries <- listByRegion(region_name = "Europe")

confirmed2 <- confirmed1 %>% # RENAME confirmed_adj
  filter(Country %in% listCountries)

death2 <- death1 %>% # RENAME deaths_adj
  filter(Country %in% listCountries)

######################################################
###### VISUALISE: CASES OVER TIME PER COUNTRY
######################################################
# Confirmed deaths per date after 1st death.
deaths_per_time(df = death2, 
                deathtype = confirmedDeaths,
                time = Date,
                xtitle = "Date",
                ytitle = "Covid-19 deaths (avg. rolling 7 days)") 

# Confirmed deaths per day after 1st death.
deaths_per_time(df = death2, 
                deathtype = confirmedDeaths,
                time = daysPassed,
                xtitle = "Days since the first confirmed covid-19 death",
                ytitle = "Covid-19 deaths (avg. rolling 7 days)") +
  scale_x_continuous(breaks = seq(0, 1000, 10)) + 
  scale_y_continuous(breaks = seq(0, 1000000, 10000)) 

# Same with log-scale.
deaths_per_time(df = death2, 
                deathtype = confirmedDeaths,
                time = daysPassed,
                xtitle = "Days since the first confirmed covid-19 death",
                ytitle = "Covid-19 deaths (avg. rolling 7 days)") +
  scale_x_continuous(breaks = seq(0, 1000, 10)) + 
  scale_y_continuous(trans = "log10", 
                     labels = trans_format("log10", math_format(10^.x)))


# Adjust the first death-graph for population.
deaths_per_time(df = death2, 
                deathtype = deathPerCapita,
                time = daysPassed,
                xtitle = "Days since the first confirmed covid-19 death",
                ytitle = "Covid-19 deaths per 100,000 inhabitants (avg. rolling 7 days)") + 
  scale_x_continuous(breaks = seq(0, 1000, 10)) + 
  scale_y_continuous(breaks = seq(0, 200, 10))


######################################################
###### VISUALISE: NEW DEATHS PER DAY AND REGION 
######################################################

addNewDeathsMetric <- function(segment) {
  newDeaths <- death1 %>% 
    select({{segment}}, Date, confirmedDeaths) %>% 
    group_by({{segment}}, Date) %>% 
    summarise(totalDeaths = sum(confirmedDeaths)) %>% 
    mutate(newConfirmedDeaths = totalDeaths - lag(totalDeaths)) %>% 
    mutate(newConfirmedDeaths = ifelse(newConfirmedDeaths < 0, 0, newConfirmedDeaths))
  
  return(newDeaths)
}

plotRegionalDistribution <- function() {
  
  # Caculate daily NEW deaths per region
  df <- addNewDeathsMetric(segment = Region)
  
  # Relevel the regions to get a nicer graph
  forPlot_death1$Region %<>% 
    fct_relevel( 
      "Oceania", 
      "Africa",
      "Asia",
      "Americas",
      "Europe")
  
  par(mfrow=c(1,2))
  
  # And plot
  A <- ggplot(data=forPlot_death1, 
              aes(x=Date, 
                  y=newConfirmedDeaths)) + 
    geom_col(aes(fill = Region)) +
    labs(title = "New deaths per continent",
         y = "New confirmed deaths (avg. rolling 7 days)") +
    scale_fill_manual(name = "Regions", values = c("Asia" = "seagreen3", 
                                                   "Africa" = "goldenrod3", 
                                                   "Europe" = "steelblue4",
                                                   "Americas" = "coral2",
                                                   "Oceania" = "palevioletred3")) +
    theme_clean()
  
  B <- ggplot(data=forPlot_death1, 
              aes(x=Date, 
                  y=newConfirmedDeaths)) + 
    geom_col(position = "fill", stat = "identity", aes(fill = Region)) +
    labs(title = "Share of deaths per continent",
         x = "Date",
         y = "Share of new confirmed deaths (avg. rolling 7 days)",
         caption = "Source: John Hopkins Coronavirus Resource Center") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(name = "Regions", values = c("Asia" = "seagreen3", 
                                                   "Africa" = "goldenrod3", 
                                                   "Europe" = "steelblue4",
                                                   "Americas" = "coral2",
                                                   "Oceania" = "palevioletred3")) +
    theme_clean()
  
  ggarrange(A, B, 
            labels = c("A", "B"),
            ncol = 1, nrow = 2)
  
}

plotRegionalDistribution()

# Caculate daily NEW deaths per region
forPlot_death1 <- addNewDeathsMetric(segment = Region)

# Relevel the regions to get a nicer graph
forPlot_death1$Region %<>% 
  fct_relevel( 
    "Oceania", 
    "Africa",
    "Asia",
    "Americas",
    "Europe")

# And plot!

ggplot(data=forPlot_death1, 
       aes(x=Date, 
           y=newConfirmedDeaths)) + 
  geom_col(aes(fill = Region)) +
  labs(title = "Daily new covid-related deaths per world region",
       x = "Date",
       y = "New confirmed deaths (avg. rolling 7 days)",
       caption = "Source: John Hopkins Coronavirus Resource Center") +
  scale_fill_manual(name = "Regions", values = c("Asia" = "seagreen3", 
                                                 "Africa" = "goldenrod3", 
                                                 "Europe" = "steelblue4",
                                                 "Americas" = "coral2",
                                                 "Oceania" = "palevioletred3")) +
  theme_clean()


########################################################################
########################################################################
########################################################################
ggplot(data=forPlot_death1, 
       aes(x=Date, 
           y=newConfirmedDeaths)) + 
  geom_col(position = "fill", stat = "identity", aes(fill = Region)) +
  labs(title = "Distribution of daily new covid-related deaths per world region",
       x = "Date",
       y = "Share of new confirmed deaths (avg. rolling 7 days)",
       caption = "Source: John Hopkins Coronavirus Resource Center") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(name = "Regions", values = c("Asia" = "seagreen3", 
                                                 "Africa" = "goldenrod3", 
                                                 "Europe" = "steelblue4",
                                                 "Americas" = "coral2",
                                                 "Oceania" = "palevioletred3")) +
  theme_clean()



# Daily new deaths per "subregion"
# Create subregional filter
listSubRegions <- 
  c("Western Europe",
    "Southern Europe", 
    "Northern Europe",
    "Northern America",
    "South-eastern Asia",
    "Southern Asia",
    "Western Asia",
    "Eastern Asia")

# Select the appropriate form of the data
forPlot_death2 <- 
  death1 %>% 
  select(subRegion, Date, confirmedDeaths) %>% 
  group_by(subRegion, Date) %>% 
  summarise(totalDeaths = sum(confirmedDeaths)) %>% 
  mutate(newConfirmedDeaths = totalDeaths - lag(totalDeaths)) %>% 
  mutate(newConfirmedDeaths = ifelse(newConfirmedDeaths < 0, 0, newConfirmedDeaths)) %>% 
  filter(subRegion %in% listSubRegions)

# Relevel the regions to get a nicer graph

forPlot_death2$subRegion %<>% 
  fct_relevel(
    "Eastern Asia",
    "Western Asia",
    "South-eastern Asia", 
    "Southern Asia", 
    "Northern America",
    "Northern Europe",
    "Western Europe",
    "Southern Europe")

# And plot!

ggplot(data=forPlot_death2, 
       aes(x=Date, 
           y=newConfirmedDeaths)) + 
  geom_col(aes(fill = subRegion)) +
  labs(title = "Daily new deaths per subregion", 
       x = "Date",
       y = "New confirmed deaths (avg. rolling 7 days)",
       caption = "Source: John Hopkins Coronavirus Resource Center") +
  scale_fill_manual(name = "Subregions", values = c("Eastern Asia" = "seagreen3", 
                                                   "Western Asia" = "goldenrod3", 
                                                   "South-eastern Asia" = "steelblue4",
                                                   "Southern Asia" = "coral2",
                                                   "Northern America" = "steelblue2",
                                                   "Northern Europe" = "palevioletred3", 
                                                   "Western Europe" = "goldenrod1", 
                                                   "Southern Europe" = "lightsalmon2")) +
  theme_clean()


ggplot(data=forPlot_death2, 
       aes(x=Date, 
           y=newConfirmedDeaths)) + 
  geom_col(position = "fill", stat = "identity", aes(fill = subRegion)) +
  labs(title = "Distribution of daily new deaths per subregion",
       x = "Date",
       y = "New confirmed deaths (avg. rolling 7 days)",
       caption = "Source: John Hopkins Coronavirus Resource Center") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(name = "Subregions", values = c("Eastern Asia" = "seagreen3", 
                                                    "Western Asia" = "goldenrod3", 
                                                    "South-eastern Asia" = "steelblue4",
                                                    "Southern Asia" = "coral2",
                                                    "Northern America" = "steelblue2",
                                                    "Northern Europe" = "palevioletred3", 
                                                    "Western Europe" = "goldenrod1", 
                                                    "Southern Europe" = "lightsalmon2")) +
  theme_clean()

# Just to get totals, as well.

forPlot_death3 <- 
  death1 %>% 
  select(Date, confirmedDeaths) %>% 
  group_by(Date) %>% 
  summarise(totalDeaths = sum(confirmedDeaths)) %>% 
  mutate(newConfirmedDeaths = totalDeaths - lag(totalDeaths)) %>% 
  mutate(newConfirmedDeaths = ifelse(newConfirmedDeaths < 0, 0, newConfirmedDeaths))


# And plot!

ggplot(data=forPlot_death3, 
       aes(x=Date, 
           y=newConfirmedDeaths)) + 
  geom_area() +
  labs(title = "Daily new deaths globally", 
       x = "Date",
       y = "New confirmed deaths (avg. rolling 7 days)",
       caption = "Source: John Hopkins Coronavirus Resource Center") +
  theme_clean()

ggplot(data=forPlot_death3, 
       aes(x=Date, 
           y=totalDeaths)) + 
  geom_line() +
  labs(title = "Total deaths globally", 
       x = "Date",
       y = "Confirmed deaths (avg. rolling 7 days)",
       caption = "Source: John Hopkins Coronavirus Resource Center") +
  scale_y_continuous(breaks = seq(0, 2000000, 100000), labels = comma) + 
  theme_clean()

######################################################
###### VISUALISE: SAME AS ABOVE BUT ON NATIONAL LEVEL
######################################################

forPlot_death4a <- 
  death1 %>% 
  filter(Country %in% c("Sweden", 
                        "Norway",
                        "Denmark")) %>% 
  select(Country, Date, confirmedDeaths) %>% 
  group_by(Country) %>% 
  mutate(newConfirmedDeaths = confirmedDeaths - lag(confirmedDeaths, 
                                                    order_by = Date)) %>% 
  mutate(newConfirmedDeaths = ifelse(newConfirmedDeaths < 0, 0, newConfirmedDeaths))


ggplot(data=forPlot_death4a, 
       aes(x=Date,
           y=newConfirmedDeaths)) + 
  geom_col(aes(fill = Country)) +
  labs(title = "Daily new deaths in the Nordics",
       subtitle = "...", 
       x = "Date",
       y = "New confirmed deaths (avg. rolling 7 days)",
       caption = "Source: John Hopkins Coronavirus Resource Center") + 
  facet_wrap(~ Country) + 
  scale_fill_manual(values = wes_palette("Rushmore1"))


ggplot(data=forPlot_death4a, 
       aes(x=Date,
           y=newConfirmedDeaths)) + 
  geom_area(aes(fill = Country)) +
  labs(title = "...",
       subtitle = "...", 
       x = "Date",
       y = "New confirmed deaths (avg. rolling 7 days)",
       caption = "Source: John Hopkins Coronavirus Resource Center") +
  scale_fill_manual(values = wes_palette("Rushmore1"))



forPlot_death4b <- 
  death1 %>% 
  filter(Country %in% c("Sweden", 
                        "Norway",
                        "Denmark")) %>% 
  select(Country, Date, deathPerCapita) %>% 
  group_by(Country) %>% 
  mutate(newdeathPerCapita = deathPerCapita - lag(deathPerCapita, 
                                                  order_by = Date)) %>% 
  mutate(newdeathPerCapita = ifelse(newdeathPerCapita < 0, 0, newdeathPerCapita))


ggplot(data=forPlot_death4b, 
       aes(x=Date,
           y=newdeathPerCapita)) + 
  geom_col(aes(fill = Country)) +
  labs(title = "Daily new deaths per capita for Nordics",
       subtitle = "...", 
       x = "Date",
       y = "New confirmed deaths per 100,000 inhabitants (avg. rolling 7 days)",
       caption = "Source: John Hopkins Coronavirus Resource Center & World Bank Population Data (2018)") + 
  facet_wrap(~ Country) +
  scale_fill_manual(values = wes_palette("Rushmore1")) +
  geom_smooth(color = "navy blue")


# Rushmore1; Cavalcanti1


######################################################
##### CASES OVER TIME WITH OTHER FACTORS
######################################################

ggplot(data = latest_deaths,
       aes(y = confirmedDeaths,
           x = percentageAbove65_2018, 
           color = Region)) + 
  geom_point(size = 2) + 
  scale_fill_manual(values = wes_palette("Rushmore1")) +
  theme_clean() 


# Create population summaries that aggregate the data in the right way. 
# We need to calculate totals for population, density and subregions.

tempData <- 
  death1

tempData %<>% 
  group_by(ageAbove65Group) %>% 
  mutate(popTotalsAge = sum(Population_2018, na.rm = TRUE))

tempData %<>% 
  group_by(populationDensityGroup) %>% 
  mutate(popTotalsDensity = sum(Population_2018, na.rm = TRUE))

tempData %<>% 
  group_by(subRegion) %>% 
  mutate(popTotalsSubRegion = sum(Population_2018, na.rm = TRUE))

tempData %<>% 
  select(alpha.3, Country, Date, popTotalsAge, popTotalsDensity, popTotalsSubRegion)

death1 %<>%  
  merge(tempData[, c("alpha.3",
                     "Country",
                     "Date",
                     "popTotalsAge",
                     "popTotalsDensity",
                     "popTotalsSubRegion")], 
        by = c("alpha.3",
               "Country",
               "Date"), 
        all.x = TRUE)



# How does region affect deadliness?

forPlot_death5c <- 
  death1 %>% 
  select(Region, subRegion, Date, confirmedDeaths, Population_2018, popTotalsSubRegion) %>% 
  group_by(Region, subRegion, Date, popTotalsSubRegion) %>% 
  summarise(confirmedDeaths = sum(confirmedDeaths)) %>% 
  mutate(deathPerCapita = (confirmedDeaths/popTotalsSubRegion)*100000)

forPlot_death5c %<>% 
  group_by(Region, subRegion) %>% 
  mutate(daysPassed = Date - min(Date)) %>% 
  filter(Region %in% c("Americas",
                       "Europe",
                       "Asia"))

forPlot_death5c$Region %<>% 
  fct_relevel( 
    "Asia",
    "Europe",
    "Americas")

# And plot!

ggplot(data=forPlot_death5c, 
       aes(x=daysPassed, 
           y=deathPerCapita)) + 
  geom_line(aes(colour = subRegion), show.legend = FALSE) + 
  scale_color_discrete(name = "Subregion") +
  geom_dl(aes(label = subRegion), 
          method = list(dl.trans(x = x + 0.2), 
                        "last.points", 
                        cex = 0.8,
                        last.bumpup)) + 
  labs(title = "The deadliness of COVID-19 varies greatly",
       caption = "Source: John Hopkins Coronavirus Resource Center", 
       x = "Days since the first confirmed covid-19 death",
       y = "Covid-19 deaths per 100,000 inhabitants (avg. rolling 7 days)") + 
  scale_x_continuous(breaks = seq(0, 500, 30)) + 
  scale_y_continuous(breaks = seq(0, 300, 0.1)) + 
  theme(legend.position = "None") +
  facet_wrap(~ Region) 


# Do a boxplot instead.

forPlot_death5c_latest <- 
  death1 %>% 
  filter(Date == max(Date))

ggplot(data=forPlot_death5c_latest, 
       aes(x=Region, 
           y=deathPerCapita)) + 
  geom_boxplot() + 
  geom_text(aes(label = Country), 
            data = . %>% filter(deathPerCapita >= 120),
            vjust = 0, 
            nudge_y = 0.3, 
            nudge_x = 0) + 
  labs(title = "Checking for outliers in the regional data",
       caption = "Source: John Hopkins Coronavirus Resource Center", 
       x = "Regions",
       y = "Covid-19 deaths per 100,000 inhabitants (avg. rolling 7 days)") + 
  scale_y_continuous(breaks = seq(0, 300, 5)) +
  theme_clean()

######################################################
##### HOW MANY PERCENT OF WORLD POPULATION AND COVID DEATHS?
######################################################

TOTAL_DEATHS <- latest_deaths %>% 
  select(confirmedDeaths) %>% 
  summarise(confirmedDeaths = sum(confirmedDeaths))

WORLD_POPULATION <- latest_deaths %>% 
  select(Population_2018) %>% 
  na.omit() %>% 
  summarise(Population_2018 = sum(Population_2018))


relative_impact <- latest_deaths %>% 
  select(Country, Region, confirmedDeaths, deathPerCapita, Population_2018) %>% 
  mutate(share_of_world_population = round(Population_2018/WORLD_POPULATION[1,1], 6), 
         share_of_covid_deaths = round(confirmedDeaths/TOTAL_DEATHS[1,1], 6))

relative_impact %<>% 
  mutate(relative_impact = share_of_covid_deaths/share_of_world_population)

listCountries_most_impacted <- 
  c("Belgium", 
    "United Kingdom",
    "Peru", 
    "Spain",
    "Italy",
    "Sweden", 
    "Chile",
    "US", 
    "Brazil",
    "France",
    "Congo (Kinshasa)", 
    "Ethiopia",
    "China", 
    "Malaysia",
    "Cote d'Ivoire",
    "Nigeria",
    "Madagascar",
    "Uzbekistan",
    "Korea, South",
    "Mali")

listCountries_biggest_populations <- 
  c("China", 
    "India",
    "US", 
    "Indonesia",
    "Pakistan",
    "Brazil", 
    "Nigeria",
    "Bangladesh", 
    "Russia",
    "Japan", 
    "Mexico", 
    "Ethiopia", 
    "Philippines", 
    "Egypt", 
    "Vietnam", 
    "Kongo (Kinshasa)", 
    "Germany", 
    "Turkey", 
    "Iran", 
    "Thailand")

listCountries_biggest_economies <- 
  c("US", 
    "China",
    "Japan", 
    "Germany",
    "India",
    "United Kingdom", 
    "France",
    "Italy", 
    "Brazil",
    "Canada", 
    "Russia", 
    "Korea, South", 
    "Spain", 
    "Australia", 
    "Mexico", 
    "Indonesia", 
    "Netherlands", 
    "Saudi Arabia", 
    "Turkey", 
    "Switzerland")

listCountries_richest_economies <- 
  c("Qatar", 
    #"Macao", N/A!
    "Luxenbourg",
    "Singapore", 
    "Brunei",
    "Ireland",
    "United Arab Emirates", 
    "Kuwait",
    "Switzerland", 
    "San Marino",
    "Norway", 
    #"Hong Kong", N/A!
    "US", 
    "Iceland", 
    "Netherlands", 
    "Denmark", 
    "Saudi Arabia", 
    "Austria", 
    "Germany", 
    "Sweden", 
    "Australia", 
    "Belgium")

relative_impact_most_impacted <- relative_impact %>% 
  filter(Country %in% listCountries_most_impacted)

relative_impact_biggest_populations <- relative_impact %>% 
  filter(Country %in% listCountries_biggest_populations)

relative_impact_biggest_economies <- relative_impact %>% 
  filter(Country %in% listCountries_biggest_economies)

relative_impact_richest_economies <- relative_impact %>% 
  filter(Country %in% listCountries_richest_economies)

relative_impact_all_with_10_deaths <- relative_impact %>% 
  filter(confirmedDeaths > 100)

relative_impact_all_with_1_percapita_death <- relative_impact %>% 
  filter(deathPerCapita > 1)

impactPlot <- function(df, title_text) {
  ggplot(data = df, 
         aes(x = reorder(Country, -relative_impact), 
             y = relative_impact, 
             fill = Region)) + 
    geom_col() + 
    geom_hline(yintercept = 1, color = "navy", size = 1) + 
    labs(title = title_text,
         subtitle = "The impact score is defined as the country's share of global covid-19 deaths divided by its share of world population.
An impact score = 1 indicates proportionate deaths in relation to population. Higher than 1 indicates overproportionate deaths.", 
         x = "Country",
         y = "Impact score = (Share of global covid-19 deaths)/(Share of world population)",
         caption = "Source: John Hopkins Coronavirus Resource Center & World Bank Population Data (2018)") + 
    scale_fill_manual("legend", values = c("Asia" = "seagreen3", 
                                           "Africa" = "goldenrod3", 
                                           "Europe" = "steelblue4",
                                           "Americas" = "coral2",
                                           "Oceania" = "palevioletred3")) + 
    theme_clean() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
}

#tomato3, 

impactPlot(df = relative_impact_most_impacted, 
           title_text = "Impact among 10 most and 10 least impacted countries, relatively speaking")

impactPlot(df = relative_impact_biggest_populations, 
           title_text = "Impact among the 20 most populated countries in the world")

impactPlot(df = relative_impact_biggest_economies, 
           title_text = "Impact among the 20 biggest economies in the world")

impactPlot(df = relative_impact_richest_economies, 
           title_text = "Impact among the 20 richest per-capita economies in the world (Macao and Hong Kong data not available)")

impactPlot(df = relative_impact_all_with_10_deaths, 
           title_text = "Impact among all countries with at least 10 deaths")





ggplot(data = relative_impact_all_with_10_deaths, 
       aes(x = reorder(Country, confirmedDeaths), 
           y = confirmedDeaths, 
           fill = Region)) + 
  geom_col() + 
  labs(title = "Impact among all countries with at least 10 deaths",
       x = "Country",
       y = "Total deaths attributed to covid-19",
       caption = "Source: John Hopkins Coronavirus Resource Center & World Bank Population Data (2018)") + 
  scale_fill_manual("legend", values = c("Asia" = "seagreen3", 
                                         "Africa" = "goldenrod3", 
                                         "Europe" = "steelblue4",
                                         "Americas" = "coral2",
                                         "Oceania" = "palevioletred3")) + 
  theme_clean() + 
  coord_flip()




ggplot(data = relative_impact_all_with_1_percapita_death, 
       aes(x = reorder(Country, deathPerCapita), 
           y = deathPerCapita, 
           fill = Region)) + 
  geom_col() + 
  labs(title = "Impact among all countries with at least 1 death per 100,000 citizens",
       x = "Country",
       y = "Total deaths attributed to covid-19",
       caption = "Source: John Hopkins Coronavirus Resource Center & World Bank Population Data (2018)") + 
  scale_fill_manual("legend", values = c("Asia" = "seagreen3", 
                                         "Africa" = "goldenrod3", 
                                         "Europe" = "steelblue4",
                                         "Americas" = "coral2",
                                         "Oceania" = "palevioletred3")) + 
  theme_clean() + 
  coord_flip()

