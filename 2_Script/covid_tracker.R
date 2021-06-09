######################################################
##### PACKAGES
######################################################
# My functions
source("2_Script/get_data.R") # Covid-specific function to import, clean and merge data.
source("2_Script/covid_graphs.R") # Covid-specific data visualising functions.

library(RcppRoll) # For rolling averages.
library(zoo) # Also used for rolling averages.
library(ggstatsplot) # Used for "ggbetweenstats" - more statistically detailed graphs.

######################################################
##### IMPORT, CLEAN AND MERGE DATA
######################################################
df <- getData()

df_deaths <- df$deaths_table
df_confirmed <- df$confirmed_table

######################################################
###### EXPORT FILES AS CSV
######################################################
#exportCovidData()

######################################################
###### SELECT COUNTRIES
######################################################
df_deaths %<>% 
  graphPreparationDeaths()

df_confirmed %<>% 
  graphPreparationConfirmedCases()

# We don't want to study all countries. Let's select a couple based on intuition AND deaths. 
# We should try to select countries with reliable data. 
latest_deaths <- 
  df_deaths %>% 
  filter(Date == max(Date))

# FILTERING METHODS
  # Manual: 
    # Enter a list of countries (country_list).
  # Worst Cases Per Sub-Region: 
    # Top x countries with populations over 1,000,000 and most per-capita deaths per subregion.
    # Enter x (cases_per_subregion).
  # Continental:
    # Countries with populations over 1,000,000 within a continent. 
    # Enter a continent (continent).

listCountries <- 
  c("Sweden", 
    "Denmark", 
    "Japan", 
    "Italy",
    "US",
    "Germany", 
    "Korea, South",
    "Singapore", 
    "France",
    "United Kingdom", 
    "Spain",
    "Indonesia",
    "Belgium",
    "India",
    "Hungary",
    "Brazil", 
    "Mexico", 
    "Peru")

df_deaths_filtered <- 
  filterData(data = df_deaths, method = "Manual", country_list = listCountries)

df_confirmed_filtered <- 
  filterData(data = df_confirmed, method = "Manual", country_list = listCountries) # For confirmed cases, only the Manual method works.

######################################################
###### VISUALISE: DEATHS IN TOTAL AND PER CAPITA OVER TIME PER COUNTRY
######################################################
# Confirmed deaths per date after 1st death.
deathsOverTime(df = df_deaths_filtered, 
                deathtype = confirmedDeaths,
                time = Date,
                xtitle = "Date",
                ytitle = "Covid-19 deaths") 

# Confirmed deaths per day after 1st death.
deathsOverTime(df = df_deaths_filtered, 
                deathtype = confirmedDeaths,
                time = daysPassed,
                xtitle = "Days since the first confirmed covid-19 death",
                ytitle = "Covid-19 deaths") +
  scale_x_continuous(breaks = seq(0, 1000, 100)) + 
  scale_y_continuous(breaks = seq(0, 1000000, 100000)) 

# Same with log-scale.
deathsOverTime(df = df_deaths_filtered, 
                deathtype = confirmedDeaths,
                time = daysPassed,
                xtitle = "Days since the first confirmed covid-19 death",
                ytitle = "Covid-19 deaths") +
  scale_x_continuous(breaks = seq(0, 1000, 100)) + 
  scale_y_continuous(trans = "log10", 
                     labels = trans_format("log10", math_format(10^.x)))


# Adjust the first death-graph for population.
deathsOverTime(df = df_deaths_filtered, 
                deathtype = deathPerCapita,
                time = daysPassed,
                xtitle = "Days since the first confirmed covid-19 death",
                ytitle = "Covid-19 deaths per 100,000 inhabitants") + 
  scale_x_continuous(breaks = seq(0, 1000, 100)) + 
  scale_y_continuous(breaks = seq(0, 700, 50))


######################################################
###### VISUALISE: DAILY NEW DEATHS PER DAY AND REGION 
######################################################
deathsByGeo(data = df_deaths, segment = Region)
deathsByGeo(data = df_deaths, segment = subRegion) # I have hard-coded certain subregions for visibility.
deathsByGeo(data = df_deaths, segment = Country) # I have hard-coded certain countries for visibility. 

# Just to get totals, as well.
ggplot(data=dailyDeathsAndRollingAvgs(data = df_deaths, segment = NULL), 
       aes(x=Date, 
           y=totalDeaths)) + 
  geom_line() +
  labs(title = "Total deaths globally", 
       x = "Date",
       y = "Confirmed deaths",
       caption = "Source: John Hopkins Coronavirus Resource Center") +
  scale_y_continuous(breaks = seq(0, 6000000, 1000000), labels = comma) + 
  theme_clean()

ggplot(data=dailyDeathsAndRollingAvgs(data = df_deaths, segment = NULL), 
       aes(x=Date, 
           y=SMA7)) + 
  geom_line() +
  labs(title = "Total daily new deaths (SMA7)", 
       x = "Date",
       y = "New confirmed deaths (SMA7)",
       caption = "Source: John Hopkins Coronavirus Resource Center") +
  theme_clean()

######################################################
###### VISUALISE: DEATH RATE RELATED TO OTHER FACTORS
######################################################
# Scatter plot of death rate per share of 65+ elders and continent
ggplot(data = latest_deaths,
       aes(y = deathPerCapita,
           x = percentageAbove65_2018, # percentageAbove65_2018
           color = Region)) + 
  geom_point(size = 5, alpha = 0.8) + 
  labs(title = "Comparison of death rates by share of elders per country", 
       x = "% Population above 65 years",
       y = "Covid-19 deaths per 100,000 inhabitants",
       caption = "Source: John Hopkins Coronavirus Resource Center (2021) & World Bank (2018)") +
  scale_fill_manual(values = wes_palette("Rushmore1")) +
  theme_clean() 

# Boxplot of death rate per continent
ggbetweenstats(data = latest_deaths, 
               x = Region,
               y = deathPerCapita,
               outlier.tagging = TRUE,
               outlier.label = Country, 
               title = "Comparison of death rates across continents",
               caption = "Source: John Hopkins Coronavirus Resource Center, World Bank (2018)", 
               ylab = "Covid-19 deaths per 100,000 inhabitants", 
               xlab = "Continent", 
               results.subtitle = FALSE)

######################################################
##### HOW MANY PERCENT OF WORLD POPULATION AND COVID DEATHS?
######################################################
TOTAL_DEATHS <- latest_deaths %>% 
  ungroup() %>% 
  select(confirmedDeaths) %>% 
  na.omit() %>% 
  summarise(confirmedDeaths = sum(confirmedDeaths))

WORLD_POPULATION <- latest_deaths %>% 
  ungroup() %>% 
  select(Population_2018) %>% 
  na.omit() %>% 
  summarise(Population_2018 = sum(Population_2018))

relative_impact <- latest_deaths %>% 
  select(Country, Region, confirmedDeaths, deathPerCapita, Population_2018) %>% 
  mutate(share_of_world_population = round(Population_2018/WORLD_POPULATION[[1,1]], 6), 
         share_of_covid_deaths = round(confirmedDeaths/TOTAL_DEATHS[[1,1]], 6)) %>% 
  mutate(relative_impact = share_of_covid_deaths/share_of_world_population)

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


impactPlot <- function(df, title_text) {
  ggplot(data = df, 
         aes(x = reorder(Country, relative_impact), 
             y = relative_impact, 
             fill = Region)) + 
    geom_col() + 
    geom_hline(yintercept = 1, color = "navy", size = 1) + 
    labs(title = title_text,
         subtitle = "The impact score is defined as the country's share of global covid-19 deaths divided by its share of world population.
An impact score = 1 indicates an average deaths-to-population ratio. Higher than 1 indicates overproportionate deaths.", 
         x = "Country",
         y = "Impact score = (Share of global covid-19 deaths) / (Share of world population)",
         caption = "Source: John Hopkins Coronavirus Resource Center & World Bank Population Data (2018)") + 
    scale_fill_manual("Continent", values = c("Asia" = "seagreen3", 
                                              "Africa" = "goldenrod3", 
                                              "Europe" = "steelblue4",
                                              "Americas" = "coral2",
                                              "Oceania" = "palevioletred3")) + 
    theme_clean() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
    coord_flip()
  
}

impactPlot(df = relative_impact %>% filter(Country %in% listCountries_biggest_populations), 
           title_text = "Impact among the 20 most populated countries in the world")

impactPlot(df = relative_impact %>% filter(Country %in% listCountries_biggest_economies), 
           title_text = "Impact among the 20 biggest economies in the world")

impactPlot(df = relative_impact %>% filter(Country %in% listCountries_richest_economies), 
           title_text = "Impact among the 20 richest per-capita economies in the world (Macao and Hong Kong data not available)")

impactPlot(df = relative_impact %>% filter(confirmedDeaths > 5000), 
           title_text = "Impact among countries with at least 5000 deaths")

impactPlot(df = relative_impact %>% filter(deathPerCapita > 50), 
           title_text = "Impact among countries with at least 50 death per capita")
