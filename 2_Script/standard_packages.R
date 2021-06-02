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

#Palettes and visualization:
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(viridis)
library(RColorBrewer)
library(wesanderson)
library(ggthemes)
library(directlabels) # FOR ADDING END LABELS TO THE LINES, USE DIRECTLABELS
library(forcats) # For re-arranging label levels in a graph.
library(scales)

#Data
library(gapminder)
library(maptools)