## 11/2023
## Anja Magister
## ANALYSIS OF ANIMAL POSITIONS - VERSION3 ##
##

# libraries
library(readr)        # load readr package for reading csv files
library(stringr)
library(dplyr)
library(lubridate)    # for rounding time, time operations in general
library(tibble)       #important for tibble operations
library(purrr)
library(ggplot2)      #for plots

# paths
working_directory <- "S:/Lab_Member/Anja/Git/AnjaIntern"
fileSourcePath <-  paste0(working_directory,"/E9_SIS_B3_CC1_AnimalPos.csv")

#fuctions
source(paste0(working_directory,"/E9_SIS_B3_CC1_AnimalPos-functions.R"))

# read csv file in tibble

overallData <- as_tibble(read_delim(fileSourcePath,delim = ";", show_col_types = FALSE))

####################################################################################################################################
###### PREPROCESSING OF overallData: ######

# delete unnecessary columns
overallData <- select(overallData, -c(RFID, AM, zPos))
# convert the DateTime column to a datetime format(also rounds the DateTime)
overallData$DateTime <- as.POSIXct(overallData$DateTime, format = "%d.%m.%Y %H:%M:%S")

# separate Animal into his ID an his system
overallData[c('AnimalID', 'System')] <- str_split_fixed(overallData$Animal, '_', 2)




###### convert xPos and yPos into one column named "PositionID" ######

#create Positions_tibble that contains every possible combination of our coordinates together with an ID
positions <- select(overallData, c(xPos,yPos))
unique_positions <- unique(positions)
Positions_tibble <- tibble(PositionID = c(1:8), xPos = c(0,100,200,300,0,100,200,300), yPos = c(0,0,0,0,116,116,116,116))

# Adding column PositionID to overallData instead of two colums with x and y coordinates
overallData_ids <- overallData %>% rowwise() %>%
  mutate(PositionID = find_id(xPos, yPos, Positions_tibble))



##### sort columns #####
overallData <- overallData_ids[c('DateTime', 'AnimalID', 'System', 'PositionID')]


##### sort by Date Time #####
overallData <- overallData%>%
  arrange(., DateTime)
################################################################################################################################

#choose system
overallData_sys1 <- overallData%>%
  filter(System=="sys.1")
################################################################################################################################

