## 11/2023
## Anja Magister
## ANALYSIS OF ANIMAL POSITIONS ##
##

# libraries
library(readr)        # load readr package for reading csv files
library(stringr)


# paths
fileSourcePath <-  "S:/Lab_Member/Anja/Git/AnjaIntern/E9_SIS_B3_CC1_AnimalPos.csv"


# read csv file in tibble

overallData <- read_delim(fileSourcePath,delim = ";", show_col_types = FALSE)


# normalization
# delete unnecessary columns
overallData <- select(overallData, -c(RFID, AM, zPos))
# separate date and time into extra columns
overallData[c('Date', 'Time')] <- str_split_fixed(overallData$DateTime, ' ', 2)
#sort columns
overallData <- overallData[c('Date', 'Time', 'Animal', 'xPos', 'yPos')]
# 2NF



# plot a grid with all the points 
