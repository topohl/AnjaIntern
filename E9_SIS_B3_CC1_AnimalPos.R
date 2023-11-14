## 11/2023
## Anja Magister
## ANALYSIS OF ANIMAL POSITIONS ##
##

# libraries
library(readr)        # load readr package for reading csv files
library(stringr)
library(dplyr)
library(lubridate)    # for rounding time


# paths
fileSourcePath <-  "S:/Lab_Member/Anja/Git/AnjaIntern/E9_SIS_B3_CC1_AnimalPos.csv"


# read csv file in tibble

overallData <- read_delim(fileSourcePath,delim = ";", show_col_types = FALSE)


# normalization
# delete unnecessary columns
overallData <- select(overallData, -c(RFID, AM, zPos))
# convert the DateTime column to a datetime format(also rounds the DateTime)
overallData$DateTime <- as.POSIXct(overallData$DateTime, format = "%d.%m.%Y %H:%M:%S")

# separate date and time into extra columns
overallData[c('Date', 'Time')] <- str_split_fixed(overallData$DateTime, ' ', 2)



# sort columns
overallData <- overallData[c('Date', 'Time', 'Animal', 'xPos', 'yPos')]

# 2NF m&3NF
### create extra tibbles out of overallData: ###
# Dates
unique_dates <- unique(overallData$Date)
Dates_tibble <- tibble(DateID = c(1:length(unique_dates)), Date = unique_dates)

# Times
# Generate a sequence of times from 00:00:00 to 23:59:59
times_vector <- seq(from = as.POSIXct("00:00:00", format = "%H:%M:%S"), to = as.POSIXct("23:59:59", format = "%H:%M:%S"), by = "1 sec")
# Convert the times to the desired format "hh:mm:ss" (Date excluded)
times_vector <- format(times_vector, format = "%H:%M:%S")

# create a tibble of every possible time in 24h with seconds(one day has 86400 seconds)
Times_tibble <- tibble(TimeID = c(1:86400), Time = times_vector)

#Animals
unique_animals <- unique(overallData$Animal)
Animals_tibble <- tibble(AnimalID = c(1:length(unique_animals)), Animal = unique_animals)

#Positions
positions <- select(overallData, c(xPos,yPos))
unique_positions <- unique(positions)
Positions_tibble <- tibble(PositionID = c(1:length(unique_positions$xPos)), xPos = unique_positions[1], yPos = unique_positions[2])

# convert xPos and yPos to PositionsID(from Positions_tibble) in overallData


# convert Animal Number to AnimalID(from Animals_tibble) in overallData
overallData_animanls <- overallData%>%
  left_join(Animals_tibble, by = c("Animal")) %>%
  mutate(AnimalID = ifelse(is.na(AnimalID), "Unknown", as.character(AnimalID)))
#######################################
library(dplyr)

# Beispiel-Datensätze (Ersetzen Sie diese mit Ihren eigenen Daten)
coordinates_table <- tibble(
  x = c(10, 20, 30),
  y = c(15, 25, 35)
)

id_mapping_table <- tibble(
  ID = c(1, 2, 3),
  x = c(10, 20, 30),
  y = c(15, 25, 35)
)

# Zusammenführen der Tabellen basierend auf den Koordinaten
result <- coordinates_table %>%
  left_join(id_mapping_table, by = c("x", "y")) %>%
  mutate(ID = ifelse(is.na(ID), "Unknown", as.character(ID)))  # Wenn keine Übereinstimmung gefunden wurde, als "Unknown" markieren oder entsprechend anpassen

# Ausgabe des Ergebnisses
print(result)

###########################################################################################
# test to acces special table rows...
testTime = as.POSIXct("12:24:52", format = "%H:%M:%S")
testTime <- format(testTime, format = "%H:%M:%S")
testAnimal="OR414_sys.3"
specific_row <- overallData%>%
  filter(Time==testTime, Animal==testAnimal)
print(specific_row)

firstSpotInCage <- overallData%>%
  filter(Date=="2023-04-24", Time== testTime)

# plot a grid with all the points 
xPositions <- overallData %>% pull(xPos)               
yPositions <- overallData %>% pull(yPos)   
plot(xPositions, yPositions)

#dichte
plot(density(yPositions))  


plot(xPositions)
boxplot(xPositions) 

# number of same x Positions
a <- table(xPositions)
barplot(a)
# y pos
b <- table(yPositions)
barplot(b)



# every change from first animal

firstAnimalData <- overallData%>%
  filter(Animal=="OR428_sys.1")

xPositionsfirstA <- firstAnimalData %>% pull(xPos)               
yPositionsfirstA <- firstAnimalData %>% pull(yPos)  
plot(xPositionsfirstA, yPositionsfirstA)

# grid to see the corners (8 coils)
#vertical
abline(v=(seq(0,300,100)), col="lightgray", lty="dotted")
#horizontal
abline(h=(seq(0,116,58)), col="lightgray", lty="dotted")

##test
x <- c("22:33:21.23","21:34:00.54")
round(x,unit="seconds")
round_date(x,unit="seconds")