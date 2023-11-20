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


# organization of overallData:
# delete unnecessary columns
overallData <- select(overallData, -c(RFID, AM, zPos))
# convert the DateTime column to a datetime format(also rounds the DateTime)
overallData$DateTime <- as.POSIXct(overallData$DateTime, format = "%d.%m.%Y %H:%M:%S")

# separate date and time into extra columns
#overallData[c('Date', 'Time')] <- str_split_fixed(overallData$DateTime, ' ', 2)

# separate Animal into his ID an his system
overallData[c('AnimalID', 'System')] <- str_split_fixed(overallData$Animal, '_', 2)


#################### convert xPos and yPos into one column named "PositionID" ###########################################################

# finds corresponding PositionID from to coordinates(x_Pos, y_pos) in lookup_tibble and returns ID 
find_id <- function(x_Pos, y_Pos, lookup_tibble) {
  result <- lookup_tibble %>%
    filter(xPos == x_Pos, yPos == y_Pos) %>%
    select(PositionID)
  
  if (nrow(result) > 0) {
    return(result$PositionID)
  } else {
    return(NA) # Return NA if no match found
  }
}

#Positions_tibble contains every possible combination of our coordinates together with an ID
positions <- select(overallData, c(xPos,yPos))
unique_positions <- unique(positions)
Positions_tibble <- tibble(PositionID = c(1:length(unique_positions$xPos)), xPos = unique_positions[1], yPos = unique_positions[2])

# Adding column PositionID to overallData instead of two colums with x and y coordinates
overallData_ids <- overallData %>% rowwise() %>%
  mutate(PositionID = find_id(xPos, yPos, Positions_tibble))


##############################################################################

# sort columns
overallData_final <- overallData_ids[c('DateTime', 'AnimalID', 'System', 'PositionID')]

########################################################################################################
## divide overallData_final into his 5 different systems (5 different mouse cages)

# maybe just safe the ids from each cage in a vector?

data_systemOne <- overallData_final%>%
  filter(System=="sys.1")
data_systemTwo <- overallData_final%>%
  filter(System=="sys.2")
data_systemThree <- overallData_final%>%
  filter(System=="sys.3")
data_systemFour <- overallData_final%>%
  filter(System=="sys.4")
data_systemFive <- overallData_final%>%
  filter(System=="sys.5")

## create empty time tibble that contains rows for every second in 5 days
# Erzeuge eine Sequenz von Zeitstempeln
start_time <- as.POSIXct("2023-04-24 00:00:00", tz = "")
end_time <- as.POSIXct("2023-04-28 23:59:59", tz = "")

time_sequence <- seq(start_time, end_time, by = "1 sec")

# Konvertiere die Zeitstempel in ein Tibble
fullTime <- tibble(DateTime = time_sequence)
############################################################################
# for every mouse from system one enter information
enter_mouseData_into_fullTime <- function(mouseTibble, fullTimeTibble){
  #vector of each mouse in mouseTibble
  unique_mice <- unique(mouseTibble$AnimalID)
  print(unique_mice)
  counter <- 1
  
  #for every time row from one mouse in mouseTibble
  for(mouse in unique_mice){
    print(mouse)
  
    #   create new column for AnimalID
    fullTimeTibble <- fullTimeTibble%>%
      mutate(!!mouse := NA)
    # filter the mouse tibble only for the changes of current mouse
    all_changes <- mouseTibble%>%
      filter(AnimalID == mouse)
    print(all_changes)
    
    for(changeTime in as.vector(all_changes$DateTime)){  
      cat("mouse: ", mouse, "\n")   
      cat("Row: ", counter, "\n")                
      date_time <- as.POSIXct(changeTime, origin = "1970-01-01")
      formatted_date <- format(date_time, "%Y-%m-%d %H:%M:%S") 
      cat("changeTime-stamp: ", changeTime, "\n")
      cat("actual changeTime: ", formatted_date, "\n")
      
      cat("fullTimeTibble: ", "\n")
      #print(fullTimeTibble[45363:45563, ])
      print_tibble <- fullTimeTibble %>%
        slice(45385:45585) %>%
        print()
      
      # save the new position at the moment of changeTime in extra variable
      print(typeof(mouseTibble$DateTime))
      print(typeof(changeTime))
      print(typeof(mouseTibble$AnimalID))
      print(typeof(mouse))
      
      #creates a vector with one (or sometimes two) positions(when time was rounded onto the same second)
      new_mouse_position_vector <- mouseTibble%>%
        filter(DateTime==changeTime)%>%
        filter(AnimalID == mouse)%>%
        select(PositionID)%>%
        as.vector()
      cat("\n", "new_mouse_position: ", "\n")
      print(new_mouse_position_vector)
      
      #   search the recorded time from changeTime in fullTimeTibble and save row index
      current_row <- which(fullTimeTibble$DateTime == changeTime)
      cat("\n", "current row: ")
      print(current_row)
      
      for(new_mouse_position in new_mouse_position_vector){
      #enter new_mouse_position into current_row X mouse(row X column) from mouse in fullTimeTibble
      if (is.na(fullTimeTibble[current_row, mouse])) {
        fullTimeTibble[current_row, mouse] <- new_mouse_position
      } else {
        if(length(new_mouse_position_vector)>1){  #if there are multiple position values for the same changeTime(rounded to second)
          
        }else{
          print(fullTimeTibble[current_row, mouse] <- new_mouse_position)
          stop("Error: The current cell already has a value.")
        }
      }
      
      }
      
      
      cat("test position after entering value", "\n")
      print(fullTimeTibble[current_row, mouse])
      cat("END OF ROUND", "\n")
      cat("\n")
      counter <- counter+1
      
    }
  }
  return(fullTimeTibble)
}

##test
new_mouse_position <- data_systemOne%>%
  filter(DateTime=="2023-04-24 12:24:52", AnimalID=="OR428")
new_mouse_position <- new_mouse_position%>%
  select(PositionID)
mouse <- "86876"
!!sym(mouse)
for (change in overallData$DateTime){
  print(change)
}

all_changes <- data_systemOne%>%
  filter(AnimalID == "OR428")
print(all_changes)

all_change_times <- as.vector(all_changes$DateTime)
print(all_change_times)

print(1682332025==1682332051)
print(distinct(as.vector(data_systemOne$DateTime)))

current_row <- fullTime%>%
  filter(DateTime==as.POSIXct("2023-04-24 14:24:52"))
cat("\n", "current row: ", "\n")
print(current_row)

#test enter value in tibble
testest <- tibble(
  a=c(1,2,NA),
  b=c(7,8,9)
)
testest[3,"a"] <- 3
rowtest <- testest%>%
  filter(a==2)%>%
  rownames()
rowtest
rownames(rowtest)  
typeof(testest)
typeof(rowtest)





bubble <- data_systemOne%>%
  filter(DateTime=="2023-04-25 23:37:38")%>%
  filter(AnimalID=="OR428")%>%
  select(PositionID)
  
bubbletwo <- c(bubble$PositionID)

##############################################

fullTimeTibble <-fullTime%>%
  filter(date(DateTime) == ymd("2023-04-25"))%>%
  mutate("OR428" := NA)

#   search the recorded time from changeTime in fullTimeTibble and save row index
current_row <- which(fullTimeTibble$DateTime == "2023-04-25 23:37:38")
mouse <- "OR428"

#####
fullTimeTibble[current_row, mouse] <- 7
fullTimeTibble <- fullTimeTibble %>%
  add_row(DateTime = ymd_hms("2023-04-25 23:37:38", tz = "UTC"), OR428 = 8, .after = current_row)

#tz = ""
#######

for(new_mouse_position in bubbletwo){
  print(new_mouse_position)
  #enter new_mouse_position into current_row X mouse(row X column) from mouse in fullTimeTibble
  if (is.na(fullTimeTibble[current_row, mouse])) {
    fullTimeTibble[current_row, mouse] <- new_mouse_position
  } else {#another position exists
    fullTimeTibble <-add_row(fullTimeTibble, DateTime = ymd_hms("2023-04-25 23:37:38"),OR428=new_mouse_position, .after = current_row)
    #fullTimeTibble <- fullTimeTibble %>% add_row(DateTime = ymd_hms("2023-04-25 23:37:38"), OR428 = 9, .after = current_row)
  }
}
##############################################


#test function
fullTime_test <- enter_mouseData_into_fullTime(data_systemOne,fullTime)


########################################################################################################maybe not necessary
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
#positions <- select(overallData, c(xPos,yPos))
#unique_positions <- unique(positions)
#Positions_tibble <- tibble(PositionID = c(1:length(unique_positions$xPos)), xPos = select(unique_positions, c(xPos)), yPos = select(unique_positions, c(yPos)))



# convert Animal Number to AnimalID(from Animals_tibble) in overallData
overallData_animals <- overallData%>%
  left_join(Animals_tibble, by = c("Animal")) %>%
  mutate(AnimalID = ifelse(is.na(AnimalID), "Unknown", as.character(AnimalID)))
#######################################

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
abline(v=(seq(0,400,100)), col="lightgray", lty="dotted")
#horizontal
abline(h=(seq(0,116,58)), col="lightgray", lty="dotted")
