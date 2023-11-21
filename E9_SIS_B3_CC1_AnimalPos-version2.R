## 11/2023
## Anja Magister
## ANALYSIS OF ANIMAL POSITIONS - VERSION2 ##
##

# libraries
library(readr)        # load readr package for reading csv files
library(stringr)
library(dplyr)
library(lubridate)    # for rounding time
library(tibble)       #important for tibble operations


# paths
fileSourcePath <-  "S:/Lab_Member/Anja/Git/AnjaIntern/E9_SIS_B3_CC1_AnimalPos.csv"


# read csv file in tibble

overallData <- read_delim(fileSourcePath,delim = ";", show_col_types = FALSE)

####################################################################################################################################
# organization of overallData:
# delete unnecessary columns
overallData <- select(overallData, -c(RFID, AM, zPos))
# convert the DateTime column to a datetime format(also rounds the DateTime)
overallData$DateTime <- as.POSIXct(overallData$DateTime, format = "%d.%m.%Y %H:%M:%S")

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
#?
overallData_final <- as_tibble(overallData_final)
########################################################################################################

# algorithm 

# declare start position and start time of mouse in one system(4mice together)
mouseOne    <- list(name="", time="", position=0)
mouseTwo    <- list(name="", time="", position=0)
mouseThree  <- list(name="", time="", position=0)
mouseFour   <- list(name="", time="", position=0)

mice_list <- list(
  "mouseOne" = mouseOne,
  "mouseTwo" = mouseTwo,
  "mouseThree" = mouseThree,
  "mouseFour" = mouseFour)

#print the list of mice 
for (sub_list in names(mice_list)) {
  print(sub_list)
  for (element in mice_list[[sub_list]]) {
    print(element)
  }
}
# save names of the first system
mice_systemOne <- overallData_final%>%
  filter(System == "sys.1")

mouse_names_systemOne <- unique(mice_systemOne$AnimalID)
mouse_names_systemOne[[1]]

# find the FIRST TIME where mouse is tracked in the cage
# aka first value of mopuse in overallData_final
for (i in 1:length(mouse_names_systemOne)){ #von i=1-4
  
  #rename
  mouse_name <- mouse_names_systemOne[[i]]
  #search first entry in whole data
  first_entry <- overallData_final%>%
    filter(AnimalID == mouse_name)%>%
    slice(1) #first row
    
  #write name, position and time into mice_list
  mice_list[i][[1]] <- mouse_name
  print(mouse_name)
  
  first_time <- first_entry$DateTime
  mice_list[[i]][[2]] <- first_time
  print(first_time)
  
  first_position <- first_entry$PositionID
  mice_list[[i]][[3]] <- first_position
  print(first_position)
}

#print(verschachtelte_liste[["tier1"]][[2]])
print(mice_list[mouse_names_systemOne[[i]]][[1]])
mice_list[mouse_names_systemOne[[i]]][[1]] <- first_time