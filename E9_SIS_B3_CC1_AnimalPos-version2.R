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

#fuctions
working_directory <- "S:/Lab_Member/Anja/Git/AnjaIntern"
source(paste0(working_directory,"/E9_SIS_B3_CC1_AnimalPos-functions.R"))

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


#create Positions_tibble that contains every possible combination of our coordinates together with an ID
positions <- select(overallData, c(xPos,yPos))
unique_positions <- unique(positions)
Positions_tibble <- tibble(PositionID = c(1:length(unique_positions$xPos)), xPos = unique_positions[1], yPos = unique_positions[2])

# Adding column PositionID to overallData instead of two colums with x and y coordinates
overallData_ids <- overallData %>% rowwise() %>%
  mutate(PositionID = find_id(xPos, yPos, Positions_tibble))


##############################################################################

# sort columns
overallData_final <- overallData_ids[c('DateTime', 'AnimalID', 'System', 'PositionID')]
# column as tibble
overallData_final <- as_tibble(overallData_final)
########################################################################################################

# algorithm: 

# initialize mice lists with empty name, start time and start position of every mouse in one system(4mice together)
mouseOne    <- list(name="", time="", position=0)
mouseTwo    <- list(name="", time="", position=0)
mouseThree  <- list(name="", time="", position=0)
mouseFour   <- list(name="", time="", position=0)

#put the four lists in one list together
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

# save unique names of the first system:
mice_systemOne <- overallData_final%>%
  filter(System == "sys.1")

mouse_names_systemOne <- unique(mice_systemOne$AnimalID)

#####################################################################################
# find the FIRST TIME where mouse is tracked in the cage
# aka first value of mouse in overallData_final
for (i in 1:length(mouse_names_systemOne)){ #i=1-4
  
  #rename
  mouse_name <- mouse_names_systemOne[[i]]
  #search first entry in whole data
  first_entry <- overallData_final%>%
    filter(AnimalID == mouse_name)%>%
    slice(1) #first row
    
  #write name, position and time into mice_list
  mice_list[i][[1]] <- mouse_name
  #print(mouse_name)
  
  first_time <- first_entry$DateTime
  mice_list[[i]][[2]] <- first_time
  #print(first_time)
  
  first_position <- first_entry$PositionID
  mice_list[[i]][[3]] <- first_position
  #print(first_position)
}

#####################################################################################

# function to check for closeness
# input: mice_list
# compare every sublist(4)to each other
check_closeness <- function(mice_list){
  
  # create every possible couple
  mice <- names(mice_list)
  combination <- as_tibble(combn(mice, 2))
  
  for (i in 1:3) {
    for (j in (i+1):4) {
      print(c(i, j))
      #print(mice_list[[i]][[3]])
      #print(mice_list[[j]][[3]])
      print(mice_list[[i]][[3]]==mice_list[[j]][[3]])
    }
  }
  # compare the third value of every couple
  # if the position is the same, save in result list?
  # return list of mice that are close to each other
  # like(("OR428","OR420"),...)
}
######################################################################

# (function?) to check if closer mice are still together
# counter for every possible couple
# reset and SAVE if not still close, count+1 if still close

#####
# function to save results in proper way
######################################################################

# function to do shift in time(one second forward)
# return eventually alterated mice_list

######### repeat over and over
