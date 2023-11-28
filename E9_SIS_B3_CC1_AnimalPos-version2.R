## 11/2023
## Anja Magister
## ANALYSIS OF ANIMAL POSITIONS - VERSION2 ##
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
fileSourcePath <-  "S:/Lab_Member/Anja/Git/AnjaIntern/E9_SIS_B3_CC1_AnimalPos.csv"

#fuctions
working_directory <- "S:/Lab_Member/Anja/Git/AnjaIntern"
source(paste0(working_directory,"/E9_SIS_B3_CC1_AnimalPos-functions.R"))

# read csv file in tibble

overallData <- read_delim(fileSourcePath,delim = ";", show_col_types = FALSE)

####################################################################################################################################
######ORGANIZATION OF overallData:######

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
Positions_tibble <- tibble(PositionID = c(1:length(unique_positions$xPos)), xPos = unique_positions[1], yPos = unique_positions[2])

# Adding column PositionID to overallData instead of two colums with x and y coordinates
overallData_ids <- overallData %>% rowwise() %>%
  mutate(PositionID = find_id(xPos, yPos, Positions_tibble))



##### sort columns #####
overallData_final <- overallData_ids[c('DateTime', 'AnimalID', 'System', 'PositionID')]
# column as tibble
overallData_final <- as_tibble(overallData_final)
################################################################################################################################


# ALGORITHM: 

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
#for (sub_list in names(mice_list)) {
#  print(sub_list)
#  for (element in mice_list[[sub_list]]) {
#    print(element)
#  }
#}

# save unique names of the first system:
mice_systemOne <- overallData_final%>%
  filter(System == "sys.1")

mouse_names_systemOne <- unique(mice_systemOne$AnimalID)

#####################################################################################

#update mice_list to first time and first position
mice_list <- find_first_pos_and_time(mouse_names_systemOne, overallData_final, mice_list)




#####################################################################################
#initialize mice closeness result
count_closeness_list <- list(   m1=c(0,0,0,0),
                                m2=c(0,0,0,0),
                                m3=c(0,0,0,0),
                                m4=c(0,0,0,0))

#update closeness list for the first time
count_closeness_list <- check_closeness1(mice_list, count_closeness_list)
#################################################################################
##assign start time(choose one of the mices start time)
start_time <- mice_list[[1]][[2]]

time <- start_time

###try a loop
#first last second:
#last_time <- "2023-04-28 11:00:14"
last_time <- "2023-04-25 11:00:14"



startTime <- Sys.time()
######### repeat over and over
#for(i in 1:432000){    #5days
for(i in 1:6600){ 
  time <- sec_shift(time)
    
  mice_list <- update_mice_list1(mouse_names_systemOne, mice_list, mice_systemOne, time)
  
  count_closeness_list <- check_closeness1(mice_list, count_closeness_list)
}
print(count_closeness_list)

endTime <- Sys.time() 
timeTaken <- endTime-startTime
cat("time taken: ", timeTaken, "\n")


print(as.POSIXct(as.numeric(time), origin = "1970-01-01"))

#as.POSIXct(as.numeric(time), origin = "1970-01-01") == "2023-04-29 12:24:52 CEST"

# Beispielzeitstempel
#changeable in numeric state
#has to be a character in the end
#new_time <- start_time%>%
#  as.numeric()%>%
#  +1%>%
#  as.character()

#printable in numeric state  
#print(as.POSIXct(as.numeric(new_time), origin = "1970-01-01"))
  

######################################################################
# HEATMAP

# calculate second entrys to hour entrys
copy_list <- count_closeness_list
count_closeness_list_hours <- lapply(copy_list, function(x) ifelse(x!=0,x/3600,x))
# print heatmap of count_closeness_list

# Konvertiere die Liste von Listen in eine Matrix
matrix_data <- do.call(rbind, count_closeness_list_hours)

dimnames(matrix_data) <- list(c("m1","m2","m3","m4"), c("m1","m2","m3","m4"))


#ggplot2
install.packages("reshape")                                       # Install reshape package
library("reshape")   

data_melt <- melt(matrix_data)                                           # Reorder data
head(data_melt) 

ggp <- ggplot(data_melt, aes(X1, X2)) +                           # Create heatmap with ggplot2
  geom_tile(aes(fill = value))+
  scale_fill_gradient(low = "white", high = "blue") +  # Farbgradient festlegen
  labs(title = "Heatmap der Daten", x = "X-Achse", y = "Y-Achse")  # Beschriftungen hinzufügen
ggp                                                               # Print heatmap



######################################################################

# (function?) to check if closer mice are still together
# counter for every possible couple
# reset and SAVE if not still close, count+1 if still close

#####
# function to save results in proper way
######################################################################

# plot existing data
#mouse1 on day 1
md1 <- overallData_final%>%
  filter(AnimalID=="OR428")%>%
  filter(grepl("2023-04-24", DateTime))%>%
  filter(PositionID<=8)%>%
  filter(hour(DateTime) == 19)
md1 <- select(md1,c(DateTime, PositionID))

md2 <- overallData_final%>%
  filter(AnimalID=="OR414")%>%
  filter(grepl("2023-04-24", DateTime))%>%
  filter(PositionID<=8)%>%
  filter(hour(DateTime) == 19)
md2 <- select(md2,c(DateTime, PositionID))

plot_micePositions_together(md1, md2)

########################################################################################################