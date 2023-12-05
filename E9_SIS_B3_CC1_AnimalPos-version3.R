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


#define systems
overallData_sys1 <- overallData%>%
  filter(System=="sys.1")%>%
  as_tibble()

overallData_sys2 <- overallData%>%
  filter(System=="sys.2")%>%
  as_tibble()

overallData_sys3 <- overallData%>%
  filter(System=="sys.3")%>%
  as_tibble()

overallData_sys4 <- overallData%>%
  filter(System=="sys.4")%>%
  as_tibble()

overallData_sys5 <- overallData%>%
  filter(System=="sys.5")%>%
  as_tibble()

# create a vector of the five systems
systems_vector <- c(overallData_sys1, overallData_sys2, overallData_sys3, overallData_sys4, overallData_sys5)


#save as csv file
#library(writexl)
#write_csv(overallData_sys1, "overallData_sys1.csv")
################################################################################################################################

# ALGORITHM: 

##INITIALIZATIONS##
mouse_names_system1 <- unique(overallData_sys1$AnimalID)


# initialize mice lists with empty name, start time and start position of every mouse in one system(4mice together)
mouseOne    <- list(name="", time="", position=0)
mouseTwo    <- list(name="", time="", position=0)
mouseThree  <- list(name="", time="", position=0)
mouseFour   <- list(name="", time="", position=0)
tempData    <- list(secTemp=0, lineTemp=0)

# combine them to a list of lists
mice_list <- list(
  "mouseOne" = mouseOne,
  "mouseTwo" = mouseTwo,
  "mouseThree" = mouseThree,
  "mouseFour" = mouseFour,
  "tempData" = tempData)


#initialize mice closeness result
#m1 on the third int means number of seconds together from m1 and m3
count_closeness_list <- list(   m1=c(0,0,0,0),
                                m2=c(0,0,0,0),
                                m3=c(0,0,0,0),
                                m4=c(0,0,0,0))


##CALCULATIONS##

#update mice_list to first time and first position
mice_list <- find_first_pos_and_time(mouse_names_system1, overallData_sys1, mice_list)

#update closeness list for the first time
#count_closeness_list <- check_closeness(mice_list, count_closeness_list)

##assign start time(choose one of the mices start time)
timeTemp <- "1682331892"

#assign first line number
lineTemp <- 5

#assign firt seconds difference
secTemp <- 0


######### repeat over and over

#while(lineTemp!=(6584)){#5 days(aka whole tibble, all lines in tibble)
theEnd <- (6584+1)
while(lineTemp!=theEnd && lineTemp<theEnd){
  
  #create a copy of the old version of the mice list for check_closeness-function
  old_mice_list <- mice_list
  
  mice_list <- update_mice_list(mouse_names_system1, mice_list, overallData_sys1, timeTemp, lineTemp)
  
  #secTemp aus mice list
  secTemp <-  mice_list[["tempData"]][["secTemp"]]
  
  count_closeness_list <- check_closeness(old_mice_list,mice_list,count_closeness_list, secTemp)
  
  #line <- line+1 not necessarry!!
  #aber:
  lineTemp <- mice_list[["tempData"]][["lineTemp"]]
  #and new time temp
  timeTemp <- mice_list[[1]][[2]]
  
  
  #if(lineTemp>2908){break}
}
print(count_closeness_list)





############################################################################################
## PLOT ##
######################################################################
# HEATMAP

# calculate second entrys to hour entrys
copy_list <- count_closeness_list
count_closeness_list_hours <- lapply(copy_list, function(x) ifelse(x!=0,x/3600,x))


# print heatmap of count_closeness_list

# Konvertiere die Liste von Listen in eine Matrix
matrix_data <- do.call(rbind, count_closeness_list_hours)

# names of the mice Ids
dimnames(matrix_data) <- list(mouse_names_system1, mouse_names_system1)


#ggplot2
install.packages("reshape")                                       # Install reshape package
library("reshape")   

data_melt <- melt(matrix_data)                                          # Reorder data
head(data_melt) 

ggp <- ggplot(data_melt, aes(X1, X2)) +                                 # Create heatmap with ggplot2
  geom_tile(aes(fill = value))+
  scale_fill_gradient(low = "white", high = "blue") +                   # define colour gradient
  labs(title = "System-1: mice closeness in hours", x = "X", y = "Y")   # add labels and caption
ggp                                                                     # Print heatmap

