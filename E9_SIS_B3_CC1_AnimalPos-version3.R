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
library(reshape2)     #for heatmap plot

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
systems_vector <- list(overallData_sys1, overallData_sys2, overallData_sys3, overallData_sys4, overallData_sys5)
typeof(systems_vector)

#save as csv file
#library(writexl)
#write_csv(overallData_sys1, "overallData_sys1.csv")
################################################################################################################################

#initialize result heatmap list
allHeatmaps <- list()

for(i in 1:length(systems_vector)){
#for(i in 5){
  cat("round", i, "\n")
  system <- systems_vector[[i]]
  #print(system)
  
  # ALGORITHM: 
  
  ##INITIALIZATIONS##
  mouse_names <- unique(system$AnimalID)
  
  
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
  mice_list <- find_first_pos_and_time(mouse_names, system, mice_list)
  
  print(mice_list)
  
  #update closeness list for the first time
  #count_closeness_list <- check_closeness(mice_list, count_closeness_list)
  
  ##assign start time(choose one of the mices start time)
  timeTemp <- mice_list[[1]][[2]]
  
  #assign first line number
  lineTemp <- 5
  
  #assign firt seconds difference
  secTemp <- 0
  
  
  ######### repeat over and over
  
  theEnd <- nrow(system)+1
  while(lineTemp!=theEnd && lineTemp<theEnd){
    
    #create a copy of the old version of the mice list for check_closeness-function
    old_mice_list <- mice_list
    
    mice_list <- update_mice_list(mouse_names, mice_list, system, timeTemp, lineTemp)
    
    #secTemp aus mice list
    secTemp <-  mice_list[["tempData"]][["secTemp"]]
    
    count_closeness_list <- check_closeness(old_mice_list,mice_list,count_closeness_list, secTemp)
    
    #line <- line+1 not necessarry!!
    #aber:
    lineTemp <- mice_list[["tempData"]][["lineTemp"]]
    #and new time temp
    timeTemp <- mice_list[[1]][[2]]
    
    
  }
  print(count_closeness_list)
  
  #generate heatmaps
  heatmap <- generateHeatMap(count_closeness_list, i, mouse_names)
  allHeatmaps <- c(allHeatmaps, list(heatmap))
}



############### show plots in R ########################################################################

# Create a grid of plots
gridExtra::grid.arrange(grobs = allHeatmaps, ncol = 2)


############################################################################################
## PLOT ##
######################################################################
# HEATMAP

generateHeatMap <- function(count_closeness_list, systemNum, mouse_names){
  # calculate second entrys to hour entrys
  count_closeness_list_hours <- lapply(count_closeness_list, function(x) ifelse(x!=0,x/3600,x))
  

  
  # convert list of lists into a matrix
  matrix_data <- do.call(rbind, count_closeness_list_hours)
  
  # names of the mice Ids
  dimnames(matrix_data) <- list(mouse_names, mouse_names)
  
    
  # melt the data, means create values combinations out of the matrix
  data_melt <- melt(matrix_data, as.is = TRUE, value.name = "hours")                                          # Reorder data
  head(data_melt) 
  
  
  #colourScale <- c("#0000FF", "#0560FF", "#2000FF", "#4000FF", "#6000FF", "#8000FF", "#A000FF", "#C000FF", "#EE00FF", "#99AAFF", "#0090FF")
  #value_ranges <- c("40", "50", "70", "90")  # Beispielwertebereiche
  #colors <- c("blue", "lightblue", "red", "darkred" )  # Beispiel-Farben
  
  #data_melt$hours <- as.factor(data_melt$hours)
  
  #create the plot
  ggp <- ggplot(data_melt, aes(Var1, Var2)) +                                 # Create heatmap with ggplot2
    geom_tile(aes(fill = hours))+
    #scale_fill_gradient(low = "white", high = "blue") +
    #scale_fill_gradientn(colors=colourScale) +# define colour gradient
    #scale_fill_gradient2(low = "darkblue",mid="blue", high = "red")+
    #scale_fill_manual(values = c("90" = "red", "70" = "lightblue", "50" = "blue"))+
    #scale_fill_manual(values = setNames(colors, value_ranges))+
    scale_fill_gradientn(colors = c("blue", "red"), values = rescale(c(min(data_melt), max(data_melt))))+
    labs(title = "system", i,": mice close contact in hours", x = "X", y = "Y")   # add labels and caption
 
  return(ggp)            
}

