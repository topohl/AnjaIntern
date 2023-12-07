## 11/2023
## Anja Magister
## ANALYSIS OF ANIMAL POSITIONS - ANALYZING ##
##

# libraries
library(readr)        # load readr package for reading csv files
library(dplyr)
library(lubridate)    # for rounding time, time operations in general
library(tibble)       #important for tibble operations
library(purrr)
library(ggplot2)      #for plots
library(reshape2)     #for heatmap plot
library(scales)       # for heatmap rescale

# paths
working_directory <- "S:/Lab_Member/Anja/Git/AnjaIntern/E9_SIS_B_CC1_AnimalPos"
#path of csv file 
csvFilePath <-  paste0(working_directory,"/overallData_preprocessed.csv")

#functions
source(paste0(working_directory,"/E9_SIS_B3_CC1_AnimalPos-functions.R"))

# read preprocessed data(csv file) in tibble
overallData <- as_tibble(read_delim(csvFilePath,delim = ",", show_col_types = FALSE))

################################################################################################################################
## given data "overallData" has already been processed ##
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


################################################################################################################################

#initialize result heatmap list
allHeatmaps <- list()

for(i in 1:length(systems_vector)){
  #cat("round", i, "\n")
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
  
  
  #print(mice_list)
  
  
  #still necessary??
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



