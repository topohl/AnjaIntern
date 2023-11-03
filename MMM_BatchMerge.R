## merge files from different batches
### Read in data from Folders and xlsx data. The data is organized in subfolders. 
#  This will read in the data of different xlsx sheets and merge them based on the name of the folder. 
#  Also, the data will be sorted and formatted
#  Later, the data points will be plotted.

library(readxl)      # load readxl package for reading excel files
library(dplyr)       # load dplyr package for data manipulation functions
library(purrr)       # load purrr package for functional programming
library(stringr)     # load stringr package for string manipulation
library(lubridate)   # load lubridate package for date and time functions
library(readr)        # load readr package for reading csv files
library(openxlsx)     # load openxlsx package for writing xlsx files


#include functions
working_directory <- "S:/Lab_Member/Anja/Git/AnjaIntern"
source(paste0(working_directory,"/MMM_functions.R"))

# set the working directory to the parent directory containing the subfolders an get a list of the B1 and B2 subfolders in the directory
setwd("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis")
#subfolders <- c("B3", "B4")
subfolders <- c("males/B1", "males/B2","females/B3", "females/B4")

#######################################################################################################################

#??include males 
#maybe setwd to..../BatchAnalysis 
#and then subfolders <- c("males/B1", "males/B2", females/B3", "females/B4")

# read all CSV files in the subfolders, transform them, and save as XLSX files
#female files
female_files <- list.files(path = "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis/females", pattern = ".csv$", recursive = TRUE, full.names = TRUE)
#male files
male_files <- list.files(path = "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis/males", pattern = ".csv$", recursive = TRUE, full.names = TRUE)
#join them together in files
files <- append(female_files, male_files)
#transform part
purrr::walk(files, process_and_save_xlsx)

# combine them into a single data frame
data <- map_dfr(subfolders, ~{
  # get a list of subfolders in the current B1 or B2 folder
  subfolders <- list.dirs(paste0(".", "/", .x), recursive = FALSE)
  
  # get files matching the pattern for Subfolders - this is for the SIS period
  all_files <- map(subfolders, ~list.files(path = .x, pattern = c("E9_SIS_B\\d+_CC\\d_ActivityIndex.csv"), full.names = TRUE)) %>% 
    flatten()
  
  ################### cookie hab #####  only activate when analysing cookie habituation ###################
  # all_files <- map(subfolders, ~list.files(path = .x, pattern = c("E9_SIS_B2_EPMaftrecagechange_ActivityIndex.csv"), full.names = TRUE)) %>% 
  # flatten()
  
  # process all files(add new columns) and combine into a single data frame
  all_data <- map(all_files, ~process_file(.x)) %>% 
    bind_rows()
  
  all_data
})})

