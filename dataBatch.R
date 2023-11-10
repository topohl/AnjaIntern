### dataBatch ###
# data contains differences between male and female mice in different actions
# directory path has to contain the sus_animals and the function sheet
# structure of file:
# - important definitions
# - code reads data out of excel files
# - necessary modifications for tests
# - test running, generating results in csv and plots
# - saving results


# Load libraries
library(ggplot2)
library(dplyr)
library(openxlsx)
library(rstatix)
library(readxl)
library(cowplot)

# Define constants for file paths, sheet names, specific animals, and group colors

# SOURCE PATHS
filePath <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/E9_Behavior_Data.xlsx"
sheetName <- "OFT_newSLEAP"
##working directory path
working_directory <- "S:/Lab_Member/Anja/Git/AnjaIntern"

#RESULT PATHS
saveResultPath <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/statistics"

# Include functions
source(paste0(working_directory,"/MMM_functions.R"))

# Read data from the Excel file
overallData <- read_excel(filePath, sheet = sheetName)

# Define SUS animals (csv file)
susAnimals <- c(readLines(paste0(working_directory,"/sus_animals.csv")))
# Define the factor to include/exclude
# Set to TRUE to include "Phase" or FALSE to exclude
include_phase <- incldeFactorExist("Phase", overallData, FALSE) 
include_sex <- incldeFactorExist("Sex", overallData, TRUE)

groupColors <- c("#1e3791", "#76A2E8", "#F79719")

#declare vectors of the variables which are not always included
phases <-  "combined phases"
if(include_phase) phases <-  c("Active", "Inactive")
sexes <-  "combined sexes"
if(include_sex) sexes <- c("male", "female")

########################### modify data ##########################################################################################


# Modify group assignment for specific animals
overallData <- overallData %>%
  mutate(Group = if_else(ID %in% susAnimals, "SUS",
                         if_else(Group == "SIS", "RES", Group)))

########################### execute tests and generate plots ############################################################################

# Get the list of columns to plot (excluding "ID", "Group", "Sex")
columnsToPlot <- setdiff(names(overallData), c("ID", "Group", "Sex", "Batch"))

# Initialize empty lists
allTestResults <- list()
allPlots <- list()
allPosthocResults <- list()

#declare vectors of the variables which are not always included
phases <-  "combined phases"
if(include_phase) phases <-  c("Active", "Inactive")
sexes <-  "combined sexes"
if(include_sex) sexes <- c("male", "female")

# Iterate through each variable and factor, and perform tests
for (variable in columnsToPlot) {
  for (phase in phases) {
    for(sex in sexes){
      result <- testAndPlotVariable(overallData, variable, phase, sex)
      # add result-list(containing the columns testResults, plot, posthocResults) to other fitting list
      if (!is.null(result)) {
        # posthocResults is always NULL for the Wilcoxon test
        if (is.null(result$posthocResults)) {
          allTestResults <- c(allTestResults, list(result$testResults))
        } else {
          allTestResults <- c(allTestResults, list(result$testResults))
          allPosthocResults <- c(allPosthocResults, list(result$posthocResults))
        }
        #add the plot
        allPlots <- c(allPlots, list(result$plot))
      }
      
    }
  }
}


############################# saving results ##############################################################################################

# Convert the list of test results to a data frame
allTestResultsDf <- bind_rows(allTestResults)

testpath <- "S:/Lab_Member/Anja/test"
# Save the test results data frame to a CSV file
write.csv(allTestResultsDf, file = paste0(testpath, "/test_results_", sheetName, ".csv"), row.names = FALSE)

# Save the post hoc results to a CSV file
if (!is.null(allPosthocResults) && length(allPosthocResults) > 0) {
  allPosthocResultsDf <- bind_rows(allPosthocResults)
  write.csv(allPosthocResultsDf, file = paste0(testpath, "/posthoc_results_", sheetName, ".csv"), row.names = FALSE)
}

#safe the plots
savePlotsInDir(allTestResults, allPlots, testpath, ".svg")

if(FALSE){
# Save the plots for males and females separately
for (i in seq_along(allPlots)) {
  variableName <- allTestResults[[i]]$Variable
  
  if (i %% 2 == 0) {
    ggsave(filename = paste0(testpath, "/female_", variableName, ".svg"), plot = allPlots[[i]], width = 2.8, height = 3)
  } else {
    ggsave(filename = paste0(testpath, "/male_", variableName, ".svg"), plot = allPlots[[i]], width = 2.8, height = 3)
  }
}
}