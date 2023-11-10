# Load necessary libraries
library(ggplot2)
library(gridExtra)
library(cowplot)
library(tidyr)  # Load tidyr for pivot_wider
library(dplyr)
library(rstatix)
library(readxl)

############## Define constants for file paths, sheet names, specific animals, and group colors ###########

# RESULT PATHS
##sleep directory path for results 
sleep_directory <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/statistics/Sleep"
##graphs directory path for result plots
graphs_directory <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/statistics/sleep/graphs"

# SOURCE PATHS
##working directory path
working_directory <- "S:/Lab_Member/Anja/Git/AnjaIntern"
# Include functions
source(paste0(working_directory,"/MMM_functions.R"))

# Define SUS animals (csv file)
sus_animals <- readLines(paste0(working_directory,"/sus_animals.csv"))



# Define group colors
groupColors <- c("#1e3791", "#76A2E8", "#F79719")

# Define the factor to include/exclude
# Set to TRUE to include "Phase" or FALSE to exclude
# includeFactorExists prohibits the use of a factor that does not exist in data
include_phase <- incldeFactorExist("Phase", data_filtered, FALSE) 
include_sex <- incldeFactorExist("Sex", data_filtered, TRUE)  

#include_gender <- incldeFactorExist("Gender", data_filtered, TRUE)



################################ Calculating and summarizing #########################################################

#add new columns(SleepCount, TotalCount...) to dataframe containing the required calculations
total_sleep_info_per_change <- data_filtered %>%
  group_by_at(intersect(c("AnimalNum", "Batch", "Change", "Group", "Phase", "Sex"), names(data_filtered))) %>%  #checks if all column names(esp."Phase" and "Sex") exist in data_filtered
  summarize(
    SleepCount = sum(ActivityIndex == 0),
    TotalCount = n(),
    PercentageSleep = (SleepCount / TotalCount) * 100,
    TotalSleepingTime = sum(ActivityIndex == 0),
    SleepBouts = sum(ActivityIndex == 0 & lag(ActivityIndex, default = 1) != 0),
    AvgSleepBoutDuration = ifelse(SleepBouts == 0, 0, TotalSleepingTime / SleepBouts)
  )


# Update Group column based on SUS animals
# overwrites some SIS and CON to SUS and RES groups
total_sleep_info_per_change <- total_sleep_info_per_change %>%
  mutate(
    Group = if_else(AnimalNum %in% sus_animals, "SUS", if_else(Group == "SIS", "RES", Group))
  )

# Summarize the data
# creates an overview of the sleep-activity of each individual
summarized_data <- total_sleep_info_per_change %>%
  group_by_at(intersect(c("AnimalNum", "Phase", "Sex"), names(total_sleep_info_per_change))) %>%  #checks if "Phase", "AnimalNum" and "Sex" exist in total_sleep_info_per_change
  summarize(
    Batch = first(Batch),
    Group = first(Group),
    SleepBouts = sum(SleepBouts),
    TotalSleepingTime = sum(TotalSleepingTime),
    PercentageSleep = mean(PercentageSleep),
    AvgSleepBoutDuration = mean(AvgSleepBoutDuration)
    
  ) %>%
  ungroup()


# Rename
overallData <- summarized_data

###################### execute tests and generate plots ###################################################

# Get the list of columns to plot (excluding non numeric values, which are added manually)
columnsToPlot <- setdiff(names(overallData), c("AnimalNum", "Group", "Phase", "Batch", "Sex"))

# run statistic tests on given data with wanted colums>
#generates three result dataframes

# Initialize empty lists
allTestResults <- list()
allPlots <- list()
allPosthocResults <- list()


#declare vectors of the variables which are not always included
phases <-  " "
if(include_phase) phases <-  c("Active", "Inactive")
sexes <-  " "
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



####################### saving the results #############################################################

# Convert the list of test results to a data frame
allTestResultsDf <- bind_rows(allTestResults)
# Save the test results data frame to a CSV file
write.csv(allTestResultsDf, file = paste0(sleep_directory,"/test_results_Sleep.csv"), row.names = FALSE)
# Save the post hoc results to a CSV file
if (!is.null(allPosthocResults) && length(allPosthocResults) > 0) {
  allPosthocResultsDf <- bind_rows(allPosthocResults)
  write.csv(allPosthocResultsDf, file = paste0(sleep_directory,"/posthoc_results_Sleep.csv"), row.names = FALSE)
}

#save the plots
savePlotsInDir(allTestResults, allPlots, graphs_directory, ".svg")


############### show plots in R ########################################################################

# Create a grid of plots
grid.arrange(grobs = allPlots, ncol = 4)
