# Load necessary libraries
library(ggplot2)
library(gridExtra)
library(cowplot)
library(tidyr)  # Load tidyr for pivot_wider
library(dplyr)
library(rstatix)


############## Define constants for file paths, sheet names, specific animals, and group colors ###########

# Define Paths
##sleep directory path for results 
sleep_directory <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/statistics/Sleep"
##graphs directory path for plots
graphs_directory <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/statistics/sleep/graphs"
##working directory path
working_directory <- "S:/Lab_Member/Anja/Git/AnjaIntern"


# Include functions
source(paste0(working_directory,"/MMM_functions.R"))


# Define the variable to include/exclude some columns
include_phase <- FALSE # Set to TRUE to include "Phase" or FALSE to exclude
include_sex <- FALSE  #same with sex

#if Phase column doesnt exist in dataframe dont include them
if (include_phase && !("Phase" %in% colnames(data_filtered))){
  cat("data_filtered does not contain the column Phase, include_phase stays FALSE")
  include_phase <- FALSE
}
#if Sex column doesnt exist in dataframe dont include them
if (include_sex && !("Sex" %in% colnames(data_filtered))){
  cat("data_filtered does not contain the column Sex, include_sex stays FALSE")
  include_sex <- FALSE
}

# Define SUS animals
##csv path for sus animals
csv_sus_animals <-  paste0(working_directory,"/sus_animals.csv")
sus_animals <- readLines(csv_sus_animals)

# Define group colors
groupColors <- c("#1e3791", "#76A2E8", "#F79719")


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

# Initialize empty lists
allTestResults <- list()
allPlots <- list()
allPosthocResults <- list()


#declare vectors of the variables which are not always included
phases <-  "combined phases"
if(include_phase) phases <-  c("Active", "Inactive")
sexes <-  "combined sexes"
if(include_sex) sexes <- c("male", "female")

# Iterate through each variable and phase, and perform tests
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

# Convert the list of test results to a data frame
allTestResultsDf <- bind_rows(allTestResults)

####################### saving the results #############################################################

# Save the test results data frame to a CSV file
write.csv(allTestResultsDf, file = paste0(sleep_directory,"/test_results_Sleep.csv"), row.names = FALSE)
# Save the post hoc results to a CSV file
if (!is.null(allPosthocResults) && length(allPosthocResults) > 0) {
  allPosthocResultsDf <- bind_rows(allPosthocResults)
  write.csv(allPosthocResultsDf, file = paste0(sleep_directory,"/posthoc_results_Sleep.csv"), row.names = FALSE)
}

# Save the plots as graphs(number and name of graph-file depends on included factors)
for (i in seq_along(allPlots)) { #for every plot that is documented
  #declare variables that are (potentially) included in filename
  variableName <- allTestResults[[i]]$Variable   
  ifelse(include_phase, phaseName <- paste0(allTestResults[[i]]$Phase, "_"), phaseName <-  "combined_phase_")    #new variable phaseName to use in the name of the file
  ifelse(include_sex, sexName <- paste0(allTestResults[[i]]$Sex, "_"), sexName <-  "combined_sex_") 
     
  #save in the directory for graphs 
  #alterate path if extra factors included
  if(include_phase) factorDir <-  "/include_phase"
  if(include_sex) factorDir <-  "/include_sex" 
  if(include_phase && include_sex) factorDir <- "/include_phase_and_sex"
  if(!include_phase && !include_sex) factorDir <- "/combine_phase_and_sex"
  ggsave(filename = paste0(graphs_directory, factorDir, "/", phaseName, sexName, variableName, ".svg"), plot = allPlots[[i]], width = 2.8, height = 3)
  
}


############### show plots in R ########################################################################

# Create a grid of plots
grid.arrange(grobs = allPlots, ncol = 4)
