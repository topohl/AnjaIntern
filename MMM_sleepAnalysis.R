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
include_phase <- TRUE # Set to TRUE to include "Phase" or FALSE to exclude
include_sex <- FALSE  #same with sex

# Define SUS animals
##csv path for sus animals
csv_sus_animals <-  paste0(working_directory,"/sus_animals.csv")
sus_animals <- readLines(csv_sus_animals)

# Define group colors
groupColors <- c("#1e3791", "#76A2E8", "#F79719")


#########################################################################################################

#desired_columns <- c("AnimalNum", "Batch", "Change", "Group", "Phase", "Sex")
#existing_columns <- intersect(desired_columns, names(data_filtered))

total_sleep_info_per_change <- data_filtered %>%
  group_by_at(intersect(c("AnimalNum", "Batch", "Change", "Group", "Phase", "Sex"), names(data_filtered))) %>%  #checks if all column names(esp."Phase" and "Sex") exist in data_filtered
  summarise(
    SleepCount = sum(ActivityIndex == 0),
    TotalCount = n(),
    PercentageSleep = (SleepCount / TotalCount) * 100,
    TotalSleepingTime = sum(ActivityIndex == 0),
    SleepBouts = sum(ActivityIndex == 0 & lag(ActivityIndex, default = 1) != 0),
    AvgSleepBoutDuration = ifelse(SleepBouts == 0, 0, TotalSleepingTime / SleepBouts)
  )


# Update Group column based on SUS animals
total_sleep_info_per_change <- total_sleep_info_per_change %>%
  mutate(
    Group = if_else(AnimalNum %in% sus_animals, "SUS", if_else(Group == "SIS", "RES", Group))
  )

# Summarize the data
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

###################### testing and plotting ############################################################

# Get the list of columns to plot (excluding non numeric values, which are added manually)
columnsToPlot <- setdiff(names(overallData), c("AnimalNum", "Group", "Phase", "Batch", "Sex"))

# Initialize empty lists
allTestResults <- list()
allPlots <- list()
allPosthocResults <- list()


#sex <- c("male", "female") ???

# Iterate through each variable and phase, and perform tests
for (variable in columnsToPlot) {
  for (phase in c("Active", "Inactive")) {
    result <- testAndPlotVariable(overallData, variable, phase)
    if (!is.null(result)) {
      if (is.null(result$posthocResults)) {
        allTestResults <- c(allTestResults, list(result$testResults))
      } else {
        allTestResults <- c(allTestResults, list(result$testResults))
        allPosthocResults <- c(allPosthocResults, list(result$posthocResults))
      }
      allPlots <- c(allPlots, list(result$plot))
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

# Save the plots for Phase1 and Phase2 separately if include_phase is TRUE
if (include_phase) {
  for (i in seq_along(allPlots)) {
    variableName <- allTestResults[[i]]$Variable      
    
    if (i %% 2 == 0) {
      ggsave(filename = paste0(graphs_directory,"/Phase2_", variableName, ".svg"), plot = allPlots[[i]], width = 2.8, height = 3)
    } else {
      ggsave(filename = paste0(graphs_directory,"/Phase1_", variableName, ".svg"), plot = allPlots[[i]], width = 2.8, height = 3)
    }
  }
} else {
  # Save all plots in a consistent manner when include_phase is FALSE
  for (i in seq_along(allPlots)) {
    variableName <- allTestResults[[i]]$Variable
    
    ggsave(filename = paste0(graphs_directory,"/", variableName, ".svg"), plot = allPlots[[i]], width = 2.8, height = 3)
  }
}

########################################################################################################

# Initialize an empty list to store plots
allPlots <- list()

# Iterate through each variable and phase, and perform tests
for (variable in columnsToPlot) {
  for (phase in c("Active", "Inactive")) {
    result <- testAndPlotVariable(overallData, variable, phase)
    if (!is.null(result)) {
      allPlots <- c(allPlots, list(result$plot))
    }
  }
}

# Create a grid of plots
grid.arrange(grobs = allPlots, ncol = 4)