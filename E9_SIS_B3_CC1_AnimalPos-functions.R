
## 11/2023
## Anja Magister
## ANALYSIS OF ANIMAL POSITIONS - FUNCTIONS ##
##








##############################################################################################################
# finds corresponding PositionID from two coordinates(x_Pos, y_pos) in lookup_tibble and returns ID 
find_id <- function(x_Pos, y_Pos, lookup_tibble) {
  result <- lookup_tibble %>%
    filter(xPos == x_Pos, yPos == y_Pos) %>%
    select(PositionID)
  
  if (nrow(result) > 0) {
    return(result$PositionID)
  } else {
    return(NA) # Return NA if no match found
  }
}
##############################################################################################################
# function to check for closeness
# input: mice_list
# compare every sublist(4)to each other
check_closeness <- function(mice_list,count_closeness_list){
  
  # compare the third value of every couple
  # if the position is the same, save in count_closeness_list list
  for (i in 1:3) {
    for (j in (i+1):4) {
      #print(c(i, j))
      #print(mice_list[[i]][[3]]==mice_list[[j]][[3]])
      if(mice_list[[i]][[3]]==mice_list[[j]][[3]]){
        count_closeness_list[[i]][[j]] <- +1
      }
    }
  }
  print(count_closeness_list)
  # return updated list of mice that are close to each other
  return(count_closeness_list)
}

##############################################################################################################
## plot ##

# Function for adjusting the Y-axis labelling
custom_labels <- function(x) {
  labels <- ifelse(x %% 1 == 0, "", as.character(x - 0.5))  # show even numbers on odd place(number 1 on place 1.5), dont show even place numbers 
  return(labels)
}

#function to plot 4 different mice an their movement in the cage
plot_micePositions_together <- function(m1, m2){#startTime,, m2, m3, m4 ...
  
  #alterate y-points to 0,125
  m1 <- m1%>%mutate(PrettyPos = map_dbl(m1$PositionID, ~ .+0.125))#(m1$PositionID, ~ .+0.125))
  m2 <- m2%>%mutate(PrettyPos = map_dbl(m2$PositionID, ~ .+0.325))#(m1$PositionID, ~ .+0.325))
  #define data
  data1 <- data.frame(
    time = m1$DateTime,
    position = m1$PrettyPos
  )
  data2 <- data.frame(
    time = m2$DateTime,
    position = m2$PrettyPos
  )
  
  
  ggplot() +
    geom_point(data = data1, aes(x = time, y = position), size = 1, color = "blue") +
    geom_segment(data = data1 %>% mutate(next_time = lead(time, default = last(time))),
                 aes(x = time, xend = next_time, y = position, yend = position),
                 arrow = arrow(length = unit(0.1, "cm")), size = 0.5, color = "blue") +
    geom_point(data = data2, aes(x = time, y = position), size = 1, color = "red") +
    geom_segment(data = data2 %>% mutate(next_time = lead(time, default = last(time))),
                 aes(x = time, xend = next_time, y = position, yend = position),
                 arrow = arrow(length = unit(0.1, "cm")), size = 0.5, color = "red") +
    geom_hline(yintercept = 1:8, color = "black", linetype = "solid", size = 0.5) + 
    scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +  # X-Achsenbeschriftung
    scale_y_continuous(breaks = seq(1.0,9.0, by=0.5), labels = custom_labels) +  # Y-Achsenbeschriftung
    labs(x = "Zeitspanne (24h)", y = "Felder (1-8)",
         title = "Vier Reihen von Punkten mit waagerechten Linien",
         color = "Datenreihe") +
    theme_minimal()
}
