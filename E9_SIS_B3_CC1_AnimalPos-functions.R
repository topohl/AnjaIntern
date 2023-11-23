
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
# find the FIRST TIME where mouse is tracked in the cage
# aka first value of mouse in overallData_final
find_first_pos_and_time <- function(system_mouse_names, overallData_final, mice_list){
  
  for (i in 1:length(system_mouse_names)){ #i=1-4
    
    #rename
    mouse_name <- system_mouse_names[[i]]
    #search first entry in whole data
    first_entry <- overallData_final%>%
      filter(AnimalID == mouse_name)%>%
      slice(1) #first row
    
    #write name, position and time into mice_list
    mice_list[i][[1]] <- mouse_name
    #print(mouse_name)
    
    first_time <- first_entry$DateTime
    mice_list[[i]][[2]] <- first_time
    #print(first_time)
    
    first_position <- first_entry$PositionID
    mice_list[[i]][[3]] <- first_position
    #print(first_position)
  }
  return(mice_list)
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
        count_closeness_list[[i]][[j]] <- count_closeness_list[[i]][[j]]+1
      }
    }
  }
  print(count_closeness_list)
  # return updated list of mice that are close to each other
  return(count_closeness_list)
}

##############################################################################################################
# function to do shift in time(one second forward)
# return eventually alterated mice_list
sec_shift <- function(system_mouse_names, mice_list, overallData_final, old_time){
  
  #put one second on top of old_time
  new_time <- old_time%>%
    as.numeric()%>%
    +1%>%
    as.character()
  cat("old time: ", old_time, "\n")
  cat("new time: ", new_time, "\n")
  #filter overallData_final with new_time
  new_time_rows <- overallData_final%>%
    filter(DateTime == as.POSIXct(as.numeric(new_time), origin = "1970-01-01"))
  print(new_time_rows)
  
  # enter new data in mice_list
  for (i in 1:length(system_mouse_names)){ #i=1-4
    
    #rename
    mouse_name <- system_mouse_names[[i]]
    #search current animal
    mouse_entry <- new_time_rows%>%
      filter(AnimalID == mouse_name)
    
    print(mouse_entry)
    
    #if double position entrys for same second, take first entry
    if(nrow(mouse_entry)>1){mouse_entry <- mouse_entry%>%slice(1)}
    #if new position happened during this second
    if(nrow(mouse_entry)==1){
      print("blub")
      #write name, position and time into mice_list
      mice_list[i][[1]] <- mouse_name
      
      mice_list[[i]][[2]] <- new_time #mouse_entry$DateTime
      
      mice_list[[i]][[3]] <- mouse_entry$PositionID
    }
    
  }
  
  return(mice_list)
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
