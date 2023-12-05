
## 11/2023
## Anja Magister
## ANALYSIS OF ANIMAL POSITIONS - FUNCTIONS ##
##








##############################################################################################################
# finds corresponding PositionID from two coordinates(x_Pos, y_pos) in lookup_tibble and returns ID 
find_id <- function(x_Pos, y_Pos, lookup_tibble) {
  
  #give non-standard positions a standard position
  # y value
  if(y_Pos<116){y_Pos <- 0}
  if(y_Pos>=116){y_Pos <- 116}
  # x value
  if(x_Pos<100){x_Pos <- 0}
  if(x_Pos<200){x_Pos <- 100}
  if(x_Pos<300){x_Pos <- 200}
  if(x_Pos>=300){x_Pos <- 300}
  
  # search position in lookup table
  result <- lookup_tibble %>%
    filter(xPos == x_Pos, yPos == y_Pos) %>%
    select(PositionID)
  
  # return new ID of specific position
  if (nrow(result) > 0) {
    return(result$PositionID)
  } else {
    cat("xpos: ", x_Pos, " , ypos: ", y_Pos, "\n")
    print("NA!")
    return(NA) # Return NA if no match found
  }
}
##############################################################################################################
# find the FIRST TIME where mouse is tracked in the cage
# aka first value of mouse in overallData_final
find_first_pos_and_time <- function(system_mouse_names, data, mice_list){
  
  # create empty vector with 4 variables
  times_vec <- rep(NA, times=4)
  
  for (i in 1:length(system_mouse_names)){ #i=1-4
    
    #rename
    mouse_name <- system_mouse_names[[i]]
    #search first entry in whole data
    first_entry <- data%>%
      filter(AnimalID == mouse_name)%>%
      slice(1) #first row
    
    #write name, position and time into mice_list
    mice_list[i][[1]] <- mouse_name
    #print(mouse_name)
    
    first_time <- first_entry$DateTime
    mice_list[[i]][[2]] <- first_time
    #print(first_time)
    
    #enter time in times vec fot later to compare
    times_vec[i] <- first_time
    
    first_position <- first_entry$PositionID
    mice_list[[i]][[3]] <- first_position
    #print(first_position)
  }
  
  #check, if all times are similar
  #if not, change them all to the first shared time
  print(times_vec)
  if(length(unique(times_vec)) != 1){
    latest_time <- max(times_vec)
    print(latest_time)
    for(i in 1:4){
      mice_list[[i]][[2]] <- latest_time
    }
  }
  
  return(mice_list)
}


##############################################################################################################
# function to check for closeness
# input: mice_list
# compare every sublist(4)to each other
check_closeness1 <- function(mice_list,count_closeness_list){
  
  # compare the third value of every couple
  # if the position is the same, save in count_closeness_list list
  for (i in 1:4) {
    for (j in i:4) {
      #print(c(i, j))
      #print(mice_list[[i]][[3]]==mice_list[[j]][[3]])
      if(mice_list[[i]][[3]]==mice_list[[j]][[3]]){
        count_closeness_list[[i]][[j]] <- count_closeness_list[[i]][[j]]+1
        if(j!=i){
          count_closeness_list[[j]][[i]] <- count_closeness_list[[j]][[i]]+1
        }
      }
    }
  }
  #print(count_closeness_list)
  # return updated list of mice that are close to each other
  return(count_closeness_list)
}


# function to check for closeness
# input: mice_list
# compare every sublist(4)to each other
#second version for new algorithm
# more complex
check_closeness <- function(old_mice_list,new_mice_list,count_closeness_list,secTemp){
  
  # compare the third value of every couple
  # if the position is the same, save in count_closeness_list list
  for (i in 1:4) {
    for (j in i:4) {
      #comparisation of old_mice_list for last (secTemp-1)-missing seconds
      if(old_mice_list[[i]][[3]]==old_mice_list[[j]][[3]]){
        count_closeness_list[[i]][[j]] <- count_closeness_list[[i]][[j]]+(secTemp-1)  #secTemp contains x-1 seconds of old positions and one sec of new pos
        if(j!=i){
          count_closeness_list[[j]][[i]] <- count_closeness_list[[j]][[i]]+(secTemp-1)
        }
      }
      #comparisation of new_mice_list for first new second
      if(new_mice_list[[i]][[3]]==new_mice_list[[j]][[3]]){
        count_closeness_list[[i]][[j]] <- count_closeness_list[[i]][[j]]+1
        if(j!=i){
          count_closeness_list[[j]][[i]] <- count_closeness_list[[j]][[i]]+1
        }
      }
    }
  }
  
  # return updated list of mice that are close to each other
  return(count_closeness_list)
}

##############################################################################################################
# function to do shift in time(one second forward)
sec_shift <- function( old_time){
  #put one second on top of old_time
  new_time <- old_time%>%
    as.numeric()%>%
    +1%>%
    as.character()
  #cat("old time: ", old_time, "\n")
  #cat("new time: ", new_time, "\n")
  return(new_time)
}

##############################################################################################################
# update mice_list(if its possible) and return it
# similarity to find_first_pos_and_time
update_mice_list1 <- function(system_mouse_names, mice_list, data, time){
  
  
  #filter data with new_time
  new_time_rows <- data%>%
    filter(DateTime == as.POSIXct(as.numeric(time), origin = "1970-01-01"))
  #print(new_time_rows)
  
  # enter new data in mice_list
  for (i in 1:length(system_mouse_names)){ #i=1-4
    
    #rename
    mouse_name <- system_mouse_names[[i]]
    #search current animal
    mouse_entry <- new_time_rows%>%
      filter(AnimalID == mouse_name)
    
    #print(mouse_entry)
    
    #if double position entrys for same second, take first entry
    if(nrow(mouse_entry)>1){mouse_entry <- mouse_entry%>%slice(nrow(mouse_entry))}
    #if new position happened during this second
    if(nrow(mouse_entry)==1){
      #print("new entrys")
      #write name, position and time into mice_list
      mice_list[i][[1]] <- mouse_name#redundant!
      
      mice_list[[i]][[2]] <- time #mouse_entry$DateTime?
      
      mice_list[[i]][[3]] <- mouse_entry$PositionID
    }
    
  }
  
  return(mice_list)
}

# update mice_list(if its possible) and return it
# similarity to find_first_pos_and_time
#second version for new algorithm
# more complex
update_mice_list <- function(system_mouse_names, mice_list, data, time, line){
  
  #next_second <- sec_shift(time)
  new_time <- as.numeric(data[line,"DateTime"])

  # write sec difference between new and old time into secTemp
  mice_list[["tempData"]][["secTemp"]] <- new_time-as.numeric(time)
  
  # write new time into every mouse information
  for(i in 1:4){mice_list[[i]][[2]] <- new_time}
 
  
  # while line(and especially the next lines) is still same time
  while(as.numeric(data[line,"DateTime"])==new_time){
    # write new position into special mouse
    for(i in 1:4){
      if(mice_list[[i]][[1]]==as.character(data[line,"AnimalID"])){mice_list[[i]][[3]] <- as.numeric(data[line,"PositionID"])}
    }
    #if line is not the last line, check next line, else break while loop
    if(line==nrow(data)){
      line <- line+1
      break
    }
    #continue with the while condition
    line <- line+1
    
  }
  
  # write new line into mice_list
  mice_list[["tempData"]][["lineTemp"]] <- line
  
 
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
