
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
## plot ##

# Function for adjusting the Y-axis labelling
custom_labels <- function(x) {
  labels <- ifelse(x %% 1 == 0, "", as.character(x - 0.5))  # show even numbers on odd place(number 1 on place 1.5), dont show even place numbers 
  return(labels)
}

#function to plot 4 different mice an their movement in the cage
plot_micePositions_together <- function(m1){#startTime,, m2, m3, m4 ...
  
  #alterate y-points to 0,125
  #map 
  #print(m1$PositionID)
  #print(typeof(m1$PositionID))
  
  #alterated_pos <- map(m1$PositionID, ~ .+0.125)
  m1 <- m1%>%
    mutate(PrettyPos = map_dbl(m1$PositionID, ~ .+0.125))#(m1$PositionID, ~ .+0.125))
  print(m1$PrettyPos)
  #print(alterated_pos)
  #print(typeof(alterated_pos))
  
  # m1 = DateTime, PositionID from mouse one
  data1 <- data.frame(
    time = m1$DateTime,
    position = m1$PrettyPos
  )
  
  
  ggplot() +
    geom_point(data = data1, aes(x = time, y = position), size = 3, color = "blue") +
    geom_segment(data = data1 %>% mutate(next_time = lead(time, default = last(time))),
                 aes(x = time, xend = next_time, y = position, yend = position),
                 arrow = arrow(length = unit(0.2, "cm")), size = 0.5, color = "blue") +
    geom_hline(yintercept = 1:8, color = "black", linetype = "solid", size = 0.5) + 
    scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +  # X-Achsenbeschriftung
    scale_y_continuous(breaks = seq(1.0,9.0, by=0.5), labels = custom_labels) +  # Y-Achsenbeschriftung
    labs(x = "Zeitspanne (24h)", y = "Felder (1-8)",
         title = "Vier Reihen von Punkten mit waagerechten Linien",
         color = "Datenreihe") +
    theme_minimal()
}
