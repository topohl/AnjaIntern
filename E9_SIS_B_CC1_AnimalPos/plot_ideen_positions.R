#test plots

# Laden der benötigten Pakete
library(ggplot2)
library(dplyr)

#geom segment macht die datenpfeile

###### Beispiel-Daten erstellen (Zeitpunkte und zugehörige Y-Werte)
set.seed(123)
data <- data.frame(
  time = seq(from = as.POSIXct("2023-11-22 00:00:00"), 
             by = "hour", length.out = 20),
  y_value = sample(1.125:8.125, 20, replace = TRUE)
)

###### zweites datenset
set.seed(456)
data2 <- data.frame(
  time = seq(from = as.POSIXct("2023-11-22 00:00:00"), 
             by = "hour", length.out = 20),
  y_value = sample(1.325:8.325, 20, replace = TRUE)
)
###### noch zwei weitere
set.seed(496)
data3 <- data.frame(
  time = seq(from = as.POSIXct("2023-11-22 00:00:00"), 
             by = "hour", length.out = 20),
  y_value = sample(1.625:8.625, 20, replace = TRUE)
)
set.seed(477)
data4 <- data.frame(
  time = seq(from = as.POSIXct("2023-11-22 00:00:00"), 
             by = "hour", length.out = 20),
  y_value = sample(1.825:8.825, 20, replace = TRUE)
)
#####################################################################################

# Plot erstellen
ggplot(data, aes(x = time, y = y_value)) +
  geom_point(size = 3) +
  geom_segment(aes(x = time, xend = lead(time, default = last(time)),
                   y = y_value, yend = y_value), 
               arrow = arrow(length = unit(0.2, "cm")), 
               linewidth = 1, color = "blue") +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  labs(x = "Zeitspanne (24h)", y = "Feste Zahlen (1-8)",
       title = "Punkte mit waagerechten Linien") +
  theme_minimal()

#plot mit zwei werten erstellen
ggplot() +
  geom_point(data = data, aes(x = time, y = y_value), size = 3, color = "blue") +
  geom_segment(data = data %>% mutate(next_time = lead(time, default = last(time))),
               aes(x = time, xend = next_time, y = y_value, yend = y_value),
               arrow = arrow(length = unit(0.2, "cm")), size = 1, color = "blue") +
  geom_point(data = data2, aes(x = time, y = y_value), size = 3, color = "red") +
  geom_segment(data = data2 %>% mutate(next_time = lead(time, default = last(time))),
               aes(x = time, xend = next_time, y = y_value, yend = y_value),
               arrow = arrow(length = unit(0.2, "cm")), size = 1, color = "red") +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  labs(x = "Zeitspanne (24h)", y = "Feste Zahlen (1-8)",
       title = "Zwei Reihen von Punkten mit waagerechten Linien",
       color = "Datenreihe") +
  theme_minimal()

#
#scale_y_continuous(breaks = seq(1:8), labels = 1:8) +  # Y-Achsenbeschriftung für jeden ganzen Wert






##################favourite:


# Function for adjusting the Y-axis labelling
custom_labels <- function(x) {
  labels <- ifelse(x %% 1 == 0, "", as.character(x - 0.5))  # show even numbers on odd place(number 1 on place 1.5), dont show even place numbers 
  return(labels)
}

#plot
ggplot() +
  geom_point(data = data, aes(x = time, y = y_value), size = 3, color = "blue") +
  geom_segment(data = data %>% mutate(next_time = lead(time, default = last(time))),
               aes(x = time, xend = next_time, y = y_value, yend = y_value),
               arrow = arrow(length = unit(0.2, "cm")), size = 1, color = "blue") +
  geom_point(data = data2, aes(x = time, y = y_value), size = 3, color = "red") +
  geom_segment(data = data2 %>% mutate(next_time = lead(time, default = last(time))),
               aes(x = time, xend = next_time, y = y_value, yend = y_value),
               arrow = arrow(length = unit(0.2, "cm")), size = 1, color = "red") +
  geom_point(data = data3, aes(x = time, y = y_value), size = 3, color = "yellow") +
  geom_segment(data = data3 %>% mutate(next_time = lead(time, default = last(time))),
               aes(x = time, xend = next_time, y = y_value, yend = y_value),
               arrow = arrow(length = unit(0.2, "cm")), size = 1, color = "yellow") +
  geom_point(data = data4, aes(x = time, y = y_value), size = 3, color = "green") +
  geom_segment(data = data4 %>% mutate(next_time = lead(time, default = last(time))),
               aes(x = time, xend = next_time, y = y_value, yend = y_value),
               arrow = arrow(length = unit(0.2, "cm")), size = 1, color = "green") +
  geom_hline(yintercept = 1:8, color = "black", linetype = "solid", size = 0.5) + 
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +  # X-Achsenbeschriftung
  scale_y_continuous(breaks = seq(1.0,9.0, by=0.5), labels = custom_labels) +  # Y-Achsenbeschriftung
  labs(x = "Zeitspanne (24h)", y = "Felder (1-8)",
       title = "Vier Reihen von Punkten mit waagerechten Linien",
       color = "Datenreihe") +
  theme_minimal()


