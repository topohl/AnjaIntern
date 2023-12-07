## ANJAS R CHEATSHEET

## time and the different versions time can be shown ##################
## normal: "2023-04-29 12:24:52 CEST"
## time stamp char: "1682673513" 
## time stamp num: 1682673513

as.POSIXct(as.numeric(time), origin = "1970-01-01") == "2023-04-29 12:24:52 CEST"

# Beispielzeitstempel
#changeable in numeric state
#has to be a character in the end
new_time <- start_time%>%
  as.numeric()%>%
  +1%>%
  as.character()

#printable in numeric state  
print(as.POSIXct(as.numeric(new_time), origin = "1970-01-01"))


#character time from time format to numeric: 
as.numeric(overallData_sys1[5,"DateTime"])

## filter ideas ##########################################################
#mouse1 on day 1
md1 <- overallData_final%>%
  filter(AnimalID=="OR428")%>%
  filter(grepl("2023-04-24", DateTime))%>%
  filter(PositionID<=8)%>%
  filter(hour(DateTime) == 19)
md1 <- select(md1,c(DateTime, PositionID))

md2 <- overallData_final%>%
  filter(AnimalID=="OR414")%>%
  filter(grepl("2023-04-24", DateTime))%>%
  filter(PositionID<=8)%>%
  filter(hour(DateTime) == 19)
md2 <- select(md2,c(DateTime, PositionID))
