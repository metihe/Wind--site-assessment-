#####Averaging wind speeds and directions

# Load packages:
# An air quality data analysis package
library(openair)
library(lubridate)
library(readr)
library(ggplot2)
library(dplyr)
library(reshape)

wind <- read_csv("C:/Users/user/Desktop/Capstone/Project1.csv")
wind1<- wind%>% select(Time,C1Avg50m,C2Avg35m,C3Avg50m,C4Avg20m)
wind1$A1Avg50m<- wind$`A1 Avg50m`
wind1$A2Avg35m<- wind$`A2 Avg35m`
wind1$A3Avgtemp<- wind$A3Avgtemp

#rename
wind1<-rename(wind1, c(Time="date"))
names(wind1)
wind1<-rename(wind1, c(A3Avgtemp="temp"))
head(wind1)
# Process the dates into the POSIXct format
wind1$date <- dmy_hms(wind1$date)#this gives 966 failed to parse.
wind1$date <- (dmy_hm(wind1$date, tz="Europe/Berlin"))

#for each anomenter a seperate subset
anm1<-wind1%>% select(date,C1Avg50m,A1Avg50m,temp)

#rename variables
anm1<-rename(anm1, c(C1Avg50m="ws",A1Avg50m="wd" ))
head(anm1)

#To convert from "meteorological direction" 
#to "math direction", use use this formula
#For winds, the u wind is parallel to the x axis. A positive u wind is from the west. 
#A negative u wind is from the east. The v wind runs parallel to the y axis. 
#A positive v wind is from the south, and a negative v wind is from the north.
# Calculate the u and v wind components
#assign new variables for u and v
anm1$u.wind <- - anm1$ws * sin(2 * pi * anm1$wd/360)
anm1$v.wind <- - anm1$ws * cos(2 * pi * anm1$wd/360)

# Calculate the average wind vectors
mean.u <- mean(anm1$u.wind, na.rm = T)
mean.v <- mean(anm1$v.wind, na.rm = T)

# Calculate the resultant vector average wind direction with atan2
wd.average <- (atan2(mean.u, mean.v) * 360/2/pi) + 180
# Display
wd.average
#The average wind direction for the monitoring period was therefore 317 degrees.

# Calculate the vector average wind speed
ws.vector.average <- ((mean.u^2 + mean.v^2)^0.5)
ws.vector.average
#the two types of wind speeds, vector and scalar, have produced very different
#values; compare 1.15 and 4.45 m s???1

# Calculate the scalar average wind speed, the standard mean
ws.scalar.average <- mean(anm1$ws, na.rm = T)
ws.scalar.average

str(anm1$date)

# Daily averages hourly
anm1.hour <- timeAverage(anm1, avg.time = 'hour')
head(anm1.hour, 3)

# Daily averages daily
anm1.day <- timeAverage(anm1, avg.time = 'day')
head(anm1.day, 3)

# Display the wind rose
windRose(anm1.hour, paddle = F)



ggplot(data = anm1.hour, aes( x = ws, y = temp ) ) + 
  geom_point()  + 
  xlab('Wind Speed') 
  ylab('Wind Speed')
  
       

