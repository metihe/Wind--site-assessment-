library(readr)
library(visdat)
library(ggplot2)
library(lubridate)
library(dplyr)

wind <- read_csv("C:/Users/user/Desktop/Capstone/Project1.csv")
#print the first 10 rows of the frame
wind[1:10,]

#na.rm doesn not consider the missing values in a calculation

# install the visdat package, if it is not available yet
if (!require(visdat)) {
  devtools::install_github("njtierney/visdat")
  library(visdat)
}

vis_miss(wind, warn_large_data=F)

#split the dataset first 20.000
wind1<-wind[1:20000,]
#check missing values
vis_miss(wind1)

#tabel of missing values summary
table(rowSums(is.na(wind)))

#Replacing 0 to NA:
wind[wind == 0] <- NA

str(wind$Time)
names(wind) 
summary(wind)

#subset selecting 50m average

wind50<- wind[1:2]
names(wind50)
wind_avg<- wind%>% select(Time,C1Avg50m,C2Avg35m,C3Avg50m,C4Avg20m)
summary(wind_avg)
str(wind_avg$Time)


###Convert Time into a Date&Time format
head(wind$Time)
summary(dmy_hm(wind_avg$Time, tz="Europe/Berlin"))
?ymd_hm

#to eliminate values that failed to parse when changing time variable 
Sys.setlocale("LC_TIME", "German")

#Try if any error
head(dmy_hm(wind_avg$Time, tz="Europe/Berlin"))
which(is.na(wind_avg$Time))

###Assign the new format and a Date variable
wind_avg$Date <- (dmy_hm(wind_avg$Time, tz="Europe/Berlin"))
str(wind_avg$Date)


##investigating missing data
vis_miss(wind_avg, warn_large_data=F)

#remove the missing data at the time point 00:00
na.omit(wind_avg)

#split the date & time

###Extract hour from Date vaiable
#Assign new variable hour
wind_avg$Hour <- hour(wind_avg$Date)
#we have some zeros here and they indicate time 24 or 00

###Extract minute from Date vaiable
#Assign new variable Minute
wind_avg$Minute <- minute(wind_avg$Date)

#exclude the 7 colum second
wind_avg<- wind_avg[-7]


###paste hour and minute 
#Assign new Time variable
wind_avg$timeA<-paste(wind_avg$Hour, wind_avg$Minute,sep=":")

str(wind_avg$timeA) #thi is now the character variable we need to covert it to time variable

#convert a time variable
#problem with time 00 and how to deal with it NA
#"10M 0S"     "20M 0S"     "30M 0S"     "40M 0S"     "50M 0S"
head(hm(wind_avg$timeA))
wind_avg$timeA<-hm(wind_avg$timeA)
??timezones

##codes time variable
wind_avg$Date <- (dmy_hm(wind_avg$Time, tz="Europe/Berlin"))
wind_avg$Hour <- hour(wind_avg$Date)
wind_avg$Minute <- minute(wind_avg$Date)
wind_avg$Year <- year(wind_avg$Date)
wind_avg$Month <- month(wind_avg$Date)

wind_avg$timeA<-paste(wind_avg$Hour, wind_avg$Minute,sep=":")
wind_avg$timeA<-hm(wind_avg$timeA)

#assign new variables
wind_avg$A1Avg50m<- wind$`A1 Avg50m`
wind_avg$A2Avg35m<- wind$`A2 Avg35m`
wind_avg$A3Avgtemp<- wind$A3Avgtemp

#investigate temp variable
Hightemp<- which(wind_avg$A3Avgtemp>50)
wind_avg$A3Avgtemp[Hightemp]<- NA


remove<- which(wind_avg$C2Avg35m>75 | is.na(wind_avg$C2Avg35m))
wind_avg<- wind_avg[-remove,]
names(wind_avg)


#NEW DATASET WEANGLING

wind_new<- wind_avg%>% select(Date,C1Avg50m,C2Avg35m,C3Avg50m,C4Avg20m)

windgather<-gather(wind_new, anemometer, windspeed, -Date)

head(windgather)
unique(windgather$anemometer)
head(wind_avg)
windgather$anemometer<- as.factor(windgather$anemometer)

ggplot(windgather, mapping = aes(x=Date, y=windspeed))+
  geom_line(mapping = aes(color=anemometer))+theme(legend.position = "bottom")+
  labs(x="Date", y = "Wind Speed", title = "Monthly Average Wind Speeds for all Measured Heights ")
#should try to agverage monthly data to better show them on the graph 

#investigate outliers
outliers<-which(windgather$windspeed>15 | is.na(windgather$windspeed))
head(outliers)
windgather[1064:1069,]
windgather[19155:19159,]

#code to remove the outliers by index
#not applied yet
remove<- which(wind_avg$Year>=2008, wind_avg$Month>=6| is.na(wind_avg$C3Avg50m))
wind_avg<- wind_avg[-remove,]

#tempreture
names(wind_avg)
plot(wind_avg$Date, wind_avg$A3Avgtemp)

#substitute with 0 all the values of temperature
#month March to October

tempNAdata<-wind_avg%>% filter(Month>=3, Month<11,A3Avgtemp==0)

tempNA<- which(wind_avg$Month>=3, wind_avg$Month<11, wind_avg$A3Avgtemp==0)
wind_avg$A3Avgtemp[tempNA]<- NA

summary(wind_avg$A3Avgtemp)


 

 
 
 