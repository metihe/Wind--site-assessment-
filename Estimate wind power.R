
###Estimate wind power  ####

names(wind)
wind1<-subset(wind, select=c(1:17))
wind1$temp<-capstone$Celcius2
wind1$power_output<-capstone2$output50ave
wind1$power_output35<-capstone2$output35ave
wind1$power_output20<-capstone2$output20ave

names(wind1)
NAs<-sum(is.na(wind1))

###Make Outlier to take the last known good value
wind1$temp[wind1$temp>50]<-NA
wind1$temp<-na.locf(wind1$temp)
wind1$C1Max50m[wind1$C1Max50m>35]<-NA
wind1$C1Max50m<-na.locf(wind1$C1Max50m)
wind1$C2Max35m[wind1$C2Max35m>35]<-NA
wind1$C2Max35m<-na.locf(wind1$C2Max35m)
wind1$C4Max20m[wind1$C4Max20m>35]<-NA
wind1$C4Max20m<-na.locf(wind1$C4Max20m)

wind1$C1Avg50m[wind1$C1Avg50m>35]<-NA
wind1$C1Avg50m<-na.locf(wind1$C1Avg50m)
wind1$C2Avg35m[wind1$C2Avg35m>35]<-NA
wind1$C2Avg35m<-na.locf(wind1$C2Avg35m)
wind1$C4Avg20m[wind1$C4Avg20m>35]<-NA
wind1$C4Avg20m<-na.locf(wind1$C4Avg20m)

wind1$C1Min50m[wind1$C1Min50m>35]<-NA
wind1$C1Min50m<-na.locf(wind1$C1Min50m)
wind1$C2Value35m[wind1$C2Value35m>35]<-NA
wind1$C2Value35m<-na.locf(wind1$C2Value35m)
wind1$C4Min20m[wind1$C4Min20m>35]<-NA
wind1$C4Min20m<-na.locf(wind1$C4Min20m)

# compute kinetic energy in the wind at each windspeed aver50m
# Wind Power = (1/2)*rho*area*(velocity)^3 = [kg/m^3]*[m^2]*[m/s]^3 = [kg*m^2/s^3] = [kg*m^2/s^2][1/s] = [Newton-meter]/[second] = [Joules/second] = [Watts]
rho=1.225 # density of wind (kg/m^3)
area=2174 # sweep area of wind turbines (m^2) this could be adjusted depending on th size of the turbine
turbines=7 # number of turbines (we can change those values)
c<-(1/2)*rho*area
wind1$wind_power_kw<-c*(wind1$C1Avg50m)^3*turbines/1000 # kW avg power
wind1$wind_energy_10min_kwh<-c*(wind1$C1Avg50m)^3*turbines/(1000*6) # kWh in 10 min

# compute betz limit 59%
betz.coef<- 16/27
wind1$betz_limit_10min_kwh<-wind1$wind_energy_10min_kwh*betz.coef

# compute turbine efficiency
#cant run this code beouse we dont have the power output 
wind1$turbine_eff<-wind1$energy_sentout_10min_kwh/wind1$wind_energy_10min_kwh

# compute total Possible Power
#we cant run this becouse we dont have actual power
#but we can run this if we use power output estimated
uncurtailed_power<-apply(X=dat[,2:8], MARGIN=1, FUN=sum)
dat$uncurtailed_10min_kwh<-(uncurtailed_power)/6

wind2 <- wind1
str(wind2$Time)
head(wind2$Time)
# In the raw data, midnight values are missing the hour and minute. Fix these:
get<-which(is.na(as.POSIXlt(wind2$Time, format="%d/%m/%y %H:%M"))) # return row index of date_time that cann

# covert modified date_time character string to POSIX
#this gives me all NA
wind2$Time <- as.POSIXct(wind2$Time, format="%d/%m/%y %H:%M") 

wind2$time <- (dmy_hm(wind2$Time))

# check
sum(is.na(wind2$time))
# omit
wind2 <- na.omit(wind2)
library(plyr)
#group the data with respect to time (e.g. by month, day, hour)
#This will come in handy for temporal aggregation, and for plotting
wind2$month <- cut(wind2$time, breaks = "month")
week <- cut(wind2$time, breaks = "week")
day <- cut(wind2$time, breaks = "day")
hour <- cut(wind2$time, breaks = "hour")

dummy<-strsplit(as.character(week), split=" ")
week<-laply(dummy, '[[', 1) # keep the date, drop the time
wind2$week<-as.factor(week)

dummy<-strsplit(as.character(day), split=" ")
day<-laply(dummy, '[[', 1) # keep the date, drop the time
wind2$day<-as.factor(day)

## Error in fs[[1]](x, ...) : subscript out of bounds
# dummy<-strsplit(as.character(hour), split=" ")
# hour<-laply(dummy, '[[', 2) # keep the time, drop the date
# dat$hour<-as.factor(hour)

#Save the augemented data.frame dat. This reflects the full dataset after NA checks and benchmark calculations, but prior to applying filters.
save(wind2, file="Wind_Removed.rdata")

#Now we can create time-series visualizations of the data:
# No energy data
energy<-subset(wind2, select=c("day", "wind_energy_10min_kwh", "betz_limit_10min_kwh"))
energy<-ddply(energy, .(day), numcolwise(sum))

# plot energy vs time
install.packages("reshape")
library(reshape)
test<-melt(energy, id.vars=("day"))
test <- test[complete.cases(test), ] # remove incomplete cases

ggplot(test, aes(x=day, y=value/10^3, group=variable, colour=variable, linetype=variable)) +
  geom_line() +
  scale_y_continuous(name="MWh per day") + 
  labs(title="Energy Timeseries") +
  theme_classic() +
  scale_x_discrete(breaks=test$day[seq(1, 360, by=60)], labels=abbreviate)

# facet wrap by month
energy<-subset(wind2, select=c("day","week", "month",  "wind_energy_10min_kwh", "betz_limit_10min_kwh"))
energy<-ddply(energy, .(day, week, month), numcolwise(sum))

# plot energy vs time
#error
test<-melt(energy, id.vars=c("day", "week", "month"))
levels(test$month) <- month.name[1:12]
check(test)
test <- test[complete.cases(test), ] # remove incomplete cases

ggplot(test, aes(x=day, y=value/10^3, group=variable, colour=variable, linetype=variable)) +
  geom_line() +
  facet_wrap(~month, scales="free") +
  scale_y_continuous(name="MWh per day") + 
  labs(title="Monthwise Energy Timeseries") +
  theme_classic() +
  scale_x_discrete(breaks=NULL)
#Clearly we have some problem data.


# Total energy, per week
energy<-subset(wind2, select=c("week", "betz_limit_10min_kwh"))
energy<-ddply(energy, .(week), numcolwise(sum))

# plot energy vs time
test<-melt(energy, id.vars=("week"))
ggplot(test, aes(x=week, y=value/10^3, group=variable, colour=variable, linetype=variable)) +
  geom_line() +
  scale_y_continuous(name="MWh per week") + 
  labs(title="Energy Timeseries") +
  theme_classic() +
  scale_x_discrete(breaks=levels(test$week)[seq(1,52, by=8)], labels=abbreviate)

## historgrams mean wind speed
hist(wind2$C1Avg50m, main="Histogram of Mean Windspeeds")

wind2%>% filter(C1Avg50m<40)%>% ggplot(mapping = aes(x=C1Avg50m))+
  geom_histogram(binwidth =1)+
  labs(x="wind speed", title = "Average Wind Speed Distribution 50m altitude")

wind2%>% filter(C1Min50m<40)%>% ggplot(mapping = aes(x=C1Min50m))+
  geom_histogram(binwidth =1)+
  labs(x="wind speed", title = "Min Wind Speed Distribution 50m altitude")

wind2%>% filter(C1Max50m<40)%>% ggplot(mapping = aes(x=C1Max50m))+
  geom_histogram(binwidth =1)+
  labs(x="wind speed", title = "Max Wind Speed Distribution 50m altitude")

wind3<-wind2
names(wind3)
#Plot the power curve with benchmark comparisons
library(reshape2)
energy<-subset(wind2, select=c("C1Avg50m", "power_output", "wind_energy_10min_kwh", "betz_limit_10min_kwh"))
energy<-melt(energy, id.vars=("C1Avg50m")) #getting error
head(energy)

ggplot(energy, aes(x=C1Avg50m, y=value, group=variable, colour=variable)) + 
  geom_point() +
  scale_y_continuous(name="KWh in 10min", limit=c(0, max(wind3$power_output))) + 
  scale_x_continuous(name="Windspeed (mps)") + 
  labs(title="Empirical Power Curve with Betz Limit and Theoretical Wind Energy") +
  theme_classic()

# fit a distribution to the wind speed data
library(fitdistrplus)
 
descdist(wind3$C1Avg50m) # heuristic
identical(names(energy[[1]]), names(energy[[2]]) )

descdist(wind3$C2Avg35m) # heuristic
identical(names(energy[[1]]), names(energy[[2]]) )
names(wind2)

wind3<-na.omit(wind3)
which(is.na(wind3))
# based on heuristic, and knowledge of the system, fit a weibull distribution
weibull.fit<-fitdist(wind3$C2Avg35m, distr="weibull")#getting error


#Fit a Weibull distribution to statistically-filtered windspeed data
library(fitdistrplus)
descdist(wind3$C1Avg50m) # heuristic
descdist(wind3$C2Avg35m) # heuristic

# based on heuristic, and knowledge of the system, fit a weibull distribution
weibull.fit.2<-fitdist(wind3$C1Avg50m, distr="weibull")#error
summary(weibull.fit.2)


# Now, we can use fitdistr to calculate the parameters by MLE
# The option "lower = 0" is added because the parameters of the Weibull distribution 
#need to be >= 0
#Estimate k and c by MLE
rw <- rweibull(wind3$C1Avg50m, shape=1.5, scale=1)
fitw<-fitdistr(rw, densfun="weibull")
#shape=1.499 this indicates thatlocations with lots of low wind speeds as well as some very strong winds would have a value of 
#shape of below 2, locations with fairly consistent wind speeds around the median would have a shape value of 3.
summary(fitw)# in symmary I dont get the all needed stats

# Estimate k and c using the leas square fit
n <- 100 # number of bins
breaks <- seq(0, max(rw), length.out=n)

freqs <- as.vector(prop.table(table(cut(rw, breaks = breaks))))
cum.freqs <- c(0, cumsum(freqs)) 

xi <- log(breaks)
yi <- log(-log(1-cum.freqs))

# Fit the linear regression
least.squares <- lm(yi[is.finite(yi) & is.finite(xi)]~xi[is.finite(yi) & is.finite(xi)])
lin.mod.coef <- coefficients(least.squares)

k <- lin.mod.coef[2]
k
c <- exp(-lin.mod.coef[1]/lin.mod.coef[2])
c
#All methods yield very similar results. The maximum likelihood approach has the 
#advantage that the standard errors of the Weibull parameters are directly given.

#Prediction-------------------------------
#Fit a local-polynomial power curve model
library(locfit)
names(wind3)
# Option 1: Fit a local-polynomial model to the estimated data
empirical.mod<-locfit(power_output ~ C1Avg50m, data=wind3)
summary(empirical.mod)
plot(empirical.mod, main="Model Fit to Power Estimation Data")
points(y=wind3$power_output, x=wind3$C1Avg50m, cex=0.1, pch=20, col="red")

#35M wind spped
#Fit a local-polynomial power curve model

names(wind3)
# Option 1: Fit a local-polynomial model to the observed data
empirical.mod1<-locfit(power_output35 ~ C2Max35m, data=wind3)
summary(empirical.mod1)
plot(empirical.mod1, main="Model Fit to Power Estimation Data")
points(y=wind3$power_output35, x=wind3$C2Max35m, cex=0.1, pch=20, col="red")

