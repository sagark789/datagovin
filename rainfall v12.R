# Clear the screen & variables
rm(list = ls())

#Load the libraries to use in the code, for plots we are using ggplot2
library(ggplot2)
library(dplyr)
library(ggthemes)

# These are required for Time series forecasting
library(zoo)
library(forecast)

#Set the working directory, data file is already converted to CSV
setwd("C:/r/harish")

#Read file into mydata dataframe
mydata = read.csv("Area_Weighted_Monthly_Seasonal_And_Annual_Rainfall_0.csv", stringsAsFactors = FALSE)
summary(mydata)

# Assigned zero to NA values in data 
mydata[is.na(mydata)] <- 0

# Get the summary of data
summary(mydata)


# Subset the data for annual trend for all the regions in India

annual_data <- subset(mydata,select=c(SD_NO,SD_Name,YEAR,ANNUAL))

# Trying t create table between number & region mapping, not working, need to check with Harish
region_table <- table(annual_data$SD_Name,annual_data$SD_NO)

regions <- as.table(region_table)


#India trend by regio over years
 
# Plot of annual rainfall for all regions by years on legand

# LIke to print all the values of SD_NO, looking for better way to do it.

ggplot(annual_data,aes(x= SD_NO, y= ANNUAL)) +geom_point(aes(color=YEAR))+theme_solarized() +
    labs(x="Regions", 
         y="Total Rainfall(mm)",
         title="Annual Rainfall in India by region from 1951 till 2014",
         Color="YEAR")

# with line
ggplot(annual_data,aes(x= YEAR, y= ANNUAL)) +geom_point() + geom_line() 

# Further exploration

#indiarainfall_agr <- aggregate(annual_data,by=list(Category=indianrainall$YEAR), FUN=sum)
# ggplot(annual_data) + geom_bar(aes(x=YEAR))

#Get data only for VIDARBHA region

vidharbh_rainfall <- filter(mydata,SD_Name=='VIDARBHA')

# PLot for year vs Annual rain for region = Vidharbha, can be done for all the regions
ggplot(vidharbh_rainfall, aes(x=YEAR, y=ANNUAL)) + geom_point(size=2)+geom_line() +
theme_solarized() +
labs(x=" Years", 
     y="Total Rainfall(mm)",
     title="Annual Rainfall in Vidharbha region from 1951 till 2014",
     Color="YEAR")

## PLot by coclor in Jun.Sep time

ggplot(vidharbh_rainfall, aes(x=YEAR, y=ANNUAL,color)) + geom_point(size=4,aes(color=Jun.Sep))+geom_line() +
    theme_bw() +
    labs(x=" Years", 
         y="Total Rainfall(mm)",
         title="Annual Rainfall in Vidharbha region from 1951 till 2014",
         Color="YEAR")

## Trying out time series for yearly prediction...does not work, need to get the monthly data

vid_ann = vidharbh_rainfall$ANNUAL
vid_ann

a=ts(vid_ann,start=1951,end=2014,frequency = 1)

# Time Series plot Annual Rains for Vidharbha
plot.ts(a)

ha=HoltWinters(a,beta=F,gamma=F)

ha

ha$fitted

ha$SSE

# FOrecast for next 5 years

fha=forecast::forecast.HoltWinters(ha,h=5)

forecast::plot.forecast(fha)
#######################################################################

#### Trying to get monthly data into time series###################
########### Below are some steps to pre-process the data to create time series####

t_vrainfall <- t(vidharbh_rainfall)

vrainfall1  <- t_vrainfall[-2,]

vrainfall2 <- vrainfall1[-1,]

vrainfall3 <- vrainfall2[-1,]

vrainfall4 <- vrainfall3[-13,]

vrainfall5 <- vrainfall4[-13,]

vrainfall6 <- vrainfall5[-13,]

vrainfall7 <- vrainfall6[-13,]

vrainfall8 <- vrainfall7[-13,]

## Alternative way but not workingvrainfall3 <- vrainfall3[-c(13,14,15,16,17)]


usq <- 0
j <- 1

for (i in vrainfall3)
{ 
  usq[j] <- i
  print(i)
  j <- j +1 
}
usq


######### Forecasting bases on monthly data ###############

rainseries=ts(usq,start=1951,end=2014,frequency = 12)

myts <- ts(usq, start=c(1951, 1), end=c(2014, 12), frequency=12)
plot(myts)

#A time series with additive trend, seasonal, and irregular components 
# can be decomposed using the stl() function.

fit <- stl(myts, s.window="period")
plot(fit)

# additional plots

seasonplot(myts)

# simple exponential - models level
fit <- HoltWinters(myts, beta=FALSE, gamma=FALSE)

# predictive accuracy
accuracy(fit)

forecast(fit, 3)

fha=forecast::forecast.HoltWinters(fit,h=5)

forecast::plot.forecast(fha)

###############trying more options######

plot.ts(rainseries)

#You can specify the initial value for the level in the HoltWinters() function
#by using the "l.start" parameter
    
rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE, l.start=4.6)

# below plt not workig

plot.ts(rainseriesforecasts)

rainseriesforecasts$fitted

## Forecasting, not working ####

rainseriesforecasts2 <- forecast.HoltWinters(rainseriesforecasts, h=2)


plot(rainseriesforecasts)

#######Model Validation 

acf(rainseriesforecasts2$residuals, lag.max=20)

Box.test(rainseriesforecasts$residuals, lag=20, type="Ljung-Box")

###############Trying some other things but not working

a=ts(usq,start=1951,end=2014,frequency = 12)

plot.ts(a)

a
ha=HoltWinters(a,beta=F,gamma=F)

ha

ha$fitted

ha$SSE

plot.ts(ha)

fha=forecast::forecast.HoltWinters(ha,h=10)

forecast::plot.forecast(fha)


