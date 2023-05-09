#####################################################
#                                                   #
#          Example Data Processing for              #
#            Continuous Rainfall Data               #
#                                                   #
#####################################################

#This code will read in rainfall data downloaded from USGS,
#isolate rain events, return a completed storm catalog,
#and return an summary of annual rain values, proportional to the timespan
#of the data.
#rain events are categorized as periods of rain separated by dry periods
#of at least 6 hours. This can be changed as required
#The code can further isolate rain events based on amount.

#Data should be examined for delimiter and order.

#clear environment so no data from previous analyses interferes
rm(list=ls())

#filepath of the working directory. This will be different for you!
filepath <- "C:/Users/justi/OneDrive/Desktop/Portfolio/R/Rainfall/"

#set the working environment to the desired path.
setwd(filepath)

# read in and check data
library(readr)
data <- as.data.frame(readr::read_delim(
  "./raindata.txt", col_names = T,
  delim = "\t")) 
head(data, 3) 

#install these packages beforehand if needed.
library(lubridate)
library(tidyverse)


###ORGANIZE DATA---------------------------------------------------------------------------------------

#mark process start time
process <- data.frame(start = as.POSIXct(NA) , end = as.POSIXct(NA))
process[1,1] <- Sys.time()

#make copy of original data to preserve
orig <- data

#remove first row and columns 1, 2, 4, 6 - not needed
data <- data[-1,-c(1,2,4,6)]

#Rename columns 
names(data)[c(1:2)] <- c("timestamp", "rain_in")


#Identify format for each columns
data$rain_in <- as.character(data$rain_in)
data$rain_in <- as.numeric(data$rain_in)

data$timestamp <- as.character(data$timestamp)
data$timestamp <- as.POSIXct(data$timestamp, format = "%Y-%m-%d %H:%M", tz = "HST")

#remove rows where timestamp = NA
data <- data[!is.na(data$timestamp), ]

#rain values can be weird. Let's round to 2 decimals in case
data$rain_in <- round(data$rain_in, 3)


###END OF ORGANIZE DATA--------------------------------------------------------------------------------


###STORM EVENTS, GENERATING TABLES------------------------------------------------------------


#create second dataframe of just timestamp and rain
sub_rain <- data.frame(timestamp = as.POSIXct(data$timestamp), Rain = as.numeric(data$rain_in))

#remove rows that don't record rain to speed up the process
sub_rain <- sub_rain[!(sub_rain$Rain == 0), ]

#create columns for rain in surrounding times
sub_rain$last_six_hours <- sub_rain$next_six_hours <- NA

#need to identify whether rainfall in the buffer period was 0 or not
sub_rain$last_six_hours <- sub_rain$next_six_hours <- NA

for(i in seq(NROW(sub_rain))) {
  sub_rain$last_six_hours[i] <- 
    sum(sub_rain$Rain[
      (sub_rain$timestamp >= sub_rain$timestamp[i] - dhours(6))&
      (sub_rain$timestamp < sub_rain$timestamp[i])])
  sub_rain$next_six_hours[i] <- 
    sum(sub_rain$Rain[
      (sub_rain$timestamp <= sub_rain$timestamp[i] + dhours(6))&
      (sub_rain$timestamp > sub_rain$timestamp[i])]
    )
}


#Filter dataset for start and end times of storms
#The start of a rain event will always be where rain in the prev. 6 hrs is 0
#The end of a rain event will always be where rain in the next 6 hrs is 0
Start = filter(sub_rain, last_six_hours == 0 & Rain > 0) %>% pull(timestamp)
End = filter(sub_rain, next_six_hours == 0 & Rain > 0) %>% pull(timestamp)

Rain_events <- data.frame(Start_time = Start, End_time = End)

#Find sum of rain for each storm event
Rain_events$Rain_in <- NA

for(i in seq(NROW(Rain_events))){
  Rain_events$Rain_in[i] <- 
    sum(data$rain_in[
      (which(data[,1] == Rain_events[i,1])):
        (which(data[,1] == Rain_events[i,2]))])
}

#Find the maximum rain intensity for each storm event
#data is recorded in 15 minute intervals

for(i in seq(NROW(Rain_events))){
  Rain_events$'Int_in/15min'[i] <- 
    (max(data$rain_in[
      (which(data[,1] == Rain_events[i,1])):
        (which(data[,1] == Rain_events[i,2]))]))
  Rain_events$'Int_in/hr'[i] <-
    Rain_events$'Int_in/15min'[i] * 60 / 15
}

#add column for flagging storm size
#this can be adjusted as needed

Rain_events$Flag <- as.character(NA)
row_storm <- nrow(Rain_events)

for (i in 1:row_storm) {
  if(Rain_events$Rain_in[i] < 0.1){
    Rain_events$Flag[i] <- "VERY SMALL"
  }
  if(Rain_events$Rain_in[i] >= 0.1){
    Rain_events$Flag[i] <- "SMALL"
  }
  if(Rain_events$Rain_in[i] >= 0.5){
    Rain_events$Flag[i] <- "MODERATE"
  }    
  if(Rain_events$Rain_in[i] >= 2){
    Rain_events$Flag[i] <- "LARGE"
  } 
}
#We have now a dataframe with start/end times of a storm with its associated rainfall amount and size

# recode dichotomous variable for size analysis
Rain_events2 <- Rain_events
Rain_events2$Flag2[Rain_events2$Flag=="VERY SMALL"] <- 0
Rain_events2$Flag2[Rain_events2$Flag=="SMALL"] <- 1
Rain_events2$Flag2[Rain_events2$Flag=="MODERATE"] <- 2
Rain_events2$Flag2[Rain_events2$Flag=="LARGE"] <- 3

#produce a table summarizing rain events over the 10 years
Rain_events2 <- Rain_events2[order(Rain_events2$Flag2),]
lab <- c('VERY SMALL', 'SMALL', 'MODERATE', 'LARGE')
Rain_events2$Flag2 <- factor(Rain_events2$Flag2, 
                             levels=unique(Rain_events2$Flag2),labels=lab)
Group <- labels(table(Rain_events2$Flag2))
if (!require('tables')) install.packages('tables')
tab <- tables::tabular(Rain_in ~ Flag2*(length + mean + sd), data=Rain_events2 )
tab <- matrix(as.matrix(tab,format=as.numeric,),ncol=3, byrow=TRUE)
percent <- round(tab[,1]/NROW(data)*100,3)
tab <- as.data.frame(round(tab,3))
tab <- cbind(Group,tab,percent)
tab <- tab[,c(1,2,5,3,4)]
colnames(tab) <- c("Group","n","%","M","SD")
tab
write.csv(tab, "./sizefrequencies_table.csv")

#what is the long term mean annual rainfall at the sites?
#what is the annual rainfall for each year?
#the data starts and ends in may - so we will need to account for that

annual_data <- sub_rain %>%
  mutate(year = year(timestamp)) %>%
  group_by(year) %>% # group by the year column
  # calculate the SUM of all precipitation that occurred during each year
  summarise(annual_rain= sum(Rain, na.rm=T)) %>%  
  na.omit()

monthly_data <- sub_rain %>%
  mutate(month = format(as.Date(timestamp), "%Y-%m")) %>%
  group_by(month) %>% # group by the month-year column
  # calculate the SUM of all precipitation that occurred during each month
  summarise(monthly_rain= sum(Rain, na.rm=T)) %>%  
  na.omit()

#re-establith the monthly timestamp into POSIXct format
monthly_data$month <- parse_date_time(monthly_data$month, "%Y-%m", tz = "HST")


#how many days are in the start and end years of the record?
daystart <- sum(year(unique(as.Date(data$timestamp, format="%Y-%m-%d"))) == min(annual_data$year))
daysend <- sum(year(unique(as.Date(data$timestamp, format="%Y-%m-%d"))) == max(annual_data$year))

#scale observed rainfall to an annual value
annual_data$annual_rain[annual_data$year == min(annual_data$year)] <- 
  annual_data$annual_rain[annual_data$year == min(annual_data$year)] * 365 / daystart
annual_data$annual_rain[annual_data$year == max(annual_data$year)] <- 
  annual_data$annual_rain[annual_data$year == max(annual_data$year)] * 365 / daysend


###END OF DATA PROCESSING------------------------------------------------------------------------------

###SOME STATS/PLOTS OF THE DIFFERENT RAIN EVENTS-------------------------------------------------------

#when does the data start?
startdate <- min(data$timestamp)
enddate <- date(max(data$timestamp))

#what is the distribution of rain events?
png(file="rain histogram.png")
hist(Rain_events$Rain_in, breaks = 50, xlab = "Total_rain_inches", 
     main = "Distribution of Rain events at USGS Moanalua Rain Gauge", cex = 0.5)
dev.off()

png(file="event size distribution.png")
ggplot(data.frame(Rain_events2), aes(x=Flag2)) +
  geom_bar() + labs( x = "Event Size", 
        title = paste("Rain event size distribution of events from \n",
                      startdate," to ",enddate,sep=""))
dev.off()

quantile_data <- data.frame(quantile = c("0%", "25%", "50%", "75%", "100%"),
                                 rain_in = quantile(Rain_events$Rain_in))

#which months tend to have larger rain events?
png(file="rain by month.png")
plot(month(Rain_events$Start_time), Rain_events$Rain_in, 
     xlab = "Month of Event", ylab = "Event Rainfall")
title("Month of Event Date vs. Event Rainfall")
dev.off()

#Let's say we want to compare monthly rain of 2011 and 2015
#We first need to subset the monthly rain dataset into two time series
#set the years that we want to compare
year1 <- 2011
year2 <- 2015

#now create datasets for those years
year1monthly <- subset(monthly_data,
                      month >= as.POSIXct(paste(year1,'-01-01',sep="")) &
                      month <= as.POSIXct(paste(year1,'-12-31',sep="")))
year2monthly <- subset(monthly_data,
                       month >= as.POSIXct(paste(year2,'-01-01',sep="")) &
                         month <= as.POSIXct(paste(year2,'-12-31',sep="")))

png(file="rain compare.png")
ggplot(rbind(year1monthly,year2monthly),
       aes(month(month, label = T, abbr = T),
      monthly_rain, group=factor(year(month)), color=factor(year(month)))) +
  geom_line() +
  geom_point() +
  labs(title = paste("Comparison of Monthly Rain: ",
    year1," & ",year2,sep=""),
       x = "Month",y = "Rain(in)", color="Year") +
  theme_classic()
dev.off()

#suppose we just want to see what the long term mean is
ltmean <- mean(annual_data$annual_rain)

###END OF PLOTS AND STATS------------------------------------------------------------------------------

###EXPORT THE TABLES-----------------------------------------------------------------------------------

#let's say we want to add the long term mean to the annual table. We will do that now
finaltable <- data.frame(Year = c(annual_data$year, "Long Term Mean"), Rain_in = c(annual_data$annual_rain, ltmean))

write.csv(Rain_events, file = "./RainEvents.csv", row.names = F)
write.csv(finaltable, file = "./AnnualRain.csv", row.names = F)

#let's find end time of process
process[1,2] <- Sys.time()

