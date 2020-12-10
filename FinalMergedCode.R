########################################################################################################################
# Setting up the directory
#######################################################################################################################
getwd()
setwd("C:/Users/DISHANK/Desktop/IST 687")

#########################################################################################################################
# Packages
#########################################################################################################################
##install.packages("jsonlite")   
library(jsonlite)  
##install.packages("tidyverse")   
library(tidyverse)
#install.packages("fastDummies")
library(fastDummies)
##install.packages("arules")
library(arules)
##install.packages("arulesViz")
library(arulesViz)
##install.packages("kernlab")
library(kernlab) 
##install.packages("caret")
library(caret) 
##install.packages("RCurl")
library(RCurl)
##install.packages("stringr")
library(stringr)
##install.packages("ggplot2")
library(ggplot2)
##install.packages("ggmap")
library(ggmap)
##install.packages("ggrepl")
library(ggrepel)

#########################################################################################################################
# Data Manipulation for Visualization
#########################################################################################################################
mydata.list <- fromJSON("completeSurvey.json") #read in the json file called completeSurvey.json
survey <- data.frame(mydata.list) # Convert json file to a data.fram

data <- fastDummies::dummy_cols(survey, select_columns = "Gender") ##Creating Dummy Variables for the column Gender
data <- fastDummies::dummy_cols(data, select_columns = "Class") ##Creating Dummy Variables for the column Class
data <- fastDummies::dummy_cols(data, select_columns = "Type.of.Travel") ##Creating Dummy Variables for the column Type.of.Travel

data$TotalTripTime <- data$Flight.time.in.minutes + data$Arrival.Delay.in.Minutes ##Creating a new column for TotalTripTime

data$LongFlight <- ifelse(data$Flight.time.in.minutes > 60, 1, 0) ##Creating a coloumn categorizing certain flights as long
data$LongTrip <- ifelse(data$TotalTripTime > 60, 1, 0) ##Creating a coloumn categorizing certain trips as long
data$Promotors <- ifelse(data$Likelihood.to.recommend > 8, 1, 0) ##Creating a coloumn categorizing customers flights as promotors
data$Detractors <- ifelse(data$Likelihood.to.recommend < 7, 1, 0) ##Creating a coloumn categorizing customers flights as detractors
data$Flight.cancelled <- ifelse(data$Flight.cancelled=="No",0,1) ##Creating a dummy variable for flights being cancelled or not

data[is.na(data)] <- 0 ##Assigning data that is NA as 0

##########################################################################################################################
# Visualization
##########################################################################################################################
airlinecountplot <- ggplot(data, aes(x=Partner.Name, fill=Partner.Name)) + geom_bar(identity="count") + coord_flip() + theme_bw() + theme(legend.position = "none") + ggtitle("Count of Each Partner Airline") + xlab("Count") + ylab("Partner Airline Name")
airlinecountplot

TotalTripHist <- ggplot(data, aes(TotalTripTime)) + geom_histogram(color="black", fill="mediumpurple") + xlim(0,600) + ggtitle("Histogram Depicting the Total Time for Trips") + ylab("Count") + xlab("Time (in minutes)") + theme_bw()
TotalTripHist

TravelTypeScore <- ggplot(data, aes(x=Type.of.Travel, y=Likelihood.to.recommend, fill=Type.of.Travel)) + geom_boxplot() + theme_bw() +facet_wrap(~Gender) + theme(axis.text.x = element_blank()) + ylab("Recommendation Score") + xlab("Travel Type") + ggtitle("Likelihood to Recommend based on Gender and Travel Type")
TravelTypeScore

AirlineScores <- ggplot(data, aes(x=Partner.Name, y=Likelihood.to.recommend, fill=Partner.Name)) + geom_boxplot() + theme_bw() + theme(axis.text.x = element_blank()) + ylab("Recommendation Score") + xlab("Airline") + ggtitle("Likelihood to Recommend based on Airline")
AirlineScores

cancel <- data %>% group_by(Partner.Name) %>% summarise(FlightsCancelled=sum(Flight.cancelled))
Cancellation <- ggplot(cancel, aes(x=Partner.Name, y=FlightsCancelled, fill=Partner.Name)) + geom_bar(stat="identity") + theme_bw() + theme(axis.text.x = element_blank()) + ylab("Flights Cancelled") + xlab("Airline") + ggtitle("Number of Flights Cancelled")
Cancellation

##########################################################################################################################
# Map Visualization
##########################################################################################################################
us<-map_data("state")#get state map data into us
str(survey)#see data structure
mappingdata<-data#copy data into mapping data

mappingdata$Destination.City<-as.character(mappingdata$Destination.City)#convert des city into char
mappingdata$Origin.City<-as.character(mappingdata$Origin.City)#convert ori city into char
airports <- unique(c(mappingdata$Origin.City,mappingdata$Destination.City))#get the flight city

register_google(key = "AIzaSyBCVcJDlma8PiUI9qm878X7KZC4aoeZkhA", write = TRUE)#set the google api key
coords <- geocode(airports)#get the geocode of flight city
airports <- data.frame(airport=airports, coords)#create airport information

mappingdata <- merge(mappingdata, airports, by.x="Destination.City", by.y="airport")#merge from
mappingdata <- merge(mappingdata, airports, by.x="Origin.City", by.y="airport")#merge to

worldMap <- borders("world", colour="grey", fill="white")# create a layer of borders

ggplot() + worldMap#show the map

allUSA <- ggplot() + worldMap +
  geom_curve(data=mappingdata,
             aes(x=lon.y, y=lat.y, xend=lon.x, yend=lat.x, col=Likelihood.to.recommend),
             size=.5,
             curvature=0.2) +
  geom_point(data=mappingdata,
             aes(x=lon.y, y=lat.y), 
             colour="blue",
             size=1.5) +
  geom_point(data=mappingdata,
             aes(x=lon.x, y=lat.x), 
             colour="red") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks=element_blank(),
        plot.title=element_text(hjust=0.5, size=12)) +
  ggtitle("Airline Mapping")

allUSA

########################################################################################################################
# NPS Score Analysis
########################################################################################################################

airline <- as.data.frame(table(data$Partner.Name))
airline$Var1 -> airline$Airline
airline$Freq -> airline$Count
airline$Var1 <- NULL
airline$Freq <- NULL
airline$NPS <- 0

NPSdata <- data %>% filter(Partner.Name==airline$Airline[1])
NPS <- (sum(NPSdata$Promotors)/nrow(NPSdata)) - (sum(NPSdata$Detractors)/nrow(NPSdata))
airline$NPS[1] <- NPS

NPSdata <- data %>% filter(Partner.Name==airline$Airline[2])
NPS <- (sum(NPSdata$Promotors)/nrow(NPSdata)) - (sum(NPSdata$Detractors)/nrow(NPSdata))
airline$NPS[2] <- NPS

NPSdata <- data %>% filter(Partner.Name==airline$Airline[3])
NPS <- (sum(NPSdata$Promotors)/nrow(NPSdata)) - (sum(NPSdata$Detractors)/nrow(NPSdata))
airline$NPS[3] <- NPS

NPSdata <- data %>% filter(Partner.Name==airline$Airline[4])
NPS <- (sum(NPSdata$Promotors)/nrow(NPSdata)) - (sum(NPSdata$Detractors)/nrow(NPSdata))
airline$NPS[4] <- NPS

NPSdata <- data %>% filter(Partner.Name==airline$Airline[5])
NPS <- (sum(NPSdata$Promotors)/nrow(NPSdata)) - (sum(NPSdata$Detractors)/nrow(NPSdata))
airline$NPS[5] <- NPS

NPSdata <- data %>% filter(Partner.Name==airline$Airline[6])
NPS <- (sum(NPSdata$Promotors)/nrow(NPSdata)) - (sum(NPSdata$Detractors)/nrow(NPSdata))
airline$NPS[6] <- NPS

NPSdata <- data %>% filter(Partner.Name==airline$Airline[7])
NPS <- (sum(NPSdata$Promotors)/nrow(NPSdata)) - (sum(NPSdata$Detractors)/nrow(NPSdata))
airline$NPS[7] <- NPS

NPSdata <- data %>% filter(Partner.Name==airline$Airline[8])
NPS <- (sum(NPSdata$Promotors)/nrow(NPSdata)) - (sum(NPSdata$Detractors)/nrow(NPSdata))
airline$NPS[8] <- NPS

NPSdata <- data %>% filter(Partner.Name==airline$Airline[9])
NPS <- (sum(NPSdata$Promotors)/nrow(NPSdata)) - (sum(NPSdata$Detractors)/nrow(NPSdata))
airline$NPS[9] <- NPS

NPSdata <- data %>% filter(Partner.Name==airline$Airline[10])
NPS <- (sum(NPSdata$Promotors)/nrow(NPSdata)) - (sum(NPSdata$Detractors)/nrow(NPSdata))
airline$NPS[10] <- NPS

NPSdata <- data %>% filter(Partner.Name==airline$Airline[11])
NPS <- (sum(NPSdata$Promotors)/nrow(NPSdata)) - (sum(NPSdata$Detractors)/nrow(NPSdata))
airline$NPS[11] <- NPS

NPSdata <- data %>% filter(Partner.Name==airline$Airline[12])
NPS <- (sum(NPSdata$Promotors)/nrow(NPSdata)) - (sum(NPSdata$Detractors)/nrow(NPSdata))
airline$NPS[12] <- NPS

NPSdata <- data %>% filter(Partner.Name==airline$Airline[13])
NPS <- (sum(NPSdata$Promotors)/nrow(NPSdata)) - (sum(NPSdata$Detractors)/nrow(NPSdata))
airline$NPS[13] <- NPS

NPSdata <- data %>% filter(Partner.Name==airline$Airline[14])
NPS <- (sum(NPSdata$Promotors)/nrow(NPSdata)) - (sum(NPSdata$Detractors)/nrow(NPSdata))
airline$NPS[14] <- NPS


airlineNPS <- ggplot(airline, aes(x=Airline, y=NPS, fill=Airline)) + geom_bar(stat="identity") + coord_flip() + theme_bw() + theme(legend.position = "none") + ggtitle("Net Promotor Score for Each Airline") + xlab("Airline") + ylab("Net Promotor Score")
airlineNPS

######################################################################################################
# Visualizing partnered airlines for which most flights are cancelled
######################################################################################################

table(survey$Partner.Name, survey$Flight.cancelled) #Creating a table for Flight canceled
survey_cancelled <- as.data.frame.matrix(table(survey$Partner.Name, survey$Flight.cancelled)) #Creating the matrix that shows number of Fligh cancelled yes or no for each partnered airlines
View(survey_cancelled)
survey_cancelled$Partnered_airlines <- rownames(survey_cancelled) #Creating a column Partnered_airlines with names of the partnered airlines
ggplot(survey_cancelled, aes(y=Yes, x=Partnered_airlines)) + 
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # Visualizing the bar graph. Rotated the values of x axis vertically so we can see them clearly

######################################################################################################
# Data model to check the rules for cancellation of flight for FlyFastAirways Inc
######################################################################################################

#Creatng subset of dataset
survey_FlyFast <- survey[(survey$Partner.Name == 'FlyFast Airways Inc.'),] # Creating the subset of Survey datset for FlyFast Airways Inc.
View(survey_FlyFast)
survey_FlyFast_Yes = survey_FlyFast[(survey_FlyFast$Flight.cancelled == 'Yes'),] # Creating the subset for Flights.Cancelled as Yes
View(survey_FlyFast_Yes)

######################################################################################################
# Data Manipulation
######################################################################################################

#Removing extra vaiables
survey_FlyFast_Yes$Age <- NULL
survey_FlyFast_Yes$Gender<- NULL
survey_FlyFast_Yes$Price.Sensitivity <- NULL
survey_FlyFast_Yes$Year.of.First.Flight <- NULL
survey_FlyFast_Yes$Loyalty <- NULL
survey_FlyFast_Yes$Type.of.Travel <- NULL
survey_FlyFast_Yes$Total.Freq.Flyer.Accts <- NULL
survey_FlyFast_Yes$Shopping.Amount.at.Airport <- NULL
survey_FlyFast_Yes$Eating.and.Drinking.at.Airport <- NULL
survey_FlyFast_Yes$Class <- NULL
survey_FlyFast_Yes$Flight.date <- NULL
survey_FlyFast_Yes$Partner.Code <- NULL
survey_FlyFast_Yes$Partner.Name <- NULL
survey_FlyFast_Yes$Origin.City <- NULL
survey_FlyFast_Yes$Departure.Delay.in.Minutes <- NULL
survey_FlyFast_Yes$olong <- NULL
survey_FlyFast_Yes$olat <- NULL
survey_FlyFast_Yes$dlong <- NULL
survey_FlyFast_Yes$dlat <- NULL
survey_FlyFast_Yes$freeText <- NULL
survey_FlyFast_Yes$Destination.City <- NULL
survey_FlyFast_Yes$Flights.Per.Year <- NULL
survey_FlyFast_Yes$Arrival.Delay.in.Minutes <- NULL
survey_FlyFast_Yes$Flight.time.in.minutes <- NULL
View(survey_FlyFast_Yes)
str(survey_FlyFast_Yes)

######################################################################################################
# Finding the rules for cancellation in flights
######################################################################################################

##Converting numeric data into category

#Scale: Flight.Distance = 73 to 460 = Short
#Scale: Flight.Distance = 461 to 848 = Medium
#Scale: Flight.Distance = 849 to 1236 = Long
max(survey_FlyFast_Yes$Flight.Distance) #Maximum value for Flight.Distance
min(survey_FlyFast_Yes$Flight.Distance) #Minimum value for Flight.Distance
survey_FlyFast_Yes$Flight.Distance.1[survey_FlyFast_Yes$Flight.Distance>=73 & survey_FlyFast_Yes$Flight.Distance<=460] = 'Short'
survey_FlyFast_Yes$Flight.Distance.1[survey_FlyFast_Yes$Flight.Distance>=461 & survey_FlyFast_Yes$Flight.Distance<=848] = 'Medium'
survey_FlyFast_Yes$Flight.Distance.1[survey_FlyFast_Yes$Flight.Distance>=849 & survey_FlyFast_Yes$Flight.Distance<=1236] = 'Long'
survey_FlyFast_Yes$Flight.Distance <- NULL #Removing theh previous old column
View(survey_FlyFast_Yes)
survey_FlyFast_Yes$Flight.Distance.1 <- as.factor(survey_FlyFast_Yes$Flight.Distance.1) #Factoring the column

#Scale: Day.of.Month = 1 to 10 = Start month
#Scale: Day.of.Month = 11 to 20 = Mid month
#Scale: Day.of.Month = 21 to 30 = End month
survey_FlyFast_Yes$Day.of.Month.1[survey_FlyFast_Yes$Day.of.Month>= 1 & survey_FlyFast_Yes$Day.of.Month <= 10] = 'Start month'
survey_FlyFast_Yes$Day.of.Month.1[survey_FlyFast_Yes$Day.of.Month>= 11 & survey_FlyFast_Yes$Day.of.Month <= 20] = 'Mid month'
survey_FlyFast_Yes$Day.of.Month.1[survey_FlyFast_Yes$Day.of.Month>= 21 & survey_FlyFast_Yes$Day.of.Month <= 30] = 'End month'
survey_FlyFast_Yes$Day.of.Month<- NULL
survey_FlyFast_Yes$Day.of.Month.1 <- as.factor(survey_FlyFast_Yes$Day.of.Month.1)
View(survey_FlyFast_Yes)

#Scale: Schedule.Departure.Hour = 0 to 10 = Morning
#Scale: Schedule.Departure.Hour = 11 to 14 = Afternoon
#Scale: Schedule.Departure.Hour = 14 to 19 = Evening
#Scale: Schedule.Departure.Hour = 20 to 24 = Night
survey_FlyFast_Yes$Scheduled.Departure.period[survey_FlyFast_Yes$Scheduled.Departure.Hour>=0 & survey_FlyFast_Yes$Scheduled.Departure.Hour<=10] = 'Morning'
survey_FlyFast_Yes$Scheduled.Departure.period[survey_FlyFast_Yes$Scheduled.Departure.Hour>=11 & survey_FlyFast_Yes$Scheduled.Departure.Hour<=14] = 'Afternoon'
survey_FlyFast_Yes$Scheduled.Departure.period[survey_FlyFast_Yes$Scheduled.Departure.Hour>=15 & survey_FlyFast_Yes$Scheduled.Departure.Hour<=19] = 'Evening'
survey_FlyFast_Yes$Scheduled.Departure.period[survey_FlyFast_Yes$Scheduled.Departure.Hour>=20 & survey_FlyFast_Yes$Scheduled.Departure.Hour<=24] = 'Night'
survey_FlyFast_Yes$Scheduled.Departure.Hour <- NULL # Removing the old column
survey_FlyFast_Yes$Scheduled.Departure.period <- as.factor(survey_FlyFast_Yes$Scheduled.Departure.period) #Factoring the column
View(survey_FlyFast_Yes)


#Scale: Likelihood.to.recommend = 1 to 4 = Low
#Scale: Likelihood.to.recommend = 5 to 7 = Medium
#Scale: Likelihood.to.recommend = 8 to 10 = High
survey_FlyFast_Yes$Likelihood.to.recommend.1[survey_FlyFast_Yes$Likelihood.to.recommend>=0 & survey_FlyFast_Yes$Likelihood.to.recommend<=4] = 'Low'
survey_FlyFast_Yes$Likelihood.to.recommend.1[survey_FlyFast_Yes$Likelihood.to.recommend>=5 & survey_FlyFast_Yes$Likelihood.to.recommend<=7] = 'Medium'
survey_FlyFast_Yes$Likelihood.to.recommend.1[survey_FlyFast_Yes$Likelihood.to.recommend>=8 & survey_FlyFast_Yes$Likelihood.to.recommend<=10] = 'High'
survey_FlyFast_Yes$Likelihood.to.recommend <- NULL #Removing the old column
survey_FlyFast_Yes$Likelihood.to.recommend.1 <- as.factor(survey_FlyFast_Yes$Likelihood.to.recommend.1) #Factoring the column
View(survey_FlyFast_Yes)


survey_FlyFast_Nox <- as(survey_FlyFast_Yes,"transactions") #Coercing the survey_FlyFast_No dataset into transaction matrix
survey_FlyFast_Nox
inspect(survey_FlyFast_Nox) #Using inspect function to display associations and transactions
rules <- apriori(survey_FlyFast_Nox, parameter = list(supp = 0.13, conf = 0.15),
                 control=list(verbose=F),
                 appearance=list(default="lhs",rhs=("Flight.cancelled=Yes"))) #Generating most relevant set of rules from given transaction data. Finding rules for every other item in cart with Beer Bottles with 0.0008 support and 0.55 confidence


inspect(rules[1:15])
#Texas, Georgia, Illinois are the origin state with 1 as confidence and lift from where most number of flights are getting cancelled.
# Flghts going to Illonis are mostly getting cancelled.(Confidence 1 and lift 1)

rules1 <- apriori(survey_FlyFast_Nox, parameter = list(supp = 0.3, conf = 0.3),
                  control=list(verbose=F),
                  appearance=list(default="lhs",rhs=("Flight.cancelled=Yes"))) #Generating most relevant set of rules from given transaction data. Finding rules for every other item in cart with Beer Bottles with 0.0008 support and 0.55 confidence


inspect(rules1)
# 49.88% of flights in the start of month is cancelled
# 56.23% of short distance flights are cancelled
# 78% of Blue airline status flights are cancelled


######################################################################################################
# Model to predict the the likelihood of recommendation for Flyfast Airways
# Bar graph to see the highest rate of low recommendation to partnered airways
######################################################################################################

#Converting the likelihood of recommendation in categories
survey$Likelihood.to.recommend.1[survey$Likelihood.to.recommend>=0 & survey$Likelihood.to.recommend<=4] = 'Low'
survey$Likelihood.to.recommend.1[survey$Likelihood.to.recommend>=5 & survey$Likelihood.to.recommend<=7] = 'Medium'
survey$Likelihood.to.recommend.1[survey$Likelihood.to.recommend>=8 & survey$Likelihood.to.recommend<=10] = 'High'
survey$Likelihood.to.recommend <- NULL
survey$Likelihood.to.recommend.1 <- as.factor(survey$Likelihood.to.recommend.1)
View(survey)

#Creating the subset of Survey
table(survey$Partner.Name, survey$Likelihood.to.recommend.1) #Creating a table for Partnered name with Likelihood.of.recommndation
survey_cancelled <- as.data.frame.matrix(table(survey$Partner.Name, survey$Likelihood.to.recommend.1)) #Creating a datafrae by convering table into matrix
survey_cancelled$Rate <- survey_cancelled$Low / (survey_cancelled$High + survey_cancelled$Low + survey_cancelled$Medium) #Calculating Rate
View(survey_cancelled)
survey_cancelled$Partnered_airlines <- rownames(survey_cancelled) #Creating new column Partnered_airlines with name

#Plotting the graph for rate of low recommended partnered airlines
ggplot(survey_cancelled, aes(y=Rate, x=Partnered_airlines)) + 
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#FlyFastAirways has the highest rate of low likelihood to recommend among all the partnered airlines

######################################################################################################
# Data manipulation
######################################################################################################

# Removing the extra column
survey_FlyFast <- survey[(survey$Partner.Name == 'FlyFast Airways Inc.'),] # Creating the subset of Survey for FlyFast Airways Inc
survey_FlyFast$Destination.City <- NULL
survey_FlyFast$Origin.City <- NULL
survey_FlyFast$Price.Sensitivity <- NULL
survey_FlyFast$Year.of.First.Flight <- NULL
survey_FlyFast$Flights.Per.Year <- NULL
survey_FlyFast$Loyalty <- NULL
survey_FlyFast$Type.of.Travel <- NULL
survey_FlyFast$Total.Freq.Flyer.Accts <- NULL
survey_FlyFast$Shopping.Amount.at.Airport <- NULL
survey_FlyFast$Eating.and.Drinking.at.Airport <- NULL
survey_FlyFast$Flight.date <- NULL
survey_FlyFast$Partner.Code <- NULL
survey_FlyFast$Partner.Name<- NULL
survey_FlyFast$Departure.Delay.in.Minutes <- NULL
survey_FlyFast$Arrival.Delay.in.Minutes <- NULL
survey_FlyFast$olong <- NULL
survey_FlyFast$olat <- NULL
survey_FlyFast$dlong <- NULL
survey_FlyFast$dlat <- NULL
survey_FlyFast$freeText <- NULL
survey_FlyFast$Flight.cancelled <- NULL
View(survey_FlyFast)
str(survey_FlyFast)

# Converting flight.time.in.minutes column NA values to 0
NA_Vec <- which(is.na(survey_FlyFast$Flight.time.in.minutes))
survey_FlyFast[NA_Vec,10 ]<- 0

# Deleting the NA rows if present
which(is.na(survey_FlyFast))
survey_FlyFast <- survey_FlyFast[complete.cases(survey_FlyFast),]

####################################################################################################
# Rules for likelihood of recommendation
####################################################################################################


#Creating a copy of survey_FlyFast dataset
survey_FlyFast_Assoc <- survey_FlyFast 
str(survey_FlyFast_Assoc)

#Converting numerical data into categories

#Scale: Flight distancee :73 - 460 - Short
#Scale: Flight distancee :461 - 848 - Medium
#Scale: Flight distancee :849 - 1290 - Long

max(survey_FlyFast_Assoc$Flight.Distance)
min(survey_FlyFast_Assoc$Flight.Distance)
survey_FlyFast_Assoc$Flight.Distance.1[survey_FlyFast_Assoc$Flight.Distance>=73 & survey_FlyFast_Assoc$Flight.Distance<=460] = 'Short'
survey_FlyFast_Assoc$Flight.Distance.1[survey_FlyFast_Assoc$Flight.Distance>=461 & survey_FlyFast_Assoc$Flight.Distance<=848] = 'Medium'
survey_FlyFast_Assoc$Flight.Distance.1[survey_FlyFast_Assoc$Flight.Distance>=849 & survey_FlyFast_Assoc$Flight.Distance<=1290] = 'Long'
survey_FlyFast_Assoc$Flight.Distance <- NULL #Removing the old column
View(survey_FlyFast_Assoc)
survey_FlyFast_Assoc$Flight.Distance.1 <- as.factor(survey_FlyFast_Assoc$Flight.Distance.1) #Factoring the column

#Scale: Day of month :1 - 10 - Start month
#Scale: Day of month :11 - 20 - Mid month
#Scale: Day of month :21 - 30 - End month

survey_FlyFast_Assoc$Day.of.Month.1[survey_FlyFast_Assoc$Day.of.Month>= 1 & survey_FlyFast_Assoc$Day.of.Month <= 10] = 'Start month'
survey_FlyFast_Assoc$Day.of.Month.1[survey_FlyFast_Assoc$Day.of.Month>= 11 & survey_FlyFast_Assoc$Day.of.Month <= 20] = 'Mid month'
survey_FlyFast_Assoc$Day.of.Month.1[survey_FlyFast_Assoc$Day.of.Month>= 21 & survey_FlyFast_Assoc$Day.of.Month <= 30] = 'End month'
survey_FlyFast_Assoc$Day.of.Month<- NULL #Removing the column
survey_FlyFast_Assoc$Day.of.Month.1 <- as.factor(survey_FlyFast_Assoc$Day.of.Month.1) #Converting into factor
View(survey_FlyFast_Assoc)

#Scale: Flight Scheduled Departure  Hour :0 - 10 - Morning
#Scale: Flight Scheduled Departure :11 - 14 - Afternoon
#Scale: Flight Scheduled Departure :15 - 19 - Evening
#Scale: Flight Scheduled Departure :20 - 24 - Night

survey_FlyFast_Assoc$Scheduled.Departure.period[survey_FlyFast_Assoc$Scheduled.Departure.Hour>=0 
                                                & survey_FlyFast_Assoc$Scheduled.Departure.Hour<=10] = 'Morning'
survey_FlyFast_Assoc$Scheduled.Departure.period[survey_FlyFast_Assoc$Scheduled.Departure.Hour>=11 
                                                & survey_FlyFast_Assoc$Scheduled.Departure.Hour<=14] = 'Afternoon'
survey_FlyFast_Assoc$Scheduled.Departure.period[survey_FlyFast_Assoc$Scheduled.Departure.Hour>=15 
                                                & survey_FlyFast_Assoc$Scheduled.Departure.Hour<=19] = 'Evening'
survey_FlyFast_Assoc$Scheduled.Departure.period[survey_FlyFast_Assoc$Scheduled.Departure.Hour>=20 
                                                & survey_FlyFast_Assoc$Scheduled.Departure.Hour<=24] = 'Night'
survey_FlyFast_Assoc$Scheduled.Departure.Hour <- NULL #Removing the column
survey_FlyFast_Assoc$Scheduled.Departure.period <- as.factor(survey_FlyFast_Assoc$Scheduled.Departure.period) #Converting to factor
View(survey_FlyFast_Assoc)

#Scale: Age :15 - 25 - Young
#Scale: Age :26 - 50 - Adult
#Scale: Age :51 - 85 - Old

min(survey_FlyFast_Assoc$Age)
max(survey_FlyFast_Assoc$Age)
survey_FlyFast_Assoc$Age.1[survey_FlyFast_Assoc$Age>=15 & survey_FlyFast_Assoc$Age<=25] = 'Young'
survey_FlyFast_Assoc$Age.1[survey_FlyFast_Assoc$Age>=26 & survey_FlyFast_Assoc$Age<=50] = 'Adults'
survey_FlyFast_Assoc$Age.1[survey_FlyFast_Assoc$Age>=51 & survey_FlyFast_Assoc$Age<=85] = 'Old'
survey_FlyFast_Assoc$Age <- NULL #Removing the old column
View(survey_FlyFast_Assoc)
survey_FlyFast_Assoc$Age.1<- as.factor(survey_FlyFast_Assoc$Age.1) #Factoring theh column

#Scale: Flight time :13 - 90 - Short
#Scale: Flight time :91 - 150 - Medium
#Scale: Flight time :151 - 223 - Long

max(survey_FlyFast_Assoc$Flight.time.in.minutes)
min(survey_FlyFast_Assoc$Flight.time.in.minutes)
survey_FlyFast_Assoc$Flight.time.in.minutes.1[survey_FlyFast_Assoc$Flight.time.in.minutes>=13 
                                              & survey_FlyFast_Assoc$Flight.time.in.minutes<=90] = 'Short'
survey_FlyFast_Assoc$Flight.time.in.minutes.1[survey_FlyFast_Assoc$Flight.time.in.minutes>=91 
                                              & survey_FlyFast_Assoc$Flight.time.in.minutes<=150] = 'Medium'
survey_FlyFast_Assoc$Flight.time.in.minutes.1[survey_FlyFast_Assoc$Flight.time.in.minutes>=151 
                                              & survey_FlyFast_Assoc$Flight.time.in.minutes<=223] = 'Long'
survey_FlyFast_Assoc$Flight.time.in.minutes <- NULL #Removing the old column
View(survey_FlyFast_Assoc)
survey_FlyFast_Assoc$Flight.time.in.minutes.1<- as.factor(survey_FlyFast_Assoc$Flight.time.in.minutes.1) #Converting into factor
survey_FlyFast_Assoc$Likelihood.to.recommend.1 <- as.factor(survey_FlyFast_Assoc$Likelihood.to.recommend.1) #Converting into factor

#Creating Rules using Apriori
survey_FlyFast_Assocx <- as(survey_FlyFast_Assoc,"transactions") #Coercing the survey_FlyFast_No dataset into transaction matrix
survey_FlyFast_Assocx

####THIS LINE OF CODE TAKES A LONG TO RUN, RUN WITH CAUTION 
inspect(survey_FlyFast_Assocx) #Using inspect function to display associations and transactions

rules <- apriori(survey_FlyFast_Assocx, parameter = list(supp = 0.12, conf = 0.1),
                 control=list(verbose=F),
                 appearance=list(default="lhs",rhs=("Likelihood.to.recommend.1=Low"))) #Generating most relevant set of rules from given transaction data. Finding rules for every other item in cart with Beer Bottles with 0.0008 support and 0.55 confidence

inspect(rules)
# Gender Female
# Status = Blue and Class Economy has highest Lift and coverage

######################################################################################################
# SVM to predict likelihood for FlyFast Airways Inc.
######################################################################################################

#Converting string columns into numeric
survey_FlyFast$Airline.Status <- as.numeric(survey_FlyFast$Airline.Status) #Converting to numeric column
survey_FlyFast$Gender <- as.numeric(survey_FlyFast$Gender) #Converting to numeric column
survey_FlyFast$Class <- as.numeric(survey_FlyFast$Class) #Converting to numeric column
str(survey_FlyFast)
View(survey_FlyFast)

#Converting the variables into factor
survey_FlyFast$Airline.Status <- as.factor(survey_FlyFast$Airline.Status)
survey_FlyFast$Age <- as.factor(survey_FlyFast$Age)
survey_FlyFast$Gender <- as.factor(survey_FlyFast$Gender)
survey_FlyFast$Class <- as.factor(survey_FlyFast$Class)
survey_FlyFast$Day.of.Month <- as.factor(survey_FlyFast$Day.of.Month)
survey_FlyFast$Scheduled.Departure.Hour <- as.factor(survey_FlyFast$Scheduled.Departure.Hour) 
survey_FlyFast$Flight.time.in.minutes <- as.factor(survey_FlyFast$Flight.time.in.minutes)
survey_FlyFast$Flight.Distance <- as.factor(survey_FlyFast$Flight.Distance)
survey_FlyFast$Likelihood.to.recommend.1 <- as.factor(survey_FlyFast$Likelihood.to.recommend.1)

#survey_FlyFast$Origin.State <- as.numeric(survey_FlyFast$Origin.State)
#survey_FlyFast$Destination.State <- as.numeric(survey_FlyFast$Destination.State)
#survey_FlyFast$Flight.cancelled <- ifelse(survey_FlyFast$Flight.cancelled == 'Yes', 1, 0)
str(survey_FlyFast)

#Splitting data into training and testing dataet
library(kernlab) #Loading package kernlab
library(caret) #Loading package  caret
trainList <- createDataPartition(y=survey_FlyFast$Likelihood.to.recommend.1,p=.60,list=FALSE) #Partitioning the 40% of data set into train dataset
trainset <- survey_FlyFast [trainList,] #Creating the training dataset using trainList
testSet <- survey_FlyFast[-trainList, ] #Creating test dataset using trainList

#Checking the dimension of the train and test dataset
dim(trainset) #Viewing the dimension of the newly created train dataset
dim(testSet) #Viewing the dimension of the newly created test dataset

#Creating and training the svm data model with trainset 
SVMoutput <- ksvm(Likelihood.to.recommend.1~. , data = trainset, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE) #Training the model using train dataset to predict class
SVMoutput
#Cross validation error 0.397

# Predicting the datamodel for testset
SVMpred <- predict(SVMoutput, newdata = testSet) #Predicting the class using test dataset
str(SVMpred) #viewing the structure of SVMpred
head(SVMpred) #viewing the first 6 rows of SVMpred

#Creating the table for predicted output
tableout <- table(SVMpred, testSet$Likelihood.to.recommend.1) 
diag(tableout)
confusionMatrix(tableout)

#Calculating the error
error_rate = (1 -(sum(diag(tableout)) / sum(tableout))) * 100
error_rate
# Error rate 36.01%

########################################################################################################################
# Flight and Airport Factors for Higher Scores
########################################################################################################################

FlightScore <- lm(Likelihood.to.recommend~-1+Price.Sensitivity+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Flight.time.in.minutes+Arrival.Delay.in.Minutes+Scheduled.Departure.Hour+Flight.cancelled, data=data)
summary(FlightScore) ##Regression Results
##Adjusted R Squared = 0.8648

##Customer Factors for Higher Scores
CustomerScore <- lm(Likelihood.to.recommend~-1+Age+Year.of.First.Flight+Flights.Per.Year+Loyalty+Type.of.Travel+Total.Freq.Flyer.Accts+Gender_Male, data=data)
summary(CustomerScore) ##Regression Results
##Adjusted R Squared = 0.9406

##Airlines that have higher or lower recommendation scores
airline <- data %>% select(c(Likelihood.to.recommend, Partner.Name))
airline <- fastDummies::dummy_cols(airline, select_columns = "Partner.Name")
airline <- airline %>% select(-c(Partner.Name))
AirlineScore <- lm(Likelihood.to.recommend~-1+.,data=airline)
summary(AirlineScore) ##Regression Results
##Adjusted R Squared = 0.9149
