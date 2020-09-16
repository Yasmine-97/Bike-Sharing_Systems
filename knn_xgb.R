setwd("C:/Users/lenovo/Desktop/data science 2nd semester/SL project")
getwd()
# rm(list=ls())

##Required Libraries----
library(readr)
library(ggplot2)
library(dplyr)
library(relaimpo)
library(RColorBrewer)
library(plotly) # for plot rendering
library(grid)
library(gridExtra)
library(lubridate)
library(date) # for handling dates
library(pacman)
library(forecast)
library(MLmetrics)
library(sqldf)
library(dummies)
library(FNN) 
library(caret)
library(e1071)
library(caTools)

##Bike sharig data----
# Reading the csv files- loading the data set.
bike_share <- read.csv("hour.csv", header=T)
#bike_share_train <- read.csv("Bike-sharing-train.csv", header=T)

##SUMMARY AND STRUCTURE OF THE DATA
summary(bike_share)
attach(bike_share)

###descriptive statistics
str(bike_share) #check the structure of the data set
colnames(bike_share)
dim(bike_share) ##We have 17379  rows and 17 cols


##check for missing vaues-there are no missing values
print(paste("The total number of missing data are",sum(is.na(bike_share))))


##We can ignore column casual and registered coz their sum is equal to count which is our focus
bike_share <- bike_share[,-c(1,15,16)]

## we need to change the structure of season, holiday, workdday, weather
is.factor(season) ##It is not recognized as a factor

#Data Pre-processing
## convert to factor 
bike_share$season <- as.factor(season)
bike_share$holiday <- as.factor(holiday)
bike_share$workingday <- as.factor(workingday)
bike_share$weathersit <- as.factor(weathersit)
bike_share$weekday <- as.factor(weekday)


##Confirm that they are now factors
is.factor(bike_share$season) ##True
is.factor(bike_share$holiday)
is.factor(bike_share$workingday)
is.factor(bike_share$weather)

str(bike_share) ##they are now recognized as factors


#Deriving day, hour from datetime field Train & Test

##Lubridate makes it easier to do the things R does with date-times and possible to do the things R does not
##We need to convert the datetime column to actual date which are not recognized by R
##Identify the order of the year (y), month (m), day (d), hour (h), minute (m) and second (s) elements in your data. 


# bike_share$dteday <- ymd(bike_share$dteday)###convert to date format
#bike_share$yr <-year(bike_share$dteday)
#bike_share$yr
bike_share$yr <- as.factor(yr) ## 
bike_share$weekday <- as.factor(weekday)
bike_share$hr <- as.factor(hr)
bike_share$mnth <- as.factor(mnth)


str(bike_share) 

names(bike_share)

colnames(bike_share) ##we now have 14 cols


#EDA
#Exploratory Data Analysis
## It is sqldf, an R package for runing SQL statements on data frames.

# Get the average count of bikes rent by season, hour
season_summary_by_hr <- sqldf('select season, hr, avg(cnt) as count from bike_share group by season, hr')

# From this plot it shows, 
# There are more rental in morning(from 7-9th hour) and evening(16-19th hour)
# People rent bikes more in Fall, and much less in Spring

season_summary_by_hour <- sqldf('select season, hr, avg(cnt) as count from bike_share group by season, hr')

plt1<-ggplot(bike_share, aes(x=hr, y=count, color=season))+
  geom_point(data = season_summary_by_hour, aes(group = season))+
  geom_line(data = season_summary_by_hour, aes(group = season))+
  ggtitle("Bikes Rent By Season")+ theme_minimal()+
  scale_colour_hue('Season',breaks = levels(bike_share$season), 
                   labels=c('spring', 'summer', 'fall', 'winter'))
plt1


# Get the average count of bikes rent by weather, hour
weather_summary_by_hour <- sqldf('select weathersit, hr, avg(cnt) as count from bike_share group by weathersit, hr')

# From this plot it shows, 
# People rent bikes more when weather is good
# We see bike rent only at 18th hour when weather is very bad
plt2<-ggplot(bike_share, aes(x=hr, y=count, color=weathersit))+
  geom_point(data = weather_summary_by_hour, aes(group = weathersit))+
  geom_line(data = weather_summary_by_hour, aes(group = weathersit))+
  ggtitle("Bikes Rent By Weather")+ scale_colour_hue('weathersit',breaks = levels(bike_share$weathersit), 
                                                     labels=c('Good', 'Normal', 'Bad', 'Very Bad'))
plt2

# Get the average count of bikes rent by weekday, hour
day_summary_by_hour <- sqldf('select weekday, hr, avg(cnt) as count from bike_share group by weekday, hr')

# From this plot it shows, 
# There are more bikes rent on weekdays during morining and evening
# There are more bikes rent on weekends during daytime
plt3<-ggplot(bike_share, aes(x=hr, y=count, color=weekday))+
  geom_point(data = day_summary_by_hour, aes(group = weekday))+
  geom_line(data = day_summary_by_hour, aes(group = weekday))+
  ggtitle("Bikes Rent By Weekday")+ scale_colour_hue('Weekday',breaks = levels(bike_share$weekday),
                                                     labels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
plt3


#EDA
##Extract Month
# bike_share$month <-month(ymd(bike_share$dteday), label=TRUE)


yr_summary_by_month <- sqldf('select yr, month, avg(cnt) as count from bike_share group by yr, month ')

plt4<-ggplot(bike_share, aes(x=month, color=yr))+
  geom_col(data = yr_summary_by_month, position = "dodge", aes(y = count, fill= yr))+
  ggtitle("Bike Rent by Month per Yr")+ scale_colour_hue('yr',breaks = levels(bike_share$yr))
                                                  
plt4

#PLOT  1 TRIAL 1 TO REMOVE Not nice to see
daterange=c(min(bike_share$dteday), max(bike_share$dteday))
plot(bike_share$dteday,bike_share$cnt, xaxt = "n")
axis.Date(1, at=seq(daterange[1], daterange[2], by="month"),format="%m/%y",las=2)

#TRIAL 2 

#Grouping month and years with respect to count
datagrp=summarise(group_by(bike_share, bike_share$yr, bike_share$mnth), sum(cnt))

#converting the new data to a dataframe
dat.df <- as.data.frame(datagrp)
class(dat.df)

#Renaming the columns of dat.df
names(dat.df)[names(dat.df) == "bike_share$yr"] <- "Year"
names(dat.df)[names(dat.df) == "bike_share$mnth"] <- "Month"
names(dat.df)[names(dat.df) == "sum(cnt)"] <- "count"

#Renaming values 
dat.df$Year <- as.character(dat.df$Year)
dat.df$Year[dat.df$Year == "0"] <- "2011"
dat.df$Year[dat.df$Year == "1"] <- "2012"


#Useful plots using the dat.df dataframe

year2011=as.data.frame(dat.df[which(dat.df$Year=='2011'),])
year2012=as.data.frame(dat.df[which(dat.df$Year=='2012'),])
#The plot below shows Bike Users in 2011 & 2012
ggplot() + 
  geom_line(year2011, mapping= aes(x=Month, y=count, color="2011", group = 1)) +
  geom_line(year2012, mapping= aes(x=Month, y=count, color="2012", group = 1)) +
  labs(title="Bike Users in 2011 & 2012")

#Boxplots
#Boxplot of rental bikes per year
ggplot(bike_share,aes(yr,cnt)) +
  geom_boxplot(fill = c("#8DD3C7","#FFFFB3")) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes per year") +
  scale_x_discrete(labels = c("2011","2012"))

#Boxplot of rental bikes per season
col <- brewer.pal(4,"Set3")
ggplot(bike_share,aes(season,cnt)) +
  geom_boxplot(fill = col) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes per season") +
  scale_x_discrete(labels = c("Spring","Summer","Fall","Winter"))

#Boxplot of rental bikes by holiday
ggplot(bike_share,aes(holiday,cnt)) +
  geom_boxplot(fill = c("#8DD3C7","#FFFFB3")) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes by holiday") +
  scale_x_discrete(labels = c("no","yes"))

#Boxplot of rental bikes by weekday

col <- brewer.pal(7,"Set3")
ggplot(bike_share,aes(weekday,cnt)) +
  geom_boxplot(fill = col) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes by weekday")


#### Predictive Models Analysis NO NUMERICAL VARS M1####
bike_share_M1 <- bike_share[,-c(1,10,11,12,13)] 

#dummy code variables that are factors
bike_share.new1 <- dummy.data.frame(bike_share_M1, sep = ".")


# Splitting into train(70%) and test(30%) sets
set.seed(123)
split <- sample.split(bike_share.new1$cnt, SplitRatio = 0.7)
training_set_M1 <- subset(bike_share.new1, split == TRUE)
test_set_M1 <- subset(bike_share.new1, split == FALSE)

#Selecting feature Matrix X and target Variable Y
X_train_M1= training_set_M1[,-c(58)]
X_test_M1 = test_set_M1[,-c(58)]
y_train_M1 = training_set_M1["cnt"]
y_test_M1 = test_set_M1["cnt"]

#KNN Model for different values of K 
#pred_knn1_M1=knn.reg(train =X_train_M1, test =X_test_M1 , y = y_train_M1, k = 1)
pred_knn5_M1=knn.reg(train =X_train_M1, test =X_test_M1 , y = y_train_M1, k = 5)
#pred_knn10=knn.reg(train =X_train_M1, test =X_test_M1 , y = y_train_M1, k = 10)

pred_knn5_M11=knn.reg(train =X_train_M1, test =X_train_M1 , y = y_train_M1, k = 5)
#Now check for the RMSE, RQquared and MAE values
#Which are the most famous measures of performance in regression models
#https://medium.com/human-in-a-machine-world/mae-and-rmse-which-metric-is-better-e60ac3bde13d

#printing RMSE R² and MAE
postResample(pred = pred_knn5_M1$pred, obs = t(y_test_M1))
postResample(pred = pred_knn5_M11$pred, obs = t(y_train_M1))


#### Predictive Models Analysis NUM VARS  M2 ----
bike_share_M2 <- bike_share[,-c(1)]

#dummy code variables that are factors
bike_share.new2 <- dummy.data.frame(bike_share_M2, sep = ".")


# Splitting into train(70%) and test(30%) sets
set.seed(123)
split <- sample.split(bike_share.new2$cnt, SplitRatio = 0.7)
training_set_M2 <- subset(bike_share.new2, split == TRUE)
test_set_M2 <- subset(bike_share.new2, split == FALSE)

#Selecting feature Matrix X and target Variable Y
X_train_M2= training_set_M2[,-c(62)]
X_test_M2 = test_set_M2[,-c(62)]
y_train_M2 = training_set_M2["cnt"]
y_test_M2 = test_set_M2["cnt"]

#KNN Model for different values of K 
# pred_knn1_M2=knn.reg(train =X_train_M2, test =X_test_M2 , y = y_train_M2, k = 1)
pred_knn5_M2=knn.reg(train =X_train_M2, test =X_test_M2 , y = y_train_M2, k = 5)
#pred_knn10=knn.reg(train =X_train, test =X_test , y = y_train, k = 10)
pred_knn5_M22=knn.reg(train =X_train_M2, test =X_train_M2 , y = y_train_M2, k = 5)

#printing RMSE R² and MAE
postResample(pred = pred_knn5_M2$pred, obs = t(y_test_M2))
postResample(pred = pred_knn5_M22$pred, obs = t( y_train_M2))

##Using Cross_Validation M2####
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

knn_fit <- train(cnt~., data = training_set_M2, method = "knn",
                 trControl=trctrl,tuneLength = 10)

knn_fit
test_pred_M2 <- predict(knn_fit, newdata = test_set_M2)
#test_pred_M2

postResample(pred = test_pred_M2, obs = t(y_test_M2))

    #Using CV for M2: RMSE=103.6 MAE=71.77
#Using Cross_Validation M1####
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

knn_fit <- train(cnt~., data = training_set_M1, method = "knn",
                 trControl=trctrl,tuneLength = 10)

knn_fit
test_pred_M1 <- predict(knn_fit, newdata = test_set_M1)
# test_pred_M1
postResample(pred = test_pred_M1, obs = t(y_test_M1))

#Using CV for M1: RMSE=129.96 MAE=98.008

#Maybe we should scale the data for the predicted variable 







#XGboost for Regression M2----


#Training with xgboost - gives better scores than 'rf'
 trctrl <- trainControl(method = "cv", number = 10)

# Takes a long to time to run in kaggle gridsearch
#tune_grid <- expand.grid(nrounds=c(100,200,300,400), 
#                         max_depth = c(3:7),
#                         eta = c(0.05, 1),
#                         gamma = c(0.01),
#                         colsample_bytree = c(0.75),
#                         subsample = c(0.50),
#                         min_child_weight = c(0))

# Tested the above setting in local machine
tune_grid <- expand.grid(nrounds = 200,
                         max_depth = 5,
                         eta = 0.05,
                         gamma = 0.01,
                         colsample_bytree = 0.75,
                         min_child_weight = 0,
                         subsample = 0.5)

rf_fitM2 <- train(cnt~., data = training_set_M2, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = tune_grid,
                tuneLength = 10)
# have a look at the model 
rf_fitM2

# Testing
test_pred_M2 <- predict(rf_fitM2, newdata = test_set_M2)
# test_pred_M2


postResample(pred = test_pred_M2, obs = t(y_test_M2))
#RMSE= 57.91487 and MAE=40.44154 Using all the variables



#XGboost for Regression M1----


rf_fitM1 <- train(cnt~., data = training_set_M1, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = tune_grid,
                tuneLength = 10)
# have a look at the model 
rf_fitM1

# Testing
test_pred_M1 <- predict(rf_fitM1, newdata = test_set_M1)
# test_pred_M1


postResample(pred = test_pred_M1, obs = t(y_test_M1))
# RMSE: 64.25624 and MAE: 44.5345







