# Diego Munoz & Ali Ali 
# Final Project
# US Borders 


##Task 1

rm(list=ls())

library(dplyr)
library(lubridate)
#install.packages("stringr")
#install.packages(lubridate)
library(stringr)
library(ggplot2)
library(SnowballC)
library(dplyr)


setwd("C:/Users/16036/Desktop/DATA_SCIENCE")
getwd()
setwd("C:/Users/isye/Desktop/ISYE")

entry = read.csv("EntryDataF19.csv")

    #########
    # Task 1#
    #########
entry$Year = str_sub(entry$Date,1,4)
entry$Month = str_sub(entry$Date,6,7)

entry1 <- subset(entry, Year<=2017)

count_Year <- entry1 %>% group_by(Year) %>% summarize(YearlyEntries = sum(as.numeric(Value)))

ggplot(data = count_Year, aes(x=Year, y=YearlyEntries, group = 1)) + geom_line() + ggtitle("Yearly Entries into the US (1996-2017)")

count_Month <-  entry1 %>% 
    group_by(Month) %>%
    summarize(MonthlyEntries = sum(as.numeric(Value))) 

ggplot(data = count_Month, aes(x=Month, y=MonthlyEntries, group = 1)) + geom_line() + ggtitle("Montly Entries into the US (1996-2017)")

count_Year1 <-  entry1 %>% 
    group_by(Year,State) %>%
    summarize(YearlyEntries1 = sum(as.numeric(Value)))

ggplot(data = count_Year1, aes(x=Year, y=YearlyEntries1, group = 1)) + geom_line(aes(group=State,color=State),size=3)

count_Month1 <-  entry1 %>% 
    group_by(Month,State) %>%
    summarize(MonthlyEntries1 = sum(as.numeric(Value)))

ggplot(data = count_Month1, aes(x=Month, y=MonthlyEntries1, group = 1)) + geom_line(aes(group=State,color=State),size=3) +ggtitle("Montly Entries by State (1996-2017)")

Count_Border <- entry1 %>% group_by(Border,State) %>% 
    summarize(BorderEntries = sum(as.numeric(Value)))

entry1 %>% ggplot(aes(x=Year,y=Value,color=State))+geom_point()+ylab("Entries")+facet_grid(Border~.) + ggtitle("Border Entries by State from 1996 to 2017")


entry1 %>% ggplot(aes(x=Month,y=Value,color=State))+geom_point()+ylab("Entries")+facet_grid(Border~.)       

entry1 %>% ggplot(aes(x=Month,y=Value, group=1))+geom_line(aes(group=Measure, color=Measure),size=2)
       
count_Measure1 <-  entry1 %>% 
    group_by(Measure,Month) %>%
    summarize(MeasureEntries = sum(as.numeric(Value)))                  

ggplot(data = count_Measure1, aes(x=Month, y=MeasureEntries, group = 1)) + 
    geom_line(aes(group=Measure,color=Measure),size=2)+ylab("Entries")+ggtitle("Entries by Measure Per Month (1996-2017)")


count_Measure2 <-  entry1 %>% 
    group_by(Measure) %>%
    summarize(MeasureEntries1 = sum(as.numeric(Value)))  

ggplot(data = count_Measure2, aes(x=Measure, y=MeasureEntries1, group=1)) + geom_bar(stat="Identity",
                aes(group=Measure,fill=Measure))+ylab("Entries")+ggtitle("Entries from Measure (1996-2017)")


library(leaflet)

leaflet(data=entry1[1:250,]) %>%  addTiles() %>%  addCircleMarkers(~Lon, ~Lat)

##Task 2


library(caTools)
library(rpart)
library(caret)
library(randomForest)
library(rpart.plot)
library(ROCR)
library(ISLR)
library(e1071)
library(randomForest)
library(dplyr)
library(ggplot2)
library(lubridate)



rm(list=ls())
setwd("C:/Users/16036/Desktop/DATA_SCIENCE")
entry = read.csv("EntryDataF19.csv")
exchangeUSxME =read.csv("Peso to Dollar.csv")
UnemploymentCA= read.csv("CA.csv")


#Pre-Process for US-Mexico Exchange rate 

exchangeUSxME = exchangeUSxME[,-2]
exchangeUSxME$DATE = mdy(exchangeUSxME$DATE)
exchangeUSxME$Year = year(exchangeUSxME$DATE)
exchangeUSxME$Month = month(exchangeUSxME$DATE)
exchangeUSxME$Month_Year = paste(exchangeUSxME$Month,"_",exchangeUSxME$Year,sep="")

#Pre-Process for Canada Unemployment rate

UnemploymentCA$Year = year(UnemploymentCA$DATE)
UnemploymentCA$Month = month(UnemploymentCA$DATE)
UnemploymentCA$Month_Year = paste(UnemploymentCA$Month,"_",UnemploymentCA$Year,sep="")


#######
#Part 2# 
#######

library(stringr)

# entry$Month = str_sub(entry$Date,6,7)
# entry$Year = str_sub(entry$Date,1,4)
entry$Date = ymd(entry$Date)
entry$Month = month(entry$Date)
entry$Year = year(entry$Date)
entry$Month_Year = paste(entry$Month,"_",entry$Year,sep="")
table(entry$Border)

DFtest = left_join(entry,exchangeUSxME,by="Month_Year")
summary(DFtest$EXMXUS_20191202)

DF = left_join(DFtest,UnemploymentCA, by="Month_Year")
summary(DF$LRUNTTTTCAM156S)
str(DF)

DF1 = DF[DF$Border=="US-Mexico Border" & DF$Measure=="Personal Vehicles",]
DF2 = DF[DF$Border=="US-Mexico Border" & DF$Measure=="Pedestrians",]
DF3 = DF[DF$Border=="US-Mexico Border" & !(DF$Measure %in% c("Personal Vehicles", "Personal Vehicle Passengers","Pedestrians")),]
DF4 = DF[DF$Border=="US-Canada Border" & DF$Measure=="Personal Vehicles",]
DF5 = DF[DF$Border=="US-Canada Border" & DF$Measure=="Pedestrians",]
DF6 = DF[DF$Border=="US-Canada Border" & !(DF$Measure %in% c("Personal Vehicles", "Personal Vehicle Passengers","Pedestrians")),]
DF7 = DF[DF$Border=="US-Mexico Border" & DF$Measure=="Personal Vehicle Passengers",]
DF8 = DF[DF$Border=="US-Canada Border" & DF$Measure=="Personal Vehicle Passengers",]

####################
# DF 1             #
# Linear Regression#
####################

str(DF1)
summary(DF1)
set.seed(100)
split = sample.split(DF1$Value ,SplitRatio = 0.7)
train1 = subset(DF1,split==TRUE)
test1 = subset(DF1,split==FALSE)

Mod_DF1_1 = lm(Value~Port.Name+State+Date+Lon+Lat+EXMXUS_20191202, data=train1)
summary(Mod_DF1_1)
Mod_DF1_2 = lm(Value~Date+EXMXUS_20191202+Port.Name+Lon, data=train1)
summary(Mod_DF1_2)

DF1_pred = predict(Mod_DF1_2, newdata=test1)
RMSE_DF1 = sqrt(sum(DF1_pred-test1$Value)^2)/nrow(test1)
RMSE_DF1

####################
# DF 2           #
# Linear Regression#
####################

set.seed(100)
split = sample.split(DF2$Value, SplitRatio = 0.7)
train2 = subset(DF2,split==TRUE)
test2 = subset(DF2,split==FALSE)

Mod_DF2_1 = lm(Value~Port.Name+State+Date+Lon+Lat+EXMXUS_20191202, data=train2)
summary(Mod_DF2_1)
Mod_DF2_2 = lm(Value~Port.Name+Date+Lon, data=train2)
summary(Mod_DF2_2)

DF2_pred = predict(Mod_DF2_2, newdata=test2)
RMSE_DF2 = sqrt(sum(DF2_pred-test2$Value)^2)/nrow(test2)
RMSE_DF2

####################
# DF 3        #
# Linear Regression#
####################

set.seed(100)
split = sample.split(DF3$Value, SplitRatio = 0.7)
train3 = subset(DF3,split==TRUE)
test3 = subset(DF3,split==FALSE)

Mod_DF3_1 = lm(Value~Port.Name+State+Date+Lon+Lat+EXMXUS_20191202, data=train3)
summary(Mod_DF3_1)
Mod_DF3_2 = lm(Value~Port.Name+Date+Lat+EXMXUS_20191202, data=train3)
summary(Mod_DF3_2)
Mod_DF3_3 = lm(Value~Port.Name+Lat+EXMXUS_20191202, data=train3)
summary(Mod_DF3_3)
Mod_DF3_4 = lm(Value~Port.Name+EXMXUS_20191202, data=train3)
summary(Mod_DF3_4)

DF3_pred = predict(Mod_DF3_4, newdata=test3)
RMSE_DF3 = sqrt(sum(DF3_pred-test3$Value)^2)/nrow(test3)
RMSE_DF3

####################
# DF 4      #
# Linear Regression#
####################

summary(DF4)
set.seed(100)
split = sample.split(DF4$Value, SplitRatio = 0.7)
train4 = subset(DF4,split==TRUE)
test4 = subset(DF4,split==FALSE)

Mod_DF4_1 = lm(Value~Port.Name+State+Date+Lon+Lat+LRUNTTTTCAM156S, data=train4)
summary(Mod_DF4_1)
Mod_DF4_2 = lm(Value~Port.Name+Date+Lon+Lat+LRUNTTTTCAM156S, data=train4)
summary(Mod_DF4_2)

DF4_pred = predict(Mod_DF4_2, newdata=test4)
RMSE_DF4 = sqrt(sum(DF4_pred-test4$Value)^2)/nrow(test4)
RMSE_DF4

####################
# DF  5   #
# Linear Regression#
####################

summary(DF5)
set.seed(100)
split = sample.split(DF5$Value, SplitRatio = 0.7)
train5 = subset(DF5,split==TRUE)
test5 = subset(DF5,split==FALSE)

Mod_DF5_1 = lm(Value~Port.Name+State+Date+Lon+Lat+LRUNTTTTCAM156S, data=train5)
summary(Mod_DF5_1)
Mod_DF5_2 = lm(Value~Port.Name+Date+Lon+Lat+LRUNTTTTCAM156S, data=train5)
summary(Mod_DF5_2)
Mod_DF5_3 = lm(Value~Port.Name+Date+LRUNTTTTCAM156S, data=train5)
summary(Mod_DF5_3)
Mod_DF5_4 = lm(Value~Port.Name+Date, data=train5)
summary(Mod_DF5_4)

DF5_pred = predict(Mod_DF5_4, newdata=test5)
RMSE_DF5 = sqrt(sum(DF5_pred-test5$Value)^2)/nrow(test5)
RMSE_DF5

####################
# DF  6   #
# Linear Regression#
####################

summary(DF6)
set.seed(100)
split = sample.split(DF6$Value, SplitRatio = 0.7)
train6 = subset(DF6,split==TRUE)
test6 = subset(DF6,split==FALSE)

Mod_DF6_1 = lm(Value~Port.Name+State+Date+Lon+Lat+LRUNTTTTCAM156S, data=train6)
summary(Mod_DF6_1)
Mod_DF6_2 = lm(Value~Port.Name+Date+Lon+Lat+LRUNTTTTCAM156S, data=train6)
summary(Mod_DF6_2)

DF6_pred = predict(Mod_DF6_2, newdata=test6)
RMSE_DF6 = sqrt(sum(DF6_pred-test6$Value)^2)/nrow(test6)
RMSE_DF6

####################
# DF  7   #
# Linear Regression#
####################

summary(DF7)
set.seed(100)
split = sample.split(DF7$Value, SplitRatio = 0.7)
train7 = subset(DF7,split==TRUE)
test7 = subset(DF7,split==FALSE)

Mod_DF7_1 = lm(Value~Port.Name+State+Date+Lon+Lat+EXMXUS_20191202, data=train7)
summary(Mod_DF7_1)
Mod_DF7_2 = lm(Value~Port.Name+Date+Lon+Lat+EXMXUS_20191202, data=train7)
summary(Mod_DF7_2)

DF7_pred = predict(Mod_DF7_2, newdata=test7)
RMSE_DF7 = sqrt(sum(DF7_pred-test7$Value)^2)/nrow(test7)
RMSE_DF7

####################
# DF  8   #
# Linear Regression#
####################

summary(DF8)
set.seed(100)
split = sample.split(DF8$Value, SplitRatio = 0.7)
train8 = subset(DF8,split==TRUE)
test8 = subset(DF8,split==FALSE)

Mod_DF8_1 = lm(Value~Port.Name+State+Date+Lon+Lat+LRUNTTTTCAM156S, data=train8)
summary(Mod_DF8_1)
Mod_DF8_2 = lm(Value~Port.Name+Date+Lon+Lat+LRUNTTTTCAM156S, data=train8)
summary(Mod_DF8_2)

DF8_pred = predict(Mod_DF8_2, newdata=test8)
RMSE_DF8 = sqrt(sum(DF8_pred-test8$Value)^2)/nrow(test8)
RMSE_DF8

##Eval Data

evaldata = read.csv("EvalDataF19.csv")
evaldata$Date = ymd(evaldata$Date)
evaldata$Month = month(evaldata$Date)
evaldata$Year = year(evaldata$Date)
evaldata$Month_Year = paste(evaldata$Month,"_",evaldata$Year,sep="")

EvalTest = left_join(evaldata,exchangeUSxME,by="Month_Year")
summary(EvalTest$EXMXUS_20191202)

FinalEval = left_join(EvalTest,UnemploymentCA, by="Month_Year")
summary(FinalEval$LRUNTTTTCAM156S)

FinalEval$X=NULL
FinalEval$Month=NULL
FinalEval$Year=NULL
FinalEval$Month.x=NULL
FinalEval$Year.x=NULL
FinalEval$Month.y=NULL
FinalEval$Year.y=NULL
FinalEval$DATE.x=NULL
FinalEval$DATE.y=NULL
FinalEval$Month_Year=NULL
FinalEval$State=NULL
FinalEval$Port.Code=NULL
FinalEval$Location=NULL
FinalEval$X.1=NULL
##Predictions

pred = 0
for(i in 1:nrow(FinalEval)){
    
    if(FinalEval$Border[i]=="US-Mexico Border" & FinalEval$Measure[i]=="Pedestrians"){
        pred[i] = predict(Mod_DF2_2, newdata=FinalEval[i,]) }
    
    else if(FinalEval$Border[i]=="US-Mexico Border" & FinalEval$Measure[i]=="Personal Vehicles"){
        pred[i] = predict(Mod_DF1_2, newdata=FinalEval[i,]) }
    
    else if(FinalEval$Border[i]=="US-Mexico Border" & FinalEval$Measure[i]=="Personal Vehicle Passengers"){
        pred[i] = predict(Mod_DF7_2, newdata=FinalEval[i,]) }
    
    else if(FinalEval$Border[i]=="US-Mexico Border" & FinalEval$Measure[i] %in% c("Bus Passengers","Buses","Rail Containers Empty","Rail Containers Full","Train Passengers","Trains","Truck Containers Empty","Truck Containers Full","Trucks")){
        pred[i] = predict(Mod_DF3_4, newdata=FinalEval[i,]) }
    
    else if(FinalEval$Border[i]=="US-Canada Border" & FinalEval$Measure[i]=="Pedestrian"){
        pred[i] = predict(Mod_DF5_4, newdata=FinalEval[i,]) }
    
    else if(FinalEval$Border[i]=="US-Canada Border" & FinalEval$Measure[i]=="Personal Vehicles"){
        pred[i] = predict(Mod_DF4_2, newdata=FinalEval[i,]) }
    
    else if(FinalEval$Border[i]=="US-Canada Border" & FinalEval$Measure[i]=="Personal Vehicle Passengers"){
        pred[i] = predict(Mod_DF8_2, newdata=FinalEval[i,]) }
    else {
        pred[i] = predict(Mod_DF6_2, newdata=FinalEval[i,])
    }
}

pred

FinalEval$PredictedValue = pred

write.csv(FinalEval, "FinalEvaluationPredValues.csv")

