library(sqldf)
library(tcltk)
library(IDPmisc)
library(scatterplot3d)
library(Hmisc)
library (tree)
library(lubridate)
library(forecast)

##Read file
Crime2010<-read.csv("F:\\RCrime\\Crimes2010.csv",header=TRUE,stringsAsFactors=FALSE)
Crime2011<-read.csv("F:\\RCrime\\Crimes2011.csv",header=TRUE,stringsAsFactors=FALSE)
Crime2012<-read.csv("F:\\RCrime\\Crimes2012.csv",header=TRUE,stringsAsFactors=FALSE)
Crime2013<-read.csv("F:\\RCrime\\Crimes2013.csv",header=TRUE,stringsAsFactors=FALSE)
Crime2014<-read.csv("F:\\RCrime\\Crimes2014.csv",header=TRUE,stringsAsFactors=FALSE)
str(Crime2010)
str(Crime2011)
str(Crime2012)
str(Crime2013)
str(Crime2014)

##extract param
Crime2010m<-data.frame(Arrest=Crime2010$Arrest,Beat=Crime2010$Beat,  Block=Crime2010$Block,CaseNumber=Crime2010$Case.Number, Date=Crime2010$Date,Description=Crime2010$Description,  Domestic=Crime2010$Domestic,            
                       FBICode=Crime2010$FBI.Code, IUCR=Crime2010$IUCR,Latitude=Crime2010$Latitude, Location=Crime2010$Location,LocationDescription=Crime2010$Location.Description,Longitude=Crime2010$Longitude,           
                       PrimaryType=Crime2010$Primary.Type,Ward=Crime2010$Ward,XCoordinate=Crime2010$X.Coordinate,YCoordinate=Crime2010$Y.Coordinate, Year=Crime2010$Year,stringsAsFactors=FALSE)     

Crime2011m<-data.frame(Arrest=Crime2011$Arrest,Beat=Crime2011$Beat,  Block=Crime2011$Block,CaseNumber=Crime2011$Case.Number, Date=Crime2011$Date,Description=Crime2011$Description,  Domestic=Crime2011$Domestic,            
                       FBICode=Crime2011$FBI.Code, IUCR=Crime2011$IUCR,Latitude=Crime2011$Latitude, Location=Crime2011$Location,LocationDescription=Crime2011$Location.Description,Longitude=Crime2011$Longitude,           
                       PrimaryType=Crime2011$Primary.Type,Ward=Crime2011$Ward,XCoordinate=Crime2011$X.Coordinate,YCoordinate=Crime2011$Y.Coordinate, Year=Crime2011$Year,stringsAsFactors=FALSE)     

Crime2012m<-data.frame(Arrest=Crime2012$Arrest,Beat=Crime2012$Beat,  Block=Crime2012$Block,CaseNumber=Crime2012$Case.Number, Date=Crime2012$Date,Description=Crime2012$Description,  Domestic=Crime2012$Domestic,            
                       FBICode=Crime2012$FBI.Code, IUCR=Crime2012$IUCR,Latitude=Crime2012$Latitude, Location=Crime2012$Location,LocationDescription=Crime2012$Location.Description,Longitude=Crime2012$Longitude,           
                       PrimaryType=Crime2012$Primary.Type,Ward=Crime2012$Ward,XCoordinate=Crime2012$X.Coordinate,YCoordinate=Crime2012$Y.Coordinate, Year=Crime2012$Year,stringsAsFactors=FALSE)     


Crime2013m<-data.frame(Arrest=Crime2013$Arrest,Beat=Crime2013$Beat,  Block=Crime2013$Block,CaseNumber=Crime2013$Case.Number, Date=Crime2013$Date,Description=Crime2013$Description,  Domestic=Crime2013$Domestic,            
                       FBICode=Crime2013$FBI.Code, IUCR=Crime2013$IUCR,Latitude=Crime2013$Latitude, Location=Crime2013$Location,LocationDescription=Crime2013$Location.Description,Longitude=Crime2013$Longitude,           
                       PrimaryType=Crime2013$Primary.Type,Ward=Crime2013$Ward,XCoordinate=Crime2013$X.Coordinate,YCoordinate=Crime2013$Y.Coordinate, Year=Crime2013$Year,stringsAsFactors=FALSE)     

Crime2014m<-data.frame(Arrest=Crime2014$Arrest,Beat=Crime2014$Beat,  Block=Crime2014$Block,CaseNumber=Crime2014$Case.Number, Date=Crime2014$Date,Description=Crime2014$Description,  Domestic=Crime2014$Domestic,            
                       FBICode=Crime2014$FBI.Code, IUCR=Crime2014$IUCR,Latitude=Crime2014$Latitude, Location=Crime2014$Location,LocationDescription=Crime2014$Location.Description,Longitude=Crime2014$Longitude,           
                       PrimaryType=Crime2014$Primary.Type,Ward=Crime2014$Ward,XCoordinate=Crime2014$X.Coordinate,YCoordinate=Crime2014$Y.Coordinate, Year=Crime2014$Year,stringsAsFactors=FALSE)     

##dataset merged
crimeAll<-rbind(Crime2010m,Crime2011m,Crime2012m,Crime2013m,Crime2014m)
rm(Crime2010m,Crime2011m,Crime2012m,Crime2013m)
#rm(Crime2014m)

##Date processing
month<-as.numeric(substr(crimeAll$Date,1,2))
day<-as.numeric(substr(crimeAll$Date,4,5))
hour<-as.numeric(substr(crimeAll$Date,12,13))
lenhour<-length(hour)
for(iLenhour in 1:lenhour){
  if(length(grep("PM",(crimeAll$Date)[iLenhour]))>0){
    hour[iLenhour]<-hour[iLenhour]+12
  }
}
crimeAll$month<-month
crimeAll$day<-day
crimeAll$hour<-hour
rm(month,day,hour)

##monthly crime number
crimeTotal<-sqldf("select count(*) as crimeMonthCount,month,year from crimeAll where year<>2014 group by year,month order by year,month")
crimeTheftTotal<-sqldf("select count(*) as theftMonthCount,month,year from crimeAll where year<>2014 and PrimaryType='THEFT' group by year,month order by year,month")

##Arima model predict
arimats12<-ts(crimeTotal$crimeMonthCount,start=c(2000,1),frequency=12)
arimalModel<-auto.arima(arimats12,trace=T)
arimaForcast<-forecast(arimalModel,h=4)
plot(arimaForcast,main="Monthly crime quantity predict")

##predict value
arimaForcast_dtframe<-as.data.frame(arimaForcast)
predict_value<-arimaForcast_dtframe[[1]]
predict_value

##2014 monthly data
crimeTotal2014<-sqldf("select count(*)/1 as crimeMonthCount,month from crimeAll where year=2014 group by year,month order by year,month")
crimeTheft2014<-sqldf("select count(*)/1 as theftMonthCount,month from crimeAll where year=2014 and PrimaryType='THEFT' group by year,month order by year,month")
fact_value<-crimeTotal2014$crimeMonthCount

##plot the rst
plot(predict_value,type="b",lty=3,col="red",xlim=c(1,4),ylim=c(10000,30000),main="2014 monthly crime quantity predicted Vs True",xlab="Month",ylab="Crime quantity")
title(main="2014 monthly crime quantity Predicted Vs True",xlab="Month")
lines(c(1,2,3,4),fact_value,type="b",lty=2,pch=22)

diff<-fact_value-predict_value

##
plot(crimeTc2014$month,crimeTc2014$crimeMonthCount)
barplot(crimeTc2014$crimeMonthCount,crimeTc2014$month,xlab="2014",ylab="Crime count")

##hourly crime number
crimeHourTotal<-sqldf("select count(*)/12 as crimeHourCount,hour,year from crimeAll where year<>2014 and PrimaryType='THEFT' group by year,Hour order by year,hour")
arimats12<-ts(data=crimeHourTotal$crimeHourCount,start=1,frequency=24)
arimalModel<-auto.arima(arimats12,trace=T)
arimaForcast<-forecast(arimalModel,h=24)
plot(arimaForcast,main="Hourly crime quantity predict")



Crime2014m<-data.frame(Arrest=Crime2014$Arrest,Beat=Crime2014$Beat,  Block=Crime2014$Block,CaseNumber=Crime2014$Case.Number, Date=Crime2014$Date,Description=Crime2014$Description,  Domestic=Crime2014$Domestic,            
                       FBICode=Crime2014$FBI.Code, IUCR=Crime2014$IUCR,Latitude=Crime2014$Latitude, Location=Crime2014$Location,LocationDescription=Crime2014$Location.Description,Longitude=Crime2014$Longitude,           
                       PrimaryType=Crime2014$Primary.Type,Ward=Crime2014$Ward,XCoordinate=Crime2014$X.Coordinate,YCoordinate=Crime2014$Y.Coordinate, Year=Crime2014$Year,stringsAsFactors=FALSE)     

month<-as.numeric(substr(Crime2014m$Date,1,2))
day<-as.numeric(substr(Crime2014m$Date,4,5))
hour<-as.numeric(substr(Crime2014m$Date,12,13))
lenhour<-length(hour)
for(iLenhour in 1:lenhour){
  if(length(grep("PM",(Crime2014m$Date)[iLenhour]))>0){
    hour[iLenhour]<-hour[iLenhour]+12
  }
}
Crime2014m$month<-month
Crime2014m$day<-day
Crime2014m$hour<-hour
rm(month,day,hour)


crimeHourTotal<-sqldf("select count(*)/4 as crimeHourCount,hour,year from Crime2014m where year=2014 and PrimaryType='THEFT' group by year,Hour order by year,hour")
crimeHourCount2014<-crimeHourTotal2014$crimeHourCount
#plot(crimeHourCount2014,main="Hourly crime quantity 2014")
plot(crimeHourCount2014,type="b",lty=3,col="red",xlim=c(1,24),ylim=c(0,1000),main="Hourly crime quantity 2014",xlab="Hour",ylab="Theft quantity")

arimaForcast_dtframe<-as.data.frame(arimaForcast)
predict_value<-arimaForcast_dtframe[[1]]
predict_value
lines(c(1:24),predict_value,type="b",lty=2,pch=22)





