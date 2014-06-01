library(sqldf)
library(tcltk)
library(IDPmisc)
library(scatterplot3d)
library(Hmisc)
library (tree)
library(lubridate)
library(rpart)

#read files
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




					   
## Merge data
crimeAll<-rbind(Crime2010m,Crime2011m,Crime2012m,Crime2013m)
rm(Crime2010m,Crime2011m,Crime2012m,Crime2013m)
rm(Crime2014m)

##Datetime process
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

##Block distribution
blockdistribution20<-sqldf("select Block, count(*) as BlockCrimeCount,XCoordinate,YCoordinate from crimeAll group by Block order by BlockCrimeCount desc limit 20")
blockdistribution20

blockdistribution2000<-sqldf("select Block, count(*) as BlockCrimeCount,XCoordinate,YCoordinate from crimeAll group by Block order by BlockCrimeCount desc limit 2000")
with(blockdistribution2000,iplot(XCoordinate,YCoordinate,main="High frequency crimes zone distribution"))
rm(blockdistribution20,blockdistribution2000)

##location
locationdistribution20<-sqldf("select LocationDescription, count(*) as LocationCrimeCount from crimeAll group by LocationDescription order by LocationCrimeCount desc limit 20")
locationdistribution20
rm(locationdistribution20)

typedistribution10<-sqldf("select PrimaryType, count(*) as PrimaryTypeCount from crimeAll group by PrimaryType order by PrimaryTypeCount desc limit 10")
barplot(typedistribution10$PrimaryTypeCount,names.arg=typedistribution10$PrimaryType)
rm(typedistribution10)

##theft
theftdistribution2000<-sqldf("select count(*) as theftCount,XCoordinate,YCoordinate from crimeAll where PrimaryType='THEFT' group by XCoordinate,YCoordinate order by theftCount desc limit 2000")
attach(theftdistribution2000)
scatterplot3d(XCoordinate,YCoordinate,theftCount,type="h",main="Theft distribution")
detach(theftdistribution2000)
rm(theftdistribution2000)


theftTimedistribution<-sqldf("select count(*) as crimeCount,hour from crimeAll group by hour order by crimeCount desc")
theftTimedistribution
theftMonthedistribution<-sqldf("select count(*) as crimeCount,month from crimeAll group by month order by crimeCount desc")
theftMonthedistribution

##shapiro.test
describe(theftTimedistribution$crimeCount)
shapiro.test(theftTimedistribution$crimeCount)

crimesAllM<-data.frame(Block=crimeAll$Block,Description=crimeAll$Description,LocationDescription=crimeAll$LocationDescription,month=crimeAll$month,day=crimeAll$day,hour=crimeAll$hour,PrimaryType=crimeAll$PrimaryType,stringsAsFactors=FALSE)