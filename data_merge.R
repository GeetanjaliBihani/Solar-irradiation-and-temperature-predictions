#ENPHASE DATA
getwd()
setwd('/Users/geetanjalibihani/Dropbox/Solar/New Folder With Items/Enphase Data/power enphase 2014/5may2014/')
a<-list.files()
enphase2014<-do.call(rbind,(lapply(a,read.delim,sep=" ")))
enphase2014$X.W. <- gsub(pattern="-0500,", replacement="", enphase2014$X.W., fixed = TRUE)
enphase2014$X.W. <- gsub(pattern="-0600,", replacement="", enphase2014$X.W., fixed = TRUE)
head(enphase2014)
colnames(enphase2014)<-c("Date","Time","Power (W)")
tail(enphase2014)
?grepl

aa<-enphase2014[grepl("0",enphase2014$`Power (W)`),]
setwd('/Users/geetanjalibihani/Dropbox/Solar/New Folder With Items/Enphase Data/power enphase 2015/1jan2015///')
a<-list.files()
enphase2015<-do.call(rbind,(lapply(a,read.delim,sep=" ")))
enphase2015$X.W. <- gsub(pattern="-0500,", replacement="", enphase2015$X.W., fixed = TRUE)
enphase2015$X.W. <- gsub(pattern="-0600,", replacement="", enphase2015$X.W., fixed = TRUE)
head(enphase2015)
colnames(enphase2015)<-c("Date","Time","Power (W)")
tail(enphase2015)

setwd('/Users/geetanjalibihani/Dropbox/Solar/New Folder With Items/Enphase Data/power enphase 2016/1jan2016/')
a<-list.files()
enphase2016<-do.call(rbind,(lapply(a,read.delim,sep=" ")))
enphase2016$X.W. <- gsub(pattern="-0500,", replacement="", enphase2016$X.W., fixed = TRUE)
enphase2016$X.W. <- gsub(pattern="-0600,", replacement="", enphase2016$X.W., fixed = TRUE)
head(enphase2016)
colnames(enphase2016)<-c("Date","Time","Power (W)")
tail(enphase2016)

setwd('/Users/geetanjalibihani/Dropbox/Solar/New Folder With Items/Enphase Data/power enphase 2017/1jan2017/')
a<-list.files()
enphase2017<-do.call(rbind,(lapply(a,read.delim,sep=" ")))
enphase2017$X.W. <- gsub(pattern="-0500,", replacement="", enphase2017$X.W., fixed = TRUE)
enphase2017$X.W. <- gsub(pattern="-0600,", replacement="", enphase2017$X.W., fixed = TRUE)
head(enphase2017)
colnames(enphase2017)<-c("Date","Time","Power (W)")
tail(enphase2017)

setwd('/Users/geetanjalibihani/Dropbox/Solar/New Folder With Items/Enphase Data/power enphase 2018/1jan2018/')
a<-list.files()
enphase2018<-do.call(rbind,(lapply(a,read.delim,sep=" ")))
enphase2018$X.W. <- gsub(pattern="-0500,", replacement="", enphase2018$X.W., fixed = TRUE)
enphase2018$X.W. <- gsub(pattern="-0600,", replacement="", enphase2018$X.W., fixed = TRUE)
head(enphase2018)
colnames(enphase2018)<-c("Date","Time","Power (W)")
tail(enphase2018)

enphase<-rbind(enphase2014,enphase2015,enphase2016,enphase2017,enphase2018)
enphase<-unique(enphase)

rm(enphase2014,enphase2015,enphase2016,enphase2017,enphase2018,a)

datetxt <- as.Date(enphase$Date)
df <- data.frame(date = datetxt,
                 year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")))

enphase$date1<-df
head(enphase)

#SUNNY PORTAL DATA
#IRR
setwd('/Users/geetanjalibihani/Dropbox/Solar/New Folder With Items/sunny portal/irr/')
b<-list.files(pattern="*.csv")
b
sunn2014<-read.csv("irr_2014.csv", header = TRUE)
sunn2015<-read.csv("irr_2015.csv", header = TRUE)
sunn2016<-read.csv("irr_2016.csv", header = TRUE)
sunn2017<-read.csv("irr_2017.csv", header = TRUE)
sunn2018<-read.csv("irr_2018.csv", header = TRUE)
colnames(sunn2014)<-c("Time","Date","Insolation","Month","Year")
colnames(sunn2015)<-c("Time","Date","Insolation","Month","Year")
colnames(sunn2016)<-c("Time","Date","Insolation","Month","Year")
colnames(sunn2017)<-c("Time","Date","Insolation","Month","Year")
colnames(sunn2018)<-c("Time","Date","Insolation","Month","Year")
sunn<-rbind(sunn2014,sunn2015,sunn2016,sunn2017,sunn2018)
rm(sunn201,sunn2014,sunn2015,sunn2016,sunn2017,sunn2018,b)

#POWER kw
setwd('/Users/geetanjalibihani/Dropbox/Solar/New Folder With Items/sunny portal/power/')
b<-list.files(pattern="*.csv")
b
sunpower<-do.call(rbind,(lapply(b,read.delim,sep=",", row.names=NULL)))
unique(sunpower$Year)
sunpower<-unique(sunpower[!is.na(sunpower$Year),])
head(sunpower)

#Temp celsius
setwd('/Users/geetanjalibihani/Dropbox/Solar/New Folder With Items/sunny portal/temp//')
b<-list.files(pattern="*.csv")
b
suntempr<-do.call(rbind,(lapply(b,read.delim,sep=",", row.names=NULL)))
unique(suntempr$Year)
suntempr<-unique(suntempr[!is.na(suntempr$Year),])
tail(suntempr)

#windspeed m/s
setwd('/Users/geetanjalibihani/Dropbox/Solar/New Folder With Items/sunny portal/windspeed/')
b<-list.files(pattern="*.csv")
b
sunwind<-do.call(rbind,(lapply(b,read.delim,sep=",", row.names=NULL)))
unique(sunwind$Year)
sunwind<-unique(sunwind[!is.na(sunwind$Year),])
head(sunwind)

#tmy3
setwd('/Users/geetanjalibihani/Dropbox/Solar/New Folder With Items/TMY 3/')
b<-list.files(pattern="*.CSV")
b
TMY3_greenbay<-read.csv("TMY3_GreenBay_apt.CSV", header=TRUE)
head(TMY3_greenbay)
TMY3_indy<-read.csv("TMY3_Indianapolis_apt.CSV", header=TRUE)
head(TMY3_indy)
TMY3_purdue<-read.csv("TMY3_Purdue_ept.CSV", header=TRUE)
head(TMY3_purdue)
TMY3_all<-rbind(TMY3_purdue,TMY3_indy,TMY3_greenbay)
TMY3_all<-unique(TMY3_all)


#satellite weather data
setwd('/Users/geetanjalibihani/Dropbox/Solar/New Folder With Items/satellite weather data/')
combined_weather<-read.csv("combined climate data.csv", header=TRUE)
greenbay_climate<-read.csv("Green bay climate data.csv", header=TRUE)

#purdue data
setwd('/Users/geetanjalibihani/Dropbox/Solar/New Folder With Items/purdue/')
list.files()
#irr
pur_irr2014<-read.csv("purdue-irr14.csv", header = TRUE)
pur_irr2015<-read.csv("purdue-irr15.csv", header = TRUE)
pur_irr2016<-read.csv("purdue-irr16.csv", header = TRUE)
pur_irr2017<-read.csv("purdue-irr17.csv", header = TRUE)
pur_irr2018<-read.csv("purdue-irr18.csv", header = TRUE)
pur_irr<-rbind(pur_irr2014,pur_irr2015,pur_irr2016,pur_irr2017,pur_irr2018)
a<-unique(pur_irr)
library(tidyr)
pur_irr1<-separate(pur_irr,Date.Time,into=c( 'Date','Time' ),sep=' ')
pur_irr1$date<-as.Date(pur_irr$Date, format='%m/%d/%y')
datetxt <- as.Date(pur_irr1$date)
df <- data.frame(date = datetxt,
                 year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")))

pur_irr1$day<-df$day
pur_irr1$month<-df$month
pur_irr1$year<-df$year
tail(pur_irr1)

pur_irr<-pur_irr1[,c(1:3, 5:7)]
tail(pur_irr)
rm(a,df,datetxt,pur_irr1)
pur_irr<-unique(pur_irr[!is.na(pur_irr$year),])

#power
pur_power2014<-read.csv("purdue-power14.csv", header = TRUE)
pur_power2015<-read.csv("purdue-power15.csv", header = TRUE)
pur_power2016<-read.csv("purdue-power16.csv", header = TRUE)
pur_power2017<-read.csv("purdue-power17.csv", header = TRUE)
pur_power2018<-read.csv("purdue-power18.csv", header = TRUE)
pur_power<-rbind(pur_power2014,pur_power2015,pur_power2016,pur_power2017,pur_power2018)

a<-unique(pur_power)
library(tidyr)
pur_power1<-separate(pur_power,Date.Time,into=c( 'Date','Time' ),sep=' ')
pur_power1$date<-as.Date(pur_power$Date, format='%m/%d/%y')
datetxt <- as.Date(pur_power1$date)
df <- data.frame(date = datetxt,
                 year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")))

pur_power1$day<-df$day
pur_power1$month<-df$month
pur_power1$year<-df$year
tail(pur_power1)

pur_power<-pur_power1[,c(1:3, 5:7)]
tail(pur_power)
rm(a,df,datetxt,pur_power1)
pur_power<-unique(pur_power[!is.na(pur_power$year),])

#celltemp
pur_temp2014<-read.csv("purdue_temp14.csv", header = TRUE)
pur_temp2015<-read.csv("purdue_temp15.csv", header = TRUE)
pur_temp2016<-read.csv("purdue_temp16.csv", header = TRUE)
pur_temp2017<-read.csv("purdue_temp17.csv", header = TRUE)
pur_temp2018<-read.csv("purdue_temp18.csv", header = TRUE)
pur_celltemp<-rbind(pur_temp2014,pur_temp2015,pur_temp2016,pur_temp2017,pur_temp2018)

a<-unique(pur_celltemp)
library(tidyr)
pur_celltemp1<-separate(pur_celltemp,Date.Time,into=c( 'Date','Time' ),sep=' ')
pur_celltemp1$date<-as.Date(pur_celltemp$Date, format='%m/%d/%y')
datetxt <- as.Date(pur_celltemp1$date)
df <- data.frame(date = datetxt,
                 year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")))

pur_celltemp1$day<-df$day
pur_celltemp1$month<-df$month
pur_celltemp1$year<-df$year
tail(pur_celltemp1)

pur_celltemp<-pur_celltemp1[,c(1:3, 5:7)]
tail(pur_celltemp)
rm(a,df,datetxt,pur_celltemp1)
pur_celltemp<-unique(pur_celltemp[!is.na(pur_celltemp$year),])


#ambienttemp
pur_atemp2014<-read.csv("purdue_atemp14.csv", header = TRUE)
pur_atemp2015<-read.csv("purdue_atemp15.csv", header = TRUE)
pur_atemp2016<-read.csv("purdue_atemp16.csv", header = TRUE)
pur_atemp2017<-read.csv("purdue_atemp17.csv", header = TRUE)
pur_atemp2018<-read.csv("purdue_atemp18.csv", header = TRUE)
pur_ambtemp<-rbind(pur_atemp2014,pur_atemp2015,pur_atemp2016,pur_atemp2017,pur_atemp2018)

rm(pur_irr2014,pur_irr2015,pur_irr2016,pur_irr2017,pur_irr2018)
rm(pur_power2014,pur_power2015,pur_power2016,pur_power2017,pur_power2018)
rm(pur_temp2014,pur_temp2015,pur_temp2016,pur_temp2017,pur_temp2018)
rm(pur_atemp2014,pur_atemp2015,pur_atemp2016,pur_atemp2017,pur_atemp2018)

a<-unique(pur_ambtemp)
library(tidyr)
pur_ambtemp1<-separate(pur_ambtemp,Date.Time,into=c( 'Date','Time' ),sep=' ')
pur_ambtemp1$date<-as.Date(pur_ambtemp$Date, format='%m/%d/%y')
datetxt <- as.Date(pur_ambtemp1$date)
df <- data.frame(date = datetxt,
                 year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")))

pur_ambtemp1$day<-df$day
pur_ambtemp1$month<-df$month
pur_ambtemp1$year<-df$year
tail(pur_ambtemp1)

pur_ambtemp<-pur_ambtemp1[,c(1:3, 5:7)]
head(pur_ambtemp)
rm(a,df,datetxt,pur_ambtemp1)
pur_ambtemp<-unique(pur_ambtemp[!is.na(pur_ambtemp$year),])


#data summary
#enphase
head(enphase)

write.table(enphase, file="/Users/geetanjalibihani/Dropbox/Solar/data/enphase.csv", sep=",", row.names=FALSE)
#satellite weather data
head(combined_weather)
write.table(combined_weather, file="/Users/geetanjalibihani/Dropbox/Solar/data/satellite_weather.csv", sep=",", row.names=FALSE)

#sunny portal
head(sunn)
write.table(sunn, file="/Users/geetanjalibihani/Dropbox/Solar/data/sunny_irr.csv", sep=",", row.names=FALSE)

head(sunpower)
write.table(sunpower, file="/Users/geetanjalibihani/Dropbox/Solar/data/sunny_power.csv", sep=",", row.names=FALSE)

head(suntempr)
write.table(suntempr, file="/Users/geetanjalibihani/Dropbox/Solar/data/sunny_temp.csv", sep=",", row.names=FALSE)

head(sunwind)
write.table(sunwind, file="/Users/geetanjalibihani/Dropbox/Solar/data/sunny_wind.csv", sep=",", row.names=FALSE)


#tmy3
head(TMY3_all)
write.table(TMY3_all, file="/Users/geetanjalibihani/Dropbox/Solar/data/TMY3_overall.csv", sep=",", row.names=FALSE)

#purdue
head(pur_ambtemp)
write.table(pur_ambtemp, file="/Users/geetanjalibihani/Dropbox/Solar/data/Purdue_ambtemp.csv", sep=",", row.names=FALSE)

head(pur_celltemp)
write.table(pur_ambtemp, file="/Users/geetanjalibihani/Dropbox/Solar/data/Purdue_celltemp.csv", sep=",", row.names=FALSE)

head(pur_irr)
write.table(pur_irr, file="/Users/geetanjalibihani/Dropbox/Solar/data/Purdue_irr.csv", sep=",", row.names=FALSE)

head(pur_power)
write.table(pur_power, file="/Users/geetanjalibihani/Dropbox/Solar/data/Purdue_power.csv", sep=",", row.names=FALSE)










