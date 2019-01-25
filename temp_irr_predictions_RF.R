##install.packages('randomForest')
library(randomForest)
set.seed(101)

#DATA PREP
enphase<-read.csv("/Users/geetanjalibihani/Dropbox/Solar/data/enphase.csv", header = TRUE)
comb_weather<-read.csv("/Users/geetanjalibihani/Dropbox/Solar/data/satellite_weather.csv", header = TRUE)
purdue_total<-read.csv("/Users/geetanjalibihani/Dropbox/Solar/data/Purdue_total.csv", header = TRUE)
sunny_total<-read.csv("/Users/geetanjalibihani/Dropbox/Solar/data/Sunny_total.csv", header = TRUE)
sunny_new1<-sunny_total[,c(1:5,8)]
head(sunny_new1)
weather_new1<-comb_weather_wausau[,c(7:9,3,4,5,6)]
head(weather_new1)
combined_sunny_weather_n1<-sqldf('select a.Date, a.Month, a.Year, a.Time, (b.TMAX+b.TMIN)/2 as tavg, b.AWND, b.PRCP, a.Insolation, a.module
                                 from sunny_new1 a left join
                                 weather_new1 b
                                 on a.Date=b.day and a.Month=b.month and a.Year=b.year                                 ')
head(combined_sunny_weather_n1)
combined_sunny_weather_n1<-combined_sunny_weather_n1[complete.cases(combined_sunny_weather_n1),]
head(TMY3_all)
TMY3_all1<-sqldf ("select *, case when 
                  time='24:00:00' then 1
                  when time='23:00' then 2
                  when time='22:00' then 3
                  when time='21:00' then 4
                  when time='20:00' then 5
                  when time='19:00' then 6
                  when time='18:00' then 7
                  when time='17:00' then 8
                  when time='16:00' then 9
                  when time='15:00'then 10
                  when time='14:00'then 11
                  when time='13:00' then 12
                  when time='12:00' then 13
                  when time='11:00' then 14
                  when time='10:00' then 15
                  when time='9:00' then 16
                  when time='8:00' then 17
                  when time='7:00' then 18
                  when time='6:00' then 19
                  when time='5:00' then 20
                  when time='4:00' then 21
                  when time='3:00' then 22
                  when time='2:00' then 23
                  when time='1:00' then 24
                  else '0' end as t_flag from TMY3_all")
TMY3_all1[which(TMY3_all1$day==15 & TMY3_all1$month==9 & TMY3_all1$time=='7:00'),]
nrow(unique(sunny_total_1))
sunny_new2<-sunny_total_1[,c(1:6,9)]
head(sunny_new2)
TMY3_agg<-sqldf("select day, month, case when 
                time='24:00:00' then 1
                when time='23:00' then 2
                when time='22:00' then 3
                when time='21:00' then 4
                when time='20:00' then 5
                when time='19:00' then 6
                when time='18:00' then 7
                when time='17:00' then 8
                when time='16:00' then 9
                when time='15:00'then 10
                when time='14:00'then 11
                when time='13:00' then 12
                when time='12:00' then 13
                when time='11:00' then 14
                when time='10:00' then 15
                when time='9:00' then 16
                when time='8:00' then 17
                when time='7:00' then 18
                when time='6:00' then 19
                when time='5:00' then 20
                when time='4:00' then 21
                when time='3:00' then 22
                when time='2:00' then 23
                when time='1:00' then 24
                else '0' end as t_flag, time, avg(global_hirr) global_hirr, 
                avg(diff_hirr) diff_hirr,
                avg(total_skycover) total_skycover,
                avg(dry_bulb_temp) dry_bulb_temp,
                avg(dew_point_temp) dew_point_temp,
                avg(rel_humidity) rel_humidity,
                avg(station_pressure) station_pressure,
                avg(wind_speed) as tmy3_windspeed,
                avg(h_visibility) h_visibility,
                avg(aod) aod,
                avg(albedo) albedo from TMY3_all group by day,month,time order by day, month,time")

tail(TMY3_agg)
TMY3_agg[which(TMY3_agg$day==15 & TMY3_agg$month==9 & TMY3_agg$time=='7:00'),]

combined_sunny_tmy3_n1<-sqldf('select a.Date, a.Month, a.Year, a.Time, a.t_flag,
                              b.global_hirr, b.diff_hirr, b.total_skycover, b.dry_bulb_temp, b.dew_point_temp, b.rel_humidity, b.station_pressure, 
                              b.tmy3_windspeed, b.h_visibility, b.aod, b.albedo
                              ,
                              a.Insolation, a.module
                              from sunny_new2 a left join
                              TMY3_agg b
                              on a.Date=b.day and a.Month=b.month and a.t_flag=b.t_flag'   )
head(combined_sunny_tmy3_n1)

combined_sun_ws_tmy3<-sqldf('select a.*, b.tavg as ws_tavg, b.AWND as ws_ws, b.PRCP as ws_prcp 
                            from 
                            combined_sunny_tmy3_n1 a
                            left join
                            combined_sunny_weather_n1 b
                            on a.Date=b.Date and a.Month=b.Month and a.Year=b.Year and a.Time=b.Time')
head(combined_sun_ws_tmy3)

combined_sun_ws_tmy3<-combined_sun_ws_tmy3[complete.cases(combined_sun_ws_tmy3),]
rownames(combined_sun_ws_tmy3) <- 1:nrow(combined_sun_ws_tmy3)

#SEPARATING TEST AND TRAIN SET
dim(combined_sun_ws_tmy3)
nrow(combined_sun_ws_tmy3)*0.8

#training Sample
# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(combined_sun_ws_tmy3$Insolation, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- combined_sun_ws_tmy3[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- combined_sun_ws_tmy3[validation_index,]

head(validation)
head(dataset)

#running first model and finding optimal number of trees
library(randomForest)
library(caret)
comb.rf1=randomForest(factor(Insolation)~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data = combined_sun_ws_tmy3 , subset = train)
print(comb.rf1)
plot(comb.rf1)

#running RF for optimal number of trees i.e. 20
#IRR using tmy3+ws
comb.rf2=randomForest(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data = combined_sun_ws_tmy3 , subset = train, ntree=20, importance=T)
print(comb.rf2)
varImpPlot(comb.rf2,type=2)
varImp(comb.rf2)
plot(comb.rf2)
summary(comb.rf2)

#predicting for testing set
#IRR using tmy3+ws
head(dataset)
dataset<-dataset[which(dataset$Insolation!=0),]
dataset$predicted_irr<-predict(comb.rf2,dataset )
MAPE(dataset$Insolation, dataset$predicted_irr)
head(dataset)

#module temp using tmy3+ws
comb.rf3=randomForest(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data = combined_sun_ws_tmy3 , subset = train, ntree=20, importance=T)
print(comb.rf3)
varImpPlot(comb.rf3,type=2)
varImp(comb.rf3)
plot(comb.rf3)
summary(comb.rf3)

#predicting for testing set
#module temp using tmy3
comb.rf4=randomForest(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data = combined_sun_ws_tmy3 , subset = train, ntree=20, importance=T)
print(comb.rf4)
varImpPlot(comb.rf4,type=2)
varImp(comb.rf4)
plot(comb.rf4)
summary(comb.rf4)

#predicting for testing set
#module temp using tmy3
head(dataset)
dataset<-dataset[which(dataset$module!=0),]
dataset$predicted_module<-predict(comb.rf4,dataset )
MAPE(dataset$module, dataset$predicted_module)
head(dataset)

#irr using tmy3
comb.rf5=randomForest(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data = combined_sun_ws_tmy3 , subset = train, ntree=20, importance=T)
print(comb.rf5)
varImpPlot(comb.rf5,type=2)
varImp(comb.rf5)
plot(comb.rf5)
summary(comb.rf5)

#predicting for testing set
#irr using tmy3
head(dataset)
dataset<-dataset[which(dataset$module!=0),]
dataset$predicted_irr<-predict(comb.rf5,dataset )
MAPE(dataset$Insolation, dataset$predicted_irr)
head(dataset)

#irr using ws
comb.rf6=randomForest(Insolation~ws_tavg+ws_ws+ws_prcp, data = combined_sun_ws_tmy3 , subset = train, ntree=20, importance=T)
print(comb.rf6)
varImpPlot(comb.rf6,type=2)
varImp(comb.rf6)
plot(comb.rf6)
summary(comb.rf6)

#predicting for testing set
#irr using ws
head(dataset)
dataset<-dataset[which(dataset$module!=0),]
dataset$predicted_irr<-predict(comb.rf6,dataset )
MAPE(dataset$Insolation, dataset$predicted_irr)
head(dataset)


#module temp using ws
comb.rf7=randomForest(module~ws_tavg+ws_ws+ws_prcp, data = combined_sun_ws_tmy3 , subset = train, ntree=20, importance=T)
print(comb.rf7)
varImpPlot(comb.rf7,type=2)
varImp(comb.rf7)
plot(comb.rf7)
summary(comb.rf7)

#predicting for testing set
#module temp using ws
head(dataset)
dataset<-dataset[which(dataset$module!=0),]
dataset$predicted_module<-predict(comb.rf7,dataset )
MAPE(dataset$module, dataset$predicted_module)
head(dataset)


#CUMULATIVE MAPE
##IRR

###TMY3+WS
#M1
#module temp using ws
cwt1<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Date>=15 & combined_sun_ws_tmy3$Month==9 & combined_sun_ws_tmy3$Year==2014),]
cwt1<-cwt1[which(cwt1$Insolation!=0),]
comb.m1=randomForest(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data = cwt1 , ntree=20, importance=T)

#M2
cwt2<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10) & combined_sun_ws_tmy3$Year==2014),]
cwt2<-cwt2[which(cwt2$Insolation!=0),]
rownames(cwt2) <- 1:nrow(cwt2)
comb.m2=randomForest(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data = cwt2 , ntree=20, importance=T)
cwt2$predicted_irr<-predict(comb.m1, cwt2)
MAPE(cwt2$Insolation, cwt2$predicted_irr)

#M3
cwt3<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11) & combined_sun_ws_tmy3$Year==2014),]
cwt3<-cwt3[which(cwt3$Insolation!=0),]
rownames(cwt3) <- 1:nrow(cwt3)
comb.m3=randomForest(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data = cwt3 , ntree=20, importance=T)
cwt3$predicted_irr<-predict(comb.m2, cwt3)
MAPE(cwt3$Insolation, cwt3$predicted_irr)

#M4
cwt4<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12) & combined_sun_ws_tmy3$Year==2014),]
cwt4<-cwt4[which(cwt4$Insolation!=0),]
rownames(cwt4) <- 1:nrow(cwt4)
comb.m4=randomForest(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data = cwt4 , ntree=20, importance=T)
cwt4$predicted_irr<-predict(comb.m3, cwt4)
MAPE(cwt4$Insolation, cwt4$predicted_irr)

#M5
cwt5<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1) & combined_sun_ws_tmy3$Year==2014,2015),]
cwt5<-cwt5[which(cwt5$Insolation!=0),]
rownames(cwt5) <- 1:nrow(cwt5)
comb.m5=randomForest(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data = cwt5 , ntree=20, importance=T)
cwt5$predicted_irr<-predict(comb.m4, cwt5)
MAPE(cwt5$Insolation, cwt5$predicted_irr)

#M6
cwt6<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2) & combined_sun_ws_tmy3$Year==2014,2015),]
cwt6<-cwt5[which(cwt6$Insolation!=0),]
rownames(cwt6) <- 1:nrow(cwt6)
comb.m6=randomForest(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data = cwt6 , ntree=20, importance=T)
cwt6$predicted_irr<-predict(comb.m5, cwt6)
MAPE(cwt6$Insolation, cwt6$predicted_irr)

###TMY3
#M1
#module temp using ws
cwt1<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Date>=15 & combined_sun_ws_tmy3$Month==9 & combined_sun_ws_tmy3$Year==2014),]
cwt1<-cwt1[which(cwt1$Insolation!=0),]
comb.m11=randomForest(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data = cwt1 , ntree=20, importance=T)

#M2
cwt2<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10) & combined_sun_ws_tmy3$Year==2014),]
cwt2<-cwt2[which(cwt2$Insolation!=0),]
rownames(cwt2) <- 1:nrow(cwt2)
comb.m21=randomForest(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data = cwt2 , ntree=20, importance=T)
cwt2$predicted_irr<-predict(comb.m11, cwt2)
MAPE(cwt2$Insolation, cwt2$predicted_irr)

#M3
cwt3<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11) & combined_sun_ws_tmy3$Year==2014),]
cwt3<-cwt3[which(cwt3$Insolation!=0),]
rownames(cwt3) <- 1:nrow(cwt3)
comb.m31=randomForest(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data = cwt3 , ntree=20, importance=T)
cwt3$predicted_irr<-predict(comb.m21, cwt3)
MAPE(cwt3$Insolation, cwt3$predicted_irr)

#M4
cwt4<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12) & combined_sun_ws_tmy3$Year==2014),]
cwt4<-cwt4[which(cwt4$Insolation!=0),]
rownames(cwt4) <- 1:nrow(cwt4)
comb.m41=randomForest(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data = cwt4 , ntree=20, importance=T)
cwt4$predicted_irr<-predict(comb.m31, cwt4)
MAPE(cwt4$Insolation, cwt4$predicted_irr)

#M5
cwt5<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1) & combined_sun_ws_tmy3$Year==2014,2015),]
cwt5<-cwt5[which(cwt5$Insolation!=0),]
rownames(cwt5) <- 1:nrow(cwt5)
comb.m51=randomForest(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data = cwt5 , ntree=20, importance=T)
cwt5$predicted_irr<-predict(comb.m41, cwt5)
MAPE(cwt5$Insolation, cwt5$predicted_irr)

#M6
cwt6<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2) & combined_sun_ws_tmy3$Year==2014,2015),]
cwt6<-cwt6[which(cwt6$Insolation!=0),]
rownames(cwt6) <- 1:nrow(cwt6)
comb.m61=randomForest(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data = cwt6 , ntree=20, importance=T)
cwt6$predicted_irr<-predict(comb.m51, cwt6)
MAPE(cwt6$Insolation, cwt6$predicted_irr)

###WS
#M1
#module temp using ws
cwt1<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Date>=15 & combined_sun_ws_tmy3$Month==9 & combined_sun_ws_tmy3$Year==2014),]
cwt1<-cwt1[which(cwt1$Insolation!=0),]
comb.m12=randomForest(Insolation~ws_tavg+ws_ws+ws_prcp, data = cwt1 , ntree=20, importance=T)

#M2
cwt2<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10) & combined_sun_ws_tmy3$Year==2014),]
cwt2<-cwt2[which(cwt2$Insolation!=0),]
rownames(cwt2) <- 1:nrow(cwt2)
comb.m22=randomForest(Insolation~ws_tavg+ws_ws+ws_prcp, data = cwt2 , ntree=20, importance=T)
cwt2$predicted_irr<-predict(comb.m12, cwt2)
MAPE(cwt2$Insolation, cwt2$predicted_irr)

#M3
cwt3<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11) & combined_sun_ws_tmy3$Year==2014),]
cwt3<-cwt3[which(cwt3$Insolation!=0),]
rownames(cwt3) <- 1:nrow(cwt3)
comb.m32=randomForest(Insolation~ws_tavg+ws_ws+ws_prcp, data = cwt3 , ntree=20, importance=T)
cwt3$predicted_irr<-predict(comb.m22, cwt3)
MAPE(cwt3$Insolation, cwt3$predicted_irr)

#M4
cwt4<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12) & combined_sun_ws_tmy3$Year==2014),]
cwt4<-cwt4[which(cwt4$Insolation!=0),]
rownames(cwt4) <- 1:nrow(cwt4)
comb.m42=randomForest(Insolation~ws_tavg+ws_ws+ws_prcp, data = cwt4 , ntree=20, importance=T)
cwt4$predicted_irr<-predict(comb.m32, cwt4)
MAPE(cwt4$Insolation, cwt4$predicted_irr)

#M5
cwt5<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1) & combined_sun_ws_tmy3$Year==2014,2015),]
cwt5<-cwt5[which(cwt5$Insolation!=0),]
rownames(cwt5) <- 1:nrow(cwt5)
comb.m52=randomForest(Insolation~ws_tavg+ws_ws+ws_prcp, data = cwt5 , ntree=20, importance=T)
cwt5$predicted_irr<-predict(comb.m42, cwt5)
MAPE(cwt5$Insolation, cwt5$predicted_irr)

#M6
cwt6<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2) & combined_sun_ws_tmy3$Year==2014,2015),]
cwt6<-cwt6[which(cwt6$Insolation!=0),]
rownames(cwt6) <- 1:nrow(cwt6)
comb.m62=randomForest(Insolation~ws_tavg+ws_ws+ws_prcp, data = cwt6 , ntree=20, importance=T)
cwt6$predicted_irr<-predict(comb.m52, cwt6)
MAPE(cwt6$Insolation, cwt6$predicted_irr)

##MODULE TEMP

###TMY3+WS
#M1
#module temp using ws
cwt1<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Date>=15 & combined_sun_ws_tmy3$Month==9 & combined_sun_ws_tmy3$Year==2014),]
cwt1<-cwt1[which(cwt1$module!=0),]
comb.m1=randomForest(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data = cwt1 , ntree=20, importance=T)

#M2
cwt2<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10) & combined_sun_ws_tmy3$Year==2014),]
cwt2<-cwt2[which(cwt2$module!=0),]
rownames(cwt2) <- 1:nrow(cwt2)
comb.m2=randomForest(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data = cwt2 , ntree=20, importance=T)
cwt2$predicted_module<-predict(comb.m1, cwt2)
MAPE(cwt2$module, cwt2$predicted_module)

#M3
cwt3<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11) & combined_sun_ws_tmy3$Year==2014),]
cwt3<-cwt3[which(cwt3$module!=0),]
rownames(cwt3) <- 1:nrow(cwt3)
comb.m3=randomForest(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data = cwt3 , ntree=20, importance=T)
cwt3$predicted_module<-predict(comb.m2, cwt3)
MAPE(cwt3$module, cwt3$predicted_module)

#M4
cwt4<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12) & combined_sun_ws_tmy3$Year==2014),]
cwt4<-cwt4[which(cwt4$module!=0),]
rownames(cwt4) <- 1:nrow(cwt4)
comb.m4=randomForest(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data = cwt4 , ntree=20, importance=T)
cwt4$predicted_module<-predict(comb.m3, cwt4)
MAPE(cwt4$module, cwt4$predicted_module)

#M5
cwt5<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1) & combined_sun_ws_tmy3$Year==2014,2015),]
cwt5<-cwt5[which(cwt5$module!=0),]
rownames(cwt5) <- 1:nrow(cwt5)
comb.m5=randomForest(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data = cwt5 , ntree=20, importance=T)
cwt5$predicted_module<-predict(comb.m4, cwt5)
MAPE(cwt5$module, cwt5$predicted_module)

#M6
cwt6<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2) & combined_sun_ws_tmy3$Year==2014,2015),]
cwt6<-cwt6[which(cwt6$module!=0),]
rownames(cwt6) <- 1:nrow(cwt6)
comb.m6=randomForest(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data = cwt6 , ntree=20, importance=T)
cwt6$predicted_module<-predict(comb.m5, cwt6)
MAPE(cwt6$module, cwt6$predicted_module)


###TMY3
#M1
#module temp using ws
cwt1<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Date>=15 & combined_sun_ws_tmy3$Month==9 & combined_sun_ws_tmy3$Year==2014),]
cwt1<-cwt1[which(cwt1$module!=0),]
comb.m11=randomForest(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data = cwt1 , ntree=20, importance=T)

#M2
cwt2<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10) & combined_sun_ws_tmy3$Year==2014),]
cwt2<-cwt2[which(cwt2$module!=0),]
rownames(cwt2) <- 1:nrow(cwt2)
comb.m21=randomForest(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data = cwt2 , ntree=20, importance=T)
cwt2$predicted_module<-predict(comb.m11, cwt2)
MAPE(cwt2$module, cwt2$predicted_module)

#M3
cwt3<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11) & combined_sun_ws_tmy3$Year==2014),]
cwt3<-cwt3[which(cwt3$module!=0),]
rownames(cwt3) <- 1:nrow(cwt3)
comb.m31=randomForest(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data = cwt3 , ntree=20, importance=T)
cwt3$predicted_module<-predict(comb.m21, cwt3)
MAPE(cwt3$module, cwt3$predicted_module)

#M4
cwt4<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12) & combined_sun_ws_tmy3$Year==2014),]
cwt4<-cwt4[which(cwt4$module!=0),]
rownames(cwt4) <- 1:nrow(cwt4)
comb.m41=randomForest(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data = cwt4 , ntree=20, importance=T)
cwt4$predicted_module<-predict(comb.m31, cwt4)
MAPE(cwt4$module, cwt4$predicted_module)

#M5
cwt5<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1) & combined_sun_ws_tmy3$Year==2014,2015),]
cwt5<-cwt5[which(cwt5$module!=0),]
rownames(cwt5) <- 1:nrow(cwt5)
comb.m51=randomForest(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data = cwt5 , ntree=20, importance=T)
cwt5$predicted_module<-predict(comb.m41, cwt5)
MAPE(cwt5$module, cwt5$predicted_module)

#M6
cwt6<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2) & combined_sun_ws_tmy3$Year==2014,2015),]
cwt6<-cwt6[which(cwt6$module!=0),]
rownames(cwt6) <- 1:nrow(cwt6)
comb.m61=randomForest(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data = cwt6 , ntree=20, importance=T)
cwt6$predicted_module<-predict(comb.m51, cwt6)
MAPE(cwt6$module, cwt6$predicted_module)

###WS
#M1
#module temp using ws
cwt1<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Date>=15 & combined_sun_ws_tmy3$Month==9 & combined_sun_ws_tmy3$Year==2014),]
cwt1<-cwt1[which(cwt1$module!=0),]
comb.m12=randomForest(module~ws_tavg+ws_ws+ws_prcp, data = cwt1 , ntree=20, importance=T)

#M2
cwt2<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10) & combined_sun_ws_tmy3$Year==2014),]
cwt2<-cwt2[which(cwt2$module!=0),]
rownames(cwt2) <- 1:nrow(cwt2)
comb.m22=randomForest(module~ws_tavg+ws_ws+ws_prcp, data = cwt2 , ntree=20, importance=T)
cwt2$predicted_module<-predict(comb.m12, cwt2)
MAPE(cwt2$module, cwt2$predicted_module)

#M3
cwt3<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11) & combined_sun_ws_tmy3$Year==2014),]
cwt3<-cwt3[which(cwt3$module!=0),]
rownames(cwt3) <- 1:nrow(cwt3)
comb.m32=randomForest(module~ws_tavg+ws_ws+ws_prcp, data = cwt3 , ntree=20, importance=T)
cwt3$predicted_module<-predict(comb.m22, cwt3)
MAPE(cwt3$module, cwt3$predicted_module)

#M4
cwt4<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12) & combined_sun_ws_tmy3$Year==2014),]
cwt4<-cwt4[which(cwt4$module!=0),]
rownames(cwt4) <- 1:nrow(cwt4)
comb.m42=randomForest(module~ws_tavg+ws_ws+ws_prcp, data = cwt4 , ntree=20, importance=T)
cwt4$predicted_module<-predict(comb.m32, cwt4)
MAPE(cwt4$module, cwt4$predicted_module)

#M5
cwt5<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1) & combined_sun_ws_tmy3$Year==2014,2015),]
cwt5<-cwt5[which(cwt5$module!=0),]
rownames(cwt5) <- 1:nrow(cwt5)
comb.m52=randomForest(module~ws_tavg+ws_ws+ws_prcp, data = cwt5 , ntree=20, importance=T)
cwt5$predicted_module<-predict(comb.m42, cwt5)
MAPE(cwt5$module, cwt5$predicted_module)

#M6
cwt6<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2) & combined_sun_ws_tmy3$Year==2014,2015),]
cwt6<-cwt6[which(cwt6$module!=0),]
rownames(cwt6) <- 1:nrow(cwt6)
comb.m62=randomForest(module~ws_tavg+ws_ws+ws_prcp, data = cwt6 , ntree=20, importance=T)
cwt6$predicted_module<-predict(comb.m52, cwt6)
MAPE(cwt6$module, cwt6$predicted_module)

