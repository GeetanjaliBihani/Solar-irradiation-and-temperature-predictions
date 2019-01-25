enphase<-read.csv("/Users/geetanjalibihani/Dropbox/Solar/data/enphase.csv", header = TRUE)
comb_weather<-read.csv("/Users/geetanjalibihani/Dropbox/Solar/data/satellite_weather.csv", header = TRUE)
purdue_total<-read.csv("/Users/geetanjalibihani/Dropbox/Solar/data/Purdue_total.csv", header = TRUE)
sunny_total<-read.csv("/Users/geetanjalibihani/Dropbox/Solar/data/Sunny_total.csv", header = TRUE)

#predicting irr and module temperature using ws
head(sunny_total)
sunny_new1<-sunny_total[,c(1:5,8)]
head(sunny_new1)
head(comb_weather_wausau)
weather_new1<-comb_weather_wausau[,c(7:9,3,4,5,6)]
head(weather_new1)
library(sqldf)
combined_sunny_weather_n1<-sqldf('select a.Date, a.Month, a.Year, a.Time, (b.TMAX+b.TMIN)/2 as tavg, b.AWND, b.PRCP, a.Insolation, a.module
                                 from sunny_new1 a left join
weather_new1 b
on a.Date=b.day and a.Month=b.month and a.Year=b.year                                 ')
head(combined_sunny_weather_n1)
combined_sunny_weather_n1<-combined_sunny_weather_n1[complete.cases(combined_sunny_weather_n1),]
new_mod1<-lm(Insolation~tavg+AWND+PRCP, data=combined_sunny_weather_n1)
summary(new_mod1)
anova(new_mod1)
combined_sunny_weather_n1$fitted<-new_mod1$fitted.values
head(combined_sunny_weather_n1)
new_mod2<-lm(module~tavg+AWND+PRCP, data=combined_sunny_weather_n1)
summary(new_mod2)
anova(new_mod2)

#predicting irr and module temperature using tmy3
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
new_mod3<-lm(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data=combined_sunny_tmy3_n1)
summary(new_mod3)
new_mod4<-lm(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data=combined_sunny_tmy3_n1)
summary(new_mod4)

#predicting irr and module temp using weather station and TMY3 predictors
head(combined_sunny_tmy3_n1)
head(combined_sunny_weather_n1)
combined_sun_ws_tmy3<-sqldf('select a.*, b.tavg as ws_tavg, b.AWND as ws_ws, b.PRCP as ws_prcp 
                            from 
                            combined_sunny_tmy3_n1 a
                            left join
                            combined_sunny_weather_n1 b
                            on a.Date=b.Date and a.Month=b.Month and a.Year=b.Year and a.Time=b.Time')
head(combined_sun_ws_tmy3)
nrow(combined_sun_ws_tmy3[combined_sun_ws_tmy3$Year %in% c(2014,2015),])
new_mod5<-lm(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data=combined_sun_ws_tmy3)
summary(new_mod5)
new_mod6<-lm(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data=combined_sun_ws_tmy3)
summary(new_mod6)


#DOING CUMULATIVE MAPE ANALYSIS FOR FIRST 6 MONTHS
head(combined_sun_ws_tmy3)
head(combined_sun_ws_tmy3[order(combined_sun_ws_tmy3$Date & combined_sun_ws_tmy3$Month & combined_sun_ws_tmy3$Year),])

#WS+TMY3 predictors
#Sep'14
cwt1<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Date>=15 & combined_sun_ws_tmy3$Month==9 & combined_sun_ws_tmy3$Year==2014),]
head(cwt1)
mmodel1<-lm(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data=cwt1)

#Oct'14
cwt2<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10) & combined_sun_ws_tmy3$Year==2014),]
rownames(cwt2) <- 1:nrow(cwt2)
cwt2$predicted_irr<-predict(mmodel1, cwt2)
library(smooth)
MAPE(cwt2$Insolation, cwt2$predicted_irr)
mmodel2<-lm(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data=cwt2)

#Nov'14
cwt3<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11) & combined_sun_ws_tmy3$Year==2014),]
rownames(cwt3) <- 1:nrow(cwt3)
cwt3$predicted_irr<-predict(mmodel2, cwt3)
library(smooth)
MAPE(cwt3$Insolation, cwt3$predicted_irr)
mmodel3<-lm(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data=cwt3)

#Dec'14
cwt4<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12) & combined_sun_ws_tmy3$Year==2014),]
rownames(cwt4) <- 1:nrow(cwt4)
cwt4$predicted_irr<-predict(mmodel3, cwt4)
library(smooth)
MAPE(cwt4$Insolation, cwt4$predicted_irr)
mmodel4<-lm(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data=cwt4)

#Jan'15
cwt5<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt5) <- 1:nrow(cwt5)
cwt5$predicted_irr<-predict(mmodel4, cwt5)
MAPE(cwt5$Insolation, cwt5$predicted_irr)
mmodel5<-lm(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data=cwt5)

#Feb'15
cwt6<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt6) <- 1:nrow(cwt6)
cwt6$predicted_irr<-predict(mmodel5, cwt6)
MAPE(cwt6$Insolation, cwt6$predicted_irr)
mmodel6<-lm(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data=cwt6)

#Mar'15
cwt7<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2,3) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt7) <- 1:nrow(cwt7)
cwt7$predicted_irr<-predict(mmodel6, cwt7)
MAPE(cwt7$Insolation, cwt7$predicted_irr)
mmodel7<-lm(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data=cwt7)

#Apr'15
cwt8<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2,3,4) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt8) <- 1:nrow(cwt8)
cwt8$predicted_irr<-predict(mmodel7, cwt8)
MAPE(cwt8$Insolation, cwt8$predicted_irr)
mmodel8<-lm(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data=cwt8)

#WS predictors
#Sep'14
cwt1<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Date>=15 & combined_sun_ws_tmy3$Month==9 & combined_sun_ws_tmy3$Year==2014),]
head(cwt1)
mmodel11<-lm(Insolation~ws_tavg+ws_ws+ws_prcp, data=cwt11)

#Oct'14
cwt21<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10) & combined_sun_ws_tmy3$Year==2014),]
rownames(cwt21) <- 1:nrow(cwt21)
cwt21$predicted_irr<-predict(mmodel11, cwt21)
library(smooth)
MAPE(cwt21$Insolation, cwt21$predicted_irr)
mmodel21<-lm(Insolation~ws_tavg+ws_ws+ws_prcp, data=cwt21)

#Nov'14
cwt31<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11) & combined_sun_ws_tmy3$Year==2014),]
rownames(cwt31) <- 1:nrow(cwt31)
cwt31$predicted_irr<-predict(mmodel21, cwt31)
library(smooth)
MAPE(cwt31$Insolation, cwt31$predicted_irr)
mmodel31<-lm(Insolation~ws_tavg+ws_ws+ws_prcp, data=cwt31)

#Dec'14
cwt41<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12) & combined_sun_ws_tmy3$Year==2014),]
rownames(cwt41) <- 1:nrow(cwt41)
cwt41$predicted_irr<-predict(mmodel31, cwt41)
library(smooth)
MAPE(cwt41$Insolation, cwt41$predicted_irr)
mmodel41<-lm(Insolation~ws_tavg+ws_ws+ws_prcp, data=cwt41)

#Jan'15
cwt51<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt51) <- 1:nrow(cwt51)
cwt51$predicted_irr<-predict(mmodel41, cwt51)
MAPE(cwt51$Insolation, cwt51$predicted_irr)
mmodel51<-lm(Insolation~ws_tavg+ws_ws+ws_prcp, data=cwt51)

#Feb'15
cwt61<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt61) <- 1:nrow(cwt61)
cwt61$predicted_irr<-predict(mmodel51, cwt61)
MAPE(cwt61$Insolation, cwt61$predicted_irr)
mmodel61<-lm(Insolation~ws_tavg+ws_ws+ws_prcp, data=cwt61)

#Mar'15
cwt71<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2,3) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt7) <- 1:nrow(cwt71)
cwt71$predicted_irr<-predict(mmodel61, cwt71)
MAPE(cwt71$Insolation, cwt71$predicted_irr)
mmodel71<-lm(Insolation~ws_tavg+ws_ws+ws_prcp, data=cwt71)

#Apr'15
cwt81<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2,3,4) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt81) <- 1:nrow(cwt81)
cwt81$predicted_irr<-predict(mmodel71, cwt81)
MAPE(cwt81$Insolation, cwt81$predicted_irr)
mmodel81<-lm(Insolation~ws_tavg+ws_ws+ws_prcp, data=cwt71)

#TMY3 predictors
#Sep'14
head(combined_sun_ws_tmy3)
combined_sun_ws_tmy3<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Insolation!=0),c(1:4,6:18)]
head(combined_sun_ws_tmy3)
cwt12<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Date>=15 & combined_sun_ws_tmy3$Month==9 & combined_sun_ws_tmy3$Year==2014),]
tail(cwt12)

mmodel12<-lm(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data=cwt12)

#Oct'14
cwt22<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10) & combined_sun_ws_tmy3$Year==2014),]
rownames(cwt22) <- 1:nrow(cwt22)
head(cwt22)
cwt22$predicted_irr<-predict(mmodel12, cwt22)
library(smooth)
MAPE(cwt22$Insolation, cwt22$predicted_irr)
mmodel22<-lm(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data=cwt22)

#Nov'14
cwt32<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11) & combined_sun_ws_tmy3$Year==2014),]
rownames(cwt32) <- 1:nrow(cwt32)
cwt32$predicted_irr<-predict(mmodel22, cwt32)
library(smooth)
MAPE(cwt32$Insolation, cwt32$predicted_irr)
mmodel32<-lm(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data=cwt32)

#Dec'14
cwt42<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12) & combined_sun_ws_tmy3$Year==2014),]
rownames(cwt42) <- 1:nrow(cwt42)
cwt42$predicted_irr<-predict(mmodel32, cwt42)
library(smooth)
MAPE(cwt42$Insolation, cwt42$predicted_irr)
mmodel42<-lm(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data=cwt42)

#Jan'15
cwt52<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt52) <- 1:nrow(cwt52)
cwt52$predicted_irr<-predict(mmodel42, cwt52)
MAPE(cwt52$Insolation, cwt52$predicted_irr)
mmodel52<-lm(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data=cwt52)

#Feb'15
cwt62<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt62) <- 1:nrow(cwt62)
cwt62$predicted_irr<-predict(mmodel52, cwt62)
MAPE(cwt62$Insolation, cwt62$predicted_irr)
mmodel62<-lm(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data=cwt62)

#Mar'15
cwt72<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2,3) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt72) <- 1:nrow(cwt72)
cwt72$predicted_irr<-predict(mmodel62, cwt72)
MAPE(cwt72$Insolation, cwt72$predicted_irr)
mmodel72<-lm(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data=cwt72)

#Apr'15
cwt82<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2,3,4) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt82) <- 1:nrow(cwt82)
cwt82$predicted_irr<-predict(mmodel72, cwt82)
MAPE(cwt82$Insolation, cwt82$predicted_irr)
mmodel82<-lm(Insolation~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data=cwt72)


#WS+TMY3 predictors
#Sep'14
combined_sun_ws_tmy3<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$module!=0),]
cwt1<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Date>=15 & combined_sun_ws_tmy3$Month==9 & combined_sun_ws_tmy3$Year==2014),]
head(cwt1)
mmodel1<-lm(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data=cwt1)

#Oct'14
cwt2<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10) & combined_sun_ws_tmy3$Year==2014),]
rownames(cwt2) <- 1:nrow(cwt2)
cwt2$predicted_module<-predict(mmodel1, cwt2)
library(smooth)
MAPE(cwt2$module, cwt2$predicted_module)
mmodel2<-lm(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data=cwt2)

#Nov'14
cwt3<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11) & combined_sun_ws_tmy3$Year==2014),]
rownames(cwt3) <- 1:nrow(cwt3)
cwt3$predicted_module<-predict(mmodel2, cwt3)
library(smooth)
MAPE(cwt3$module, cwt3$predicted_module)
mmodel3<-lm(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data=cwt3)

#Dec'14
cwt4<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12) & combined_sun_ws_tmy3$Year==2014),]
rownames(cwt4) <- 1:nrow(cwt4)
cwt4$predicted_module<-predict(mmodel3, cwt4)
library(smooth)
MAPE(cwt4$module, cwt4$predicted_module)
mmodel4<-lm(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data=cwt4)

#Jan'15
cwt5<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt5) <- 1:nrow(cwt5)
cwt5$predicted_module<-predict(mmodel4, cwt5)
MAPE(cwt5$module, cwt5$predicted_module)
mmodel5<-lm(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data=cwt5)

#Feb'15
cwt6<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt6) <- 1:nrow(cwt6)
cwt6$predicted_module<-predict(mmodel5, cwt6)
MAPE(cwt6$module, cwt6$predicted_module)
mmodel6<-lm(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data=cwt6)

#Mar'15
cwt7<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2,3) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt7) <- 1:nrow(cwt7)
cwt7$predicted_module<-predict(mmodel6, cwt7)
MAPE(cwt7$module, cwt7$predicted_module)
mmodel7<-lm(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data=cwt7)

#Apr'15
cwt8<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2,3,4) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt8) <- 1:nrow(cwt8)
cwt8$predicted_module<-predict(mmodel7, cwt8)
MAPE(cwt8$module, cwt8$predicted_module)
mmodel8<-lm(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo+ws_tavg+ws_ws+ws_prcp, data=cwt8)

#WS predictors
#Sep'14
cwt1<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Date>=15 & combined_sun_ws_tmy3$Month==9 & combined_sun_ws_tmy3$Year==2014),]
head(cwt1)
mmodel11<-lm(module~ws_tavg+ws_ws+ws_prcp, data=cwt11)

#Oct'14
cwt21<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10) & combined_sun_ws_tmy3$Year==2014),]
rownames(cwt21) <- 1:nrow(cwt21)
cwt21$predicted_module<-predict(mmodel11, cwt21)
library(smooth)
MAPE(cwt21$module, cwt21$predicted_module)
mmodel21<-lm(module~ws_tavg+ws_ws+ws_prcp, data=cwt21)

#Nov'14
cwt31<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11) & combined_sun_ws_tmy3$Year==2014),]
rownames(cwt31) <- 1:nrow(cwt31)
cwt31$predicted_module<-predict(mmodel21, cwt31)
library(smooth)
MAPE(cwt31$module, cwt31$predicted_module)
mmodel31<-lm(module~ws_tavg+ws_ws+ws_prcp, data=cwt31)

#Dec'14
cwt41<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12) & combined_sun_ws_tmy3$Year==2014),]
rownames(cwt41) <- 1:nrow(cwt41)
cwt41$predicted_module<-predict(mmodel31, cwt41)
library(smooth)
MAPE(cwt41$module, cwt41$predicted_module)
mmodel41<-lm(module~ws_tavg+ws_ws+ws_prcp, data=cwt41)

#Jan'15
cwt51<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt51) <- 1:nrow(cwt51)
cwt51$predicted_module<-predict(mmodel41, cwt51)
MAPE(cwt51$module, cwt51$predicted_module)
mmodel51<-lm(module~ws_tavg+ws_ws+ws_prcp, data=cwt51)

#Feb'15
cwt61<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt61) <- 1:nrow(cwt61)
cwt61$predicted_module<-predict(mmodel51, cwt61)
MAPE(cwt61$module, cwt61$predicted_module)
mmodel61<-lm(module~ws_tavg+ws_ws+ws_prcp, data=cwt61)

#Mar'15
cwt71<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2,3) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt7) <- 1:nrow(cwt71)
cwt71$predicted_module<-predict(mmodel61, cwt71)
MAPE(cwt71$module, cwt71$predicted_module)
mmodel71<-lm(module~ws_tavg+ws_ws+ws_prcp, data=cwt71)

#Apr'15
cwt81<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2,3,4) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt81) <- 1:nrow(cwt81)
cwt81$predicted_module<-predict(mmodel71, cwt81)
MAPE(cwt81$module, cwt81$predicted_module)
mmodel81<-lm(Insolation~ws_tavg+ws_ws+ws_prcp, data=cwt71)

#TMY3 predictors
#Sep'14
head(combined_sun_ws_tmy3)
combined_sun_ws_tmy3<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Insolation!=0),c(1:4,6:18)]
head(combined_sun_ws_tmy3)
cwt12<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Date>=15 & combined_sun_ws_tmy3$Month==9 & combined_sun_ws_tmy3$Year==2014),]
tail(cwt12)

mmodel12<-lm(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data=cwt12)

#Oct'14
cwt22<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10) & combined_sun_ws_tmy3$Year==2014),]
rownames(cwt22) <- 1:nrow(cwt22)
head(cwt22)
cwt22$predicted_module<-predict(mmodel12, cwt22)
library(smooth)
MAPE(cwt22$module, cwt22$predicted_module)
mmodel22<-lm(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data=cwt22)
summary(mmodel22)
qqnorm(residuals(mmodel22))
qqline(residuals(mmodel22))

#Nov'14
cwt32<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11) & combined_sun_ws_tmy3$Year==2014),]
rownames(cwt32) <- 1:nrow(cwt32)
cwt32$predicted_module<-predict(mmodel22, cwt32)
library(smooth)
tail(cwt32)
MAPE(cwt32$module, cwt32$predicted_module)
mmodel32<-lm(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data=cwt32)
qqnorm(residuals(mmodel22))
qqline(residuals(mmodel32))

#Dec'14
cwt42<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12) & combined_sun_ws_tmy3$Year==2014),]
rownames(cwt42) <- 1:nrow(cwt42)
cwt42$predicted_module<-predict(mmodel32, cwt42)
library(smooth)
MAPE(cwt42$module, cwt42$predicted_module)
mmodel42<-lm(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data=cwt42)
qqnorm(residuals(mmodel42))
qqline(residuals(mmodel42))

#Jan'15
cwt52<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt52) <- 1:nrow(cwt52)
cwt52$predicted_module<-predict(mmodel42, cwt52)
MAPE(cwt52$module, cwt52$predicted_module)
mmodel52<-lm(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data=cwt52)
qqnorm(residuals(mmodel52))
qqline(residuals(mmodel52))

#Feb'15
cwt62<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt62) <- 1:nrow(cwt62)
cwt62$predicted_module<-predict(mmodel52, cwt62)
MAPE(cwt62$module, cwt62$predicted_module)
mmodel62<-lm(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data=cwt62)
qqnorm(residuals(mmodel62))
qqline(residuals(mmodel62))

#Mar'15
cwt72<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2,3) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt72) <- 1:nrow(cwt72)
cwt72$predicted_module<-predict(mmodel62, cwt72)
MAPE(cwt72$module, cwt72$predicted_module)
mmodel72<-lm(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data=cwt72)
qqnorm(residuals(mmodel72))
qqline(residuals(mmodel72))

#Apr'15
cwt82<-combined_sun_ws_tmy3[which(combined_sun_ws_tmy3$Month %in% c(9,10,11,12,1,2,3,4) & combined_sun_ws_tmy3$Year %in% c(2014,2015)),]
rownames(cwt82) <- 1:nrow(cwt82)
cwt82$predicted_module<-predict(mmodel72, cwt82)
MAPE(cwt82$module, cwt82$predicted_module)
mmodel82<-lm(module~global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data=cwt72)
qqnorm(residuals(mmodel82))
qqline(residuals(mmodel82))



