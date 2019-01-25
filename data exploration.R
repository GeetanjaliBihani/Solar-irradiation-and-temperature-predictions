enphase<-read.csv("/Users/geetanjalibihani/Dropbox/Solar/data/enphase.csv", header = TRUE)
comb_weather<-read.csv("/Users/geetanjalibihani/Dropbox/Solar/data/satellite_weather.csv", header = TRUE)
purdue_total<-read.csv("/Users/geetanjalibihani/Dropbox/Solar/data/Purdue_total.csv", header = TRUE)
sunny_total<-read.csv("/Users/geetanjalibihani/Dropbox/Solar/data/Sunny_total.csv", header = TRUE)


#SUNNY DATA EXPLORE
head(sunny_total)
str(sunny_total)
summary(sunny_total)
sunny_total[which(sunny_total$module==-273.17), ] ##malfunction of weather station?
boxplot(sunny_total$module)
boxplot(Insolation ~ Year, data=sunny_total)
a<-as.data.frame((table(sunny_total$Insolation)))
head(a[order(-a$Freq),])

#after removing nas
summary(sunny_total)
sunny_total_n<-sunny_total[complete.cases(sunny_total),]
plot(sunny_total_n)
boxplot(Insolation ~ Year, data=sunny_total_n)
b<-as.data.frame((table(sunny_total_n$Insolation)))
head(b[order(-b$Freq),])
sunny_total_n<-sunny_total_n[-which(sunny_total_n$module==-273.17), ]
head(sunny_total_n)
sunny_total_n$power_kw<-(sunny_total_n$a112+sunny_total_n$a412)/2
boxplot(power_kw ~ Year, data=sunny_total_n)
sunnymod2<-lm(power_kw~Insolation+module+ambient+windspeed, sunny_total_n)

# Assessing Outliers
library(car)
outlierTest(sunnymod2) # Bonferonni p-value for most extreme obs
qqPlot(sunnymod2, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(sunnymod2) # leverage plots
cooksd<-cooks.distance(sunnymod2)
sunnymod2<-lm(power_kw~Insolation+module+windspeed, sunny_total_n)

# Improving model by removing correlated dependant variables
rownames(sunny_total_n) <- 1:nrow(sunny_total_n)
sunnymod3<-lm(power_kw~Insolation+module+windspeed, sunny_total_n)
library(car)
outlierTest(sunnymod3) # Bonferonni p-value for most extreme obs
qqPlot(sunnymod3, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(sunnymod3) # leverage plots
lev = hat(model.matrix(sunnymod3))
sunny_data_n[lev >0.2,]
# Influence Plot 
influencePlot(sunnymod3, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
cutoff <- 4/((nrow(sunny_total_n)-length(sunnymod3$coefficients)-2)) 
plot(sunnymod3, which=4, cook.levels=cutoff)
sunny_total_n<-sunny_total_n[-42159,]
sunnymod4<-lm(power_kw~Insolation+module+windspeed, sunny_total_n)
summary(sunnymod4)

head(sunny_total_n)


#doing analysis on cleaned sunny data +tmy3 DATA
library(sqldf)
sunny_total_n2<-sqldf("select Date, Month, Year, Time,
case when Time='5:15 AM' then 20
                     when Time='5:30 AM' then 20
                     when Time='5:45 AM' then 20
                     when Time='6:00 AM' then 19
                     when Time='6:15 AM' then 19
                     when Time='6:30 AM' then 19
                     when Time='6:45 AM' then 19
                     when Time='7:00 AM' then 18
                     when Time='7:15 AM' then 18
                     when Time='7:30 AM' then 18
                     when Time='7:45 AM' then 18
                     when Time='8:00 AM' then 17
                     when Time='8:15 AM' then 17
                     when Time='8:30 AM' then 17
                     when Time='8:45 AM' then 17
                     when Time='9:00 AM' then 16
                     when Time='9:15 AM' then 16
                     when Time='9:30 AM' then 16
                     when Time='9:45 AM' then 16
                     when Time='10:00 AM' then 15
                     when Time='10:15 AM' then 15
                     when Time='10:30 AM' then 15
                     when Time='10:45 AM' then 15
                     when Time='11:00 AM' then 14
                     when Time='11:15 AM' then 14
                     when Time='11:30 AM' then 14
                     when Time='11:45 AM' then 14
                     when Time='12:00 PM' then 13
                     when Time='12:15 PM' then 13
                     when Time='12:30 PM' then 13
                     when Time='12:45 PM' then 13
                     when Time='1:00 PM' then 12
                     when Time='1:15 PM' then 12
                     when Time='1:30 PM' then 12
                     when Time='1:45 PM' then 12
                     when Time='2:00 PM' then 11
                     when Time='2:15 PM' then 11
                     when Time='2:30 PM' then 11
                     when Time='2:45 PM' then 11
                     when Time='3:00 PM' then 10
                     when Time='3:15 PM' then 10
                     when Time='3:30 PM' then 10
                     when Time='3:45 PM' then 10
                     when Time='4:00 PM' then 9
                     when Time='4:15 PM' then 9
                     when Time='4:30 PM' then 9
                     when Time='4:45 PM' then 9
                     when Time='5:00 PM' then 8
                     when Time='5:15 PM' then 8
                     when Time='5:30 PM' then 8
                     when Time='5:45 PM' then 8
                     when Time='6:00 PM' then 7
                     when Time='6:15 PM' then 7
                     when Time='6:30 PM' then 7
                     when Time='6:45 PM' then 7
                     when Time='7:00 PM' then 6
                     when Time='7:15 PM' then 6
                     when Time='7:30 PM' then 6
                     when Time='7:45 PM' then 6
                     when Time='8:00 PM' then 5
                     when Time='8:15 PM' then 5
                     when Time='8:30 PM' then 5
                     when Time='8:45 PM' then 5
                     when Time='9:00 PM' then 4
                     when Time='9:15 PM' then 4
                     else '0' end as t_flag, Insolation, a112, a412, module, ambient, windspeed, power_kw  from sunny_total_n")
head(sunny_total_n2)
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
                else '0' end as t_flag, avg(global_hirr) global_hirr, 
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
compare_tmy3_sunny_n2<-sqldf("select distinct a.*, b.global_hirr, 
b.diff_hirr,
                          b.total_skycover,
                          b.dry_bulb_temp,
                          b.dew_point_temp,
                          b.rel_humidity,
                          b.station_pressure,
                          b.tmy3_windspeed,
                          b.h_visibility,
                          b.aod,
                          b.albedo
                          from sunny_total_n2 a
                          left join 
                          TMY3_agg b
                          on a.Date=b.day and a.Month=b.month and a.t_flag=b.t_flag")
head(compare_tmy3_sunny_n2)
compare_tmy3_sunny_n3<-compare_tmy3_sunny_n2[complete.cases(compare_tmy3_sunny_n2),c(6:23)]
compare_tmy3_sunny_n4<-compare_tmy3_sunny_n3[compare_tmy3_sunny_1$power_kw>0,]
com.mod1<-lm(power_kw~Insolation+module+ambient+windspeed+global_hirr+diff_hirr+total_skycover+dry_bulb_temp+dew_point_temp+rel_humidity+station_pressure+tmy3_windspeed+h_visibility+aod+albedo, data=compare_tmy3_sunny_n4)
summary(com.mod1)
anova(com.mod1)
plot(compare_tmy3_sunny_n4$power_kw)
#install.packages('smooth')
library(smooth)
SMAPE(compare_tmy3_sunny_n4$power_kw, com.mod1$fitted.values)
compare_tmy3_sunny_n4$fitted<-com.mod1$fitted.values
head(compare_tmy3_sunny_n4)
