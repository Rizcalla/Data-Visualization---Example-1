library(tidyverse)
library(lubridate)
library(gridExtra)
library(gganimate)
library(zoo)

dat <- read.csv("meteoUSB.csv")

dat$dateTime <- as_datetime(dat$dateTime)

---------------------------------------------------------------------------------------
#Explorando los datos

sum(is.na(dat$rainRate)) #Ok
sum(dat$rainRate > 0) #2042 non-zero observation

sum(is.na(dat$rain)) #Ok
sum(dat$rain > 0) #2486 non-zero observation

#rain and rainRate has only 2486 and 2042  non-zero observation, respectively, from a collection of 129102, so i will drop them under the assumtion that the instrument may be not working as intended.

sum(is.na(dat$windSpeed)) #2841 missing values
sum(!is.na(dat$windSpeed) & dat$windSpeed > 0) #74464 non-zero observations

sum(is.na(dat$windDir)) #54638 missing values
sum(!is.na(dat$windDir) & dat$windDir > 0) #72327 non-zero observations

sum(is.na(dat$windGust)) #Ok
sum(!is.na(dat$windGust) & dat$windGust > 0) #97135 non-zero observations

sum(is.na(dat$windGustDir)) #31967 missing values
sum(!is.na(dat$windGustDir) & dat$windGustDir > 0) #94096 non-zero observations

#Almost half of the windDir observations are NA values. It may be reasonable to drop it. But what if we try to predict the direction of the missing values by the given values of wind and windGustDir?

#windGust and windGustDir seem to be almost correct, but windGustDir has 31967 NA observations. We should decide whether to drop windGustDir or give NA values to it with a fixed direction -maybe related to the last observation or windDir-. We could try to predic windGustDir like with the winDir variable as some of the values seem to be present for one variable when some aren't for the other.

sum(is.na(dat$dateTime)) #Ok

sum(is.na(dat$barometer))#Ok
sum(!is.na(dat$barometer) & dat$barometer > 0)#Ok

sum(is.na(dat$radiation)) #2840 missing values
sum(!is.na(dat$radiation) & dat$radiation > 0)
#Radiation measures are probably fine, with almost half being non-zero. The other half may have been
#measured overnight

sum(is.na(dat$outTemp)) #2840 missing values
sum(!is.na(dat$outTemp) & dat$outTemp > 0) #126262 non-zero values

sum(is.na(dat$outHumidity))#2840 missing values
sum(!is.na(dat$outHumidity) & dat$outHumidity > 0) #126262 non-zero values

sum(is.na(dat$outTemp) & is.na(dat$radiation) & is.na(dat$outHumidity) & is.na(dat$windSpeed)) #the 2840 NA for this variables are related. Maybe the four measures are related to the same instrument.

#Let's take a look
Nas <- dat %>% filter(is.na(dat$outTemp) & is.na(dat$radiation) & is.na(dat$outHumidity) & is.na(dat$windSpeed))

#The 2840 missing values are from the dates between 2017-10-04 19:00:00 to 2017-10-25 13:30:00
NAdates <- interval(ymd_hms("2017-10-04 19:00:00"),ymd_hms("2017-10-25 13:30:00"))
time_length(NAdates, unit = "second")/600
sum(Nas$interval)/10
#It seems that 151 intervals are also missing from the registers (probably were not registered at all)
rm(Nas)
rm(NAdates)

#It was also noted that, for the period between 2016-02-13 03:30:00 and 2016-02-28 17:10:00, there are no registered observation at all.

-------------------------------------------------------------------------------


#Vamos a extraer los datos de máxima, mínima y media temperatura diarias y la radiación máxima

temp <- dat %>% select(dateTime,outTemp,outHumidity,radiation) %>% mutate(y = year(dateTime), day = yday(dateTime))

y <- NA
day <- NA
maxradiation <- NA
maxoutTemp <- NA
minoutTemp <- NA
meanoutTemp <- NA

k=0
for (i in 2015:2018){
  z <- subset(temp, y == i)
  a <- min(z$day)
  b <- max(z$day)
  for(j in a:b){
    k = k+1
    zd <- subset(z, day == j)
    y[k] <- i
    day[k] <- j
    maxradiation[k] <- max(zd$radiation)
    maxoutTemp[k] <- max(zd$outTemp)
    minoutTemp[k] <- min(zd$outTemp)
    meanoutTemp[k] <- mean(zd$outTemp)
    }
  }
#Hacemos un data.frame con los datos extraídos
tempd <- data.frame(y,day,radiation,maxoutTemp,minoutTemp,meanoutTemp)

#Completamos los valores de los días faltantes con NA
for (i in 197:210) {
  for (j in 3:6) {
    tempd[i,j]  <- NA
  }
}


#Por si hace falta

#Consiguiendo registros faltantes
y16 <- reduced %>% filter(year == 2016)
y17 <- reduced %>% filter(year == 2017)

`%notin%` <- Negate(`%in%`)

t17 <- seq(as.POSIXct("2017-01-01 00:00:00"),as.POSIXct("2017-12-31 23:50:00"), 600)
missing17 <- t17[t17 %notin%  y17$dateTime]
missing17

t16 <- seq(as.POSIXct("2016-01-01 00:00:00"),as.POSIXct("2016-12-31 23:50:00"), 600)
missing16 <- t16[t16 %notin%  y16$dateTime]
missing16

#Sin optimizar
#Llenando los registros faltantes del 2016 con NA

reduced <- dat %>% mutate(year = year(dateTime), month = month(dateTime)) %>% filter(year %in% 2016:2017) 

d <- seq(as.POSIXct("2016-02-12 23:10:00"),as.POSIXct("2016-02-28 12:30:00"), 600)
l <- length(d)
dummy <- reduced[1,]

for ( i in 1:l) {
dummy[i,1] <- d[i]
dummy[i,2] <- 10
for (j in 3:14){
  dummy[i,j] <- NA
}
}

reduced <- bind_rows(reduced,dummy)
reduced <- reduced %>% arrange(dateTime)

rm(d)
rm(l)
rm(i)
rm(j)
rm(dummy)



