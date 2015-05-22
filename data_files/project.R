library(data.table)
library(dplyr)
library(ggplot2)
library(ifultools)
library(lubridate)
library(sqldf)
library(Hmisc)

setwd('/Users/Aluminum/Documents/MichaelaHull.GitHub.io/data_files')

data <- fread('sfpd_incidents_2014.csv')
data <- mutate(data, month = substr(Date, 1, 2))

districts <- unique(data$PdDistrict)
categories <- unique(data$Category)
cat_freq <- data.frame(table(data$Category))
cat_freq <- arrange(cat_freq, Freq)
# The category "trea" shows up exactly once and the description is "TRESPASSING OR LOITERING NEAR POSTED
# INDUSTRIAL PROPERTY", so I think it is safe to assume that it was supposed to be "trespass" 
# Changing row 130611:
data$Category[130611] = "TRESPASS"

# Sorting crime type by more general categories
data <- mutate(data, gen_cat = "none")

theft_names <- c("EXTORTION", "BAD CHECKS", "BRIBERY", "EMBEZZLEMENT", "FORGERY/COUNTERFEITING", "STOLEN PROPERTY",
           "FRAUD", "ROBBERY", "BURGLARY", "VEHICLE THEFT", "LARCENY/THEFT")
controlled_names <- c("LIQUOR LAWS", "DRUG/NARCOTIC", "DRUNKENNESS", "DRIVING UNDER THE INFLUENCE", "WEAPON LAWS")
assault_names <- c("SEX OFFENSES, NON FORCIBLE", "SUICIDE", "ARSON", "SEX OFFENSES, FORCIBLE", "ASSAULT")
kidnapping_names <- c("RUNAWAY", "KIDNAPPING", "MISSING PERSON")
conduct_names <- c("VANDALISM", "SUSPICIOUS OCC", "TRESPASS", "DISORDERLY CONDUCT", "LOITERING",
                   "GAMBLING", "PROSTITUTION")
other_names <- c("OTHER OFFENSES", "WARRANTS", "NON-CRIMINAL", "SECONDARY CODES", "FAMILY OFFENSES",
           "PORNOGRAPHY/OBSCENE MAT")

theft <- filter(data, Category %in% theft_names)
theft <- mutate(theft, gen_cat = "theft")
controlled <- filter(data, Category %in% controlled_names)
controlled <- mutate(controlled, gen_cat = "controlled")
assault <- filter(data, Category %in% assault_names)
assault <- mutate(assault, gen_cat = "assault")
kidnapping <- filter(data, Category %in% kidnapping_names)
kidnapping <- mutate(kidnapping, gen_cat = "kidnapping")
conduct <- filter(data, Category %in% conduct_names)
conduct <- mutate(conduct, gen_cat = "vandalism")
other <- filter(data, Category %in% other_names)
other <- mutate(other, gen_cat = "other")

data <- rbind(theft, controlled, assault, kidnapping, conduct, other)

ggplot() + geom_histogram(data = data, aes(x = PdDistrict))
ggplot() + geom_histogram(data = data, aes(x = gen_cat))
res_ftab <- data.frame(table(data$Resolution))
res_ftab <- arrange(res_ftab, -Freq)
notpros <- filter(data, Resolution %in% c("Complainant refuses to prosecute", "Not prosecuted",
                                         "District attorney refuses to prosecute"))
pros <- filter(data, Resolution %nin% c("Complainant refuses to prosecute", "Not prosecuted",
                                       "District attorney refuses to prosecute"))
notpros <- mutate(notpros, Resolution = "Not prosecuted")
data <- rbind(pros, notpros)

data <- mutate(data, Category = properCase(Category))
data <- mutate(data, Descript = properCase(Descript))
data <- mutate(data, DayOfWeek = properCase(DayOfWeek))
data <- mutate(data, PdDistrict = properCase(PdDistrict))
data <- mutate(data, Resolution = properCase(Resolution))
data <- mutate(data, gen_cat = properCase(gen_cat))
data <- mutate(data, Time = paste(Time, ':00', sep = ''))

data <- mutate(data, datetime = paste(Date, Time, sep = " "))
data <- mutate(data, datetime = mdy_hms(datetime))

oct <- filter(data, month == 10)
write.csv(oct, "sfpd_incidents_10_2014.csv", row.names = FALSE)

write.csv(data, "sfpd_incidents_2014.csv", row.names = FALSE)

# Manually creating a time series
ts_data <- mutate(data, theft = 0)
ts_data <- mutate(ts_data, vandalism = 0)
ts_data <- mutate(ts_data, kidnapping = 0)
ts_data <- mutate(ts_data, controlled = 0)
ts_data <- mutate(ts_data, assault = 0)
ts_data <- mutate(ts_data, other = 0)

theft <- filter(ts_data, gen_cat == "Theft")
theft$theft = 1
vandalism <- filter(ts_data, gen_cat == "Vandalism")
vandalism$vandalism = 1
kidnapping <- filter(ts_data, gen_cat == "Kidnapping")
kidnapping$kidnapping = 1
controlled <- filter(ts_data, gen_cat == "Controlled")
controlled$controlled = 1
assault <- filter(ts_data, gen_cat == "Assault")
assault$assault = 1
other <- filter(ts_data, gen_cat == "Other")
other$other = 1
ts_data <- rbind(theft, vandalism, kidnapping, controlled, assault, other)

ts <- sqldf("SELECT Date, SUM(theft), SUM(vandalism), SUM(kidnapping), SUM(controlled), SUM(assault), SUM(other)
            FROM ts_data GROUP BY Date")
names(ts) <- c("Date", "Theft", "Vandalism", "Kidnapping", "Controlled", "Assault", "Other")
write.csv(ts, "sfpd_incidents_2014_ts_crime.csv", row.names = FALSE)

ts_data <- mutate(data, southern = 0)
ts_data <- mutate(ts_data, mission = 0)
ts_data <- mutate(ts_data, northern = 0)
ts_data <- mutate(ts_data, central = 0)
ts_data <- mutate(ts_data, bayview = 0)
ts_data <- mutate(ts_data, ingleside = 0)
ts_data <- mutate(ts_data, tenderloin = 0)
ts_data <- mutate(ts_data, taraval = 0)
ts_data <- mutate(ts_data, park = 0)
ts_data <- mutate(ts_data, richmond = 0)

southern <- filter(ts_data, PdDistrict == "Southern")
southern$southern = 1
mission <- filter(ts_data, PdDistrict == "Mission")
mission$mission = 1
northern <- filter(ts_data, PdDistrict == "Northern")
northern$northern = 1
central <- filter(ts_data, PdDistrict == "Central")
central$central = 1
bayview <- filter(ts_data, PdDistrict == "Bayview")
bayview$bayview = 1
ingleside <- filter(ts_data, PdDistrict == "Ingleside")
ingleside$ingleside = 1
tenderloin <- filter(ts_data, PdDistrict == "Tenderloin")
tenderloin$tenderloin = 1
taraval <- filter(ts_data, PdDistrict == "Taraval")
taraval$taraval = 1
park <- filter(ts_data, PdDistrict == "Park")
park$park = 1
richmond <- filter(ts_data, PdDistrict == "Richmond")
richmond$richmond = 1

ts_data <- rbind(southern, mission, northern, central, bayview, ingleside, tenderloin, taraval, park, richmond)

ts <- sqldf('SELECT Date, SUM(southern), SUM(mission), SUM(northern), SUM(central), SUM(bayview), SUM(ingleside),
            SUM(tenderloin), SUM(taraval), SUM(park), SUM(richmond) FROM ts_data GROUP BY Date')

names(ts) <- c('Date', 'Southern', 'Mission', 'Northern', 'Central', 'Bayview', 'Ingleside',
               'Tenderloin', 'Taraval', 'Park', 'Richmond')

write.csv(ts, 'sfpd_incidents_2014_ts_neighborhoods.csv', row.names = FALSE)

data <- mutate(data, hour = substr(Time, 1, 2))
data$hour <- as.numeric(as.character(data$hour))

timeofday_ts <- sqldf('SELECT hour, SUM(Theft), SUM(Other), SUM(Vandalism), SUM(Kidnapping), SUM(Assault),
                      SUM(Controlled) FROM ts_data GROUP BY hour')
names(timeofday_ts) <- c('hour', 'Theft', 'Other', 'Vandalism', 'Kidnapping', 'Assault', 'Controlled')
hour24 <- timeofday_ts[1, ]
hour24$hour[1] <- '24'
timeofday_ts <- rbind(timeofday_ts, hour24)

ggplot() + geom_point(data = timeofday_ts, aes(x = as.numeric(hour), y = Theft), col="yellow") +
  geom_point(data = timeofday_ts, aes(x = as.numeric(hour), y = Other)) +
  geom_point(data = timeofday_ts, aes(x = as.numeric(hour), y = Controlled), col="purple") +
  geom_point(data = timeofday_ts, aes(x = as.numeric(hour), y = Kidnapping), col="blue") +
  geom_point(data = timeofday_ts, aes(x = as.numeric(hour), y = Assault), col="red") +
  geom_point(data = timeofday_ts, aes(x = as.numeric(hour), y = Vandalism), col="green")
write.csv(timeofday_ts, 'sfpd_incidents_2014_time_ts.csv', row.names = FALSE)

dayofweek_ts <- sqldf('SELECT DayOfWeek, SUM(Theft), SUM(Other), SUM(Vandalism), SUM(Kidnapping), SUM(Assault),
                      SUM(Controlled) FROM ts_data GROUP BY DayOfWeek')
names(dayofweek_ts) <- c('DayOfWeek', 'Theft', 'Other', 'Vandalism', 'Kidnapping', 'Assault', 'Controlled')
dayofweek_ts <- mutate(dayofweek_ts, DayNum = 0)
eighthday <- dayofweek_ts[2, ]
eighthday$DayNum = 7
dayofweek_ts <- rbind(dayofweek_ts, eighthday)
dayofweek_ts$DayNum[6] = 1
dayofweek_ts$DayNum[7] = 2
dayofweek_ts$DayNum[5] = 3
dayofweek_ts$DayNum[1] = 4
dayofweek_ts$DayNum[3] = 5
dayofweek_ts$DayNum[4] = 6
dayofweek_ts <- arrange(dayofweek_ts, DayNum)
dayofweek_ts$DayOfWeek[8] = "Eighthday"

ggplot() + geom_point(data = dayofweek_ts, aes(y = Theft, x = DayNum), col="yellow") +
  geom_point(data = dayofweek_ts, aes(y = Other, x = DayNum)) +
  geom_point(data = dayofweek_ts, aes(y = Assault, x = DayNum), col="red") +
  geom_point(data = dayofweek_ts, aes(y = Vandalism, x = DayNum), col="green") +
  geom_point(data = dayofweek_ts, aes(y = Controlled, x = DayNum), col="blue") +
  geom_point(data = dayofweek_ts, aes(y = Kidnapping, x = DayNum), col="purple")
write.csv(dayofweek_ts, 'sfpd_incidents_2014_day_ts.csv')
