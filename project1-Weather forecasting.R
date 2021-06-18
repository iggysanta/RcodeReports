# READ DAILY WEATHER DATA IN 2020
dat <- NULL
current.month <- 12
for (i in 1:current.month){
  i0 <- ifelse(i<10, paste("0", i, sep=""), i)
  mth <- paste("2020", i0, sep="")
  bom <- paste("http://www.bom.gov.au/climate/dwo/", mth,
               "/text/IDCJDW2801.", mth, ".csv", sep="")
  dat.i <- read.csv(bom, skip=6, check.names=FALSE,
                    na.strings = c("NA", "", " "))
  # USE month.name() TO GET FULL MONTH NAMES
  dat.i[, 1] <-toupper(month.abb[i])
  dat <- rbind(dat, dat.i)
}
dim(dat); head(dat); tail(dat)

#dat <- dat %>%
#  rename(new=old) %>%
#  head(n=5)

table(dat$Date, useNA = 'ifany')
table(dat$`Minimum temperature (°C)`, useNA = 'ifany')
table(dat$`Maximum temperature (°C)`, useNA = 'ifany')
table(dat$`Rainfall (mm)`, useNA = 'ifany')
table(dat$`Evaporation (mm)`, useNA = 'ifany')
table(dat$`Sunshine (hours)`, useNA = 'ifany')
table(dat$`Direction of maximum wind gust `, useNA = 'ifany')
table(dat$`Speed of maximum wind gust (km/h)`, useNA = 'ifany')
table(dat$`Time of maximum wind gust`, useNA = 'ifany')
table(dat$`9am Temperature (°C)`, useNA = 'ifany')
table(dat$`9am relative humidity (%)`, useNA = 'ifany')
table(dat$`9am cloud amount (oktas)`, useNA = 'ifany')
table(dat$`9am wind direction`, useNA = 'ifany')
table(dat$`9am cloud amount (oktas)`, useNA = 'ifany')
table(dat$`9am wind direction`, useNA = 'ifany')
table(dat$`9am wind speed (km/h)`, useNA = 'ifany')
table(dat$`9am MSL pressure (hPa)`, useNA = 'ifany')
table(dat$`3pm Temperature (°C)`, useNA = 'ifany')
table(dat$`3pm relative humidity (%)`, useNA = 'ifany')
table(dat$`3pm cloud amount (oktas)`, useNA = 'ifany')
table(dat$`3pm wind direction`, useNA = 'ifany')
table(dat$`3pm wind speed (km/h)`, useNA = 'ifany')
table(dat$`3pm MSL pressure (hPa)`, useNA = 'ifany')

dat <- dat[, -c(10)]
dat <- dat[, -c(6)]
dat <- dat[, -c(6)]

names(dat) <- c("Month", "Date", "MinTemp", "MaxTemp", "Rainfall",
                "WindGustDir", "WindGustSpeed",
                "Temp9am", "Humidity9am", "Cloud9am", "WindDir9am",
                "WindSpeed9am", "Pressure9am", "Temp3pm", "Humidity3pm",
                "Cloud3pm", "WindDir3pm", "WindSpeed3pm", "Pressure3pm")
dim(dat);
names(dat)

dat$WindSpeed9am <- as.numeric(dat$WindSpeed9am)
dat$WindSpeed3pm <- as.numeric(dat$WindSpeed3pm)


dat$WindSpeed9am[dat$WindSpeed9am == "Calm"] <- '0'
dat$WindSpeed3pm[dat$WindSpeed3pm == "Calm"] <- '0'

dat[is.na(dat)] = 0

rain <- ifelse(dat$Rainfall > 1, 1,0)
rain  
dat$RainToday <- ifelse(dat$Rainfall > 1, 1,0)
dat$RainTomorrow <- c(dat$RainToday[2:nrow(dat)], NA)
help(save)
head(dat[,3:7])

