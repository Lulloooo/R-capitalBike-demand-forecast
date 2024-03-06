
#----------------------------------------------------------------------------------------------------------------
#Name: Bike-sharing demand forecasting
#Author: Luca Albertini (Lullo)
#date: 06 March 2023
#data source: https://archive.ics.uci.edu/ml/datasets/bike+sharing+dataset
#----------------------------------------------------------------------------------------------------------------


### GET THE ENVIRONMENT READY
## set the working directory (with its path)
#setwd()
##install the needed packagaes
#install.packages()
## load the needed packages
library(ggplot2)
library(dplyr)
library(dbplyr)
library(timetk)
library(expss)
library(psych)
library(knitr)
library(zoo)
library(tseries)
library(forecast)
library(lubridate)
library(seastests)
library(trend)
library(sos)
library(astsa)
library(urca)
library(egcm)
library(pracma)
library(clordr)
library(TSA)
library(scales)
# clear workspace
rm(list = ls())
#find a function
#findFn("")

#---------------------------------------------- FUNCTIONS -----------------------------------------------------#
###### OUTLIERS
# count mild outliers
find_mild_out <- function(X) {
  iqr = IQR(X)
  lowerq = quantile(X)[2]
  upperq = quantile(X)[4]
  lowmildborder = lowerq - (iqr*1.5)
  upmildborder = upperq + (iqr*1.5)
  print(mild_outliers <- length(which( X > upmildborder | X < lowmildborder)))
}
# function to count severe outliers
find_severe_out <- function(X) {
  iqr = IQR(X)
  lowerq = quantile(X)[2]
  upperq = quantile(X)[4]
  lowsevborder = lowerq - (iqr*3)
  upsevborder = upperq + (iqr*3)
  print(severe_outliers <- length(which( X > upsevborder | X < lowsevborder)))
}
#function to replace outliers with avg
rem_out <- function(c){
  b <- boxplot(c, plot = FALSE)
  s1 <- c
  s1[which(c %in% b$out)] <- mean(c[which(! c %in% b$out)],na.rm=TRUE)
  return(s1)
}
#--------------------------------------------------------------------------------------------------------------#

############# LOAD & EXPLORE DATA
#Note. to follow the script, download the database as .csv call it "day" and place it in a folder in_data within the working directory
#data are aggregated on a daily basis
## load ddata
day = read.csv("in_data/day.csv", header =  TRUE, sep = ",")
## have a quick look to data
day %>% glimpse()
## display the first and last 3 obs of the df
head(day, 3)
tail(day, 3)
## rename some variables
names(day)[names(day) == "instant"] <- "index"
names(day)[names(day) == "dteday"] <- "date"
names(day)[names(day) == "yr"] <- "year"
names(day)[names(day) == "mnth"] <- "month"
names(day)[names(day) == "weathersit"] <- "weather"
names(day)[names(day) == "atemp"] <- "temp_feel"
names(day)[names(day) == "hum"] <- "humidity"
names(day)[names(day) == "cnt"] <- "trb"
#transform dates into date format
day$date <- as.Date(day$date)
## add lables
day <- day %>%
  apply_labels (season = "1winter;2spring;3summer;4fall",
                        year= "0=2011;1=2012",
                        workingday = "1=yes",
                        temp = "normalized temperature",
                        temp_feel = "perceived temperature",
                        casual = "count of casual users",
                        registered = "count of registered users",
                        trb = "count of total rental bikes")
###### Exploratory statistics
### summary statistics about the variables
daystat <- describe(day[,c("casual","registered","trb")], IQR = TRUE)
daystas <- data.frame(daystat)
kable(daystat, digits = 2)
### build a df with casual, registered and trb min and max
users <- data.frame(max(day$casual), max(day$registered), max(day$trb),
                    min(day$casual), min(day$registered), min(day$trb))
users
#reg rides/total rides * 100
day$regvstot <- (day$registered / day$trb)*100
day <- day %>%
  apply_labels (regvstot = "% of reg vs the total users")
#max-scaling reg
day$perreg <- day$registered / max(day$registered)
day <- day %>%
  apply_labels (perreg = "% respect the max registered")
#max-scaling trb
day$pertrb <- day$trb / max(day$trb)
day <- day %>%
  apply_labels (pertrb = "% respect the max total rides")
#max-scaling cas
day$percas <- day$casual / max(day$casual)
day <- day %>%
  apply_labels (percas = "% respect the max casuals")
#exploratory stastics for all the df
day %>% summary()
####advanced statistics
#compute the total and avg rides (trb) for each year
year <- subset(data.frame(aggregate(day$trb  ~ day$year, FUN = mean), aggregate(day$trb  ~ day$year, FUN = sum)),
              select = -day.year.1)
colnames(year) <- c("year", "avg", "total")
## compute the total and avg rides (trb) for each season
szn <- subset(data.frame(aggregate(day$trb  ~ day$season, FUN = mean), aggregate(day$trb  ~ day$season, FUN = sum)),
                  select = -day.season.1)
colnames(szn) <- c("season", "avg", "total")
## compute the total and avg rides (trb) for each month
mth <- subset(data.frame(aggregate(day$trb  ~ day$month, FUN = mean), aggregate(day$trb  ~ day$month, FUN = sum)),
                  select = -day.month.1)
colnames(mth) <- c("month", "avg", "total")
## compute the total and avg rides (trb) based on whether or not it is a working day
wdy <- subset(data.frame(aggregate(day$trb  ~ day$workingday, FUN = mean), aggregate(day$trb  ~ day$workingday, FUN = sum)),
              select = -day.workingday.1)
colnames(wdy) <- c("workingday", "avg", "total")
## compute the total and avg rides based on weather
wth <- subset(data.frame(aggregate(day$trb  ~ day$weather, FUN = mean), aggregate(day$trb  ~ day$weather, FUN = sum)),
              select = -day.weather.1)
colnames(wth) <- c("weather", "avg", "total")
#show the results
year
szn
mth
wdy
wth

##################### Plotting based on conditions
#trb x year
ggplot(day, aes(x=year, y=trb, fill = factor(year), color = factor(year))) +
  geom_boxplot(aes(group=year)) +
  geom_jitter(color = "grey", size =0.4, alpha=0.9) +
  scale_fill_manual(values = c("orange", "darkblue"), labels = c("2011","2012")) +
  scale_color_manual(values = c("darkblue", "orange"), labels = c("2011","2012")) +
  ggtitle("Users per year") +
  theme(panel.grid = element_line(color = "grey"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank())
#trb x season
ggplot(day, aes(x=season, y=trb, fill = factor(season))) +
  geom_boxplot(aes(group=season))+
  geom_jitter(color = "grey", size =0.4, alpha=0.9) +
  scale_fill_manual(values = c("lightblue", "darkgreen", "yellow", "orange"), labels = c("winter","spring", "summer","fall"))+
  ggtitle("Users per seasons") +
  theme(panel.grid = element_line(color = "grey"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank())
#trb x workingday
ggplot(day, aes(x=workingday, y=trb, fill = factor(workingday), color = factor(workingday))) +
  geom_boxplot(aes(group=workingday)) +
  geom_jitter(color = "grey", size =0.4, alpha=0.9) +
  scale_fill_manual(values = c("orange", "darkblue"), labels = c("notwork","workingday")) +
  scale_color_manual(values = c("darkblue", "orange"), labels = c("notwork","workingday")) +
  ggtitle("Users based on working day") +
  theme(panel.grid = element_line(color = "grey"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank())
#trb x weather
ggplot(day, aes(x=weather, y=trb, fill = factor(weather), color = factor(weather))) +
  geom_boxplot(aes(group=weather)) +
  geom_jitter(color = "grey", size =0.4, alpha=0.9) +
  scale_fill_manual(values = c("orange", "darkblue", "lightblue"), labels = c("clear","mist + cloudy", "snow")) +
  scale_color_manual(values = c("darkblue", "orange", "darkgreen"), labels = c("clear","mist + cloudy", "snow")) +
  ggtitle("Users based on weather") +
  theme(panel.grid = element_line(color = "grey"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank())
#scatterplot trb against temperature
ggplot(day, aes(x=temp, y=trb)) +
  geom_point(size = 1, color = "darkblue") +
  geom_smooth(method=lm, linetype = "dashed", color = "darkorange", fill = "orange") +
  ggtitle("Total users vs Normalized temperature") +
  xlab("Norm. Temperature") + ylab("Total Users") +
  theme(panel.grid = element_line(color = "lightgrey"),
        panel.background = element_blank())
#scatterplot trb against windspeed
ggplot(day, aes(x=windspeed, y=trb)) +
  geom_point(size = 1, color = "darkblue") +
  geom_smooth(method=lm, linetype = "dashed", color = "darkorange", fill = "orange") +
  ggtitle("Total users vs wind speed") +
  xlab("Wind Speed") + ylab("Total Users") +
  theme(panel.grid = element_line(color = "lightgrey"),
        panel.background = element_blank())
#scatterplot trb against humidity
ggplot(day, aes(x=humidity, y=trb)) +
  geom_point(size = 1, color = "darkblue") +
  geom_smooth(method=lm, linetype = "dashed", color = "darkorange", fill = "orange") +
  ggtitle("Total users vs humidity") +
  xlab("Humidity") + ylab("Total Users") +
  theme(panel.grid = element_line(color = "lightgrey"),
        panel.background = element_blank())

############# (interactive) Plotting single variables
#transform data into time series objects (with zoo pckg)
casts <- zoo(day$casual, seq(from = as.Date("2011-01-01"), to = as.Date("2012-12-31"), by = 1))
regts <- zoo(day$registered, seq(from = as.Date("2011-01-01"), to = as.Date("2012-12-31"), by = 1))
trbts <- zoo(day$trb, seq(from = as.Date("2011-01-01"), to = as.Date("2012-12-31"), by = 1))
##### environmental variables
#temperature vs temperature felt
colors <- c("temp" = "darkblue", "temp_feel" = "orange")
tempvstfeel_plot <- day %>% 
  ggplot(aes(x=date)) +
  geom_line(aes(y=temp, color = "temp")) +
  geom_line(aes(y=temp_feel, color = "temp_feel"), alpha = 0.6) +
  labs ( x = "date", y = "value", color = "Legend", title = "Temperature vs Feel Temperature") +
  scale_color_manual(values = colors) +
  theme(panel.grid = element_line(color = "grey"),
        panel.background = element_blank())
tempvstfeel_plot
#humidity plot
hum_plot <- day %>% 
  plot_time_series(.date_var = date, .value = humidity, .interactive=TRUE, 
                   .title = "Humidity", .y_lab = "humidity", .x_lab = "date",
                   .line_color= "darkblue", .smooth_color = "orange")
hum_plot
#windspeed plot
wind_plot <- day %>% 
  plot_time_series(.date_var = date, .value = windspeed, .interactive=TRUE, 
                   .title = "Wind speed", .y_lab = "windspeed", .x_lab = "date",
                   .line_color= "darkblue", .smooth_color = "orange")
wind_plot
##### bike rides related variables
#trb with smooth line
trb_plot <- day %>% 
  plot_time_series(.date_var = date, .value = trb, .interactive=TRUE, 
                   .title = "Total rides", .y_lab = "number of total rides", .x_lab = "date",
                   .line_color= "darkblue", .smooth_color = "orange")
trb_plot
#registered with smooth line
reg_plot <- day %>% 
  plot_time_series(.date_var = date, .value = registered, .interactive=TRUE, 
                   .title = "Registered rides", .y_lab = "number of registered rides", .x_lab = "date",
                   .line_color= "darkblue", .smooth_color = "orange")
reg_plot
#casuals with smooth line
cas_plot <- day %>% 
  plot_time_series(.date_var = date, .value = casual, .interactive=TRUE, 
                   .title = "Casual rides", .y_lab = "number of casual rides", .x_lab = "date",
                   .line_color= "darkblue", .smooth_color = "orange")
cas_plot
#registered (szn, day, Date)
regtot <- day %>% 
  group_by(year) %>% 
  plot_time_series(date, registered, .color_var = as.factor(season),
                   .x_lab = "Date", .y_lab = "Registered trips", .title = "Registred trips vs Date", .interactive = TRUE)
regtot
#casuals (szn, day, Date)
castot <- day %>% 
  group_by(year) %>% 
  plot_time_series(date, casual, .color_var = as.factor(season),
                   .x_lab = "Date", .y_lab = "Casual trips", .title = "Casual trips vs Date", .interactive = TRUE)
castot
#total rides (szn, day, Date)
trbtot <- day %>% 
  group_by(year) %>% 
  plot_time_series(date, trb, .color_var = as.factor(season),
                   .x_lab = "Date", .y_lab = "Total rides", .title = "total rides vs Date", .interactive = TRUE)
trbtot
##### computed variables analysis
#registered rides vs total rides
regvstot_plot <- day %>% 
  group_by(year) %>% 
  plot_time_series(date, regvstot, .color_var = as.factor(season),
                   .x_lab = "Date", .y_lab = "registered rides/total rides", .title = "registered rides against total rides", .interactive = TRUE)
#registered rides vs registered maximum ride
perreg_plot <- day %>% 
  group_by(year) %>% 
  plot_time_series(date, perreg, .color_var = as.factor(season),
                   .x_lab = "Date", .y_lab = "registered rides/max registered rides", .title = "registered rides against its maximum", .interactive = TRUE)
#casual rides vs casual maximum rides
percas_plot <- day %>% 
  group_by(year) %>% 
  plot_time_series(date, percas, .color_var = as.factor(season),
                   .x_lab = "Date", .y_lab = "casual rides/max casual rides", .title = "casual rides against its maximum", .interactive = TRUE)
#trb rides vs trb maximum ried
pertrb_plot <- day %>% 
  group_by(year) %>% 
  plot_time_series(date, pertrb, .color_var = as.factor(season),
                   .x_lab = "Date", .y_lab = "total rides/max total rides", .title = "total rides against its maximum", .interactive = TRUE)
##### szn analysis
szn_plot <- day %>% 
  group_by(year(date)) %>% 
  plot_seasonal_diagnostics(.date_var = date, .value = trb, 
                            .x_lab = "Date", .y_lab = "total rides", .title = "Seasonal Analysis",
                            .geom_color = "darkblue", .geom_outlier_color = "orange" , .interactive = TRUE )
szn_plot

############# Smoothing
#smooth time series in order to better identify trends
####### Anomalies analysis
#anomaly (i.e outliers) diagnostic plot for total rides
trbanom <- day %>% 
  plot_anomaly_diagnostics(.date_var = date, .value = trb,
                           .line_color = "darkblue", .anom_color = "orange", 
                           .x_lab = "Date", .y_lab = "Total rides", .title = "Total rides anomalies")
trbanom
#### smooth trb, casual and registered by identifying and replacing outliers
trbc <- tsclean(trbts, replace.missing = TRUE, iterate = 2, lambda = NULL)
regc <- tsclean(regts, replace.missing = TRUE, iterate = 2, lambda = NULL)
casc <- tsclean(casts, replace.missing = TRUE, iterate = 2, lambda = NULL)
smth <- data.frame(day$date,trbc,regc, casc)
smth$trbc <- zoo(smth$trbc, seq(from = as.Date("2011-01-01"), to = as.Date("2012-12-31"), by = 1))
smth$regc <- zoo(smth$regc, seq(from = as.Date("2011-01-01"), to = as.Date("2012-12-31"), by = 1))
smth$casc <- zoo(smth$casc, seq(from = as.Date("2011-01-01"), to = as.Date("2012-12-31"), by = 1))
#double check for anomalies
smth %>% 
  plot_anomaly_diagnostics(.date_var = day.date, .value = trbc,
                           .line_color = "darkblue", .anom_color = "orange",
                           .alpha = 0.05, .x_lab = "Date", .y_lab = "Total rides", .title = "Total rides anomalies")
#plot them all
par(mfrow = c(3,1))
plot(trbc, type = "l", col = "darkblue")
plot(regc, type = "l", col = "darkblue")
plot(casc, type = "l", col = "darkblue")
#transform trbc into a ts object
trbc_ts <- ts(trbc, start = c(2011), frequency = 365)

##### exponential smoothing
par(mfrow = c(1,1))
#alpha = base value -> higher more emphasis on more recent obs
#beta = trend value
#gamma = seasonal component
#trb with the first as the initial value in the ts
#if the coeff is closed to zero, it may means that forecast may be bases on seasonality in general
trbHw1 <- HoltWinters(trbc_ts, gamma = FALSE, l.start = 985)
trbHw1$fitted
trbHw1
trbHw2 <- HoltWinters(trbc_ts, beta = FALSE, gamma = FALSE, l.start = 985)
trbHw2$fitted
trbHw3 <- HoltWinters(trbc_ts, seasonal = "additive", l.start = 985)
trbHw3$fitted
#plot to see what it fits best
par(mar = c(5,4,4,8),
    xpd = TRUE) #this is fundamental to place the legend outside the box of the chart
plot(trbc_ts, col = alpha("black", alpha = 0.3))
lines(trbHw1$fitted[,1], lty=2, col = "orange")
lines(trbHw2$fitted[,1], lty=2, col = "darkblue")
lines(trbHw3$fitted[,1], lty=2, col = "darkgreen")
legend(x="topright", inset = c(-0.4,0), lty = c(4,6), col = c("orange", "darkblue", "darkgreen"), legend =c("Hw1", "Hw2", "Hw3"))
#lower alpha
trbalpha <- HoltWinters(trbc, alpha = 0.1, beta = FALSE, gamma = FALSE, l.start = 985)
trbalpha$fitted
par(mar = c(5,4,4,8),
    xpd = TRUE) 
plot(trbc, col = alpha("darkblue", alpha = 0.3))
lines(trbalpha$fitted[,1], lty=2, col = "orange")
legend(x="topright", inset = c(-0.4,0), lty = c(4,6), col = c("orange"), legend =c("Hw_a"))
trbalpha$SSE
#calculate the sum of the squared errors for the insample forecast errors
#i.e the erros for the time period covered by the original ts
trbHw1$SSE
trbHw2$SSE
trbalpha$SSE
trbHw3$SSE
#the SSE are quite high
#reg
regHw1 <- HoltWinters(regc, gamma = FALSE, l.start = 654)
regHw1$fitted
regHw2 <- HoltWinters(regc, beta = FALSE, gamma = FALSE, l.start = 654)
regHw2$fitted
#plot to see what it fits best
plot(regts)
lines(regHw1$fitted[,1], lty=2, col = "orange")
lines(regHw2$fitted[,1], lty=2, col = "darkblue")
#errors
regHw1$SSE
regHw2$SSE
#cas
casHw1 <- HoltWinters(casc, gamma = FALSE, l.start = 331)
casHw1$fitted
casHw2 <- HoltWinters(casc, beta = FALSE, gamma = FALSE, l.start = 331)
casHw2$fitted
#plot to see what it fits best
plot(casts)
lines(casHw1$fitted[,1], lty=2, col = "orange")
lines(casHw2$fitted[,1], lty=2, col = "darkblue")
legend(x="topright", inset = c(-0.4,0), lty = c(4,6), col = c("orange", "darkblue", "green"), legend =c("SMA90", "SMA30", "SMA07"))
##cas
#errors
casHw1$SSE
casHw2$SSE
##### moving averages
#simple moving averages
#to take away seasonal spam, make n = seasonal spam
##trb
#szn
trbsma90 <- TTR::SMA(trbc, n = 90)
#mth
trbsma30 <- TTR::SMA(trbc, n = 30)
#week
trbsma07 <- TTR::SMA(trbc, n = 7)
#plot them altogether 
par(mar = c(5,4,4,8),
    xpd = TRUE)#this is fundamental to place the legend outside the box of the chart
plot(trbc, xlab = "date", ylab = "trb", main = "SMA of total rides", col = alpha("black", alpha = 0.3))
lines(trbsma90, lty=2, col = "darkorange")
lines(trbsma30, lty=2, col = "darkblue")
lines(trbsma07, lty=2, col = "darkred")
legend(x="topright", inset = c(-0.4,0), lty = c(4,6), col = c("orange", "darkblue", "darkgreen"), legend =c("SMA90", "SMA30", "SMA07"))
##reg
#szn
regsma90 <- TTR::SMA(regc, n = 90)
#mth
regsma30 <- TTR::SMA(regc, n = 30)
#week
regsma07 <- TTR::SMA(regc, n = 7)
#plot them altogether
par(mar = c(5,4,4,8),
    xpd = TRUE) #this is fundamental to place the legend outside the box of the chart
plot(regc, xlab = "date", ylab = "registered trips", main = "SMA of registered rides")
lines(regsma90, lty=2, col = "orange")
lines(regsma30, lty=2, col = "darkblue")
lines(regsma07, lty=2, col = "darkred")
legend(x="topright", inset = c(-0.4,0), lty = c(4,6), col = c("orange", "darkblue", "darkred"), legend =c("SMA90", "SMA30", "SMA07"))
##cas
#szn
cassma90 <- TTR::SMA(casc, n = 90)
#mth
cassma30 <- TTR::SMA(casc, n = 30)
#week
cassma07 <- TTR::SMA(casc, n = 7)
#plot them altogether 
par(mar = c(5,4,4,8),
    xpd = TRUE) #this is fundamental to place the legend outside the box of the chart
plot(casc, xlab = "date", ylab = "registered trips", main = "SMA of registered rides" )
lines(cassma90, lty=2, col = "orange")
lines(cassma30, lty=2, col = "darkblue")
lines(cassma07, lty=2, col = "darkred")
legend(x="topright", inset = c(-0.4,0), lty = c(4,6), col = c("orange", "darkblue", "darkred"), legend =c("SMA90", "SMA30", "SMA07"))
####Exponential moving avgs
##trb
#szn
trbema90 <- TTR::EMA(trbc, n = 90)
#mth
trbema30 <- TTR::EMA(trbc, n = 30)
#week
trbema07 <- TTR::EMA(trbc, n = 7)
#plot them altogether 
par(mar = c(5,4,4,8),
    xpd = TRUE) #this is fundamental to place the legend outside the box of the chart
plot(trbc, xlab = "date", ylab = "trb", main = "EMA of total rides", col = alpha("black", alpha = 0.3) )
lines(trbema90, lty=2, col = "orange")
lines(trbema30, lty=2, col = "darkblue")
lines(trbema07, lty=2, col = "darkred")
legend(x="topright", inset = c(-0.4,0), lty = c(4,6), col = c("orange", "darkblue", "darkred"), legend =c("EMA90", "EMA30", "EMA07"))
##reg
#szn
regema90 <- TTR::EMA(regc, n = 90)
#mth
regema30 <- TTR::EMA(regc, n = 30)
#week
regema07 <- TTR::EMA(regc, n = 7)
#plot them altogether 
par(mar = c(5,4,4,8),
    xpd = TRUE) #this is fundamental to place the legend outside the box of the chart
plot(regc, xlab = "date", ylab = "registered trips", main = "SMA of registered rides" )
lines(regema90, lty=2, col = "orange")
lines(regema30, lty=2, col = "darkblue")
lines(regema07, lty=2, col = "darkred")
legend(x="topright", inset = c(-0.4,0), lty = c(4,6), col = c("orange", "darkblue", "darkred"), legend =c("EMA90", "EMA30", "EMA07"))
##cas
#szn
casema90 <- TTR::EMA(casc, n = 90)
#mth
casema30 <- TTR::EMA(casc, n = 30)
#week
casema07 <- TTR::EMA(casc, n = 7)
#plot them altogether 
par(mar = c(5,4,4,8),
    xpd = TRUE) #this is fundamental to place the legend outside the box of the chart
plot(casc, xlab = "date", ylab = "registered trips", main = "SMA of registered rides" )
lines(casema90, lty=2, col = "orange")
lines(casema30, lty=2, col = "darkblue")
lines(casema07, lty=2, col = "darkred")
legend(x="topright", inset = c(-0.4,0), lty = c(4,6), col = c("orange", "darkblue", "darkred"), legend =c("EMA90", "EMA30", "EMA07"))
#### Fourier smoothing
#trb
par(mfrow = c(2,1))
plot(trbc, type = "l", main = "trbts")
plot(Mod(fft(trbc)), type = "l", main = "FFT")
#reset the plotting area to 1:1
par(mfrow = c(1,1))

######################STATIONARITY
#Seasonal and trend analysis
#seasonality
isSeasonal(trbc, freq = 364)
isSeasonal(regc, freq = 364)
isSeasonal(casc, freq = 364)
#trnd test
mk.test(trbc)
mk.test(regc)
mk.test(casc)
#focus on trend
trend(trbc)
trend(regc)
trend(casc)
######## Autocorrelation
# clear the space
dev.off()
# set the plot space in a way to contain 2 charts
par(mfrow = c(2, 1))
#trb
acf(na.omit(trbc), lag.max = 731, plot = F)
acf(na.omit(trbc), lag.max = 731, main ="trb autocorrelation")
#trb pacf
pacf(trbc, lag.max = 731, main = "trb partial-autocorrelation")
#high autocorr; there could be a trend and a seasonality
#autocor
#reg
acf(na.omit(regc), lag.max = 731, plot = F)
acf(na.omit(regc), lag.max = 731, main ="reg autocorrelation")
#highly autocorr; there could be a trend
#cas
acf(na.omit(casc), lag.max = 731, plot = F)
acf(na.omit(casc), lag.max = 731, main ="cas autocorrelation")
#high autocorr, there could be a trend DEF there is seasonality

######## Stationarity tests
#trb
#ADF
summary(ur.df(trbc, 
              type = "trend", 
              lags = 180, 
              selectlags = "BIC"))
#unit root, no trend & no drift
#try other tests to confirm the results
#adf test, =/ command
adf.test(trbc)
#unit root is present
#pp test
pp.test(trbc, type = "Z(alpha)") #stationarity
pp.test(trbc, type = "Z(t_alpha") #stationarity
#kpss test
kpss.test(trbc, null = "Trend")#there is a unit root
#test for unit roots variance ratio (the rho is the variance, the p-value is as usual)
bvr.test(trbc)#there is a unit root, but variance is close to the case of no unit root
##reg
summary(ur.df(regc, 
              type = "trend", 
              lags = 180, 
              selectlags = "BIC"))
#unit root, no trend no drift
#try other tests to confirm the results
#adf test, =/ command
adf.test(regc)
#unit root
#pp test
pp.test(regc, type = "Z(alpha)")#stationarity
pp.test(regc, type = "Z(t_alpha")#stationarity
#kpss test
kpss.test(regc, null = "Trend") #there is a unit root
#test for unit roots variance ratio (the rho is the variance, the p-value is as usual)
bvr.test(regc) #there is unit root, close to the case of none
#cas
summary(ur.df(casc, 
              type = "trend", 
              lags = 180, 
              selectlags = "BIC"))
#unit root, no trend no drift
#adf test, =/ command
adf.test(casc)
#unit root
#pp test
pp.test(casc, type = "Z(alpha)")#stationarity
pp.test(casc, type = "Z(t_alpha")#stationarity
#kpss test
kpss.test(casc, null = "Trend") #unit root
#test for unit roots variance ratio (the rho is the variance, the p-value is as usual)
bvr.test(casc) #stationarity, really close to the no unit root situation
#decompose the data
freq <- 365
#trb
trbtsd <- ts(regc, frequency = freq)
trbd <- stl(trbtsd, "periodic")
plot(trbd$time.series[,2], ylab = "trb trend", xlab = "date")
plot(trbd$time.series[,1], ylab = "trb season", xlab = "date")
#check the residuals
checkresiduals(trbd$time.series[,3])
#reg
regtsd <- ts(regc, frequency = freq)
regd <- stl(regtsd, "periodic")
plot(regd$time.series[,2], ylab = "reg trend", xlab = "date")
plot(regd$time.series[,1], ylab = "reg season", xlab = "date")
#check the residuals
checkresiduals(regd$time.series[,3])
#cas
castsd <- ts(casc, frequency = freq)
casd <- stl(castsd, "periodic")
plot(casd$time.series[,2], ylab = "cas trend", xlab = "date")
plot(casd$time.series[,1], ylab = "cas season", xlab = "date")
#check the residuals
checkresiduals(casd$time.series[,3])
## difference in order to achieve stationarity
diftrb <- diff(trbc, lag=1)
difreg <- diff(regc, lag=1)
difcas <- diff(casc, lag=1)
plot(diftrb)
plot(difreg)
plot(difcas)
#data now seem all stationarity, try to test them
#diftrb
summary(ur.df(diftrb, 
              type = "trend", 
              lags = 180, 
              selectlags = "BIC"))
#no unit root
#adf test, =/ command
adf.test(diftrb)
#no unit root
#pp test
pp.test(diftrb, type = "Z(alpha)")#stationarity
pp.test(diftrb, type = "Z(t_alpha")#stationarity
#kpss test
kpss.test(diftrb, null = "Trend") #trend stat
#test for unit roots variance ratio (the rho is the variance, the p-value is as usual)
bvr.test(diftrb) #stationarity, close to the case of no unit root
#difreg
summary(ur.df(difreg, 
              type = "trend", 
              lags = 180, 
              selectlags = "BIC"))
#no unit root
#adf test, =/ command
adf.test(difreg)
#no unit root
#pp test
pp.test(difreg, type = "Z(alpha)")#stationarity
pp.test(difreg, type = "Z(t_alpha")#stationarity
#kpss test
kpss.test(difreg, null = "Trend") #trend stat
#test for unit roots variance ratio (the rho is the variance, the p-value is as usual)
bvr.test(difreg) #stationarity, close to the case of no unit root
#difcas
summary(ur.df(difcas, 
              type = "trend", 
              lags = 180, 
              selectlags = "BIC"))
#no unit root
#adf test, =/ command
adf.test(difcas)
#no unit root
#pp test
pp.test(difcas, type = "Z(alpha)")#stationarity
pp.test(difcas, type = "Z(t_alpha")#stationarity
#kpss test
kpss.test(difcas, null = "Trend") #trend stat
#test for unit roots variance ratio (the rho is the variance, the p-value is as usual)
bvr.test(difcas) #stationarity, close to the case of no unit root
#after differencing, all the variables became stationary

################## Forecasting and testing
#decompose the ts (as done before)
componentstrb <- decompose(trbc_ts)
plot(componentstrb)

##### HW exponential forecasting
trbHw3$fitted
#forecast
Hw3for <- forecast(trbHw3, h = 30)
#plot the forecasts
trbc_ts %>% 
  autoplot(col = "darkblue", xlab = "Date", ylab = "Number of total rides", 
           main = "Seasonal Holt Winter Testing") +
  autolayer(Hw3for, series = "Hw forecasts", col = "darkorange") +
  theme(panel.grid = element_line(color = "lightgrey"),
        panel.background = element_blank())

#### ARIMA
#the approach is to fit different models and see what does the best job in approx the ts
#split the data into training (tr, 700 obs) and test (tt, 30 obs)
tr <- trbc_ts[1:700]
tt <- trbc_ts[701:730]
tr <- ts(data = tr, start = c(2011), frequency = 365)
tt <- ts(data = tt, start = c(2012, 335), frequency = 365)
#check again acf and pacf to see for hints about p and q (d=1)
acf(trbc_ts[1:731], main = "Total rides autocorrelogram", lag = 50) #there is an AR components, high spikes point to q
pacf(trbc_ts[1:731], main = "Total rides partial-autocorrelogram", lag = 50) #high spikes point to p
#p could be up to 6, while q is much larger, since the series is highly autocorr
#use auto.arima to compute paramters
arima1 <- auto.arima(tr, d=1)
#this miss the seasonal effect, force it with D=1 in auto.arima
arima1s <- auto.arima(tr, d=1, D=1, trace = TRUE) 
#now let try to compute manually some other arima models
arima1sm <- Arima(tr, order = c(0,1,2), seasonal = list(order = c(0,1,0)))
arima2 <- Arima(tr, order = c(1,1,1), seasonal = list(order = c(0,1,0)))
arima3 <- Arima(tr, order = c(1,1,2), seasonal = list(order = c(0,1,0)))
arima4 <- Arima(tr, order = c(2,1,1), seasonal = list(order = c(0,1,0)))
arima5 <- Arima(tr, order = c(6,1,1), seasonal = list(order =c(0,1,0)))
arima6 <- Arima(tr, order = c(6,1,2), seasonal = list(order= c(0,1,0)))
#see the AIC for every model -> the AIC is better suited for model prediction, the BIC for model explanation
arima1$aic
arima1s$aic
arima1sm$aic
arima2$aic
arima3$aic
arima4$aic
arima5$aic #lower AIC
arima6$aic
#again, arima 5 appears to have slightly better values
#forecast using arima2 and arima5 (the best 2 models)
forar2 <- forecast(arima2, h=31)
forar5 <- forecast(arima5, h=31)
forar2
forar5
#plot the forecasts against the original series
#Arima2 
#all values
par(mar = c(5,4,4,8),
    xpd = TRUE) 
forar2 %>% 
  plot(col = alpha("darkblue", alpha = 0.70), shaded = TRUE, shadecols = alpha("darkblue", alpha = 0.30), fcol ="darkblue",  
       main = "Forecasting test series with SArima (1,1,1)(0,1,0)", xlab = "Date", ylab="total rides",
       bty = "l", xaxt="n") #this removes the box
lines(tt, main = "Test Data", colour = TRUE, col = "darkorange")
ticks <- axTicks(1) #find where are the x ticks and give them a name
axis(1, at = ticks, label = c(2011.01, 2011.06, 2012.01, 2012.06, 2013.01)) #rename the x ticks
#grid(nx= NA, ny = NULL, lty = 2, col = "lightgray", lwd = 2) #this adds the grid
legend(x="topright", inset = c(-0.4,0), lty = c(1,1), legend = c("Prediction", "Actual Data"), col = c("darkblue", "darkorange"))
#zoom in on the last 90 observations
par(mar = c(5,4,4,8),
    xpd = TRUE) #place the legend outside
forar2 %>% 
plot(col = alpha("darkblue", alpha = 0.70), shaded = TRUE, shadecols = alpha("darkblue", alpha = 0.30), fcol ="darkblue",  
     main = "Forecasting test series with SArima (1,1,1)(0,1,0)", xlab = "Last 90 days of 2012", ylab="total rides",
     include = 90, bty = "l",  xaxt = "n") #this removes the box
lines(tt, main = "Test Data", colour = TRUE, col = "darkorange")
legend(x="topright", inset = c(-0.4,0), lty = c(1,1), legend = c("Prediction", "Actual Data"), col = c("darkblue", "darkorange"))
#Arima 5
#all values
par(mar = c(5,4,4,8),
    xpd = TRUE) 
forar5 %>% 
  plot(col = alpha("darkblue", alpha = 0.70), shaded = TRUE, shadecols = alpha("darkblue", alpha = 0.30), fcol ="darkblue",  
       main = "Forecasting test series with SArima (6,1,1)(0,1,0)", xlab = "Date", ylab="total rides",
       bty = "l", xaxt="n") #this removes the box
lines(tt, main = "Test Data", colour = TRUE, col = "darkorange")
ticks <- axTicks(1) #find where are the x ticks and give them a name
axis(1, at = ticks, label = c(2011.01, 2011.06, 2012.01, 2012.06, 2013.01)) #rename the x ticks
#grid(nx= NA, ny = NULL, lty = 2, col = "lightgray", lwd = 2) #this adds the grid
legend(x="topright", inset = c(-0.4,0), lty = c(1,1), legend = c("Prediction", "Actual Data"), col = c("darkblue", "darkorange"))
#zoom in on the last 90 observations
par(mar = c(5,4,4,8),
    xpd = TRUE) #place the legend outside
forar5 %>% 
  plot(col = alpha("darkblue", alpha = 0.70), shaded = TRUE, shadecols = alpha("darkblue", alpha = 0.30), fcol ="darkblue",  
       main = "Forecasting test series with SArima (6,1,1)(0,1,0)", xlab = "Last 90 days of 2012", ylab="total rides",
       include = 90, bty = "l",  xaxt = "n") #this removes the box
lines(tt, main = "Test Data", colour = TRUE, col = "darkorange")
legend(x="topright", inset = c(-0.4,0), lty = c(1,1), legend = c("Prediction", "Actual Data"), col = c("darkblue", "darkorange"))
#the two models look similar, let's try to plot them altogether
par(mar = c(5,4,4,8),
    xpd = TRUE) #place the legend outside
forar5 %>% 
  plot(col = alpha("darkblue", alpha = 0.70), shaded = TRUE, shadecols = alpha("darkblue", alpha = 0.30), fcol ="darkblue",  
       main = "Sarima (6,1,1) vs Sarima(1,1,1)", xlab = "Last 90 days of 2012", ylab="total rides",
       include = 90, bty = "l",  xaxt = "n") #this removes the box
lines(tt, main = "Test Data", colour = TRUE, col = "darkorange")
lines(forar2$mean, main = "Arima2", col = "darkred")
legend(x="topright", inset = c(-0.4,0), lty = c(1,1), legend = c("SArima (6,1,1)", "Actual Data", "Sarima(1,1,1)"), col = c("darkblue", "darkorange", "darkred"))
#check for accuracy of two models
accuracy(arima5)
accuracy(arima2)
#again, arima 5 appears to have slightly better values
#try to fit the SARIMA(6,1,1)(0,1,0) to the whole data
Sarima <- Arima(trbc_ts, order = c(6,1,1), seasonal = list(order=c(0,1,0)))
#forecast for the next month
forSar <- Sarima %>% 
            forecast(h = 31)
#plot the results
par(mar = c(5,4,4,8),
    xpd = TRUE) 
forSar %>% 
  plot(col = alpha("darkblue", alpha = 0.60), shaded = TRUE, shadecols = alpha("darkblue", alpha = 0.30), fcol ="blue",  
       main = "Forecasting test series with SArima (6,1,1)(0,1,0)", xlab = "Date", ylab="total rides",
       bty = "l", xaxt="n") #this removes the box
ticks <- axTicks(1) #find where are the x ticks and give them a name
axis(1, at = ticks, label = c(2011.01, 2011.06, 2012.01, 2012.06, 2013.01)) #rename the x ticks
#grid(nx= NA, ny = NULL, lty = 2, col = "lightgray", lwd = 2) #this adds the grid
legend(x="topright", inset = c(-0.4,0), lty = c(1,1), legend = c("Historical data", "Prediction"), col = c(alpha("darkblue", alpha = 0.60), "blue"))
#zoom in
par(mar = c(5,4,4,8),
    xpd = TRUE) 
forSar %>% 
  plot(col = alpha("darkblue", alpha = 0.60), shaded = TRUE, shadecols = alpha("darkblue", alpha = 0.30), fcol ="blue",  
       main = "Forecasting test series with SArima (6,1,1)(0,1,0)", xlab = "Last 60 days of 2012 + 31 days forecast", ylab="total rides",
       bty = "l", xaxt="n", include = 91) #this removes the box
#grid(nx= NA, ny = NULL, lty = 2, col = "lightgray", lwd = 2) #this adds the grid
legend(x="topright", inset = c(-0.4,0), lty = c(1,1), legend = c("Historical data", "Prediction"), col = c(alpha("darkblue", alpha = 0.60), "blue"))
#check for accuracy
accuracy(Sarima)
#residuals analysis
checkresiduals(Sarima)

################## Explaining
#ACF tests
#temp
summary(ur.df(day$temp, 
                  type = "trend", 
                  lags = 180, 
                  selectlags = "BIC"))
#humidity
summary(ur.df(day$humidity, 
                  type = "trend", 
                  lags = 180, 
                  selectlags = "BIC"))
#windspeed
summary(ur.df(day$windspeed, 
                  type = "trend", 
                  lags = 180, 
                  selectlags = "BIC"))
#check for the correlation among variables
#create a database with the correlation of the variables
corday <- select(day, trb,temp, temp_feel,humidity, windspeed, casual, registered)
corMat <- cor(corday)
#show it
corMat
#plot the correlogram
library(corrplot)
corrplot(corMat, title = "Variable Correlation", type = "lower", method = "color", 
         outline = T, addCoef.col = "white", tl.col = "black")
#estimate an ARDL model
library(ARDL)
#transform objects into ts
temp <- ts(day$temp, start = c(2011), frequency = 365)
hum <- ts(day$humidity, start = c(2011), frequency = 365)
wind <- ts(day$windspeed, start = c(2011), frequency = 365)
#build the df for the ardl's variables
dfardl <- data.frame(trbc_ts, temp, hum, wind, day$workingday)
#rename a column
names(dfardl)[names(dfardl) == "day.workingday"] <- "workday"
#find the proper ARDL paramters
#why ARDL? becasue y is of order 1 and at least one of x is of order 1 (temp)
#BIC
library(dLagM)
ardlBIC <- dfardl %>% 
    ardlBound(trbc_ts ~ temp + hum + wind + workday, case = 4, p = NULL,
                       remove = NULL, autoOrder = FALSE, ic = "BIC" , max.p = 5, max.q = 5, 
                       ECM = TRUE, stability = TRUE)
#AIC
ardlAIC <- dfardl %>% 
  ardlBound(trbc_ts ~ temp + hum + wind + workday, case = 4, p = NULL,
            remove = NULL, autoOrder = FALSE, ic = "AIC" , max.p = 5, max.q = 5, 
            ECM = TRUE, stability = TRUE)
#fit the ardl with trend
ardl <- ardl(trbc_ts ~ temp + hum + wind + trend(trbc_ts)| workday, data=dfardl, order = c(5,4,2,4))
#show the paramters
summary(ardl)
#compute the fitted values
fv <- fitted.values(ardl)
fv <- ts(fv, start = c(2011), frequency = 365)
#plot the model estimation and the real values
library(scales)
par(mfrow=c(1,1))
par(mar = c(5,4,4,8),
    xpd = TRUE)
plot.ts(fv,type="l",col=alpha("darkblue", alpha=0.7), main ="Total Rides - ARDL vs actual values", 
        xlab = "Date", ylab = "total rides", axes = TRUE, bty="l", xaxt="n")
lines(trbc_ts,col= alpha("darkorange",alpha=0.5))
ticks3 <- axTicks(1)
axis(1, at=ticks3, label = c(2011.01, 2011.06, 2012.01, 2012.06, 2013.01))
legend(x="topright", inset = c(-0.4,0), lty = c(1,1), legend = c("ARDL", "Actual Data"), col = c(alpha("darkblue",alpha=0.7), alpha("darkorange",alpha=0.5)))
#!!!! problem with point 67 and 68 -> there may be outliers in the data
#Check for outliers in the independent variables
# hum outliers
find_mild_out(hum) #2
find_severe_out(hum) #0
#temp out
find_mild_out(temp) #0
find_severe_out(temp) #0
#wind out
find_mild_out(wind) #13
find_severe_out(wind) #0
#remove outliers
library(MASS)
#humidity
humc <- rem_out(hum)
#wind
windc <- rem_out(wind)
#plot noth humidity and wind
par(mfrow = c(1,1))
boxplot(humc, border = "darkblue", col = "darkorange")
title("Corrected humidity")
#no longer outliers
#plot it
boxplot(windc, border = "darkblue", col = "darkorange")
title("Corrected Wind speed")
#still has some outliers
#add those to ardl's df (dfardl)
dfardl$humc = humc
dfardl$windc = windc
#compute the BIC for ARDL again
ardlBIC2 <- dfardl %>% 
  ardlBound(trbc_ts ~ temp + humc + windc, case = 4, p = NULL,
            remove = NULL, autoOrder = FALSE, ic = "BIC" , max.p = 4, max.q = 4, 
            ECM = TRUE, stability = TRUE)
#lags: 5,1,1,4
#compute the ARDL
ardl2 <- ardl(trbc_ts ~ temp + humc + windc + trend(trbc_ts) | workday, data=dfardl, order = c(5,1,1,4))
#show parameters
summary(ardl2)
#compute the fitted values (and trasnfrom them into a ts object)
fv2 <- ardl2 %>% 
  fitted.values() %>% 
  ts(start = c(2011), frequency = 365)
#plot the model estimation and the real values
library(scales)
par(mfrow=c(1,1))
par(mar = c(5,4,4,8),
    xpd = TRUE)
fv2 %>% plot.ts(type="l",col=alpha("darkblue", alpha=0.7), main ="Total Rides - ARDL vs actual values", 
        xlab = "Date", ylab = "total rides", axes = TRUE, bty="l", xaxt="n")
lines(trbc_ts,col= alpha("darkorange",alpha=0.5))
ticks3 <- axTicks(1)
axis(1, at=ticks3, label = c(2011.01, 2011.06, 2012.01, 2012.06, 2013.01))
legend(x="topright", inset = c(-0.4,0), lty = c(1,1), legend = c("ARDL", "Actual Data"), col = c(alpha("darkblue",alpha=0.7), alpha("darkorange",alpha=0.5)))
#problem at point 67-68 resolved
ardl2 %>% accuracy()
###### residual analysis
#check for autocorrelation, heterskodestaicity and normality and act as a consequence
#save the residuals
resdt2 <- ardl2$residuals
resdt22 <- resdt2^2
#show them
resdt2 %>% summary()
# look for residuals autocorrelation
par(mfrow=c(1,1), xpd=FALSE)
acf(ardl2$residuals, type = "correlation", lag=700)
# test for it with Durban Watson Test
library(lmtest)
library(sandwich)
ardl2 %>% dwtest()
#p-value > 0.05 -> not autocorrelation among residuals
#START FROM HERE
#heterosked
ardl2 %>% bptest() #H0 is homoskedasticity
#heteroskedasticity
#re-estimate the model with robust se
ardlrob<- coeftest(ardl2, vcov = sandwich)
#check for normality
jarque.bera.test(ardl2$residuals) #H0 normality
#data is not normally distributed
#qq-plot 
library(car)
qqPlot(ardl2$residuals)
rstand <- rstandard(ardl2)
#cusum
library(qcc)
cusum1 <- cumsum(ardl2$residuals)
#cusum plot
cusum(ardl2$residuals)
################################################## END ############################################################