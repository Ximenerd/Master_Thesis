library(tseries)
library(ggplot2)
library(urca)
library(mFilter)



#First, we keep the data only from 2011 onwards:


data <-APP_redemptions_history[(APP_redemptions_history$fecha >= "2011-01-01"),] 

data[is.na(data)]<-0

#for GDP:

data_1 <-gdp_zone_euro_final[(gdp_zone_euro_final$fecha >= "2011-01-01"),] 



# Some graphs to have an idea of the behavior of the series:

ggplot(data,aes(fecha, policy_rate)) + geom_line(color = "#00AFBB", size = 2) + xlab("Date") + ylab("ECB policy rate")
ggplot(data,aes(fecha, yield)) + geom_line(color = "#E7B800", size = 2) + xlab("Date") + ylab("10-year government bond yield")
ggplot(data,aes(fecha, app)) + geom_line(color = "black", size = 2) + xlab("Date") + ylab("APP")
ggplot(data,aes(fecha, eonia)) + geom_line(color = "purple", size = 2) + xlab("Date") + ylab("Eonia")
ggplot(data,aes(fecha, pao)) + geom_line(color = "purple", size = 2) + xlab("Date") + ylab("PAO")
ggplot(data,aes(fecha, pao_cc)) + geom_line(color = "purple", size = 2) + xlab("Date") + ylab("PAO_CC")
ggplot(data_1,aes(fecha, gdp_ln)) + geom_line(color = "#00AFBB", size = 2) + xlab("Date") + ylab("GDP Euro Area")

# Unit-root tests:

#Policy_rate: 

adf.test(data$policy_rate, k = 1) # There is an unit root (null hyphotesis not rejected = No stationarity)
kpss.test(data$policy_rate, "Level") # We reject the null of stationarity

#yield:

adf.test(data$yield, k = 1) # There is an unit root (null hyphotesis not rejected = No stationarity)
kpss.test(data$yield, "Level") # We reject the null of stationarity


#app:


app14<- subset(data$app, subset = data$fecha >= "2014-11-01")

ur.df(app14, type = "trend", lags = 1, selectlags = "BIC")
adf.test(app14, k = 1) # There is an unit root (null hyphotesis not rejected = No stationarity)
kpss.test(app14, "Level") # We reject the null of stationarity

#eonia:

adf.test(data$eonia, k = 1) # There is an unit root (null hyphotesis not rejected = No stationarity)
kpss.test(data$eonia, "Level") # We reject the null of stationarity


#gdp: 

adf.test(data_1$gdp_ln, k = 1) # There is an unit root (null hyphotesis not rejected = No stationarity)
kpss.test(data_1$gdp_ln, "Level") # We reject the null of stationarity

#Filtering the series with an ideal band-pass filter as in Christiano-Fitzgerald

policy_rate_filtered <- cffilter(data$policy_rate, root = TRUE)
yield_filtered <- cffilter(data$yield, root = TRUE)
app_filtered <- cffilter(data$app, root = TRUE)
eonia_filtered <- cffilter(data$eonia, root = TRUE)
gdp_filtered <- cffilter(data_1$gdp_ln, root = TRUE)


policy_cycle <-policy_rate_filtered$cycle

yield_cycle <- yield_filtered$cycle

app_cycle <- app_filtered$cycle

eonia_cycle <- eonia_filtered$cycle

gdp_cycle <- gdp_filtered$cycle


data_filtered <- data.frame(policy_cycle, yield_cycle, app_cycle, eonia_cycle, data$fecha)




#plots:

ggplot(data_filtered,aes(data.fecha, policy_cycle)) + geom_line(color = "#00AFBB", size = 2) + xlab("Date") + ylab("Policy rate filtered")

ggplot(data_filtered,aes(data.fecha, yield_cycle)) + geom_line(color = "#00AFBB", size = 2) + xlab("Date") + ylab("Yield filtered")

ggplot(data_filtered,aes(data.fecha, app_cycle)) + geom_line(color = "#00AFBB", size = 2) + xlab("Date") + ylab("APP filtered")

ggplot(data_filtered,aes(data.fecha, eonia_cycle)) + geom_line(color = "#00AFBB", size = 2) + xlab("Date") + ylab("eonia filtered")


# Again, run the UR tests on the filtered series:


#Policy_rate: 

adf.test(policy_cycle, k = 1) # There is no unit root (null hyphotesis rejected )
kpss.test(policy_cycle, "Level") # We aren't able the reject the null of stationarity

#yield:

adf.test(yield_cycle, k = 1) # There is no unit root (null hyphotesis rejected )
kpss.test(yield_cycle, "Level") # We aren't able the reject the null of stationarity


#app:


adf.test(app_cycle, k = 1) # TThere is no unit root (null hyphotesis rejected )
kpss.test(app_cycle, "Level") # We aren't able the reject the null of stationarity

#eonia:

adf.test(eonia_cycle, k = 1) # TThere is no unit root (null hyphotesis rejected )
kpss.test(eonia_cycle, "Level") #  We aren't able the reject the null of stationarity

#gdp: 

adf.test(data_1$gdp_ln, k = 1) # TThere is no unit root (null hyphotesis rejected )
kpss.test(data_1$gdp_ln, "Level")  #  We aren't able the reject the null of stationarity

