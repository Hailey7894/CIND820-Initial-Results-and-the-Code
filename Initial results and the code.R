options(repos = c(CRAN = "https://cran.rstudio.com/"))

#1.data peparetion
install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)

gdp<- read.csv("D:/TMU/CIND820/Datasets/gdp_data.csv")
gdp_data<- gdp[, -c(2, 3, 4)]
head(gdp_data)
gdp_data <- gdp_data %>% filter(Indicator == "Gross Domestic Product, Real, Domestic Currency")
head(gdp_data)
gdp_data_long <-gdp_data %>%
  gather(key = "YEAR", value = "GDP",-Indicator)
gdp_data_long$YEAR <- as.numeric(gsub("X", "",gdp_data_long$YEAR))
head(gdp_data_long)


cpi<- read.csv("D:/TMU/CIND820/Datasets/cpi_data.csv")
cpi_data <- cpi %>% 
  filter(SUBJECT == "CP040100") %>% 
  select(TIME, Value) %>%
  rename(YEAR = TIME, CPI = Value)
str(cpi_data)
head(cpi_data)

unemployment<- read.csv("D:/TMU/CIND820/Datasets/unemployment_data.csv")
unemployment_data<-unemployment %>% filter(SUBJECT == "LRUNTTTT",TIME>="1976")
unemployment_data<-unemployment_data %>%
  select(YEAR = TIME, Unemployment_Rate = Value)
str(unemployment_data)
head(unemployment_data)

full_dataset<-merge(gdp_data_long,cpi_data,by="YEAR")
full_dataset<-merge(full_dataset,unemployment_data,by="YEAR")
full_dataset <- full_dataset %>% 
  select(-Indicator)
str(full_dataset)
head(full_dataset)


#2.Johansen Cointegration test#
install.packages("urca")
install.packages("tseries")
install.packages("vars")
library(tseries)
library(urca)
library(vars)

#check whether series is stationary or not#
adf_gdp <- adf.test(full_dataset$GDP)
adf_cpi <- adf.test(full_dataset$CPI)
adf_unemployment <- adf.test(full_dataset$Unemployment_Rate)
print(adf_gdp)#p-value = 0.4285
print(adf_cpi)# p-value = 0.5368
print(adf_unemployment)#p-value =0.03843

#The p-value>0.05,the gdp & cpi series are non-stationary,the null hypothesis is not rejected.Need to do the Johansen Cointegration test#

test_data_ts <- ts(full_dataset[, c("GDP", "CPI", "Unemployment_Rate")], start = c(1976), frequency = 1)
johansen_test<-ca.jo(test_data_ts,type="trace",ecdet="const",K=2)
summary(johansen_test)

#The cointegration is found, estimate the Error Correction Model to examine the short-run dynamics.#
vecm_model<-cajorls(johansen_test,r=1)
summary(vecm_model)

#3. Evaluate the causality between variables by apply Non-liner Granger Causality Tests#
install.packages("nonlinearTseries")
install.packages("tseriesChaos")
library(nonlinearTseries)
library(tseriesChaos)
gdp_ts <- ts(full_dataset$GDP, start = c(1976), frequency = 1)
cpi_ts <- ts(full_dataset$CPI, start = c(1976), frequency = 1)
unemployment_ts <- ts(full_dataset$Unemployment_Rate, start = c(1976), frequency = 1)

#Test if CPI causes GDP
granger_test_cpi_gdp <- grangertest(gdp_ts ~ cpi_ts, order = 1)
print(granger_test_cpi_gdp)

#Test if GDP causes CPI
granger_test_gdp_cpi <- grangertest(cpi_ts ~ gdp_ts, order = 1)
print(granger_test_gdp_cpi)

# Test if Unemployment Rate causes GDP
granger_test_unemp_gdp <- grangertest(gdp_ts ~ unemployment_ts, order = 1)
print(granger_test_unemp_gdp)

#Test if GDP causes Unemployment Rate
granger_test_gdp_unemp <- grangertest(unemployment_ts ~ gdp_ts, order = 1)
print(granger_test_gdp_unemp)

#Test if Unemployment Rate causes CPI
granger_test_unemp_cpi <- grangertest(cpi_ts ~ unemployment_ts, order = 1)
print(granger_test_unemp_cpi)

#Test if CPI causes Unemployment Rate
granger_test_cpi_unemp <- grangertest(unemployment_ts ~ cpi_ts, order = 1)
print(granger_test_cpi_unemp)


#4. Perform bounds test in the context of an ARDL model
install.packages("ARDL")
library(ARDL)
gdp_ts <- ts(full_dataset$GDP, start = c(1976), frequency = 1)
cpi_ts <- ts(full_dataset$CPI, start = c(1976), frequency = 1)
unemployment_ts <- ts(full_dataset$Unemployment_Rate, start = c(1976), frequency = 1)

ardl_model <- auto_ardl(GDP ~ CPI + Unemployment_Rate, data = full_dataset, max_order = c(4, 4, 4))
summary(ardl_model$best_model)

bounds_test <- bounds_f_test(ardl_model$best_model, case = 3)
print(bounds_test)