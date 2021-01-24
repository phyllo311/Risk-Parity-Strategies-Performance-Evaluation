# Load libraries
#install.packages("fBasics")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("ggthemes")
#install.packages("lubridate")
#install.packages("readr")
#install.packages("tidyr")
#install.packages("lmtest")
#install.packages("sandwich")
#install.packages("nloptr")
#install.packages("systemfit")
#install.packages("pracma")
#install.packages("xlsx")
#install.packages("tidyverse")


library(xlsx)
library(fBasics)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(readr)
library(tidyr)
library(lmtest)
library(sandwich)
library(nloptr)
library(systemfit)
library(pracma)


# -----------------------------------------------------------------------------
#Read data
#Bonds
dat1 <- read.xlsx("P1.xlsx", sheetIndex = 1)
str(dat1)
colnames(dat1) <-c("Date","LHUSTRY", "LH13T15", "LHAGGBD", "LHAGGBE")
dat1$Date <- as.Date(dat1$Date, format = '%Y-%m-%d')


#Equities, REITs, Commodity Private Equity
dat2 <- read.xlsx("P2.xlsx", sheetIndex = 1)
str(dat2)
colnames(dat2) <-c("Date","MSEMKF","MSEROP","MSEAFE","SP500","F3USRN",
                   "F3PARN","F3WDRN","F3EURN","M3DWRL","M3EFRL",
                   "DJCICOP","DJUBSSP", "UPSP")
dat2$Date <- as.Date(dat2$Date, format = '%Y-%m-%d')


#russell 2000, russell 3000
russell <- read.xlsx("Russell.xlsx",sheetIndex = 1)  
colnames(russell) <-c("Date","Russell2000", "Russell3000")
russell$Date <- as.Date(russell$Date, format = '%Y-%m-%d')


#bonds
bonds <- read.xlsx("Russell.xlsx",sheetIndex = 3) 
colnames(bonds) <-c("Date","BOFACorp", "AAABONDS")
bonds$Date <- as.Date(bonds$Date, format = '%Y-%m-%d')


#
MSNORD <- read.xlsx("Book2.xlsx",sheetIndex = 1) 
colnames(MSNORD) <-c("Date","MSNORD")
MSNORD$Date <- as.Date(MSNORD$Date, format = '%Y-%m-%d')

bonds <- read.xlsx("Book2.xlsx",sheetIndex = 2) 
colnames(bonds) <-c("Date","MLU3BTL","MLHMACL","MLCORML","MLGALML")
bonds$Date <- as.Date(bonds$Date, format = '%Y-%m-%d')

comm <- read.xlsx("Book2.xlsx",sheetIndex = 3) 
colnames(comm) <-c("Date","GSCLTOT","GSSITOT","GSGCTOT")
comm$Date <- as.Date(comm$Date, format = '%Y-%m-%d')

dat <- merge(dat2, bonds, by = "Date") #merge price indeces
dat <- merge(dat, MSNORD, by = "Date")
dat <- merge(dat, comm, by = "Date")


#select data for backtest
dat3 <- dat[,c("Date","MSEMKF","MSEAFE","MSEROP","SP500","Russell2000",
               "MSNORD","MLCORML","MLGALML",
                "GSCLTOT","GSSITOT","GSGCTOT")]

dat4 <- sapply(dat3, function(x) as.numeric(as.character(x)))
class(dat4)

#log return
#logret <- diff(log(as.matrix(dat4[,-1])),lag=1)

#simple returns
#In the R language if you have a vector of prices (for the same asset at different times), 
#you can compute the simple returns with:
#https://www.r-bloggers.com/a-tale-of-two-returns/
#R <- priceVec[-1] / priceVec[-length(priceVec)] - 1
#It is slightly different if you have a price matrix (with times in the rows and assets along the columns):
R <- dat4[-1, ] / dat4[-nrow(dat4), ] - 1

#merge date to return
m <- as.Date(dat3[-1,c(1)],format = '%Y-%m-%d')
str(m)

#dd <- data.frame(m,logret)
#dd <- data.frame(Date = m,logret)
dd <- data.frame(Date = m, R[,-1])
dd$Date <- as.Date(dd$Date, format = '%Y-%m-%d')
logret <- dd %>% gather(SYMBOL,RET,-Date)

write.csv(logret,"logret.csv")
#write.csv(dd,"simpleret.csv")



#Carhart factors
carhart <- read.csv("F-F_Research_Data_Factors.CSV")
carhart <- sapply(carhart, function(x) as.numeric(as.character(x)))
carhart <- carhart[866:1122,2:5]/100  #199808 - 201912
carhart <- data.frame(m,carhart)

mom <- read.csv("F-F_Momentum_Factor.CSV")
mom <- sapply(mom,function(x) as.numeric(as.character(x)))
mom <- mom[860:1116,2]/100
carhart <- data.frame(carhart,mom)
colnames(carhart) <-c("Date","MKT_RF","SMB","HML","RF","MOM")

write.csv(carhart,"carhart.csv")

BAB <- read.xlsx("Betting Against Beta Equity Factors Monthly.xlsx", sheetIndex = 1)
BAB <- BAB[,c(1,25,26)] #extract date, US, global BAB
BAB <- BAB[814:1070,c(26)] #199808 - 201912, global equity
colnames(BAB) <- c("BAB")
BAB$Date <- as.Date(BAB$Date, format = '%Y-%m-%d')
factors <- merge(carhart,BAB)

#AQR data
car <- read.xlsx('AQR.xlsx',sheetIndex = 1)


#In sample testing: use first 120 months obs
T0 <- dat[1:120, 1]
R <- as.matrix(dat[1:120, -1])
# Use remaining data for evaluating out-of-sample performance
T1 <- dat[-c(1:120), 1]
R1 <- as.matrix(dat[-c(1:120), -1])


# Collect all returns into one data frame
dat <- data_frame(Date = T0,
                  RB = R_RB,
                  MDP = R_MDP,
                  DRP = R_DRP,
                  IV = R_IV,
                  EW = R_EW)

# Read in Carhart factors
car_m <- read_csv('Carhart.csv')

# Merge returns and factors
dat1 <- merge(dat, car_m, by = 'Date')

# Calculate excess returns to portfolios
dat1[,2:5] <- dat1[,2:5] - dat1$RF

# Mean
barplot(apply(dat1[,2:5], 2, mean), main = 'Mean')

# Volatility
barplot(apply(dat1[,2:5], 2, sd), main = 'Volatility')

# Sharpe
barplot(apply(dat1[,2:5], 2, mean) / apply(dat1[,2:5], 2, sd), main = 'Sharpe')

##Summary statistics of IV & EW portfolio 
summary_RB<-data.frame( 
  avg = mean(R_RB), 
  t = t.test(R_RB)$statistic, 
  sd = sd(R_RB), 
  skewness = moments::skewness(R_RB), 
  ekurtosis = moments::kurtosis(R_RB) - 3, 
  VaR = PerformanceAnalytics::VaR(R_RB, .99, method = 'historical') 
) 
# Comparing Portfolios
# summary stats (mean excess return, volatility, skewness, kurtosis)
# risk -return measures:
# Sharpe ratio w t-value, p-value from robustness tests
# Risk measures: Information Ratio, VAR, Expected Shortfall, Max Drawdown




#Backtest using 60 month rolling window
#result: plot of cumulative returns of portfolios




#Overfitting backtest: 
#Probability of Backtest Overfitting
#Haircutting Sharpe Ratios using Holm tests and Bonferroni tests



#Analysing factor exposures: OLS factor regression model . Carhart factors, Volatility, Quality



# Measure diversification: Gini Coef, time varying weights


#Leverage portfolio


