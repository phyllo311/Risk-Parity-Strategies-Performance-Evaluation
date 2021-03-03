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
#install.packages("viridis")


library(viridis)
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
library(PerformanceAnalytics)
library(RcppRoll)
#--------------A. Evaluation of Assets------------------
#read index returns
logret <- read.csv('logret.csv')
logret <- logret[,-1]  #remove extra order column from data construction step
logret$Date <- as.Date(logret$Date, format = '%Y-%m-%d')
str(logret)

logret <- logret %>% 
 # filter(Date >= '1991-02-28')  %>%
 spread(SYMBOL,RET)

#select stocks
logret <- logret[,-c(11)]

# Assets evaluation
basicStats(logret[,-1])

mu <- apply(logret[,-1], 2, mean)
barplot(mu)

# Calculate covariance matrix
cova <- round(cov(logret[,-1]),3)

# Calculate correlation matrix
crr <- cor(logret[,-1])
write.xlsx(crr,"crr.xlsx")
#correlogram
#install.packages("corrpolot")
library(corrplot)
corrplot(crr, method="number")
#corrplot(crr, method="circle")
corrplot(crr, type = "lower")

# Plot cumulative returns   
plot(x = logret$Date, y = cumprod(1 + logret$GSGCTOT), type = 'l', col ='black',xlab = 'Date', 
     ylab = 'Cumulative return',ylim = c(-1.5,30))
lines(x = logret$Date, y = cumprod(1 + logret$GSCLTOT), type = 'l', col = 'pink')
lines(x = logret$Date, y = cumprod(1 + logret$GSSITOT), type = 'l', col = 'orange')
lines(x = logret$Date, y = cumprod(1 + logret$MLCORML), type = 'l', col = 'blue')
lines(x = logret$Date, y = cumprod(1 + logret$MLGALML), type = 'l', col = 'lightgreen')
lines(x = logret$Date, y = cumprod(1 + logret$MSEAFE), type = 'l', col = 'green')
lines(x = logret$Date, y = cumprod(1 + logret$MSEMKF), type = 'l', col = 'red')
lines(x = logret$Date, y = cumprod(1 + logret$MSNORD), type = 'l', col = 'tan')
lines(x = logret$Date, y = cumprod(1 + logret$SP500), type = 'l', col = 'plum')

text =  c('GOLD','CRUDE OIL','SILVER',"CORP BONDS",
          'MLGAML','MSCI EAFE','MSCI EMKF',
          'MSCI NORDIC', 'SP500')
strwidth(text)

legend('topleft', cex = 0.75, c('GOLD','CRUDE OIL','SILVER',"CORP BONDS",
                                'MLGAML','MSCI EAFE','MSCI EMKF',
                                'MSCI NORDIC', 'SP500'), lty = 1,x.intersp=0.5,y.intersp=0.5,
       text.width=strwidth(text)[1]/2,
       col=c("black",'orange',"blue","lightgreen","green","red","tan",'plum'))


#plot cumulative returns of each index - EFR
#transform to long format
logret1 <- logret %>%
  gather("SYMBOL","R",-Date)


#write.csv(logret1,"logretlong.csv")

ggplot(logret1 %>%
         group_by(SYMBOL) %>%
         mutate(TRI=ifelse(row_number()==1, 1, 1+R),
                TRI=cumprod(TRI)),
       aes(x=Date, y=TRI, color=SYMBOL)) +
  coord_trans(y='log') +
  facet_wrap(~SYMBOL) +
  geom_line() +
  theme(legend.position='bottom')


#calc rolling 60 month rolling volatility
voltest <- logret1 %>%
  group_by(SYMBOL) %>%
  do({
    cbind(., VOL=sqrt(12)*roll_sd(x=.$R, n=60, fill=NA, align='right', na.rm=TRUE))
  }) %>%
  ungroup() %>%
  filter(!is.na(VOL)) %>%
  select(Date, SYMBOL, VOL)

#area plot of volatility
ggplot(voltest,aes(x=Date,y=VOL)) +
  geom_area(aes(fill=SYMBOL),position = 'fill') +
  theme(legend.position='bottom')

#car_m <- read.csv("carhart.csv")
#car_m <- car_m[,-c(1)]
#car_m$Date <- as.Date(car_m$Date, format = '%Y-%m-%d')

car_m <- read.xlsx('AQR.xlsx',sheetIndex = 1)
car_m <- car_m[c(26:408),-c(1)]
#car_m <- car_m[c(62:408),-c(1)]
car_m <- sapply(car_m,function(x) as.numeric(as.character(x)))
car_m <- data.frame(logret$Date,car_m)
colnames(car_m)[1] <- c("Date")
car_m$Date <- as.Date(car_m$Date, format = '%Y-%m-%d')

# Merge ETF returns and factor returns by date
dat <- merge(logret,car_m,by = "Date")

# Calculate excess returns by subtracting risk-free rate
dat[,2:11] <- dat[,2:11] - dat[,'RF']


# Estimate CAPM for each asset return
m1_1 <- lm(GSGCTOT ~ MKT_RF, data = dat)
summary(m1_1)

m1_2 <- lm(GSSITOT ~ MKT_RF, data = dat)
summary(m1_2)

m1_3 <- lm(MLCORML ~ MKT_RF, data = dat)
summary(m1_3)

m1_4 <- lm(GSCLTOT ~ MKT_RF, data = dat)
summary(m1_4)

m1_5 <- lm(MSEAFE ~ MKT_RF, data = dat)
summary(m1_5)

m1_6 <- lm(SP500 ~ MKT_RF, data = dat)
summary(m1_6)

m1_7 <- lm(MSNORD ~ MKT_RF, data = dat)
summary(m1_7)

m1_8 <- lm(MLGALML ~ MKT_RF, data = dat)
summary(m1_8)

m1_9 <- lm(MSEMKF ~ MKT_RF, data = dat)
summary(m1_9)

m1_10 <- lm(MSEROP ~ MKT_RF, data = dat)
summary(m1_10)

# This function calculates the drawdown (cumulative loss since losses started) based on portfolio returns
DrawDown <- function(x) {
  cum_ret <- cumprod(1 + x)
  max_cum_ret <- cummax(cum_ret)
  drawdown <- (max_cum_ret - cum_ret) / max_cum_ret
  return(drawdown)
}

summary_GSGCTOT<-data.frame( 
  skewness = moments::skewness(logret$GSGCTOT),  #use returns
  kurtosis = moments::kurtosis(logret$GSGCTOT) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(logret$GSGCTOT, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(logret$GSGCTOT, .95, method = 'historical'), #use returns
  DD = max(DrawDown(logret$GSGCTOT)),
  MeanER = 12 * 100 * mean(dat$GSGCTOT),  #annualized mean excess return,
  t1 = t.test(dat$GSGCTOT)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat$GSGCTOT), #annualized std excess return ,
  SR = sqrt(12) * mean(dat$GSGCTOT) / sd(dat$GSGCTOT),  #use ER
  IR = unname(coef(m1_1)['(Intercept)']) / sd(residuals(m1_1)), # Information ratio (relative to CAPM)
  TR = mean(dat$GSGCTOT) / unname(coef(m1_1)['MKT_RF']) # Treynor ratio
) 

summary_GSSITOT<-data.frame( 
  skewness = moments::skewness(logret$GSSITOT),  #use returns
  kurtosis = moments::kurtosis(logret$GSSITOT) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(logret$GSSITOT, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(logret$GSSITOT, .95, method = 'historical'), #use returns
  DD = max(DrawDown(logret$GSSITOT)),
  MeanER = 12 * 100 * mean(dat$GSSITOT),  #annualized mean excess return,
  t1 = t.test(dat$GSSITOT)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat$GSSITOT), #annualized std excess return ,
  SR = sqrt(12) * mean(dat$GSSITOT) / sd(dat$GSSITOT),  #use ER
  IR = unname(coef(m1_2)['(Intercept)']) / sd(residuals(m1_2)), # Information ratio (relative to CAPM)
  TR = mean(dat$GSSITOT) / unname(coef(m1_2)['MKT_RF']) # Treynor ratio
) 

summary_MLCORML<-data.frame( 
  skewness = moments::skewness(logret$MLCORML),  #use returns
  kurtosis = moments::kurtosis(logret$MLCORML) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(logret$MLCORML, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(logret$MLCORML, .95, method = 'historical'), #use returns
  DD = max(DrawDown(logret$MLCORML)),
  MeanER = 12 * 100 * mean(dat$MLCORML),  #annualized mean excess return,
  t1 = t.test(dat$MLCORML)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat$MLCORML), #annualized std excess return ,
  SR = sqrt(12) * mean(dat$MLCORML) / sd(dat$MLCORML),  #use ER
  IR = unname(coef(m1_3)['(Intercept)']) / sd(residuals(m1_3)), # Information ratio (relative to CAPM)
  TR = mean(dat$MLCORML) / unname(coef(m1_3)['MKT_RF']) # Treynor ratio
) 


summary_GSCLTOT<-data.frame( 
  skewness = moments::skewness(logret$GSCLTOT),  #use returns
  kurtosis = moments::kurtosis(logret$GSCLTOT) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(logret$GSCLTOT, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(logret$GSCLTOT, .95, method = 'historical'), #use returns
  DD = max(DrawDown(logret$GSCLTOT)),
  MeanER = 12 * 100 * mean(dat$GSCLTOT),  #annualized mean excess return,
  t1 = t.test(dat$GSCLTOT)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat$GSCLTOT), #annualized std excess return ,
  SR = sqrt(12) * mean(dat$GSCLTOT) / sd(dat$GSCLTOT),  #use ER
  IR = unname(coef(m1_4)['(Intercept)']) / sd(residuals(m1_4)), # Information ratio (relative to CAPM)
  TR = mean(dat$GSCLTOT) / unname(coef(m1_4)['MKT_RF']) # Treynor ratio
) 


summary_MSEAFE<-data.frame( 
  skewness = moments::skewness(logret$MSEAFE),  #use returns
  kurtosis = moments::kurtosis(logret$MSEAFE) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(logret$MSEAFE, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(logret$MSEAFE, .95, method = 'historical'), #use returns
  DD = max(DrawDown(logret$MSEAFE)),
  MeanER = 12 * 100 * mean(dat$MSEAFE),  #annualized mean excess return,
  t1 = t.test(dat$MSEAFE)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat$MSEAFE), #annualized std excess return ,
  SR = sqrt(12) * mean(dat$MSEAFE) / sd(dat$MSEAFE),  #use ER
  IR = unname(coef(m1_5)['(Intercept)']) / sd(residuals(m1_5)), # Information ratio (relative to CAPM)
  TR = mean(dat$MSEAFE) / unname(coef(m1_5)['MKT_RF']) # Treynor ratio
) 


summary_SP500<-data.frame( 
  skewness = moments::skewness(logret$SP500),  #use returns
  kurtosis = moments::kurtosis(logret$SP500) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(logret$SP500, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(logret$SP500, .95, method = 'historical'), #use returns
  DD = max(DrawDown(logret$SP500)),
  MeanER = 12 * 100 * mean(dat$SP500),  #annualized mean excess return,
  t1 = t.test(dat$SP500)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat$SP500), #annualized std excess return ,
  SR = sqrt(12) * mean(dat$SP500) / sd(dat$SP500),  #use ER
  IR = unname(coef(m1_6)['(Intercept)']) / sd(residuals(m1_6)), # Information ratio (relative to CAPM)
  TR = mean(dat$SP500) / unname(coef(m1_6)['MKT_RF']) # Treynor ratio
) 


summary_MSNORD<-data.frame( 
  skewness = moments::skewness(logret$MSNORD),  #use returns
  kurtosis = moments::kurtosis(logret$MSNORD) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(logret$MSNORD, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(logret$MSNORD, .95, method = 'historical'), #use returns
  DD = max(DrawDown(logret$MSNORD)),
  MeanER = 12 * 100 * mean(dat$MSNORD),  #annualized mean excess return,
  t1 = t.test(dat$MSNORD)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat$MSNORD), #annualized std excess return ,
  SR = sqrt(12) * mean(dat$MSNORD) / sd(dat$MSNORD),  #use ER
  IR = unname(coef(m1_7)['(Intercept)']) / sd(residuals(m1_7)), # Information ratio (relative to CAPM)
  TR = mean(dat$MSNORD) / unname(coef(m1_7)['MKT_RF']) # Treynor ratio
) 

summary_MLGALML<-data.frame( 
  skewness = moments::skewness(logret$MLGALML),  #use returns
  kurtosis = moments::kurtosis(logret$MLGALML) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(logret$MLGALML, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(logret$MLGALML, .95, method = 'historical'), #use returns
  DD = max(DrawDown(logret$MLGALML)),
  MeanER = 12 * 100 * mean(dat$MLGALML),  #annualized mean excess return,
  t1 = t.test(dat$MLGALML)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat$MLGALML), #annualized std excess return ,
  SR = sqrt(12) * mean(dat$MLGALML) / sd(dat$MLGALML),  #use ER
  IR = unname(coef(m1_8)['(Intercept)']) / sd(residuals(m1_8)), # Information ratio (relative to CAPM)
  TR = mean(dat$MLGALML) / unname(coef(m1_8)['MKT_RF']) # Treynor ratio
) 

summary_MSEMKF<-data.frame( 
  skewness = moments::skewness(logret$MSEMKF),  #use returns
  kurtosis = moments::kurtosis(logret$MSEMKF) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(logret$MSEMKF, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(logret$MSEMKF, .95, method = 'historical'), #use returns
  DD = max(DrawDown(logret$MSEMKF)),
  MeanER = 12 * 100 * mean(dat$MSEMKF),  #annualized mean excess return,
  t1 = t.test(dat$MSEMKF)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat$MSEMKF), #annualized std excess return ,
  SR = sqrt(12) * mean(dat$MSEMKF) / sd(dat$MSEMKF),  #use ER
  IR = unname(coef(m1_9)['(Intercept)']) / sd(residuals(m1_9)), # Information ratio (relative to CAPM)
  TR = mean(dat$MSEMKF) / unname(coef(m1_9)['MKT_RF']) # Treynor ratio
) 

summary_MSEROP<-data.frame( 
  skewness = moments::skewness(logret$MSEROP),  #use returns
  kurtosis = moments::kurtosis(logret$MSEROP) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(logret$MSEROP, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(logret$MSEROP, .95, method = 'historical'), #use returns
  DD = max(DrawDown(logret$MSEROP)),
  MeanER = 12 * 100 * mean(dat$MSEROP),  #annualized mean excess return,
  t1 = t.test(dat$MSEROP)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat$MSEROP), #annualized std excess return ,
  SR = sqrt(12) * mean(dat$MSEROP) / sd(dat$MSEROP),  #use ER
  IR = unname(coef(m1_10)['(Intercept)']) / sd(residuals(m1_10)), # Information ratio (relative to CAPM)
  TR = mean(dat$MSEROP) / unname(coef(m1_10)['MKT_RF']) # Treynor ratio
) 

knitr::kable(rbind("Global Gov Bonds" = summary_MLGALML,"US Corporate Bonds" = summary_MLCORML,
                   "MSCI Emerging Market"=summary_MSEMKF,
                   "MSCI EAFE" = summary_MSEAFE, "MSCI EUROPE" = summary_MSEROP, "SP500" = summary_SP500, 
                    "MSNORD" = summary_MSNORD, "GOLD"=summary_GSGCTOT, "SILVER"=summary_GSSITOT, 
                   "Crude Oil" = summary_GSCLTOT),digits = 3)

#-------B. Evaluate Performance of RP portfolios and Benchmarks with static weights (Omit In Sample and Out of Sample)-----------------------------------
#Use first 120 observations for In Sample backtest
#T0 <- dat[1:120, 1]
#R <- as.matrix(dat[1:120, -1])

#The rest observations for OS backtest
#T1 <- dat[-c(1:120), 1]
#R1 <- as.matrix(dat[-c(1:120), -1])

# setting weight functions: Risk budgeting - ERC

risk_budgeting <- function(V, b) {
  fn <- function(w, V, b) sum((w * as.vector(V %*% w) - b * as.numeric(t(w) %*% V %*% w)) ^ 2)
  gr <- function(w, V, b) pracma::grad(fn, w, V = V, b = b)
  heq <- function(w, V, b) sum(w) - 1
  jac_heq <- function(w, V, b) pracma::jacobian(heq, w, V = V, b = b)
  
  stopifnot(all(b > 0))
  
  b <- b / sum(b)
  
  N <- ncol(V)
  
  res <- nloptr(x0 = rep(1 / N, N),
                eval_f = fn,
                eval_grad_f = gr,
                lb = rep(0, N),
                ub = rep(1, N),
                eval_g_eq = heq,
                eval_jac_g_eq = jac_heq,
                opts = list('algorithm' = 'NLOPT_LD_SLSQP',
                            'xtol_rel' = 1.0e-8,
                            'maxeval' = 1000),
                V = V,
                b = b)
  
  stopifnot(res$status > 0)
  
  return(setNames(res$solution, colnames(V)))
}


# Maximum Diversification Portfolio (MD)

maximum_diversification <- function(R) {
  fn <- function(w, R)  -t(w) %*% apply(R, 2, sd) / sqrt(t(w) %*% cov(R) %*% w)
  gr <- function(w, R) pracma::grad(fn, w, R = R)
  heq <- function(w, R) sum(w) - 1
  jac_heq <- function(w, R) pracma::jacobian(heq, w, R = R)
  
  N <- ncol(R)
  
  res <- nloptr(x0 = rep(1 / N, N),
                eval_f = fn,
                eval_grad_f = gr,
                lb = rep(0, N),
                ub = rep(1, N),
                eval_g_eq = heq,
                eval_jac_g_eq = jac_heq,
                opts = list('algorithm' = 'NLOPT_LD_SLSQP',
                            'xtol_rel' = 1.0e-8,
                            'maxeval' = 1000),
                R = R)
  
  stopifnot(res$status > 0)
  
  return(setNames(res$solution, colnames(R)))
}


# Diversified Risk Parity Portfolio (DRP)

diversified_risk_parity <- function(V) {
  fn <- function(w, V) {
    Lambda <- eigen(V)$values
    E <- eigen(V)$vectors
    wtilde <- t(E) %*% w
    p <- wtilde ^ 2 * Lambda / sum(wtilde ^ 2 * Lambda)
    return(-exp(-sum(p * log(p))))
  }
  gr <- function(w, V) pracma::grad(fn, w, V = V)
  heq <- function(w, V) sum(w) - 1
  jac_heq <- function(w, V) pracma::jacobian(heq, w, V = V)
  
  N <- ncol(V)
  
  res <- nloptr(x0 = rep(1 / N, N),
                eval_f = fn,
                eval_grad_f = gr,
                lb = rep(0, N),
                ub = rep(1, N),
                eval_g_eq = heq,
                eval_jac_g_eq = jac_heq,
                opts = list('algorithm' = 'NLOPT_LD_SLSQP',
                            'xtol_rel' = 1.0e-8,
                            'maxeval' = 1000),
                V = V)
  
  stopifnot(res$status > 0)
  
  return(setNames(res$solution, colnames(V)))
}


# This function estimates weights for minimum variance portfolio
min_var <- function(V, shorts = TRUE) {
  if (shorts) {
    N <- ncol(V)
    ones <- rep(1, N)
    res <- as.vector(solve(V) %*% ones / as.numeric(t(ones) %*% solve(V) %*% ones))
    return(setNames(res, colnames(V)))
  } else {
    eval_f <- function(w, V) as.numeric(t(w) %*% V %*% w)
    eval_grad_f <- function(w,V) pracma::grad(eval_f, w, V = V)
    eval_g_eq <- function(w,V) sum(w) - 1
    eval_jac_g_eq <- function(w,V) pracma::jacobian(eval_g_eq, w, V = V)
    N <- ncol(V)
    res <- nloptr(   #solve minimum optimization
      x0 = rep(1 / N, N), #start, initial vector is equal weights
      eval_f = eval_f, #funct set above, main objective function
      eval_grad_f = eval_grad_f, #funct set above, returns gradient of objective funct
      lb = rep(0, N), #lower bound
      ub = rep(1, N),  #upper bound
      eval_g_eq = eval_g_eq, #funct evaluates (non-)linear equality constraints that should hold in soluN.
      eval_jac_g_eq = eval_jac_g_eq, #funct evaluates jacobian of (non-)linear equality constraints
      opts = list(  #list with opNs., option "algorithm" required
        'algorithm' = 'NLOPT_LD_SLSQP',
        'xtol_rel' = 1.0e-8,
        'maxeval' = 1000
      ),
      V = V
    )
    stopifnot(res$status > 0)
    return(setNames(res$solution, colnames(V)))
  }
}

# Calculate percentage risk contributions
prc <- function(w, V) {
  res <- w * as.vector(V %*% w) / as.numeric(sqrt(t(w) %*% V %*% w))
  return(100 * res / sum(res))
}


#----------Calculating portfolio weights using entire sample---------------------------------
T0 <- logret[, 1]
R <- as.matrix(logret[, -c(1)])
V <- cov(R) # covariance matrix
N <- ncol(R) #number of assets
sigma2 <- diag(V)


# Solve portfolio weights
w_RB <- risk_budgeting(V, rep(1/N, N))
w_MDP <- maximum_diversification(R)
w_DRP <- diversified_risk_parity(V)
w_MV <- min_var(V)
w_IV <- (1/sigma2)/(sum(1/sigma2)) 
w_EW <- setNames(rep(1/N, N), colnames(R))


----------------# Factor Risk Parity (FRP)-----------------------------------------
#read returns

# Read in Carhart factors
#car_m <- read.csv("carhart.csv")
#car_m <- car_m[,-c(1)]
#car_m$Date <- as.Date(car_m$Date, format = '%Y-%m-%d')

#transform to long format
#logret1 <- logret %>%
 # gather("SYMBOL","R",-Date)

# Calculate simple returns #transform long format to wide format
#ret <- logret1 %>%
#  spread(SYMBOL, R)


#calc excess returns
#excess_returns <- merge(logret,car_m, by = 'Date')
#excess_returns[,2:9] <- excess_returns[,2:9] - excess_returns$RF


# Calculate excess returns      #to double check, giving different results for FRP weights, same results of excess returns
excess_returns <- inner_join(logret1, car_m, by = 'Date') %>% #join long form logret to wide form car_m
  mutate(ER = R - RF) %>%
  select(Date, SYMBOL, ER, MKT_RF, SMB, HML, MOM, RF) %>%
  spread(SYMBOL, ER) #transform to wide format with ETFs and factors

# Extract asset names
assets <- colnames(select(excess_returns, -Date, -MKT_RF, -SMB, -HML, -MOM, -RF))

# Extract factor names
#factors <- c('MKT_RF', 'SMB', 'HML', 'MOM')
factors <- c('MKT_RF', 'SMB')
# Construct system of OLS equations
eqs1 <- setNames(lapply(assets, function(lhs, rhs) {
  as.formula(sprintf('%s~%s', lhs, paste(rhs, collapse = '+')))
}, rhs = factors), assets)

# Estimate system of OLS equations
fit1 <- systemfit(eqs1, data = excess_returns, method = 'OLS')

# Extract factor loadings
A <- do.call(cbind, lapply(factors, function(fac) { coef(fit1)[paste0(assets, '_', fac)]}))

# Set rownames and colnames
dimnames(A) <- list(assets, factors)

# Calculate covariance matrix of factors
Omega <- cov(excess_returns[, factors])

# Extract variances of residuals into a diagonal matrix
D <- diag(apply(residuals(fit1), 2, var))

# Set rownames and colnames
dimnames(D) <- list(assets, assets)

# Estimate asset covariances using the factor model
V1 <- A %*% Omega %*% t(A) + D

# Set rownames and colnames
dimnames(V1) <- list(assets, assets)

# This function solves factor risk parity portfolio
factor_risk_budgeting <- function(A, V1, b) {
  fn <- function(x, A, V1, b) {
    rc <- as.vector((t(A) %*% x) * (MASS::ginv(A) %*% V1 %*% x)) / as.numeric(sqrt(t(x) %*% V1 %*% x))
    pv <- as.numeric(sqrt(t(x) %*% V1 %*% x))
    sum((rc - b * pv) ^ 2)
  }
  gr <- function(x, A, V1, b) pracma::grad(fn, x, A = A, V1 = V1, b = b)
  heq <- function(x, A, V1, b) sum(x) - 1
  jac_heq <- function(x, A, V1, b) pracma::jacobian(heq, x, A = A, V1 = V1, b = b)
  
  b <- b/sum(b)
  
  N <- ncol(V1)
  
  res <- nloptr(x0 = rep(1 / N, N),
                eval_f = fn,
                eval_grad_f = gr,
                eval_g_eq = heq,
                eval_jac_g_eq = jac_heq,
                opts = list('algorithm' = 'NLOPT_LD_SLSQP',
                            'xtol_rel' = 1.0e-8,
                            'maxeval' = 100),
                A = A,
                V1 = V1,
                b = b)
  
  stopifnot(res$status > 0)
  
  return(setNames(res$solution, colnames(V1)))
}

# This function calculates percentage factor risk contributions
prc1 <- function(x, A, V1) {
  rc <- as.vector((t(A) %*% x) * (MASS::ginv(A) %*% V1 %*% x)) / as.numeric(sqrt(t(x) %*% V1 %*% x))
  return(setNames(100 * rc / sum(rc), colnames(A)))
}

# Number of factors
M <- ncol(A)

# Number of assets
N <- ncol(V1)

# Solve weights for 1/N benchmark and factor risk parity portfolio
w_EW <- rep(1/N, N)
w_FRP <- factor_risk_budgeting(A, V1, b = rep(1 / M, M))

# Plot weights
par(mfrow = c(2,2))
barplot(w_EW,col="lightgreen") 
barplot(w_FRP,col="yellow")

# Plot factor risk contributions
barplot(prc1(w_EW, A, V1))
barplot(prc1(w_FRP, A, V1))
par(mfrow = c(1,1))


#--------------Performance Analysis of portfolios with static weights--------------
#assemble portfolio weights into 1 dataframe
w <- data.frame(w_RB,w_MDP,w_DRP,w_MV,w_IV,w_EW,w_FRP)*100

# Plot weights and PRC              
par(mfrow=c(3,2))
par(mar=c(1,1,1,1))
barplot(w_IV,col="blue") 
barplot(prc(w_IV, V))

barplot(w_RB,col="red")
barplot(prc(w_RB, V))

barplot(w_MDP,col="black")
barplot(prc(w_MDP, V))

barplot(w_DRP,col="green")
barplot(prc(w_DRP, V))

barplot(w_FRP,col="yellow")
barplot(prc(w_FRP, V))

barplot(w_MV,col = "orange")
barplot(prc(w_MV, V))

barplot(w_EW,col="lightgreen")
barplot(prc(w_EW, V))

par(mfrow=c(1,1))

# -----------------portfolio returns with static weights-----------------
#Demo 3 - Omit In sample and Out Sample performance, use whole period data
port_ret <- function(w, R) {
  res <- matrix(0, nrow = nrow(R), ncol = 1)
  for (i in 1:nrow(R)) res[i,] <- sum(w*R[i,])
  res
}


T0 <- logret[, 1]
ret <- logret1 %>%
  spread(SYMBOL, R)

R_FRP <- port_ret(w_FRP, ret[,-c(1)])
R_EW <- port_ret(w_EW, ret[,-c(1)])

plot(T0, cumprod(1 + R_EW), type = 'l', col = 'blue', ylim = c(1,6), 
     xlab = 'Date', ylab = 'Cumulative return')
lines(T0, cumprod(1 + R_FRP), col = 'tan')


# -----------------portfolio returns with static weights-----------------
#Demo 3 - Omit In sample and Out Sample performance, use whole period data
port_ret <- function(w, R) {
  res <- matrix(0, nrow = nrow(R), ncol = 1)
  for (i in 1:nrow(R)) res[i,] <- sum(w*R[i,])
  res
}

# Calc port returns  #double check
R_IV <- port_ret(w_IV, R) 
R_RB <- port_ret(w_RB, R)
R_MDP <- port_ret(w_MDP, R)
R_DRP <- port_ret(w_DRP, R)
R_MV <- port_ret(w_MV, R)
R_EW <- port_ret(w_EW, R)


#combine port returns
portret <- data_frame(Date = T0, 
                  IV = R_IV, 
                  RB = R_RB, 
                  MDP = R_MDP, 
                  DRP = R_DRP, 
                  FRP = R_FRP,
                  MV = R_MV,
                  EW = R_EW)

# Plot cumulative returns for comparison
plot(T0, cumprod(1 + R_EW), type = 'l', col = 'blue', ylim = c(0, 10), 
     xlab = 'Date', ylab = 'Cumulative return')
lines(T0, cumprod(1 + R_RB), col = 'red')
lines(T0, cumprod(1 + R_MDP), col = 'black')
lines(T0, cumprod(1 + R_DRP), col = 'green')
lines(T0, cumprod(1 + R_FRP), col = 'tan')
lines(T0, cumprod(1 + R_MV), col = 'turquoise')
lines(T0, cumprod(1 + R_IV), col = 'orange')

text =  c('Equal Weight', 'Risk Budgeting', 'Maximum Diversification',
          'Diversified Risk Parity','Factor Risk Parity',
          "Minimum Variance",'Inverse Volatility')
strwidth(text)

legend('topleft', cex = 1, c('Equal Weight', 'Risk Budgeting', 'Maximum Diversification',
                                'Diversified Risk Parity','Factor Risk Parity',
                                "Minimum Variance",'Inverse Volatility'), lty = 1,x.intersp=0.5,y.intersp=0.5,
       text.width=strwidth(text)[1]/2,
       col=c('blue', 'red', 'black','green','tan',
             'turquoise','orange'))

#write portfolio returns with static weights
#write.csv(portret,"portretstatic.csv")

#Risk - Return measures


#Regression

# Merge returns and factors
dat1 <- merge(portret, car_m, by = 'Date')

# Calculate excess returns to portfolios
dat1[,2:8] <- dat1[,2:8] - dat1$RF


#--------------Summary statistics----------------------
# Estimate CAPM for portfolios
m1_iv <- lm(IV ~ MKT_RF, data = dat1)
summary(m1_iv)

m1_rb <- lm(RB ~ MKT_RF, data = dat1)
summary(m1_rb)

m1_mdp <- lm(MDP ~ MKT_RF, data = dat1)
summary(m1_mdp)

m1_drp <- lm(DRP ~ MKT_RF, data = dat1)
summary(m1_drp)

m1_mv <- lm(MV ~ MKT_RF, data = dat1)
summary(m1_mv)

m1_ew <- lm(EW ~ MKT_RF, data = dat1)
summary(m1_ew)

m1_frp <- lm(FRP ~ MKT_RF, data = dat1)
summary(m1_frp)

summary_iv<-data.frame( 
  skewness = moments::skewness(R_IV),  #use returns
  kurtosis = moments::kurtosis(R_IV) - 3,  #use returns
  #VaR = PerformanceAnalytics::VaR(R_IV, .95, method = 'historical') , #use returns
  #ES = PerformanceAnalytics::ES(R_IV, .95, method = 'historical'), #use returns
  MeanER = 12 * 100 * mean(dat1$IV),  #annualized mean excess return,
  t = t.test(dat1$IV)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat1$IV), #annualized std excess return ,
  SR = sqrt(12) * mean(dat1$IV) / sd(dat1$IV),  #annualized Sharpe
  IR = unname(coef(m1_iv)['(Intercept)']) / sd(residuals(m1_iv)), # Information ratio (relative to CAPM)
  TR = mean(dat1$IV) / unname(coef(m1_iv)['MKT_RF']) # Treynor ratio
)  

summary_rb<-data.frame( 
  skewness = moments::skewness(portret$RB),  #use returns
  kurtosis = moments::kurtosis(portret$RB) - 3,  #use returns
#  VaR = PerformanceAnalytics::VaR(portret$RB, .95, method = 'historical') , #use returns
#  ES = PerformanceAnalytics::ES(portret$RB, .95, method = 'historical'), #use returns
  MeanER = 12 * 100 * mean(dat1$RB),  #annualized mean excess return,
  t = t.test(dat1$RB)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat1$RB), #annualized std excess return ,
  SR = sqrt(12) * mean(dat1$RB) / sd(dat1$RB),  #annualized Sharpe
  IR = unname(coef(m1_rb)['(Intercept)']) / sd(residuals(m1_rb)), # Information ratio (relative to CAPM)
  TR = mean(dat1$RB) / unname(coef(m1_rb)['MKT_RF']) # Treynor ratio
)  
summary_mdp<-data.frame( 
  skewness = moments::skewness(portret$MDP),  #use returns
  kurtosis = moments::kurtosis(portret$MDP) - 3,  #use returns
  #VaR = PerformanceAnalytics::VaR(portret$MDP, .95, method = 'historical') , #use returns
  #ES = PerformanceAnalytics::ES(portret$MDP, .95, method = 'historical'), #use returns
  MeanER = 12 * 100 * mean(dat1$MDP),  #annualized mean excess return,
  t = t.test(dat1$MDP)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat1$MDP), #annualized std excess return ,
  SR = sqrt(12) * mean(dat1$MDP) / sd(dat1$MDP),  #annualized Sharpe
  IR = unname(coef(m1_mdp)['(Intercept)']) / sd(residuals(m1_mdp)), # Information ratio (relative to CAPM)
  TR = mean(dat1$MDP) / unname(coef(m1_mdp)['MKT_RF']) # Treynor ratio
)  
summary_drp<-data.frame( 
  skewness = moments::skewness(portret$DRP),  #use returns
  kurtosis = moments::kurtosis(portret$DRP) - 3,  #use returns
  #VaR = PerformanceAnalytics::VaR(portret$DRP, .95, method = 'historical') , #use returns
  #ES = PerformanceAnalytics::ES(portret$DRP, .95, method = 'historical'), #use returns
  MeanER = 12 * 100 * mean(dat1$DRP),  #annualized mean excess return,
  t = t.test(dat1$DRP)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat1$DRP), #annualized std excess return ,
  SR = sqrt(12) * mean(dat1$DRP) / sd(dat1$DRP),  #annualized Sharpe
  IR = unname(coef(m1_drp)['(Intercept)']) / sd(residuals(m1_drp)), # Information ratio (relative to CAPM)
  TR = mean(dat1$DRP) / unname(coef(m1_drp)['MKT_RF']) # Treynor ratio
)  
summary_frp<-data.frame( 
  skewness = moments::skewness(portret$FRP),  #use returns
  kurtosis = moments::kurtosis(portret$FRP) - 3,  #use returns
 # VaR = PerformanceAnalytics::VaR(portret$FRP, .95, method = 'historical') , #use returns
#  ES = PerformanceAnalytics::ES(portret$FRP, .95, method = 'historical'), #use returns
  MeanER = 12 * 100 * mean(dat1$FRP),  #annualized mean excess return,
  t = t.test(dat1$FRP)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat1$FRP), #annualized std excess return ,
  SR = sqrt(12) * mean(dat1$FRP) / sd(dat1$FRP),  #annualized Sharpe
  IR = unname(coef(m1_frp)['(Intercept)']) / sd(residuals(m1_frp)), # Information ratio (relative to CAPM)
  TR = mean(dat1$FRP) / unname(coef(m1_frp)['MKT_RF']) # Treynor ratio
)  
summary_mv<-data.frame( 
  skewness = moments::skewness(portret$MV),  #use returns
  kurtosis = moments::kurtosis(portret$MV) - 3,  #use returns
#  VaR = PerformanceAnalytics::VaR(portret$MV, .95, method = 'historical') , #use returns
 # ES = PerformanceAnalytics::ES(portret$MV, .95, method = 'historical'), #use returns
  MeanER = 12 * 100 * mean(dat1$MV),  #annualized mean excess return,
  t = t.test(dat1$MV)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat1$MV), #annualized std excess return ,
  SR = sqrt(12) * mean(dat1$MV) / sd(dat1$MV),  #annualized Sharpe
  IR = unname(coef(m1_mv)['(Intercept)']) / sd(residuals(m1_mv)), # Information ratio (relative to CAPM)
  TR = mean(dat1$MV) / unname(coef(m1_mv)['MKT_RF']) # Treynor ratio
)  
summary_ew<-data.frame( 
  skewness = moments::skewness(portret$EW),  #use returns
  kurtosis = moments::kurtosis(portret$EW) - 3,  #use returns
 # VaR = PerformanceAnalytics::VaR(portret$EW, .95, method = 'historical') , #use returns
#  ES = PerformanceAnalytics::ES(portret$EW, .95, method = 'historical'), #use returns
  MeanER = 12 * 100 * mean(dat1$EW),  #annualized mean excess return,
  t = t.test(dat1$EW)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat1$EW), #annualized std excess return ,
  SR = sqrt(12) * mean(dat1$EW) / sd(dat1$EW),  #annualized Sharpe
  IR = unname(coef(m1_ew)['(Intercept)']) / sd(residuals(m1_ew)), # Information ratio (relative to CAPM)
  TR = mean(dat1$EW) / unname(coef(m1_ew)['MKT_RF']) # Treynor ratio
)  
knitr::kable(rbind(IV=summary_iv,RB=summary_rb,MDP=summary_mdp,
                   DRP=summary_drp,FRP=summary_frp,
                   MV=summary_mv,EW=summary_ew),digits = 3) 


# ---------------------C. Backtesting Rolling 36 month window----------------------------------------------------------------------

#Initialize time-series of weights
w_IV <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[, -1]), dimnames = list(NULL, colnames(logret[, -1])))
w_RB <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))
w_MDP <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))
w_DRP <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))
w_EW <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))
w_MV <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))
w_FRP <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))


# Estimate weights using 36 month telescope window
for (i in 37:nrow(logret)) {
  R <- logret[1:(i - 1), -1]
  mu <- apply(R, 2, mean)
  V <- cov(R)
  N <- ncol(R)
  w_RB[i,] <- risk_budgeting(V, rep(1/N, N))
  w_MDP[i,] <- maximum_diversification(R)
  w_DRP[i,] <- diversified_risk_parity(V)
  sigma2 <- diag(V)
  w_IV[i,] <- (1/sigma2)/(sum(1/sigma2))
  w_EW[i,] <- rep(1/N, N)
  w_MV[i,] <- min_var(V, shorts = FALSE)
  }


# Estimate FRP weights using 37 month telescope window 
for (i in 37:nrow(excess_returns)) {
  ER <- excess_returns[1:(i - 1), ]
  M <- ncol(A)
  N <- ncol(V1)
  b = rep(1 / M, M)
  assets <- colnames(select(ER, -Date, -MKT_RF, -SMB, -HML, -MOM, -RF))
  #factors <- c('MKT_RF', 'SMB', 'HML', 'MOM')
  factors <- c('MKT_RF', 'SMB')
  eqs1 <- setNames(lapply(assets, function(lhs, rhs) {
    as.formula(sprintf('%s~%s', lhs, paste(rhs, collapse = '+')))
  }, rhs = factors), assets)
  fit1 <- systemfit(eqs1, data = ER, method = 'OLS')
  A <- do.call(cbind, lapply(factors, function(fac) { coef(fit1)[paste0(assets, '_', fac)]}))
  dimnames(A) <- list(assets, factors)
  Omega <- cov(ER[, factors])
  D <- diag(apply(residuals(fit1), 2, var))
  dimnames(D) <- list(assets, assets)
  # Estimate asset covariances using the factor model
  V1 <- A %*% Omega %*% t(A) + D
  dimnames(V1) <- list(assets, assets)
  w_FRP[i,] <- factor_risk_budgeting(A, V1, b)
  }

# Calculate portfolio returns with time varying weights
eval_port <- function(w, R) {
  res <- matrix(0, nrow = nrow(R), ncol = 1)
  for (i in 1:nrow(R)) {
    res[i,] <- sum(w[i,]*R[i,])
  }
  res
}

#portfolio returns with time varying weights from rolling window
R_RB <- eval_port(w_RB, logret[,-1])[-(1:37)]
R_MDP <- eval_port(w_MDP, logret[,-1])[-(1:37)]
R_DRP <- eval_port(w_DRP, logret[,-1])[-(1:37)]
R_IV <- eval_port(w_IV, logret[,-1])[-(1:37)] 
R_EW <- eval_port(w_EW, logret[,-1])[-(1:37)]
R_MV <- eval_port(w_MV, logret[,-1])[-(1:37)]
R_FRP <- eval_port(w_FRP, logret[,-1])[-(1:37)]


# Extract dates for x-axis of plot
T0 <- logret$Date[-(1:37)]

# Plot in sample cumulative returns
plot(x = T0, y = cumprod(1 + R_EW), type = 'l', col = 'blue', ylim = c(0.5, 8), ylab = 'Cumulative return', xlab = 'Date')
lines(x = T0, y = cumprod(1 + R_RB), col = 'red')
lines(x = T0, y = cumprod(1 + R_MDP), col = 'black')
lines(x = T0, y = cumprod(1 + R_DRP), col = 'green')
lines(x = T0, y = cumprod(1 + R_FRP), col = 'tan')
lines(x = T0, y = cumprod(1 + R_MV), col = 'turquoise')
lines(x = T0, y = cumprod(1 + R_IV), col = 'orange')
text =  c('Equal Weight', 'Risk Budgeting', 'Maximum Diversification',
          'Diversified Risk Parity','Factor Risk Parity',
          "Minimum Variance",'Inverse Volatility')
strwidth(text)
legend('topleft', cex = 0.75, c('Equal Weight', 'Risk Budgeting', 'Maximum Diversification',
                             'Diversified Risk Parity','Factor Risk Parity',
                             "Minimum Variance",'Inverse Volatility'), lty = 1,x.intersp=0.5,y.intersp=0.5,
       text.width=strwidth(text)[1]/2,
       col=c('blue', 'red', 'black','green','tan',
             'turquoise','orange'))
# Collect all returns into one data frame 
dat <- data_frame(Date = T0, 
                  IV = R_IV, 
                  RB = R_RB, 
                  MDP = R_MDP, 
                  DRP = R_DRP, 
                  FRP = R_FRP,
                  MV = R_MV,
                  EW = R_EW)

#Regression

# Merge returns and factors
dat1 <- merge(dat, car_m, by = 'Date')

# Calculate excess returns to portfolios
dat1[,2:8] <- dat1[,2:8] - dat1$RF


#--------------Summary statistics----------------------
# Estimate CAPM for portfolios
m1_iv <- lm(IV ~ MKT_RF, data = dat1)
summary(m1_iv)

m1_rb <- lm(RB ~ MKT_RF, data = dat1)
summary(m1_rb)

m1_mdp <- lm(MDP ~ MKT_RF, data = dat1)
summary(m1_mdp)

m1_drp <- lm(DRP ~ MKT_RF, data = dat1)
summary(m1_drp)

m1_mv <- lm(MV ~ MKT_RF, data = dat1)
summary(m1_mv)

m1_ew <- lm(EW ~ MKT_RF, data = dat1)
summary(m1_ew)

m1_frp <- lm(FRP ~ MKT_RF, data = dat1)
summary(m1_frp)

summary_iv<-data.frame( 
  skewness = moments::skewness(R_IV),  #use returns
  kurtosis = moments::kurtosis(R_IV) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(R_IV, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(R_IV, .95, method = 'historical'), #use returns
  MeanER = 12 * 100 * mean(dat1$IV),  #annualized mean excess return,
  t = t.test(dat1$IV)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat1$IV), #annualized std excess return ,
  SR = sqrt(12) * mean(dat1$IV) / sd(dat1$IV),  #annualized Sharpe
  IR = unname(coef(m1_iv)['(Intercept)']) / sd(residuals(m1_iv)), # Information ratio (relative to CAPM)
  TR = mean(dat1$IV) / unname(coef(m1_iv)['MKT_RF']) # Treynor ratio
)  

summary_rb<-data.frame( 
  skewness = moments::skewness(R_RB),  #use returns
  kurtosis = moments::kurtosis(R_RB) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(R_RB, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(R_RB, .95, method = 'historical'), #use returns
  MeanER = 12 * 100 * mean(dat1$RB),  #annualized mean excess return,
  t = t.test(dat1$RB)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat1$RB), #annualized std excess return ,
  SR = sqrt(12) * mean(dat1$RB) / sd(dat1$RB),  #annualized Sharpe
  IR = unname(coef(m1_rb)['(Intercept)']) / sd(residuals(m1_rb)), # Information ratio (relative to CAPM)
  TR = mean(dat1$RB) / unname(coef(m1_rb)['MKT_RF']) # Treynor ratio
)  
summary_mdp<-data.frame( 
  skewness = moments::skewness(R_MDP),  #use returns
  kurtosis = moments::kurtosis(R_MDP) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(R_MDP, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(R_MDP, .95, method = 'historical'), #use returns
  MeanER = 12 * 100 * mean(dat1$MDP),  #annualized mean excess return,
  t = t.test(dat1$MDP)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat1$MDP), #annualized std excess return ,
  SR = sqrt(12) * mean(dat1$MDP) / sd(dat1$MDP),  #annualized Sharpe
  IR = unname(coef(m1_mdp)['(Intercept)']) / sd(residuals(m1_mdp)), # Information ratio (relative to CAPM)
  TR = mean(dat1$MDP) / unname(coef(m1_mdp)['MKT_RF']) # Treynor ratio
)  
summary_drp<-data.frame( 
  skewness = moments::skewness(R_DRP),  #use returns
  kurtosis = moments::kurtosis(R_DRP) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(R_DRP, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(R_DRP, .95, method = 'historical'), #use returns
  MeanER = 12 * 100 * mean(dat1$DRP),  #annualized mean excess return,
  t = t.test(dat1$DRP)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat1$DRP), #annualized std excess return ,
  SR = sqrt(12) * mean(dat1$DRP) / sd(dat1$DRP),  #annualized Sharpe
  IR = unname(coef(m1_drp)['(Intercept)']) / sd(residuals(m1_drp)), # Information ratio (relative to CAPM)
  TR = mean(dat1$DRP) / unname(coef(m1_drp)['MKT_RF']) # Treynor ratio
)  
summary_frp<-data.frame( 
  skewness = moments::skewness(R_FRP),  #use returns
  kurtosis = moments::kurtosis(R_FRP) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(R_FRP, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(R_FRP, .95, method = 'historical'), #use returns
  MeanER = 12 * 100 * mean(dat1$FRP),  #annualized mean excess return,
  t = t.test(dat1$FRP)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat1$FRP), #annualized std excess return ,
  SR = sqrt(12) * mean(dat1$FRP) / sd(dat1$FRP),  #annualized Sharpe
  IR = unname(coef(m1_frp)['(Intercept)']) / sd(residuals(m1_frp)), # Information ratio (relative to CAPM)
  TR = mean(dat1$FRP) / unname(coef(m1_frp)['MKT_RF']) # Treynor ratio
)  
summary_mv<-data.frame( 
  skewness = moments::skewness(R_MV),  #use returns
  kurtosis = moments::kurtosis(R_MV) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(R_MV, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(R_MV, .95, method = 'historical'), #use returns
  MeanER = 12 * 100 * mean(dat1$MV),  #annualized mean excess return,
  t = t.test(dat1$MV)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat1$MV), #annualized std excess return ,
  SR = sqrt(12) * mean(dat1$MV) / sd(dat1$MV),  #annualized Sharpe
  IR = unname(coef(m1_mv)['(Intercept)']) / sd(residuals(m1_mv)), # Information ratio (relative to CAPM)
  TR = mean(dat1$MV) / unname(coef(m1_mv)['MKT_RF']) # Treynor ratio
)  
summary_ew<-data.frame( 
  skewness = moments::skewness(R_EW),  #use returns
  kurtosis = moments::kurtosis(R_EW) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(R_EW, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(R_EW, .95, method = 'historical'), #use returns
  MeanER = 12 * 100 * mean(dat1$EW),  #annualized mean excess return,
  t = t.test(dat1$EW)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat1$EW), #annualized std excess return ,
  SR = sqrt(12) * mean(dat1$EW) / sd(dat1$EW),  #annualized Sharpe
  IR = unname(coef(m1_ew)['(Intercept)']) / sd(residuals(m1_ew)), # Information ratio (relative to CAPM)
  TR = mean(dat1$EW) / unname(coef(m1_ew)['MKT_RF']) # Treynor ratio
)  
knitr::kable(rbind(IV=summary_iv,RB=summary_rb,MDP=summary_mdp,
                   DRP=summary_drp,FRP=summary_frp,
                   MV=summary_mv,EW=summary_ew),digits = 3) 




# -------------------Time varying weights-------------------------------------------
wDRP <- data.frame(Date = as.Date(logret$Date[-(1:36)]),w_DRP[-c(1:36),])
wDRP <- wDRP %>% 
  gather("SYMBOL","WEIGHT",-Date)  %>%
  mutate(PORT = "DRP")

wEW <- data.frame(Date = as.Date(logret$Date[-(1:36)]),w_EW[-c(1:36),])
wEW <- wEW %>% 
  gather("SYMBOL","WEIGHT",-Date)  %>%
  mutate(PORT = "EW")

wIV <- data.frame(Date = as.Date(logret$Date[-(1:36)]),w_IV[-c(1:36),])
wIV <- wIV %>% 
  gather("SYMBOL","WEIGHT",-Date)  %>%
  mutate(PORT = "IV")  
  
wMV <- data.frame(Date = as.Date(logret$Date[-(1:36)]),w_MV[-c(1:36),])
wMV <- wMV %>% 
  gather("SYMBOL","WEIGHT",-Date)  %>%
  mutate(PORT = "MV") 

wMDP <- data.frame(Date = as.Date(logret$Date[-(1:36)]),w_MDP[-c(1:36),])
wMDP <- wMDP %>% 
  gather("SYMBOL","WEIGHT",-Date)  %>%
  mutate(PORT = "MDP")

wRB <- data.frame(Date = as.Date(logret$Date[-(1:36)]),w_RB[-c(1:36),])
wRB <- wRB %>% 
  gather("SYMBOL","WEIGHT",-Date)  %>%
  mutate(PORT = "RB")

wFRP <- data.frame(Date = as.Date(logret$Date[-(1:36)]),w_FRP[-c(1:36),])
wFRP <- wFRP %>% 
  gather("SYMBOL","WEIGHT",-Date)  %>%
  mutate(PORT = "FRP")

w <- bind_rows(wIV,wRB)
w <- bind_rows(w,wMDP)
w <- bind_rows(w,wDRP)
w <- bind_rows(w,wMV)
w <- bind_rows(w,wEW)
w <- bind_rows(w,wFRP)

#plot separately
ggplot(wDRP,
       aes(x=Date, y=WEIGHT, fill=SYMBOL)) +
  geom_area(position = 'stack')+ 
  theme(legend.position='bottom') + scale_fill_viridis_d() +
  ggtitle("Diversity Risk Parity")

ggplot(wMDP,
      aes(x=Date, y=WEIGHT, fill=SYMBOL)) +
  geom_area(position = 'stack')+ scale_fill_viridis_d() +
 theme(legend.position='bottom') + 
  ggtitle("Maximum Diversification")
ggplot(wRB,
      aes(x=Date, y=WEIGHT, fill=SYMBOL)) +
  geom_area(position = 'stack')+ scale_fill_viridis_d() +
 theme(legend.position='bottom') + 
  ggtitle("Risk Budget")
ggplot(wMV,
      aes(x=Date, y=WEIGHT, fill=SYMBOL)) +
  geom_area(position = 'stack')+ scale_fill_viridis_d() +
  theme(legend.position='bottom') + 
  ggtitle("Minimum Variance")

ggplot(wFRP,
       aes(x=Date, y=WEIGHT, fill=SYMBOL)) +
  geom_area(position = 'stack')+  scale_fill_viridis_d() +
  theme(legend.position='bottom') + 
  ggtitle("Factor Risk Parity")


#plot time-varying weights of all portfolios at once
ggplot(w %>% group_by(PORT), 
       aes(x=Date, y=WEIGHT, fill=SYMBOL)) + 
  scale_fill_viridis_d() +
  coord_trans() +
  facet_wrap(~PORT) +
  geom_area(position = 'stack') +
  theme(legend.position='bottom')


# ---------------------D. Rolling 60 month window backtesting----------------------------------------------------------------------
#Initialize time-series of weights
w_IV <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[, -1]), dimnames = list(NULL, colnames(logret[, -1])))
w_RB <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))
w_MDP <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))
w_DRP <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))
w_EW <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))
w_MV <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))
w_FRP <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))

# Estimate weights using 60 month telescope window
for (i in 61:nrow(logret)) {
  R <- logret[1:(i - 1), -1]
  mu <- apply(R, 2, mean)
  V <- cov(R)
  N <- ncol(R)
  w_RB[i,] <- risk_budgeting(V, rep(1/N, N))
  w_MDP[i,] <- maximum_diversification(R)
  w_DRP[i,] <- diversified_risk_parity(V)
  sigma2 <- diag(V)
  w_IV[i,] <- (1/sigma2)/(sum(1/sigma2))
  w_EW[i,] <- rep(1/N, N)
  w_MV[i,] <- min_var(V, shorts = FALSE)
}

# Estimate FRP weights using 60 month telescope window 
for (i in 61:nrow(excess_returns)) {
  ER <- excess_returns[1:(i - 1), ]
  M <- ncol(A)
  N <- ncol(V)
  b = rep(1 / M, M)
  assets <- colnames(select(ER, -Date, -MKT_RF, -SMB, -HML, -MOM, -RF))
  #factors <- c('MKT_RF', 'SMB', 'HML', 'MOM')
  factors <- c('MKT_RF', 'SMB')
  eqs1 <- setNames(lapply(assets, function(lhs, rhs) {
    as.formula(sprintf('%s~%s', lhs, paste(rhs, collapse = '+')))
  }, rhs = factors), assets)
  fit1 <- systemfit(eqs1, data = ER, method = 'OLS')
  A <- do.call(cbind, lapply(factors, function(fac) { coef(fit1)[paste0(assets, '_', fac)]}))
  dimnames(A) <- list(assets, factors)
  Omega <- cov(ER[, factors])
  D <- diag(apply(residuals(fit1), 2, var))
  dimnames(D) <- list(assets, assets)
  V <- A %*% Omega %*% t(A) + D
  dimnames(V) <- list(assets, assets)
  w_FRP[i,] <- factor_risk_budgeting(A, V, b)
}


# Calculate portfolio returns with time varying weights
eval_port <- function(w, R) {
  res <- matrix(0, nrow = nrow(R), ncol = 1)
  for (i in 1:nrow(R)) {
    res[i,] <- sum(w[i,]*R[i,])
  }
  res
}

#portfolio returns with time varying weights from rolling window
R_RB <- eval_port(w_RB, logret[,-1])[-(1:61)]
R_MDP <- eval_port(w_MDP, logret[,-1])[-(1:61)]
R_DRP <- eval_port(w_DRP, logret[,-1])[-(1:61)]
R_IV <- eval_port(w_IV, logret[,-1])[-(1:61)] 
R_EW <- eval_port(w_EW, logret[,-1])[-(1:61)]
R_MV <- eval_port(w_MV, logret[,-1])[-(1:61)]
R_FRP <- eval_port(w_FRP, logret[,-c(1)])[-(1:61)]

# Extract dates for x-axis of plot
T0 <- logret$Date[-(1:61)]

# Plot in sample cumulative returns
plot(x = T0, y = cumprod(1 + R_EW), type = 'l', col = 'blue', ylim = c(0.9, 8), ylab = 'Cumulative return', xlab = 'Date')
lines(x = T0, y = cumprod(1 + R_RB), col = 'red')
lines(x = T0, y = cumprod(1 + R_MDP), col = 'black')
lines(x = T0, y = cumprod(1 + R_DRP), col = 'green')
lines(x = T0, y = cumprod(1 + R_FRP), col = 'tan')
lines(x = T0, y = cumprod(1 + R_MV), col = 'turquoise')
lines(x = T0, y = cumprod(1 + R_IV), col = 'orange')
text =  c('Equal Weight', 'Risk Budgeting', 'Maximum Diversification',
          'Diversified Risk Parity','Factor Risk Parity',
          "Minimum Variance",'Inverse Volatility')
strwidth(text)
legend('topleft', cex = 0.75, c('Equal Weight', 'Risk Budgeting', 'Maximum Diversification',
                                'Diversified Risk Parity','Factor Risk Parity',
                                "Minimum Variance",'Inverse Volatility'), lty = 1,x.intersp=0.5,y.intersp=0.5,
       text.width=strwidth(text)[1]/2,
       col=c('blue', 'red', 'black','green','tan',
             'turquoise','orange'))
# Collect all returns into one data frame      
dat <- data_frame(Date = T0, 
                  IV = R_IV, 
                  RB = R_RB, 
                  MDP = R_MDP, 
                  DRP = R_DRP, 
                  FRP = R_FRP,
                  MV = R_MV,
                  EW = R_EW)

#Regression
# Merge returns and factors
dat1 <- merge(dat, car_m, by = 'Date')

# Calculate excess returns to portfolios
dat1[,2:8] <- dat1[,2:8] - dat1$RF


#--------------Summary statistics----------------------
# Estimate CAPM for portfolios
m1_iv <- lm(IV ~ MKT_RF, data = dat1)
summary(m1_iv)

m1_rb <- lm(RB ~ MKT_RF, data = dat1)
summary(m1_rb)

m1_mdp <- lm(MDP ~ MKT_RF, data = dat1)
summary(m1_mdp)

m1_drp <- lm(DRP ~ MKT_RF, data = dat1)
summary(m1_drp)

m1_mv <- lm(MV ~ MKT_RF, data = dat1)
summary(m1_mv)

m1_ew <- lm(EW ~ MKT_RF, data = dat1)
summary(m1_ew)

m1_frp <- lm(FRP ~ MKT_RF, data = dat1)
summary(m1_frp)

summary_iv<-data.frame( 
  skewness = moments::skewness(R_IV),  #use returns
  kurtosis = moments::kurtosis(R_IV) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(R_IV, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(R_IV, .95, method = 'historical'), #use returns
  MeanER = 12 * 100 * mean(dat1$IV),  #annualized mean excess return,
  t = t.test(dat1$IV)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat1$IV), #annualized std excess return ,
  SR = sqrt(12) * mean(dat1$IV) / sd(dat1$IV),  #annualized Sharpe
  IR = unname(coef(m1_iv)['(Intercept)']) / sd(residuals(m1_iv)), # Information ratio (relative to CAPM)
  TR = mean(dat1$IV) / unname(coef(m1_iv)['MKT_RF']) # Treynor ratio
)  

summary_rb<-data.frame( 
  skewness = moments::skewness(R_RB),  #use returns
  kurtosis = moments::kurtosis(R_RB) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(R_RB, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(R_RB, .95, method = 'historical'), #use returns
  MeanER = 12 * 100 * mean(dat1$RB),  #annualized mean excess return,
  t = t.test(dat1$RB)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat1$RB), #annualized std excess return ,
  SR = sqrt(12) * mean(dat1$RB) / sd(dat1$RB),  #annualized Sharpe
  IR = unname(coef(m1_rb)['(Intercept)']) / sd(residuals(m1_rb)), # Information ratio (relative to CAPM)
  TR = mean(dat1$RB) / unname(coef(m1_rb)['MKT_RF']) # Treynor ratio
)  
summary_mdp<-data.frame( 
  skewness = moments::skewness(R_MDP),  #use returns
  kurtosis = moments::kurtosis(R_MDP) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(R_MDP, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(R_MDP, .95, method = 'historical'), #use returns
  MeanER = 12 * 100 * mean(dat1$MDP),  #annualized mean excess return,
  t = t.test(dat1$MDP)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat1$MDP), #annualized std excess return ,
  SR = sqrt(12) * mean(dat1$MDP) / sd(dat1$MDP),  #annualized Sharpe
  IR = unname(coef(m1_mdp)['(Intercept)']) / sd(residuals(m1_mdp)), # Information ratio (relative to CAPM)
  TR = mean(dat1$MDP) / unname(coef(m1_mdp)['MKT_RF']) # Treynor ratio
)  
summary_drp<-data.frame( 
  skewness = moments::skewness(R_DRP),  #use returns
  kurtosis = moments::kurtosis(R_DRP) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(R_DRP, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(R_DRP, .95, method = 'historical'), #use returns
  MeanER = 12 * 100 * mean(dat1$DRP),  #annualized mean excess return,
  t = t.test(dat1$DRP)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat1$DRP), #annualized std excess return ,
  SR = sqrt(12) * mean(dat1$DRP) / sd(dat1$DRP),  #annualized Sharpe
  IR = unname(coef(m1_drp)['(Intercept)']) / sd(residuals(m1_drp)), # Information ratio (relative to CAPM)
  TR = mean(dat1$DRP) / unname(coef(m1_drp)['MKT_RF']) # Treynor ratio
)  
summary_mv<-data.frame( 
  skewness = moments::skewness(R_MV),  #use returns
  kurtosis = moments::kurtosis(R_MV) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(R_MV, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(R_MV, .95, method = 'historical'), #use returns
  MeanER = 12 * 100 * mean(dat1$MV),  #annualized mean excess return,
  t = t.test(dat1$MV)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat1$MV), #annualized std excess return ,
  SR = sqrt(12) * mean(dat1$MV) / sd(dat1$MV),  #annualized Sharpe
  IR = unname(coef(m1_mv)['(Intercept)']) / sd(residuals(m1_mv)), # Information ratio (relative to CAPM)
  TR = mean(dat1$MV) / unname(coef(m1_mv)['MKT_RF']) # Treynor ratio
)  
summary_ew<-data.frame( 
  skewness = moments::skewness(R_EW),  #use returns
  kurtosis = moments::kurtosis(R_EW) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(R_EW, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(R_EW, .95, method = 'historical'), #use returns
  MeanER = 12 * 100 * mean(dat1$EW),  #annualized mean excess return,
  t = t.test(dat1$EW)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat1$EW), #annualized std excess return ,
  SR = sqrt(12) * mean(dat1$EW) / sd(dat1$EW),  #annualized Sharpe
  IR = unname(coef(m1_ew)['(Intercept)']) / sd(residuals(m1_ew)), # Information ratio (relative to CAPM)
  TR = mean(dat1$EW) / unname(coef(m1_ew)['MKT_RF']) # Treynor ratio
) 
summary_frp<-data.frame( 
  skewness = moments::skewness(R_FRP),  #use returns
  kurtosis = moments::kurtosis(R_FRP) - 3,  #use returns
  VaR = PerformanceAnalytics::VaR(R_FRP, .95, method = 'historical') , #use returns
  ES = PerformanceAnalytics::ES(R_FRP, .95, method = 'historical'), #use returns
  MeanER = 12 * 100 * mean(dat1$FRP),  #annualized mean excess return,
  t = t.test(dat1$FRP)$statistic, 
  sdER = sqrt(12) * 100 * sd(dat1$FRP), #annualized std excess return ,
  SR = sqrt(12) * mean(dat1$FRP) / sd(dat1$FRP),  #annualized Sharpe
  IR = unname(coef(m1_frp)['(Intercept)']) / sd(residuals(m1_frp)), # Information ratio (relative to CAPM)
  TR = mean(dat1$FRP) / unname(coef(m1_frp)['MKT_RF']) # Treynor ratio
) 
knitr::kable(rbind(IV=summary_iv,RB=summary_rb,MDP=summary_mdp,
                   DRP=summary_drp,FRP=summary_frp,
                   MV=summary_mv,EW=summary_ew),digits = 3) 


#--------------------------------Regression--------------------------------
# Regression on Carhart factors
summary(lm(IV ~ MKT_RF + SMB + HML + MOM, data = dat1))
summary(lm(RB ~ MKT_RF + SMB + HML + MOM, data = dat1))
summary(lm(MDP ~ MKT_RF + SMB + HML + MOM, data = dat1))
summary(lm(DRP ~ MKT_RF + SMB + HML + MOM, data = dat1))
summary(lm(MV ~ MKT_RF + SMB + HML + MOM, data = dat1))
summary(lm(EW ~ MKT_RF + SMB + HML + MOM, data = dat1))
summary(lm(FRP ~ MKT_RF + SMB + HML + MOM, data = dat1))

#summary(lm(IV ~ MKT_RF + SMB + HML + MOM + BAB, data = dat1))
#summary(lm(RB ~ MKT_RF + SMB + HML + MOM + BAB, data = dat1))
#summary(lm(MDP ~ MKT_RF + SMB + HML + MOM + BAB, data = dat1))
#summary(lm(DRP ~ MKT_RF + SMB + HML + MOM + BAB, data = dat1))
#summary(lm(MV ~ MKT_RF + SMB + HML + MOM + BAB, data = dat1))
#summary(lm(EW ~ MKT_RF + SMB + HML + MOM + BAB, data = dat1))
#summary(lm(FRP ~ MKT_RF + SMB + HML + MOM + BAB, data = dat1))


# -------------------Time varying weights-------------------------------------------
wDRP <- data.frame(Date = as.Date(logret$Date[-(1:60)]),w_DRP[-c(1:60),])
wDRP <- wDRP %>% 
  gather("SYMBOL","WEIGHT",-Date)  %>%
  mutate(PORT = "DRP")

wEW <- data.frame(Date = as.Date(logret$Date[-(1:60)]),w_EW[-c(1:60),])
wEW <- wEW %>% 
  gather("SYMBOL","WEIGHT",-Date)  %>%
  mutate(PORT = "EW")

wIV <- data.frame(Date = as.Date(logret$Date[-(1:60)]),w_IV[-c(1:60),])
wIV <- wIV %>% 
  gather("SYMBOL","WEIGHT",-Date)  %>%
  mutate(PORT = "IV")  

wMV <- data.frame(Date = as.Date(logret$Date[-(1:60)]),w_MV[-c(1:60),])
wMV <- wMV %>% 
  gather("SYMBOL","WEIGHT",-Date)  %>%
  mutate(PORT = "MV") 

wMDP <- data.frame(Date = as.Date(logret$Date[-(1:60)]),w_MDP[-c(1:60),])
wMDP <- wMDP %>% 
  gather("SYMBOL","WEIGHT",-Date)  %>%
  mutate(PORT = "MDP")

wRB <- data.frame(Date = as.Date(logret$Date[-(1:60)]),w_RB[-c(1:60),])
wRB <- wRB %>% 
  gather("SYMBOL","WEIGHT",-Date)  %>%
  mutate(PORT = "RB")

wFRP <- data.frame(Date = as.Date(logret$Date[-(1:60)]),w_FRP[-c(1:60),])
wFRP <- wFRP %>% 
  gather("SYMBOL","WEIGHT",-Date)  %>%
  mutate(PORT = "FRP")

w <- bind_rows(wIV,wRB)
w <- bind_rows(w,wMDP)
w <- bind_rows(w,wDRP)
w <- bind_rows(w,wMV)
w <- bind_rows(w,wEW)
w <- bind_rows(w,wFRP)

#plot separately
ggplot(wDRP,
       aes(x=Date, y=WEIGHT, fill=SYMBOL)) +
  geom_area(position = 'stack')+ 
  theme(legend.position='bottom') + scale_fill_viridis_d() +
  ggtitle("Diversity Risk Parity")

ggplot(wMDP,
       aes(x=Date, y=WEIGHT, fill=SYMBOL)) +
  geom_area(position = 'stack')+ scale_fill_viridis_d() +
  theme(legend.position='bottom') + 
  ggtitle("Maximum Diversification")
ggplot(wRB,
       aes(x=Date, y=WEIGHT, fill=SYMBOL)) +
  geom_area(position = 'stack')+ scale_fill_viridis_d() +
  theme(legend.position='bottom') + 
  ggtitle("Risk Budget")
ggplot(wMV,
       aes(x=Date, y=WEIGHT, fill=SYMBOL)) +
  geom_area(position = 'stack')+ scale_fill_viridis_d() +
  theme(legend.position='bottom') + 
  ggtitle("Minimum Variance")

ggplot(wFRP,
       aes(x=Date, y=WEIGHT, fill=SYMBOL)) + ylim(-0.5,1) +
  geom_area(position = 'stack')+  scale_fill_viridis_d() +
  theme(legend.position='bottom') + 
  ggtitle("Factor Risk Parity")


#plot time-varying weights of all portfolios at once  
ggplot(w %>% group_by(PORT), 
       aes(x=Date, y=WEIGHT, fill=SYMBOL)) +
  scale_fill_viridis_d() +
  coord_trans() +
  facet_wrap(~PORT) +
  geom_area(position = 'stack') +
  theme(legend.position='bottom')


#percentage risk contribution
prc_EW <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))
prc_IV <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))
prc_DRP <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))
prc_MDP <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))
prc_RB <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))
prc_MV <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))
prc_FRP <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))



for (i in 61:nrow(logret)) {
  R <- logret[1:(i - 1), -1]
  V <- cov(R)
  w_EWi = w_EW[i,]
  w_IVi = w_IV[i,]
  w_DRPi = w_DRP[i,]
  w_MDPi = w_MDP[i,]
  w_RBi = w_RB[i,]
  w_MVi = w_MV[i,]
  w_FRPi = w_FRP[i,]
  prc_EW[i,] <- prc(w_EWi, V)
  prc_IV[i,] <- prc(w_IVi, V)
  prc_DRP[i,] <- prc(w_DRPi, V)
  prc_MDP[i,] <- prc(w_MDPi, V)
  prc_RB[i,] <- prc(w_RBi, V)
  prc_MV[i,] <- prc(w_MVi, V)
  prc_FRP[i,] <- prc(w_FRPi, V)
    }

prcEW <- data.frame(Date = as.Date(logret$Date[-(1:60)]),prc_EW[-c(1:60),])
prcEW <- prcEW %>% 
  gather("SYMBOL","PRC",-Date)  %>%
  mutate(PORT = "EW")
ggplot(prcEW,
       aes(x=Date, y=PRC, fill=SYMBOL)) +
  geom_area(position = 'stack')+ 
  theme(legend.position='bottom') + scale_fill_viridis_d() +
  ggtitle("EW")

prcIV <- data.frame(Date = as.Date(logret$Date[-(1:60)]),prc_IV[-c(1:60),])
prcIV <- prcIV %>% 
  gather("SYMBOL","PRC",-Date)  %>%
  mutate(PORT = "IV")
ggplot(prcIV,
       aes(x=Date, y=PRC, fill=SYMBOL)) +
  geom_area(position = 'stack')+ 
  theme(legend.position='bottom') + scale_fill_viridis_d() +
  ggtitle("IV")

prcDRP <- data.frame(Date = as.Date(logret$Date[-(1:60)]),prc_DRP[-c(1:60),])
prcDRP <- prcDRP %>% 
  gather("SYMBOL","PRC",-Date)  %>%
  mutate(PORT = "DRP")
ggplot(prcDRP,
       aes(x=Date, y=PRC, fill=SYMBOL)) +
  geom_area(position = 'stack')+ 
  theme(legend.position='bottom') + scale_fill_viridis_d() +
  ggtitle("DRP")

prcMDP <- data.frame(Date = as.Date(logret$Date[-(1:60)]),prc_MDP[-c(1:60),])
prcMDP <- prcMDP %>% 
  gather("SYMBOL","PRC",-Date)  %>%
  mutate(PORT = "MDP")
ggplot(prcMDP,
       aes(x=Date, y=PRC, fill=SYMBOL)) +
  geom_area(position = 'stack')+ 
  theme(legend.position='bottom') + scale_fill_viridis_d() +
  ggtitle("MDP")

prcRB <- data.frame(Date = as.Date(logret$Date[-(1:60)]),prc_RB[-c(1:60),])
prcRB <- prcRB %>% 
  gather("SYMBOL","PRC",-Date)  %>%
  mutate(PORT = "RB")
ggplot(prcRB,
       aes(x=Date, y=PRC, fill=SYMBOL)) +
  geom_area(position = 'stack')+ 
  theme(legend.position='bottom') + scale_fill_viridis_d() +
  ggtitle("RB")

prcMV <- data.frame(Date = as.Date(logret$Date[-(1:60)]),prc_MV[-c(1:60),])
prcMV <- prcMV %>% 
  gather("SYMBOL","PRC",-Date)  %>%
  mutate(PORT = "MinVar")
ggplot(prcMV,
       aes(x=Date, y=PRC, fill=SYMBOL)) +
  geom_area(position = 'stack')+ 
  theme(legend.position='bottom') + scale_fill_viridis_d() +
  ggtitle("MinVar")

prcFRP <- data.frame(Date = as.Date(logret$Date[-(1:60)]),prc_FRP[-c(1:60),])
prcFRP <- prcEW %>% 
  gather("SYMBOL","PRC",-Date)  %>%
  mutate(PORT = "FRP")
ggplot(prcFRP,
       aes(x=Date, y=PRC, fill=SYMBOL)) +
  geom_area(position = 'stack')+ 
  theme(legend.position='bottom') + scale_fill_viridis_d() +
  ggtitle("FRP")


#Gini coef
Gini(x, n = rep(1, length(x)), unbiased = TRUE,
     conf.level = 0.95, R = 1000, type = "bca", na.rm = FALSE)


#----------------------LEVERAGE-------------------------------------------















#Initialize time-series of weights
w_IV <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[, -1]), dimnames = list(NULL, colnames(logret[, -1])))
w_RB <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))
w_MDP <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))
w_DRP <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))
w_EW <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))
w_MV <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))
w_FRP <- matrix(0, nrow = nrow(logret), ncol = ncol(logret[,-1]), dimnames = list(NULL, colnames(logret[,-1])))


# Estimate weights using 12 month telescope window
for (i in 13:nrow(logret)) {
  R <- logret[1:(i - 1), -1]
  mu <- apply(R, 2, mean)
  V <- cov(R)
  N <- ncol(R)
  w_RB[i,] <- risk_budgeting(V, rep(1/N, N))
  w_MDP[i,] <- maximum_diversification(R)
  w_DRP[i,] <- diversified_risk_parity(V)
  sigma2 <- diag(V)
  w_IV[i,] <- (1/sigma2)/(sum(1/sigma2))
  w_EW[i,] <- rep(1/N, N)
  w_MV[i,] <- min_var(V, shorts = FALSE)
}


# Estimate FRP weights using 12 month telescope window 
for (i in 13:nrow(excess_returns)) {
  ER <- excess_returns[1:(i - 1), ]
  M <- ncol(A)
  N <- ncol(V1)
  b = rep(1 / M, M)
  assets <- colnames(select(ER, -Date, -MKT_RF, -SMB, -HML, -MOM, -RF))
  factors <- c('MKT_RF', 'SMB')
  eqs1 <- setNames(lapply(assets, function(lhs, rhs) {
    as.formula(sprintf('%s~%s', lhs, paste(rhs, collapse = '+')))
  }, rhs = factors), assets)
  fit1 <- systemfit(eqs1, data = ER, method = 'OLS')
  A <- do.call(cbind, lapply(factors, function(fac) { coef(fit1)[paste0(assets, '_', fac)]}))
  dimnames(A) <- list(assets, factors)
  Omega <- cov(ER[, factors])
  D <- diag(apply(residuals(fit1), 2, var))
  dimnames(D) <- list(assets, assets)
  # Estimate asset covariances using the factor model
  V1 <- A %*% Omega %*% t(A) + D
  dimnames(V1) <- list(assets, assets)
  w_FRP[i,] <- factor_risk_budgeting(A, V1, b)
}


#portfolio returns with time varying weights from rolling window
R_RB <- eval_port(w_RB, logret[,-1])[-(1:13)]
R_MDP <- eval_port(w_MDP, logret[,-1])[-(1:13)]
R_DRP <- eval_port(w_DRP, logret[,-1])[-(1:13)]
R_IV <- eval_port(w_IV, logret[,-1])[-(1:13)] 
R_EW <- eval_port(w_EW, logret[,-1])[-(1:13)]
R_MV <- eval_port(w_MV, logret[,-1])[-(1:13)]
R_FRP <- eval_port(w_FRP, logret[,-1])[-(1:13)]


# Extract dates for x-axis of plot
T0 <- logret$Date[-(1:13)]

# Plot in sample cumulative returns
plot(x = T0, y = cumprod(1 + R_EW), type = 'l', col = 'blue', ylim = c(0, 8), ylab = 'Cumulative return', xlab = 'Date')
lines(x = T0, y = cumprod(1 + R_RB), col = 'red')
lines(x = T0, y = cumprod(1 + R_MDP), col = 'black')
lines(x = T0, y = cumprod(1 + R_DRP), col = 'green')
lines(x = T0, y = cumprod(1 + R_FRP), col = 'tan')
lines(x = T0, y = cumprod(1 + R_MV), col = 'turquoise')
lines(x = T0, y = cumprod(1 + R_IV), col = 'orange')
text =  c('Equal Weight', 'Risk Budgeting', 'Maximum Diversification',
          'Diversified Risk Parity','Factor Risk Parity',
          "Minimum Variance",'Inverse Volatility')
strwidth(text)
legend('topleft', cex = 1, c('Equal Weight', 'Risk Budgeting', 'Maximum Diversification',
                             'Diversified Risk Parity','Factor Risk Parity',
                             "Minimum Variance",'Inverse Volatility'), lty = 1,x.intersp=0.5,y.intersp=0.5,
       text.width=strwidth(text)[1],
       col=c('blue', 'red', 'black','green','tan',
             'turquoise','orange'))
# Collect all returns into one data frame 
dat <- data_frame(Date = T0, 
                  IV = R_IV, 
                  RB = R_RB, 
                  MDP = R_MDP, 
                  DRP = R_DRP, 
                  FRP = R_FRP,
                  MV = R_MV,
                  EW = R_EW)

#Regression

# Merge returns and factors
dat1 <- merge(dat, car_m, by = 'Date')

# Calculate excess returns to portfolios
dat1[,2:8] <- dat1[,2:8] - dat1$RF

#--------------------------------Regression--------------------------------
# Regression on Carhart factors
summary(lm(IV ~ MKT_RF + SMB + HML + MOM, data = dat1))
summary(lm(RB ~ MKT_RF + SMB + HML + MOM, data = dat1))
summary(lm(MDP ~ MKT_RF + SMB + HML + MOM, data = dat1))
summary(lm(DRP ~ MKT_RF + SMB + HML + MOM, data = dat1))
summary(lm(FRP ~ MKT_RF + SMB + HML + MOM, data = dat1))
summary(lm(MV ~ MKT_RF + SMB + HML + MOM, data = dat1))
summary(lm(EW ~ MKT_RF + SMB + HML + MOM, data = dat1))


# -------------------Time varying weights-------------------------------------------
wDRP <- data.frame(Date = as.Date(logret$Date[-(1:36)]),w_DRP[-c(1:36),])
wDRP <- wDRP %>% 
  gather("SYMBOL","WEIGHT",-Date)  %>%
  mutate(PORT = "DRP")

wEW <- data.frame(Date = as.Date(logret$Date[-(1:36)]),w_EW[-c(1:36),])
wEW <- wEW %>% 
  gather("SYMBOL","WEIGHT",-Date)  %>%
  mutate(PORT = "EW")

wIV <- data.frame(Date = as.Date(logret$Date[-(1:36)]),w_IV[-c(1:36),])
wIV <- wIV %>% 
  gather("SYMBOL","WEIGHT",-Date)  %>%
  mutate(PORT = "IV")  

wMV <- data.frame(Date = as.Date(logret$Date[-(1:36)]),w_MV[-c(1:36),])
wMV <- wMV %>% 
  gather("SYMBOL","WEIGHT",-Date)  %>%
  mutate(PORT = "MV") 

wMDP <- data.frame(Date = as.Date(logret$Date[-(1:36)]),w_MDP[-c(1:36),])
wMDP <- wMDP %>% 
  gather("SYMBOL","WEIGHT",-Date)  %>%
  mutate(PORT = "MDP")

wRB <- data.frame(Date = as.Date(logret$Date[-(1:36)]),w_RB[-c(1:36),])
wRB <- wRB %>% 
  gather("SYMBOL","WEIGHT",-Date)  %>%
  mutate(PORT = "RB")

wFRP <- data.frame(Date = as.Date(logret$Date[-(1:36)]),w_FRP[-c(1:36),])
wFRP <- wFRP %>% 
  gather("SYMBOL","WEIGHT",-Date)  %>%
  mutate(PORT = "FRP")

w <- bind_rows(wIV,wRB)
w <- bind_rows(w,wMDP)
w <- bind_rows(w,wDRP)
w <- bind_rows(w,wMV)
w <- bind_rows(w,wEW)
w <- bind_rows(w,wFRP)

#plot separately
#ggplot(wIV,
#      aes(x=Date, y=WEIGHT, fill=SYMBOL)) +
#  geom_area(position = 'stack')+ scale_fill_viridis_d() +
#  theme(legend.position='bottom') + 
#  ggtitle("Inverse Volatility Portfolio")

ggplot(wDRP,
       aes(x=Date, y=WEIGHT, fill=SYMBOL)) +
  geom_area(position = 'stack')+ 
  theme(legend.position='bottom') + scale_fill_viridis_d() +
  ggtitle("Diversity Risk Parity")

ggplot(wMDP,
       aes(x=Date, y=WEIGHT, fill=SYMBOL)) +
  geom_area(position = 'stack')+ scale_fill_viridis_d() +
  theme(legend.position='bottom') + 
  ggtitle("Maximum Diversification")
ggplot(wRB,
       aes(x=Date, y=WEIGHT, fill=SYMBOL)) +
  geom_area(position = 'stack')+ scale_fill_viridis_d() +
  theme(legend.position='bottom') + 
  ggtitle("Risk Budget")
ggplot(wMV,
       aes(x=Date, y=WEIGHT, fill=SYMBOL)) +
  geom_area(position = 'stack')+ scale_fill_viridis_d() +
  theme(legend.position='bottom') + 
  ggtitle("Minimum Variance")
#ggplot(wEW,
#      aes(x=Date, y=WEIGHT, fill=SYMBOL)) +
#  geom_area(position = 'stack')+  scale_fill_viridis_d() +
#  theme(legend.position='bottom') + 
#  ggtitle("Equal Weights")
ggplot(wFRP,
       aes(x=Date, y=WEIGHT, fill=SYMBOL)) + 
  geom_area(position = 'stack')+  scale_fill_viridis_d() +
  theme(legend.position='bottom') + 
  ggtitle("Factor Risk Parity")


#plot time-varying weights of all portfolios at once   ----- to be continue
ggplot(w %>% group_by(PORT), 
       aes(x=Date, y=WEIGHT, fill=SYMBOL)) +
  scale_fill_viridis_d() +
  coord_trans() +
  facet_wrap(~PORT) +
  geom_area(position = 'stack') +
  theme(legend.position='bottom')

















