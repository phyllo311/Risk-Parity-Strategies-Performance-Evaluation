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







# Comparing Portfolios - to be continue


# risk -return measures:
# Sharpe ratio w t-value, p-value from robustness tests - Demo 6
library(dplyr)
library(readr)
library(lmtest)
library(sandwich)
library(gmm)

# We can test Sharpe ratios using Generalized Method of Moments with
# the gmm package. The idea here is to apply same parameterization as in
# Ledoit and Wolf (2008) and use gmm to estimate the parameter covariance
# matrix. Standard error for difference in Sharpe ratios is calculated
# using delta method.
#
# This function takes the excess returns of the two portfolios as input
# and returns in a list the difference in Sharpe ratios, the standard error
# and the t-statistic of the estimate.
sharpe_difference <- function(i1, n1) {
  
  # Moment conditions
  g <- function(t, x) cbind(t[1] - x[,1], t[2] - x[,2],
                            t[3] - x[,1]^2, t[4] - x[,2]^2)
  
  # Estimate parameters using GMM
  fit <- gmm(g = g, x = cbind(i1, n1), t0 = colMeans(cbind(i1, n1, i1^2, n1^2)))
  
  # Extract estimates
  v <- coef(fit)
  
  # Calculate Sharpe ratio difference
  delta <- v[1] / sqrt(v[3] - v[1]^2) - v[2] / sqrt(v[4] - v[2]^2)
  
  # Evaluate gradient
  gr <- c(v[3] / (v[3] - v[1]^2) ^ 1.5,
          -v[4] / (v[4] - v[2]^2) ^ 1.5,
          -0.5 * v[1] / (v[3] - v[1]^2) ^ 1.5,
          0.5 * v[2] / (v[4] - v[2]^2) ^ 1.5)
  
  # Estimate standard error
  s <- sqrt(as.numeric(t(gr) %*% vcovHAC(fit) %*% gr))
  
  # Return result as list
  list(sr_diff = delta, std_err = s, t_value = delta / s)
}

# Testing volatilities can be done similarly.
#
# This function takes the excess returns of the two portfolios as input
# and returns in a list the difference in volatilities, the standard error
# and the t-statistic of the estimate.
vol_difference <- function(i1, n1) {
  g <- function(t, x) cbind(t[1] - x[,1],   t[2] - x[,2],
                            t[3] - x[,1]^2, t[4] - x[,2]^2)
  
  fit <- gmm(g, cbind(i1, n1), colMeans(cbind(i1, n1, i1^2, n1^2)))
  
  v <- coef(fit)
  
  delta <- sqrt(v[3] - v[1]^2) - sqrt(v[4] - v[2]^2)
  
  gr <- c( v[1] / sqrt(v[3] - v[1]^2),
           -v[2] / sqrt(v[4] - v[2]^2),
           0.5 / sqrt(v[3] - v[1]^2),
           -0.5 / sqrt(v[4] - v[2]^2))
  
  s <- sqrt(as.numeric(t(gr) %*% vcovHAC(fit) %*% gr))
  
  list(vol_diff = delta, std_err = s, t_value = delta / s)
}

# Read in ETF returns
etfs <- read_csv('Demo_6.csv')

# Read in Carhart factors
fac <- read_csv('Carhart.csv')

# Calculate excess returns
exc <- inner_join(etfs, fac, by = c('DATE' = 'Date')) %>%
  mutate(ER = R - RF)

# Calculate Sharpe ratios
exc %>%
  group_by(SYMBOL) %>%
  summarise(Mean = mean(ER), Volatility = sd(ER), Sharpe = mean(ER) / sd(ER)) %>%
  ungroup()

# Extract excess returns
i1 <- exc %>% filter(SYMBOL == 'IWM') %>% pull(ER)
n1 <- exc %>% filter(SYMBOL == 'SPY') %>% pull(ER)

# Test Sharpe ratios
sharpe_difference(i1, n1)

# Test volatilities
vol_difference(i1, n1)




#Sharpe, Carhart, Fung-Hsien EFR 2
sharpe_ratios <- equally_weighted %>%
  group_by(MainStrategy) %>%
  summarise(
    `Mean ER` = 12 * 100 * mean(ER),
    `Volatility` = sqrt(12) * 100 * sd(ER),
    `SR` = sqrt(12) * mean(ER) / sd(ER)
  )



carhart_fits <- equally_weighted %>%
  group_by(MainStrategy) %>%
  do({
    fit <- lm(ER ~ Mkt_RF + SMB + HML + Mom, data = .)
    est <- coef(fit)
    est['(Intercept)'] <- 12 * 100 * est['(Intercept)']
    names(est)[1] <- 'Alpha'
    res <- cbind(data_frame(`Main strategy` = c(.$MainStrategy[1], NA)),
                 rbind(est, coef(fit) / sqrt(diag(vcov(fit)))),
                 data_frame('$R^2$' = c(100 * summary(fit)$r.squared, NA)))
    res
  }) %>%
  ungroup() %>%
  select(-MainStrategy)

carhart_fits

write_csv(carhart_fits, 'carhart_fits.csv')

funghsieh_fits <- equally_weighted %>%
  group_by(MainStrategy) %>%
  do({
    fit <- lm(ER ~ PTFSBD + PTFSFX + PTFSCOM + SP + SCLC + CGS10 + CREDSPR, data = .)
    est <- coef(fit)
    est['(Intercept)'] <- 12 * 100 * est['(Intercept)']
    names(est)[1] <- 'Alpha'
    res <- cbind(data_frame(`Main strategy` = c(.$MainStrategy[1], NA)),
                 rbind(est, coef(fit) / sqrt(diag(vcov(fit)))),
                 data_frame('$R^2$' = c(100 * summary(fit)$r.squared, NA)))
    res
  }) %>%
  ungroup() %>%
  select(-MainStrategy)

funghsieh_fits

write_csv(funghsieh_fits, 'funghsieh_fits.csv')


# Measure diversification: Gini Coef, time varying weights


#Leverage portfolio


# rb_weights <- assets %>%
#   spread(SYMBOL, RET) %>%
#   do({
#     tmp <- rollApply(data=select(., -DATE, -MONTH), window=60, align='right', fun=function(x) {
#       if (nrow(x) < 60) {
#         setNames(rep(NA, ncol(x)+1), c(colnames(x), 'LEVERAGE'))
#       } else {
#         N <- ncol(x)
#         V <- cov(x)
#         b <- ifelse(grepl('TOT', colnames(x)), 0.3, 0.7)
#         b <- b/sum(b)
#         res <- nlminb(start=rep(1/N, N),
#                       objective=function(w, Sigma, b) 0.5*as.numeric(t(w)%*%Sigma%*%w)-as.numeric(crossprod(b,log(w))),
#                       lower=rep(0,N),
#                       upper=rep(+Inf,N),
#                       Sigma=V,
#                       b=b)
#         w <- setNames(res$par/sum(res$par), colnames(x))
#         res <- nlminb(start=c(0),
#                       objective=function(x, N, w, Sigma) {
#                         sigma_ew <- sqrt(as.numeric(t(rep(1/N,N))%*%Sigma%*%rep(1/N,N)))
#                         sigma_lev <- sqrt(as.numeric(t((1+x)*w)%*%Sigma%*%((1+x)*w)))
#                         (sigma_ew-sigma_lev)^2
#                       },
#                       N=N,
#                       w=w,
#                       Sigma=V)
#         leverage <- res$par
#         return(c(w, LEVERAGE=leverage))
#       }
#     })
#     cbind(select(., MONTH), as_data_frame(t(tmp)))
#   }) %>%
#   mutate(MONTH=MONTH+1) %>%
#   gather(SYMBOL, WEIGHT, -MONTH, -LEVERAGE)




#plot weights of rolling window portfolios
ggplot(assets %>%
         left_join(minvar_weights, by=c('SYMBOL', 'MONTH')) %>%
         filter(!is.na(WEIGHT)),
       aes(x=DATE, y=WEIGHT, color=SYMBOL, fill=SYMBOL)) +
  geom_area(position='stack') +
  theme(legend.position='bottom')




#to be continue
#Portfolio evaluation: Mean, Vol, Sharpe   #see demo 3 for IR, TR...

#group returns of portfolio into 1 dataframe

#Excess returns of portfolio

# Calculate excess returns by subtracting risk-free rate
dat1[,2:10] <- dat1[,2:10] - dat1[,'RF']

# Estimate Carhart model for each portfolio
m <- lm(R_IV ~ MKT_RF + SMB + HML + MOM, data = dat1)
summary(m)

m <- lm(R_IV ~ MKT_RF + SMB + HML + MOM, data = dat1)
summary(m)




# Estimate CAPM for XLF
m1 <- lm(R_IV ~ MKT_RF, data = dat1)
summary(m1)


# IR, VAR, Expected Shortfall, Max Drawdown


# Calculate alpha, beta and omega (idiosyncratic volatility) relative to CAPM
alpha <- unname(coef(m1)['(Intercept)'])
beta <- unname(coef(m1)['MKT_RF'])
omega <- sd(residuals(m1))

# Sharpe ratio
mean(dat1$XLF) / sd(dat1$XLF)

# Information ratio (relative to CAPM)
alpha / omega

# Treynor ratio
mean(dat1$XLF) / beta

# This function calculates Value-at-Risk for given confidence level alpha and nominal investment
# using either historical or normal approximation
VaR <- function(x, alpha = 0.95, nominal = 1e5, method = c('historical', 'normal')) {
  method <- match.arg(method)
  loss <- -x
  if (method == 'historical') {
    n <- length(loss)
    m <- ceiling(n * (1 - alpha))
    return(-nominal * sort(loss, decreasing = TRUE)[m])
  } else if (method == 'normal') {
    mu <- mean(loss)
    sigma <- sd(loss)
    return(-nominal * (mu + sigma * qnorm(alpha)))
  } else {
    stop('Method', method, 'not recognized')
  }
}

# This function calculates expected shortfall for given confidence level alpha and nominal investment
# using either historical or normal approximation
ES <- function(x, alpha = 0.95, nominal = 1e5, method = c('historical', 'normal')) {
  method <- match.arg(method)
  loss <- -x
  if (method == 'historical') {
    n <- length(loss)
    m <- ceiling(n * (1 - alpha))
    return(-nominal * mean(sort(loss, decreasing = TRUE)[1:m]))
  } else if (method == 'normal') {
    mu <- mean(loss)
    sigma <- sd(loss)
    return(-nominal * (mu + sigma * dnorm(qnorm(alpha)) / (1 - alpha)))
  } else {
    stop('Method', method, 'not recognized')
  }
}

# This function calculates high-water mark (cumulative maximum of NAV) based on portfolio returns
HighWaterMark <- function(x) {
  cummax(cumprod(1 + x))
}

# This function calculates the drawdown (cumulative loss since losses started) based on portfolio returns
DrawDown <- function(x) {
  cum_ret <- cumprod(1 + x)
  max_cum_ret <- cummax(cum_ret)
  drawdown <- (max_cum_ret - cum_ret) / max_cum_ret
  return(drawdown)
}

# Use the provided function to estimate Value-at-Risk for XLF
VaR(dat$XLF, method = 'historical')
VaR(dat$XLF, method = 'normal')

# Use the provided function to estimate expected shortfall for XLF
ES(dat$XLF, method = 'historical')
ES(dat$XLF, method = 'normal')

# Calculate maximum drawdown for XLF
max(DrawDown(dat$XLF))

# Plot cumulative return, high-water mark and drawdown for XLF
plot(x = dat$Date, y = cumprod(1 + dat$XLF), type = 'l', col = 'blue', ylim = c(0, 3), xlab = 'Date', ylab = 'Cumulative return')
lines(x = dat$Date, y = HighWaterMark(dat$XLF), col = 'red')
lines(x = dat$Date, y = DrawDown(dat$XLF), col = 'black')
legend('topleft', c('CR', 'HWM', 'DD'), col = c('blue', 'red', 'black'), lty = 1)



#robust test of Sharpe ratio (Demo 6)
library(dplyr)
library(readr)
library(lmtest)
library(sandwich)
library(gmm)

# We can test Sharpe ratios using Generalized Method of Moments with
# the gmm package. The idea here is to apply same parameterization as in
# Ledoit and Wolf (2008) and use gmm to estimate the parameter covariance
# matrix. Standard error for difference in Sharpe ratios is calculated
# using delta method.
#
# This function takes the excess returns of the two portfolios as input
# and returns in a list the difference in Sharpe ratios, the standard error
# and the t-statistic of the estimate.
sharpe_difference <- function(i1, n1) {
  
  # Moment conditions
  g <- function(t, x) cbind(t[1] - x[,1], t[2] - x[,2],
                            t[3] - x[,1]^2, t[4] - x[,2]^2)
  
  # Estimate parameters using GMM
  fit <- gmm(g = g, x = cbind(i1, n1), t0 = colMeans(cbind(i1, n1, i1^2, n1^2)))
  
  # Extract estimates
  v <- coef(fit)
  
  # Calculate Sharpe ratio difference
  delta <- v[1] / sqrt(v[3] - v[1]^2) - v[2] / sqrt(v[4] - v[2]^2)
  
  # Evaluate gradient
  gr <- c(v[3] / (v[3] - v[1]^2) ^ 1.5,
          -v[4] / (v[4] - v[2]^2) ^ 1.5,
          -0.5 * v[1] / (v[3] - v[1]^2) ^ 1.5,
          0.5 * v[2] / (v[4] - v[2]^2) ^ 1.5)
  
  # Estimate standard error
  s <- sqrt(as.numeric(t(gr) %*% vcovHAC(fit) %*% gr))
  
  # Return result as list
  list(sr_diff = delta, std_err = s, t_value = delta / s)
}

# Testing volatilities can be done similarly.
#
# This function takes the excess returns of the two portfolios as input
# and returns in a list the difference in volatilities, the standard error
# and the t-statistic of the estimate.
vol_difference <- function(i1, n1) {
  g <- function(t, x) cbind(t[1] - x[,1],   t[2] - x[,2],
                            t[3] - x[,1]^2, t[4] - x[,2]^2)
  
  fit <- gmm(g, cbind(i1, n1), colMeans(cbind(i1, n1, i1^2, n1^2)))
  
  v <- coef(fit)
  
  delta <- sqrt(v[3] - v[1]^2) - sqrt(v[4] - v[2]^2)
  
  gr <- c( v[1] / sqrt(v[3] - v[1]^2),
           -v[2] / sqrt(v[4] - v[2]^2),
           0.5 / sqrt(v[3] - v[1]^2),
           -0.5 / sqrt(v[4] - v[2]^2))
  
  s <- sqrt(as.numeric(t(gr) %*% vcovHAC(fit) %*% gr))
  
  list(vol_diff = delta, std_err = s, t_value = delta / s)
}

# Read in ETF returns
etfs <- read_csv('Demo_6.csv')

# Read in Carhart factors
fac <- read_csv('Carhart.csv')

# Calculate excess returns
exc <- inner_join(etfs, fac, by = c('DATE' = 'Date')) %>%
  mutate(ER = R - RF)

# Calculate Sharpe ratios
exc %>%
  group_by(SYMBOL) %>%
  summarise(Mean = mean(ER), Volatility = sd(ER), Sharpe = mean(ER) / sd(ER)) %>%
  ungroup()

# Extract excess returns
i1 <- exc %>% filter(SYMBOL == 'IWM') %>% pull(ER)
n1 <- exc %>% filter(SYMBOL == 'SPY') %>% pull(ER)

# Test Sharpe ratios
sharpe_difference(i1, n1)

# Test volatilities
vol_difference(i1, n1)








# Mean, Vol, Sharpe using plain excess return - Read in Carhart factors
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

