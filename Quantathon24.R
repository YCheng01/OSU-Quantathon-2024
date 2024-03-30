library(tidyverse)


# Data WTI Oil
CLF_data_1 <- read.csv("~/Downloads/CLF_data_1.csv")
WTI <- CLF_data_1
# Data of Delta Airline
DAL_data_1 <- read.csv("~/Downloads/DAL_data_1.csv")
DAL <- DAL_data_1

# Change to date format
WTI$Date <-as.Date(WTI$Date, format = "%Y-%m-%d")
DAL$Date <-as.Date(DAL$Date, format = "%Y-%m-%d")


# 1. Plot of high price of WTI Oil
plot(x = WTI$Date,
     y = WTI$High,
     type = 'l',
     ylab = 'Price',
     xlab = 'Date',
     main = 'Daily Price of WTI Oil and Delta Stock')
# Combine with high price of Delta Stock
lines(x = DAL$Date,
      y = DAL$High,
      type = 'l',
      col = 'red')
legend(x = 'topleft',
       legend = c('WTI Oil',
                  'Delta Stock'),
       fill = c('black','red'))



# Combine 2 dataset (.x = DAL, .y = WTI)
mydata <- full_join(x = DAL,
                    y = WTI,
                    by = 'Date')
mydata <- na.omit(mydata)
mydata1 <- na.omit(mydata)
rm(mydata)
mydata1[,'Volume.x'] <- mydata1[,'Volume.x']/1000
mydata1[,'Volume.y'] <- mydata1[,'Volume.y']/1000


# linear model of DAL ~ WTI + volume of DAL + volume of WTI
lmLowx <- lm(Low.x ~ Low.y + Volume.x + Volume.y,
             data = mydata1)
summary(lmLowx)
coef(lmLowx)
# Check assumptions
plot(lmLowx)

#Plot the relationship
with(mydata1, plot(x = coef(lmLowx)[1]+
                    coef(lmLowx)[2]*Low.y+
                    coef(lmLowx)[3]*Volume.x+
                    coef(lmLowx)[4]*Volume.y,
                  y = High.x,
                  pch = 20,
                  col = alpha('black',.4),
                  xlim = c(5,65),
                  ylim = c(5,65),
                  xlab = 'Estimated Delta Stock Price',
                  ylab = 'Actual Delta Stock Price',
                  main = 'Visualization of the Estimated and Fitted Value'))
lines(x = c(0:70),
      y = c(0:70),
      lty = 'dashed',
      col = 'red')

# Plot of volumes
layout(matrix(c(1,2),
              ncol = 2,
              nrow = 1,
              byrow = TRUE))
with(mydata1, plot(x = Date,
                  y = Volume.x,
                  xlab = 'Date',
                  ylab = 'Traded Delta Stock Volume (in 10^3)',
                  type = 'l'))
abline(h = mean(mydata1$Volume.x, na.rm = TRUE),
       col="red", lwd=3, lty= 2)
with(mydata1, plot(x = Date,
                  y = Volume.y,
                  xlab = 'Date',
                  ylab = 'Traded Oil Volume (in 10^3)',
                  type = 'l'))
dev.off()

# add day of the week
mydata1 <- mydata1 %>% 
  mutate(wday = wday(Date)-1) %>% 
  mutate(mday = mday(Date))



# Differencing to remove seasonality to get a stationary thing
with(mydata1, plot(x = Date,
                  y = Volume.y,
                  xlab = 'Date',
                  ylab = 'Traded Oil Volume (in 10^3)',
                  type = 'l'))
plot.ts(diff(mydata1$Volume.y,lag =1),
        ylab = 'Difference',
        main = 'Differencing Between Traded Oil Volume w/ lag = 1')
acf(diff(na.omit(mydata1$Volume.y)), lag.max = 12,
    ylab = 'Autocovariance Function',
    main = 'ACF Plot')
acf(diff(na.omit(mydata1$Volume.y)), lag.max = 12,plot = FALSE)


# mday for oil
na.omit(mydata1) %>% 
  group_by(mday) %>% 
  summarize(mean = mean(Volume.y)) %>% 
  plot(xlab = 'Day of the Month',
       ylab = 'Average Traded Volume (in 10^3)',
       main = 'Average of Traded Volume in Each Day of the Month',
       pch = 17)












rm(list = ls())









