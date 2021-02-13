setwd("C:/Users/phuongntx32510/Downloads/CEL")
library(quantmod)
library(fBasics)
library(timeSeries)
library(fGarch)
library(Rmetrics)
library(tseries)
library( fArma )
library(CombMSC)
library(TSstudio)
library(FinTS)
library(foreach)
library(dplyr)
library(ggpubr)
library(predict.Arima)
library(forecast)
library(RColorBrewer)
library(naniar)
library(forecast)
library(ggplot2)
library(xgboost)
library(readr)
library(stringr)
library(caret)
############################################
######                     #################
###### CREATING FUNCTION   #################
######                     #################
############################################


############################################
####     EXPLORING DATA    #################
############################################


#read data
df <- read.csv('physical_network.csv')
#summary(df)
#str(df)

#delect n/a column
df_1 <- subset(df,select = -origin_simple)
max(as.numeric(as.character(df_1$Order.Date)))

#analysed data
df_t <- subset(df,select = c('Order.Date','destination_simple','Commodity.Group',
                             'Quantity.Ordered'))

############################################
######     SUBSETTING DATA    ##############
############################################

df_t_3 <- subset(df_t,is.na(Commodity.Group))
df_t_4 <- subset(df_t,Commodity.Group == 'Skim Milk Powder')
df_t_5 <- subset(df_t,Commodity.Group == 'Liquid Milk')
df_t_6 <- df_t_3[df_t_3$destination_simple == 'Facilities',]
df_t_7 <- df_t_3[df_t_3$destination_simple == 'Customers',]
df_t_8 <- df_t_4[df_t_4$destination_simple == 'Facilities',]
df_t_9 <- df_t_4[df_t_4$destination_simple == 'Customers',]
df_t_10 <- df_t_5[df_t_5$destination_simple == 'Facilities',]
df_t_11 <- df_t_5[df_t_5$destination_simple == 'Customers',]

max_8 <- max(as.Date(as.character(df_t_8$Order.Date)))
min_8 <- min(as.Date(as.character(df_t_8$Order.Date)))
max_9 <- max(as.Date(as.character(df_t_9$Order.Date)))
min_9 <- min(as.Date(as.character(df_t_9$Order.Date)))
max_10 <- max(as.Date(as.character(df_t_10$Order.Date)))
min_10 <- min(as.Date(as.character(df_t_10$Order.Date)))
max_11 <- max(as.Date(as.character(df_t_11$Order.Date)))
min_11 <- min(as.Date(as.character(df_t_11$Order.Date)))

df_t_9_s <- df_t_9[!df_t_9$Order.Date == '1899-12-30',]
df_t_11_s <- df_t_11[!df_t_11$Order.Date == '1899-12-30',]

#group_by date and transform to time series data:
transform_ts <- function(df_t){
  df_t_grp <- aggregate(df_t['Quantity.Ordered'], by=df_t['Order.Date'], FUN=sum)
  df_t_grp$Order.Date <- as.Date(df_t_grp$Order.Date)
  rownames(df_t_grp) <- df_t_grp$Order.Date
  df_t_grp <- subset(df_t_grp,select = c('Order.Date',
                       'Quantity.Ordered'))
  df_t_grp <- xts(df_t_grp$Quantity.Ordered,order.by=df_t_grp$Order.Date,
                  born = '2016-01-01')
  return(df_t_grp)
}
df_t <- df_t_8
#transform diff to time series
transform_diff_ts <- function(df_t){
  df_t_grp <- aggregate(df_t['Quantity.Ordered'], by=df_t['Order.Date'], FUN=sum)
  n <- nrow(df_t_grp)
  df_t_grp$Quantity_diff_1 <- rep(0,n)
  for (i in 2: n){
    df_t_grp$Quantity_diff_1[i] <- (df_t_grp$Quantity.Ordered[i]-
                                      df_t_grp$Quantity.Ordered[i-1])
  }
  df_t_grp$Quantity_diff_1[1] <- 0
  df_t_grp$Order.Date <- as.Date(df_t_grp$Order.Date)
  
  
  rownames(df_t_grp) <- df_t_grp$Order.Date
  df_t_grp <- subset(df_t_grp,select = 
                       'Quantity_diff_1')
  df_t_grp <- ts(df_t_grp,start = 2016, frequency = 365.25)
  #autoplot(df_t_grp)
  # ,frequency=365.25)
  #df_t_grp <- df_t_grp[order(sort(df_t_grp)),]
  x <- rownames(df_t_grp)
  df_t_grp <- xts(df_t_grp,order.by= x)
  return(df_t_grp)
}

df_8_diff <- transform_diff_ts(df_t_8)
df_8_grp <- transform_ts(df_t_8)
df_9_grp <- transform_ts(df_t_9_s)
df_10_grp <- transform_ts(df_t_10)
df_11_grp <- transform_ts(df_t_11_s)

############################################
############################################
######     TIME SERIES DAILY PLOT    #######
############################################
############################################

#plot daily values
png(filename="daily_%03d.png",width = 673, height = 354)
autoplot(df_8_grp) +
  ggtitle("Time series of Skim Milk Powder of Facilities") +
  xlab("Year") +
  ylab("Quantity Ordered")

autoplot(df_9_grp) +
  ggtitle("Time series of Skim Milk Powder of Customers") +
  xlab("Year") +
  ylab("Quantity Ordered")

autoplot(df_10_grp) +
  ggtitle("Time series of Liquid Milk Powder of  Facilities") +
  xlab("Year") +
  ylab("Quantity Ordered")

autoplot(df_11_grp) +
  ggtitle("Time series of Liquid Milk Powder of Customers") +
  xlab("Year") +
  ylab("Quantity Ordered")
dev.off()

############################################
############################################
######     TIME SERIES MONTHLY PLOT    #####
############################################
############################################
monthly_8 <- apply.monthly(df_8_grp, sum)
monthly_9 <- apply.monthly(df_9_grp, sum)
monthly_10 <- apply.monthly(df_10_grp, sum)
monthly_11 <- apply.monthly(df_11_grp, sum)

#plot daily values
png(filename="Monthly_%03d.png",width = 673, height = 354)
autoplot(monthly_8) +
  ggtitle("Monthly data: Time series of Skim Milk Powder of Facilities") +
  xlab("Month.Year") +
  ylab("Quantity Ordered")

autoplot(monthly_9) +
  ggtitle("Monthly data: Time series of Skim Milk Powder of Customers") +
  xlab("Month.Year") +
  ylab("Quantity Ordered")

autoplot(monthly_10) +
  ggtitle("Monthly data: Time series of Liquid Milk Powder of  Facilities") +
  xlab("Month.Year") +
  ylab("Quantity Ordered")

autoplot(monthly_11) +
  ggtitle("Monthly data: Time series of Liquid Milk Powder of Customers") +
  xlab("Month.Year") +
  ylab("Quantity Ordered")
dev.off()

#Plot time series pattern
png(filename="seasonal_%03d.png",width = 673, height = 354)
ggseasonplot(ts(monthly_8), month.labels=TRUE, month.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: Skim Milk Powder of Facilities")

ggseasonplot(df_t_9_s, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: Skim Milk Powder of Customers")

ggseasonplot(df_10_grp, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: Liquid Milk Powder of  Facilities")

ggseasonplot(df_11_grp, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: Liquid Milk Powder of Customers")
dev.off()

monthly_8 <- apply.monthly(df_8_grp, mean)
df_8_grp %>% group_by(year,month) %>% summarize(sum = sum(num)) %>% as.data.frame


ggseasonplot(ts(monthly_8), polar=TRUE) +
  ylab("$ million") +
  ggtitle("Polar seasonal plot: antidiabetic drug sales")

ggsubseriesplot(df_8_grp) +
  ylab("$ million") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")
dev.off()
#lags plot
lag_dt8 <- window(monthly_8, start='2016-01-01')
gglagplot(lag_dt8)

#acf
ggAcf(monthly_8,lag = 48)
acf(monthly_8,lag = 48)
acf(df_8_grp,lag=48)

#trend and seasonality of acf plot
aelec <- window(df_8_grp, start='2016-01-01')
autoplot(aelec) + xlab("Year") + ylab("GWh")


############################################
############################################
######     TESTING STATIONARY    ###########
############################################
############################################

#test stationary
dat <- df_8_grp
stationary <- function(dat) {
  i <- 1
  if (adf.test(dat,alternative='stationary')$p.value <=0.05 & 
      kpss.test(dat,null=c("Level","Trend"))$p.value > 0.05) {
    print('stationary')
  } 
  else {
    print('not stationary')
    while(1){
      diff_s <- diff(dat)
      print(i)
      if (adf.test(diff_s[2:length(diff_s)],alternative='stationary')$p.value <=0.05 & 
          kpss.test(diff_s[2:length(diff_s)],null=c("Level","Trend"))$p.value > 0.05) {
        print('stationary') & return(list(dat[2:length(diff_s)],TRUE)) & 
          acf(dat[2:length(diff_s)]) & pacf(dat[2:length(diff_s)])
        break
      }
      print('not stationary') & return(list(dat[2:nrow(diff_s)],FALSE))
      i <- i + 1
    }
  }
  
  #if (i>1){dat=log_s} else {dat=diff(log_s)}
  return(i)
}


stationary_df_8 <- stationary(df_8_grp) #d=1 m???i stationary
stationary_df_9 <- stationary(df_9_grp) #d = 1 m???i stationary
stationary_df_10 <- stationary(df_10_grp) #d = 1 m???i stationary
stationary_df_11 <- stationary(df_11_grp) #d = 1 m???i stationary

############################################
############################################
######     SPLIT TRAIN AND TEST    #########
############################################
############################################
df_8_grp.ts <- ts(df_8_grp)
diff_8 <- diff(df_8_grp.ts)
split <- splitTrainTest(diff_8, numTrain = length(diff_8)*0.8)

train_8 <- split$train
test_8 <- split$test

############################################
######     fIT ARIMA    ####################
############################################

library(vctrs)
library(forecast)
library(FinTS)
m1 <- auto.arima(diff(train_8))

fit_train_arima <- forecast(m1)
fit_test_arima <- forecast(diff(test_8),model = m1)

accuracy(fit_test_arima$fitted[14:110],diff(test_8)[14:110])
accuracy(fit_train_arima$fitted[14:447],diff(train_8)[14:447])

checkresiduals(m1)
length(fit_train_arima$fitted)
qqnorm(m1$residuals)
plot.zoo(cbind(fit_train_arima$fitted,diff(train_8)),
         plot.type = "single", 
         col = c("red", "blue"))

plot.zoo(cbind(fit_test_arima$fitted,diff(test_8)),
         plot.type = "single", 
         col = c("red", "blue"))


############################################
######     ETS method    ###################
############################################

train_8 <- window(train_8)
h <- length(test_8)
m2 <- ets(diff(train_8))

fit_train_ets <- forecast(m2)
fit_test_ets <- forecast(h = length(diff(test_8)),m2, 
                         bootstrap = T, level = 0.95, lamda = 'auto')

length(fit_train_ets$fitted)
fit_train_ets$fitted[1]
length(fit_test_ets$upper)
fit_train_ets$x[1]

plot.zoo(cbind( fit_test_ets$upper,fit_test_ets$lower ,diff(test_8)),
         plot.type = "single", 
         col = c("green","yellow","black"))

checkresiduals(m2)
length(fit_test_arima$fitted)
qqnorm(m2$residuals)
plot.zoo(cbind(fit_test_ets$fitted,diff(train_8)),
         plot.type = "single", 
         col = c("red", "blue"))

fit_test_ets <- predict(m2,diff(test_8))

ETS = diff(train_8) %>% 
  ets() %>% 
  forecast(h = length(diff(test_8)), bootstrap = T, level = 0.89, lamda = 'auto')

length(ETS)
ETS %>% 
  forecast_eval()

############################################
######     NEURAL NETWORK   ################
############################################
df_8_grp.ts <- ts(df_8_grp)
diff_8 <- diff(df_8_grp.ts)
diff_8_std <- standardize(diff_8)
split <- splitTrainTest(diff_8_std, numTrain = length(diff_8)*0.8)

train_8 <- split$train
test_8 <- split$test

m3 <- nnetar(train_8)

fit_train_nnet <- forecast(m3)
fit_test_nnet <- forecast(diff(test_8),model = m3,
                          use.initial.values=TRUE, 
                          start = length(diff(train_8) - 13))

accuracy(fit_test_nnet$fitted[14:110],diff(test_8)[14:110])
accuracy(fit_train_nnet$fitted[14:447],diff(train_8)[14:447])

checkresiduals(m3)
length(fit_train_nnet$fitted)
length(fit_test_nnet$fitted)
qqnorm(m3$residuals)

plot.zoo(cbind(fit_train_nnet$fitted,fit_test_nnet$fitted,diff(test_8)),
         plot.type = "single", 
         col = c("red", "blue","black"))

plot.zoo(cbind(fit_test_nnet$fitted,diff(test_8)),
         plot.type = "single", 
         col = c("red", "blue"))

mape <- mean(abs((diff(train_8[14:447]) - fit_train_nnet$fitted[14:447])/
                   diff(train_8[14:447])))*100


mape
############################################
######     TBATS method    #################
############################################
m4 <- tbats(train_8)

fit_train_tbat <- forecast(m4)
fit_test_tbat <- forecast(diff(test_8),model = m4)

accuracy(fit_test_tbat$fitted[14:110],diff(test_8)[14:110])
accuracy(fit_train_tbat$fitted[14:447],diff(train_8)[14:447])

checkresiduals(m4)
length(fit_train_tbat$fitted)
qqnorm(m4$errors)
plot.zoo(cbind(fit_train_tbat$fitted,diff(train_8)),
         plot.type = "single", 
         col = c("red", "blue"))

plot.zoo(cbind(fit_test_tbat$fitted,diff(test_8)),
         plot.type = "single", 
         col = c("red", "blue"))

c(ETS = accuracy(ETS, test_8)["Test set","RMSE"],
  ARIMA = accuracy(ARIMA, test_8)["Test set","RMSE"],
  #`STL-ETS` = accuracy(STL, auscafe)["Test set","RMSE"],
  NNAR = accuracy(NNAR, test_8)["Test set","RMSE"],
  TBATS = accuracy(TBATS, test_8)["Test set","RMSE"],
  Combination =
    accuracy(Combination, test_8)["Test set","RMSE"])

m1_acc <- accuracy(m1$fitted,train_8)
m1_acc

############################################
############################################
######     GARCH MODEL    ##################
############################################
############################################
m5 <- garch(diff(train_8))
train_8 <- window(train_8)
m5 <- garchFit(~ arma(1, 0)+garch(1,1), data = diff(train_8))
fit_train_garch <- m5@fitted

fit_test_garch <- forecast(m5,h = 110)
                                 
fit_test_garch <- fGarch::predict(m5, n.ahead = 11,mse="uncond")

accuracy(fit_test_garch$fitted[14:110],diff(test_8)[14:110])
accuracy(fit_train_garch[14:447],diff(train_8)[14:447])

checkresiduals(m5)
length(fit_train_garch$fitted)
qqnorm(m5$residuals)

plot.zoo(cbind(fit_train_garch,diff(train_8)),
         plot.type = "single", 
         col = c("red", "blue"))

plot.zoo(cbind(fit_test_garch,diff(test_8)),
         plot.type = "single", 
         col = c("red", "blue"))

c(ETS = accuracy(ETS, test_8)["Test set","RMSE"],
  ARIMA = accuracy(ARIMA, test_8)["Test set","RMSE"],
  #`STL-ETS` = accuracy(STL, auscafe)["Test set","RMSE"],
  NNAR = accuracy(NNAR, test_8)["Test set","RMSE"],
  TBATS = accuracy(TBATS, test_8)["Test set","RMSE"],
  Combination =
    accuracy(Combination, test_8)["Test set","RMSE"])

m1_acc <- accuracy(m1$fitted,train_8)
m1_acc

############################################
############################################
######     FITTING XGBOOSTING    #######
############################################
############################################


y_train <- train_8[1:(length(train_8)-1)]
y_test <- test_8[1:(length(test_8)-1)]

train_8 <- split$train[2:length(train_8)]
test_8 <- split$test[2:length(test_8)]

x_train <- xgboost::xgb.DMatrix(as.matrix(train_8))
x_test <- xgboost::xgb.DMatrix(as.matrix(test_8))
                                
xgb_trcontrol <- caret::trainControl(
  method = "cv", 
  number = 5,
  allowParallel = TRUE, 
  verboseIter = FALSE, 
  returnData = FALSE
)
xgb_grid <- base::expand.grid(
  list(
    nrounds = c(100, 200),
    max_depth = c(10, 15, 20), # maximum depth of a tree
    colsample_bytree = seq(0.5), # subsample ratio of columns when construction each tree
    eta = 0.1, # learning rate
    gamma = 0, # minimum loss reduction
    min_child_weight = 1,  # minimum sum of instance weight (hessian) needed ina child
    subsample = 1 # subsample ratio of the training instances
  ))
colnames(x_train) <- value
xgb_model <- caret::train(
  x_train, y_train,
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid,
  method = "xgbTree",
  nthread = 1
)

best_model <- xgb_model$bestTune

f_train <- predict(xgb_model,train_8)
f_test <- predict(xgb_model,test_8)

accuracy(f_train,train_8)
accuracy(f_test,test_8)
plot.ts(f_test)
plot.ts(test_8)

#plot test and fitted value of test
plot.zoo(cbind(test_8,f_test), 
         plot.type = "single", 
         col = c("red", "blue"))

plot.zoo(cbind(train_8,f_train),
         plot.type = "single", 
         col = c("red", "blue"))
length(train_8)
length(f_train)

## Using an external regressor in a neural net
decompose(df_8_grp.ts)
diff_8.ts <- ts(diff_8,frequency = 365)
decompose(diff_8.ts)
periodicity(diff_8.ts)
summary(df_8_grp.ts)
head(diff_8)
tail(diff_8)
diff_8
