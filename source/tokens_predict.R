

#' 
#' Tokens price prediction
#' 


### 0. Import Dependecies ----
suppressPackageStartupMessages({
  library(lightgbm)
  
  library(dplyr)
  library(data.table)
  library(tidyr)
  library(purrr)
  
  library(xts)
  
  library(ggplot2)
})

setwd("source/tickets_analysis")
source("crypto_funs.R")
source("prediction_funs.R")



### 1. Load and preprocessing data ----
source("load_datasets.R")

zrx.tickers <- tickers %>% 
  filter(ticker == "ZRX") %>% 
  transmute(
    Token = ticker,
    Volume = volume,
    Target = priceBtc,
    Time = datetime,
    Date = Time
  ) %>% 
  left_join(
    onChainStats %>% rename(Total = Volume), by = c("Token", "Date")
  ) %>%
  left_join(
    zrx_media, by = "Date"
  ) %>% 
  select(-Token, -Date)

glimpse(zrx.tickers)

# Calculate market statistics
token.1d <- zrx.tickers %>% 
  mutate(
    Diff = c(NA_real_, diff(Target)),
    Lag = lag(Diff),
    Return = Diff/Target, # returns: return(t) = (price(t) - price(t-1)) / price(t-1)
    LogReturn = c(NA_real_, diff(log(Target))) # logreturns: logreturn(t) = ln(price(t)/price(t-1))
  ) %>% 
  na.omit

glimpse(token.1d)

ts.plot(token.1d$Target)
ts.plot(token.1d$LogReturn)
ts.plot(token.1d$Volume)


timeSteps <- 12*2 # 2d
predictingPeriod <- timeSteps * 15 # 30d

data <- tokens.addLags(token.1d, timeSteps)
glimpse(data)



### 3. Train model  ----
splitBy <- nrow(data) - predictingPeriod


## convert to lgb datasets
mTrain <- lgb.Dataset(data = convertToMatrix(data[1:splitBy, ], "LogReturn", "Time"),
                      label = data[1:splitBy, ]$LogReturn)

mTest <- lgb.Dataset(data = convertToMatrix(data[splitBy:nrow(data), ], "LogReturn", "Time"),
                     label = data[splitBy:nrow(data), ]$LogReturn)

params <- getHyperparams(.size = 72L,
                         .learning_rate = c(.04, .06, .08),
                         .max_depth = c(4:5), 
                         .max_bin = 255L,
                         .num_leaves = c(24, 31, 48),
                         .min_data_in_leaf = 20L, 
                         .min_sum_hessian_in_leaf = 1e-3,
                         .feature_fraction = 1,
                         .bagging_fraction = 1,
                         .bagging_freq = 0,
                         .lambda_l1 = c(.01, .02, .04),
                         .lambda_l2 = c(.01, .02),
                         .min_split_gain = 0,
                         .scale_pos_weight = 1)

gridSearch <- selectHyperparams(mTrain, mTest, params)
View(gridSearch)

modelParams <- gridSearch %>% top_n(1, -Eval_test) %>% select(-starts_with("Eval")) %>% as.list
model <- trainModel(mTrain, NULL, modelParams, 1e4)

lgb.importance(model, percentage = T) %>% head(50)



### 3. Score model  ----
predictions <- predict(model, convertToMatrix(data[splitBy:nrow(data), ], "LogReturn", "Time"))



### 4. Eval and visualize result ----
results <- combineResults(data[splitBy:nrow(data), ]$LogReturn, 
                          predictions,
                          data[splitBy:nrow(data), ]$Time,
                          10L)

View(
  results %>% 
    gather(., "Model", "Residuals", Predicted_residuals:EMA_residuals, factor_key = T) %>% 
    group_by(Model) %>% 
    summarise(
      TotalLoss = sum(abs(Residuals))
    ) %>% 
    arrange(TotalLoss)
)


ggplot(results %>% gather(., "Model", "Price", Predicted:EMA, factor_key = T), aes(x = Time)) +
  geom_line(aes(y = Actual), color = "red", alpha = .3) +
  geom_line(aes(y = Price, color = Model)) +
  facet_grid(Model ~ .) +
  labs(title = "0x/BTC Stock Price", 
       x = "Date", y = "LogReturn of Price") +
  theme_bw()





