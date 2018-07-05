

#'
#' Trading analysis
#'


library(dplyr)
library(data.table)
library(lubridate)



### 1. Load dataset ----
tickers.raw <- fread("data/train/tickers_train.csv", 
                  header = T, 
                  sep = ",", 
                  quote = "\"",
                  stringsAsFactors = F)

glimpse(tickers.raw)
anyNA(tickers.raw)

View(tickers.raw %>% filter(is.na(volume)))



### 2. Preprocessing data
tickers <- tickers.raw %>% 
  mutate(
    datetime = ymd_hms(datetime),
    timestamp = as.numeric(datetime)
  ) %>% 
  distinct(
    ticker, datetime, .keep_all = T
  ) %>% 
  inner_join(
    tickers.raw %>% count(ticker) %>% filter(n > 99),
    by = "ticker"
  ) %>% 
  filter(
    !is.na(volume)
  ) %>% 
  select(-`Unnamed: 0`, -name, -n) 




