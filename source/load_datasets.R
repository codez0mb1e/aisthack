

### Load tokens trades ----
library(lubridate)

from <- Sys.time() - years(1)
to <- Sys.time()


btcusd.ticks <- cryptocurrency.loadTrades("kraken", "BTC", from, to)

drgnusd.1d <- cryptocurrency.loadMarketStatsByCoin("DRGN", from, to)
zrxusd.1d <- cryptocurrency.loadMarketStatsByCoin("ZRX", from, to)

usdtusd.1d <- cryptocurrency.loadMarketStatsByCoin("USDT", from, to)
btgusd.1d <- cryptocurrency.loadMarketStatsByCoin("BTG", from, to)



### Load tickers ----

loadTickers <- function() {
  library(dplyr)
  library(data.table)
  library(lubridate)
  
  
  tickers.raw <- fread("data/train/tickers_train.csv", 
                       header = T, 
                       sep = ",", 
                       quote = "\"",
                       stringsAsFactors = F)
  
  glimpse(tickers.raw)
  anyNA(tickers.raw)
  
  
  tickers.raw %>% 
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
}

tickers <- loadTickers()



### Load chain data and calc chain stats ----

readOnChain <- function(tokens) {
  require(readr)
  require(dplyr)
  library(purrr)
  library(lubridate)
  stopifnot(
    is.character(tokens)
  )
  
  tokens %>% 
    map(
      ~ fread(sprintf("../../data/train/onchain/%s_train.csv", .x)) %>% 
        mutate(
          Token = .x,
          Date = round_date(as_datetime(timestamp), unit = "hour")
        )
    ) %>% 
    bind_rows
}


calcChainStats <- function(dt) {
  
  groupBy <- c("Token", "Date")
  
  stats.all <- dt %>% 
    group_by(Token, Date) %>% 
    summarise(
      N = n(),
      Volume = sum(value)
    )
  
  stats.from <- dt %>% 
    group_by(Token, Date, from) %>% 
    summarise(
      N_from = n(),
      Volume_from = sum(value)
    ) %>% 
    inner_join(stats.all, by = c("Token", "Date")) %>% 
    mutate(
      N_from = N_from/N,
      Volume_from = Volume_from/Volume
    ) %>% 
    group_by(Token, Date) %>% 
    summarise(
      N_from_min = min(N_from),
      N_from_mean = mean(N_from),
      N_from_max = max(N_from),
      
      Volume_from_min = min(Volume_from),
      Volume_from_mean = mean(Volume_from),
      Volume_from_max = max(Volume_from)
    )
  
  stats.to <- dt %>% 
    group_by(Token, Date, to) %>% 
    summarise(
      N_to = n(),
      Volume_to = sum(value)
    ) %>% 
    inner_join(stats.all, by = groupBy) %>% 
    mutate(
      N_to = N_to/N,
      Volume_to = Volume_to/Volume
    ) %>% 
    group_by(Token, Date) %>% 
    summarise(
      N_to_min = min(N_to),
      N_to_mean = mean(N_to),
      N_to_max = max(N_to),
      
      Volume_to_min = min(Volume_to),
      Volume_to_mean = mean(Volume_to),
      Volume_to_max = max(Volume_to)
    )
  
  
  stats.all %>% 
    inner_join(stats.from, by = ) %>% 
    inner_join(stats.to, by = groupBy)
}



onChainStats <- readOnChain(c("DRGN", "ZRX")) %>% calcChainStats



### Load news data ----
library(readr)

zrx_media <- read_csv("data/train/news/ZRX.csv", 
                      col_types = cols(X1 = col_skip())) %>% 
  mutate(
    Date = round_date(ymd_hms(DateTime), unit = "hour")
  ) %>% 
  select(
    -DateTime
  )


