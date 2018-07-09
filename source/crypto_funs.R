

#'
#' Bitcoin trades functions
#'


#' Load Bitcoin/Currency pair trades
#'
#' @param market Market
#' @param symbol Symbol (support only BTCUSD)
#' @param .from From time (inclusive bound)
#' @param .to To time (exclusive bound)
#' @param dataSource Trades source (support only bitcoincharts API) 
#' @param apiKey API key
#' 
#' @return Dataframe w/ Bitcoin/Currency pair trades
#' 
#' @example cryptocurrency.getTrades("ITBIT", "BTCUSD", fromTime, toTime)
#' @dtails http://api.bitcoincharts.com/v1/csv
#' 
cryptocurrency.loadTrades <- function(market, symbol, .from, .to, dataSource = "api.bitcoincharts.com/v1", apiKey = NULL) {
  require(dplyr)
  require(xts)
  stopifnot(
    is.character(market),
    is.character(dataSource) && dataSource == "api.bitcoincharts.com/v1",
    is.POSIXt(.from),
    is.POSIXt(.to) && .to > .from,
    is.null(apiKey) || is.character(apiKey)
  )
  
  
  dt <- data.frame()
  
  temp <- tempfile()
  url <- sprintf("http://%s/csv/%sUSD.csv.gz", dataSource, tolower(market))
  try({
    # set_config(config(ssl_verifypeer = 0L)) # note: if SSL cert verification failed
    download.file(url, temp)
    dt <- read.table(temp, sep = ",", header = F, stringsAsFactors = F)
  })
  unlink(temp)
  
  
  if (nrow(dt) == 0) {
    stop("Empty dataset")
  } else {
    dt <- dt %>% 
      transmute(
        Time = as.POSIXct(V1, origin = "1970-01-01"),
        Price = V2, 
        Volume = V3
      ) %>% 
      filter(
        Time >= .from & Time < .to
      )
  }
  
  
  merge.xts(
    Price = xts(dt$Price, order.by = dt$Time),
    Volume = xts(dt$Volume, order.by = dt$Time),
    join = "inner"
  )
}



#' Load market statistics
#'
#' @param coinCode 
#' @param .from From time (inclusive bound)
#' @param .to To time (exclusive bound)
#' @param dataSource Data source (support only coincap.io/history)
#' @param apiKey API key
#' 
#' @return Dataframe w/ market statistics
#' @seealso https://github.com/CoinCapDev/CoinCap.io
cryptocurrency.loadMarketStatsByCoin <- function(coinCode, .from, .to, dataSource = "coincap.io/history", apiKey = NULL) {
  require(jsonlite)
  require(purrr)
  require(xts)
  
  stopifnot(
    is.character(coinCode),
    is.POSIXt(.from),
    is.POSIXt(.to) && .to > .from,
    is.character(dataSource),
    is.null(apiKey) || is.character(apiKey)
  )
  
  
  dt <- map(fromJSON(sprintf("http://%s/%s", dataSource, coinCode), simplifyVector = T),
            ~ xts(.x[, 2], order.by = as.POSIXct(.x[, 1]/1000, origin = "1970-01-01")))
  
  merge.xts(
    MarketCapitalization = dt[[1]],
    Price = dt[[2]],
    Volume = dt[[3]]
  )[sprintf("%s/%s", format(.from, "%F"), format(.to, "%F"))]
}



#' Load market statistics
#'
#' @param coinCodes List of coin codes
#' @param .from From time (inclusive bound)
#' @param .to To time (exclusive bound)
#' @param apiKey API key
#' 
#' @return Dataframe w/ market statistics
#' @example cryptocurrency.getMarketStats(cryptocurrency.Codes, fromTime, toTime)
cryptocurrency.loadMarketStats <- function(coinCodes, .from, .to, apiKey = NULL) {
  require(purrr)
  stopifnot(
    is.vector(coinCodes) && is.character(coinCodes)
  )
  
  r <- map(coinCodes,
           ~ cryptocurrency.loadMarketStatsByCoin(.x, .from, .to, apiKey= apiKey))
  
  checkDailyTrades(r, .from, .to)
  
  
  r
}


