

library(purrr)
library(tidyr)

library(ggplot2)
library(corrplot)


### Vizualize trades ----

to1dCandles <- function(ts, token) {
  require(data.table)
  require(dplyr)
  require(xts)
  stopifnot(
    is.xts(ts),
    is.character(token)
  )
  
  if (token == "BTC") {
    ts %>% 
      to.daily %>% 
      as.data.table %>% 
      transmute(
        Date = as.Date(index),
        Close = `..Close`,
        Volume = `..Volume`,
        Token = token
      )
  } else {
    ts %>% 
      as.data.table %>% 
      transmute(
        Date = as.Date(index),
        Close = Price,
        Volume,
        Token = token
      )
  }
}


trades <- list(
    BTC = btcusd.ticks %>% to1dCandles(., "BTC"),
    BTG = btgusd.1d %>% to1dCandles(., "BTG"),
    ZRX = zrxusd.1d %>% to1dCandles(., "ZRX"),
    USDT = usdtusd.1d %>% to1dCandles(., "USDT"),
    DRGN = drgnusd.1d %>% to1dCandles(., "DRGN")
  ) %>% 
  map_df(~ .x) %>% 
  inner_join(
    btcusd.ticks %>% to1dCandles(., "BTC") %>% transmute(Date, BTC_close = Close),
    by = "Date"
  ) %>% 
  mut9ate(
    token_in_BTC = Close/BTC_close
  ) %>% 
  select(
    -BTC_close
  )

trades %>% 
  group_by(Token) %>% 
  summarise(
    N = n(), Min = min(Close), Max = max(Close), MinDate = max(Date)
  )


ggplot(trades %>% filter(Date)) +
  geom_line(aes(x = Date, y = log(c(NA_real_, diff(token_in_BTC))))) +
  facet_wrap(~ Token)


ggplot(trades %>% filter(Date > Sys.Date() - month(8) & Token != "BTC"), aes(x = Date)) +
  geom_line(aes(y = log(token_in_BTC), color = Token))



### Oultliers visualization ----

prob <- quantile(token.1d$LogReturn, probs = c(.05, .95))

token.1d <- token.1d %>% 
  mutate(Outlier = if_else(LogReturn < min(prob) | LogReturn > max(prob), T, F))


ggplot(token.1d) +
  #geom_density(aes(Target), alpha = .5) +
  geom_density(aes(LogReturn), alpha = .5) +
  geom_vline(xintercept = max(prob), color = "blue", linetype = "dotted") +
  geom_vline(xintercept = min(prob), color = "blue", linetype = "dotted") +
  labs(x = "LogReturn, BTC") +
  theme_minimal()


ggplot(token.1d) +
  geom_point(aes(x = Time, y = Target, size = LogReturn, color = Outlier), alpha = .35) +
  labs(x = "Date", y = "Close price, BTC") +
  theme_minimal()


onchainFields <- c("Volume", "N", "LogReturn", "Total", "N_from_min", "N_from_mean", "N_from_max", "Volume_from_min", "Volume_from_mean",
                   "Volume_from_max", "N_to_min", "N_to_mean", "N_to_max", "Volume_to_min", "Volume_to_mean", 
                   "Volume_to_max")

token.m <- as.matrix(token.1d %>% 
                       filter(Outlier) %>% 
                       select(Target, one_of(onchainFields))
                     )
m <- cor(token.m) %>% replace_na(., 0)

corrplot.mixed(m, upper = "square", tl.col = "black", number.cex = .7, order = "hclust")
summary(lm(Target ~ ., token.1d %>% filter(!Outlier) %>% select(-Time)))


