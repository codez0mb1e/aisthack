

#'
#' News analysis
#'


library(dplyr)
library(stringr)
library(lubridate)

library(tm)
library(textstem)
library(tidyr)



### 1. Load dataset ----
news.raw <- fread("data/train/news_train.csv", 
                  header = T, 
                  sep = ",", 
                  quote = "\"",
                  stringsAsFactors = F)


glimpse(news.raw)
anyNA(news.raw)
news.raw %>% count(section) %>% arrange(-n)



### 2. Preprocessing ----
## 2.1. prepare data
news <- news.raw %>% 
  mutate_at(
    c("publication_date", "date"),
    ymd_hms
  ) %>% 
  mutate(
    date_diff = difftime(date, publication_date, units = "min"),
    timestamp = as.numeric(publication_date)
  ) %>% 
  mutate_at(
    c("content", "title"),
    funs(removeWords( # ignore common words: e.g., the/a
      removePunctuation( # remove punctuation
        stem_strings( # stemming: e.g., cats => cat
          lemmatize_strings( # lemmatization, e.g., run/runs/ran => run
            tolower(.)))), # down-case, e.g., CODE => code
      stopwords()))
  ) %>% 
  mutate_at(
    c("content", "title"),
    funs(if_else(. == "NA" | trimws(.) == "", NA_character_, .))
  )

saveRDS(news, "source/temp/news.rds")



## 2.2. create Document Term Matrix
corpus <- VCorpus(VectorSource(news$title))

minTermFreq <- length(corpus) * .01 # ignore extremely rare words i.e. terms that appear in less then 1% of the documents
maxTermFreq <- length(corpus) * .5 # ignore overly common words i.e. terms that appear in more than 50% of the documents

news.m <- DocumentTermMatrix(corpus,
                             control = list(
                               wordLengths = c(2, 22),
                               stopwords = F, 
                               removePunctuation = F,
                               removeNumbers = F,
                               stemming = F,
                               bounds = list(global = c(minTermFreq, maxTermFreq))
                          ))

inspect(news.m)
findFreqTerms(news.m, 10)


