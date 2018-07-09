

#' 
#' Model train and predictions functions
#' 


#'
#' @param .period 
#' @param dt 
#' 
getCandles <- function(ts, .period) {
  require(zoo)
  require(xts)
  require(dplyr)
  require(data.table)
  require(lubridate)
  stopifnot(
    is.xts(ts)
  )
  
  ts %>% 
    .period(., names = "") %>% 
    as.data.table %>% 
    rename(
      Time = index,
      Open = `..Open`,
      High = `..High`,
      Low = `..Low`,
      Close = `..Close`,
      Volume = `..Volume`
    ) %>% 
    ## Calculate market statistics
    transmute(
      Target = Close,
      
      HighLowDiff = High - Low,
      Diff = c(NA_real_, diff(Close)),
      Lag = lag(Diff),
      Return = Diff/Lag, # returns: return(t) = (price(t) - price(t-1)) / price(t-1)
      LogReturn = c(NA_real_, diff(log(Close))), # logreturns: logreturn(t) = ln(price(t)/price(t-1))
      
      Volume,
      Time
    ) %>% 
    na.omit
}



#' 
#'
#' @param dt 
#'
addLags <- function(dt, .timeSteps) {
  require(dplyr)
  require(data.table)
  stopifnot(
    is.data.frame(dt),
    is.numeric(.timeSteps)
  )
  
  setDT(dt)[, paste0('Target_lag_', 1:.timeSteps) := shift(Target, 1:.timeSteps)][]
  setDT(dt)[, paste0('HighLowDiff_lag_', 1:.timeSteps) := shift(HighLowDiff, 1:.timeSteps)][]
  setDT(dt)[, paste0('Diff_lag_', 1:.timeSteps) := shift(Diff, 1:.timeSteps)][]
  setDT(dt)[, paste0('Lag_lag_', 1:.timeSteps) := shift(Lag, 1:.timeSteps)][]
  setDT(dt)[, paste0('LogReturn_lag_', 1:.timeSteps) := shift(LogReturn, 1:.timeSteps)][]
  setDT(dt)[, paste0('Return_lag_', 1:.timeSteps) := shift(Return, 1:.timeSteps)][]
  setDT(dt)[, paste0('Volume_lag_', 1:.timeSteps) := shift(Volume, 1:.timeSteps)][]
  
  dt %>% 
    select(-HighLowDiff, -Diff, -LogReturn, -Return, -Volume, -Lag) %>% 
    na.omit
}


#' 
#'
#' @param dt 
#'
addLags2 <- function(dt, .timeSteps) {
  require(dplyr)
  require(data.table)
  stopifnot(
    is.data.frame(dt),
    is.numeric(.timeSteps)
  )
  
  setDT(dt)[, paste0('HighLowDiff_lag_', 1:.timeSteps) := shift(HighLowDiff, 1:.timeSteps)][]
  setDT(dt)[, paste0('Diff_lag_', 1:.timeSteps) := shift(Diff, 1:.timeSteps)][]
  setDT(dt)[, paste0('Lag_lag_', 1:.timeSteps) := shift(Lag, 1:.timeSteps)][]
  setDT(dt)[, paste0('LogReturn_lag_', 1:.timeSteps) := shift(LogReturn, 1:.timeSteps)][]
  setDT(dt)[, paste0('Return_lag_', 1:.timeSteps) := shift(Return, 1:.timeSteps)][]
  setDT(dt)[, paste0('Volume_lag_', 1:.timeSteps) := shift(Volume, 1:.timeSteps)][]
  
  dt %>% 
    select(-Target, -HighLowDiff, -Diff, -Return, -Volume, -Lag) %>% 
    na.omit
}



#' 
#'
#' @param dt 
#'
tokens.addLags <- function(dt, .timeSteps) {
  require(dplyr)
  require(data.table)
  stopifnot(
    is.data.frame(dt),
    is.numeric(.timeSteps)
  )
  
  #setDT(dt)[, paste0('Target_lag_', 1:.timeSteps) := shift(Target, 1:.timeSteps)][]
  setDT(dt)[, paste0('Diff_lag_', 1:.timeSteps) := shift(Diff, 1:.timeSteps)][]
  setDT(dt)[, paste0('Lag_lag_', 1:.timeSteps) := shift(Lag, 1:.timeSteps)][]
  setDT(dt)[, paste0('LogReturn_lag_', 1:.timeSteps) := shift(LogReturn, 1:.timeSteps)][]
  setDT(dt)[, paste0('Return_lag_', 1:.timeSteps) := shift(Return, 1:.timeSteps)][]
  setDT(dt)[, paste0('Volume_lag_', 1:.timeSteps) := shift(Volume, 1:.timeSteps)][]
  
  dt %>% 
    select(-Target, -Diff, -Return, -Volume, -Lag) %>% 
    na.omit
}


#' 
#'
#' @param dt 
#' @param .extraVars 
#'
convertToMatrix <- function(dt, .target, .extraVars) {
  require(dplyr)
  stopifnot(
    is.data.frame(dt),
    is.character(.extraVars)
  )
  
  as.matrix(
    dt %>% select(-one_of(c(.target, .extraVars)))
  )
}



#'
#'
#' @param .size Grid size
#' @param .learning_rate 
#' @param .max_depth Maximum depth of tree (-1 means no limit)
#' @param .min_sum_hessian_in_leaf 
#' @param .feature_fraction Fraction of features
#' @param .bagging_fraction Fraction of data to train tree
#' @param .bagging_freq 
#' @param .lambda_l1 L1 regularization term on weights
#' @param .lambda_l2 L2 regularization term on weights
#' @param .min_split_gain The minimal gain to perform split 
#' @param .num_leaves 
#' @param .scale_pos_weight 
#' @param .max_bin Max number of bins that feature values will be bucketed in
#' @param .min_data_in_leaf 
#'
#' @details
#' - http://lightgbm.apachecn.org/en/latest/Parameters.html
#' - http://lightgbm.readthedocs.io/en/latest/Parameters-Tuning.html
#'
#' @note see also https://www.kaggle.com/pranav84/lightgbm-fixing-unbalanced-data-lb-0-9680/code
#'  
getHyperparams <- function(.size = 100L,
                           .learning_rate = .01,
                           .max_depth = -1L, 
                           .max_bin = 255L,
                           .num_leaves = 31L,
                           .min_data_in_leaf = 20L, 
                           .min_sum_hessian_in_leaf = 1e-3,
                           .feature_fraction = 1,
                           .bagging_fraction = 1,
                           .bagging_freq = 0,
                           .lambda_l1 = 0,
                           .lambda_l2 = 0,
                           .min_split_gain = 0,
                           .scale_pos_weight = 1) {
  
  stopifnot(
    is.numeric(.size) && .size > 0
  )
  
  expand.grid(learning_rate = .learning_rate,
              max_depth = .max_depth,
              max_bin = .max_bin,
              num_leaves = .num_leaves,
              min_data_in_leaf = .min_data_in_leaf,
              min_sum_hessian_in_leaf = .min_sum_hessian_in_leaf,
              feature_fraction = .feature_fraction,
              bagging_fraction = .bagging_fraction,
              bagging_freq = .bagging_freq,
              lambda_l1 = .lambda_l1,
              lambda_l2 = .lambda_l2,
              min_split_gain = .min_split_gain,
              scale_pos_weight = .scale_pos_weight) %>% 
    sample_n(
      .size, replace = F
    )
}



#' 
#'
#' @param .train 
#' @param .test 
#' @param .params 
#' @param .nrounds 
#' @param .num_threads 
#'
trainModel <- function(.train, .test, .params, .nrounds = 1e3L, .num_threads = parallel::detectCores(logical = T)) {
  require(lightgbm)
  stopifnot(
    #is.list(.params),
    is.numeric(.nrounds) && .nrounds > 0
  )
  
  
  if (is.null(.test)) {
    model <- lightgbm(data = .train,
                      params = .params,
                      #metric = "auc",
                      #boosting = "dart", 
                      objective = "regression",
                      is_unbalance = F,
                      nrounds = .nrounds,
                      early_stopping_rounds = .nrounds/5,
                      num_threads = .num_threads,
                      save_binary = F,
                      verbose = 1)
  } else {
    model <- lgb.train(data = .train,
                       valids = list(Train = .train, Test = .test),
                       params = .params,
                       #metric = "auc",
                       #boosting = "gbdt",
                       objective = "regression",
                       is_unbalance = F,
                       nrounds = .nrounds,
                       early_stopping_rounds = .nrounds/2,
                       num_threads = .num_threads,
                       save_binary = F,
                       verbose = 1)
  }
  
  
  model
}



#' 
#'
#' @param .train 
#' @param .test 
#' @param .gridSearch 
#' @param .nrounds 
#' @param .num_threads 
#'
selectHyperparams <- function(.train, .test, .gridSearch, .nrounds = 64L, .num_threads = parallel::detectCores(logical = T)) {
  require(dplyr)
  stopifnot(
    is.data.frame(.gridSearch)
  )
  
  
  trainPerf <- numeric(nrow(.gridSearch))
  testPerf <- numeric(nrow(.gridSearch))
  
  
  for (i in 1:nrow(.gridSearch)) {
    model <- trainModel(.train, .test, .gridSearch[i, ] %>% as.list, .nrounds, .num_threads)
    
    trainPerf[i] <- min(as.numeric(model$record_evals$Train$l2$eval))
    testPerf[i] <- min(as.numeric(model$record_evals$Test$l2$eval))
  }
  
  .gridSearch %>% 
    cbind(list(
      Eval_train = trainPerf,
      Eval_test = testPerf
    )) %>% 
    mutate(
      Eval_diff = abs(Eval_train - Eval_test)
    ) %>% 
    arrange(Eval_test)
}



#' 
#'
#' @param actual 
#' @param predicted 
#' 
combineResults <- function(actual, predicted, .time, .step) {
  require(dplyr)
  require(lubridate)
  require(TTR)
  
  stopifnot(
    is.vector(actual), is.vector(predicted),
    length(predicted) > 0, length(predicted) == length(actual),
    length(predicted) == length(.time)
  )
  
  
  data.frame(
      Time = .time,
      Actual = actual,
      Predicted = predicted
    ) %>% 
    mutate(
      # naive (baseline)
      Prev = lag(Actual),
      # classis statistics
      SMA = SMA(Actual, n = .step), 
      EMA = EMA(Actual, n = .step)
    ) %>% 
    mutate_if(
      is.numeric, funs("residuals" = . - Actual)
    ) %>% 
    select(-Actual_residuals) %>% 
    na.omit 
}


