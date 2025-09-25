## set working directory to root directory of rpeo

library(argo)
library(MMWRweek)
library(bestNormalize)
library(viridis) 
library(dplyr)
library(xts)
library(ggplot2)
library(patchwork)
library(kableExtra)
library(gridExtra)



#### useful functions ####
transY <- function(y){
  logit((y+1e-1) / 100)
}

inv_transY <- function(y){
  100*logit_inv(y)-1e-1
}

mse <- function(x, y){
  mean((x-y)^2)
}

bn <- function(x){
  set.seed(1000)
  bn <- bestNormalize(x)
  print(bn$chosen_transform)
  bn$x.t
}

orq <- function(x){
  set.seed(1000)
  orq <- orderNorm(as.numeric(x), k = 5)
  tmp <- orq$x.t
  xts(tmp, order.by = index(x))
}

log_1 <- function(x){
  log(x + 1)
}

org <- function(x){
  x
}

inv_log_1 <- function(x){
  exp(x) - 1
}

inv_org <- function(x){
  x
}

inv_bn <- function(fit, x){
  set.seed(1000)
  bn <- bestNormalize(fit)
  predict(bn, newdata = x, inverse = TRUE)
}

inv_orq <- function(fit, x){
  set.seed(1000)
  orq <- orderNorm(as.numeric(fit), k = 5)
  tmp <- predict(orq, newdata = as.numeric(x), inverse = TRUE)
  xts(tmp, order.by = index(x))
}

#### data processing ####
## GT & ili 
ili.folder <- paste0("./data/", "ili20250905/")
gt.folder <- paste0("./data/", "api20250521/")
population.file <- "./data/population2016.csv"


# out.folder <- paste0("./data_hosp/", GTpub_all)
# if(!exists(out.folder)){
#   dir.create(out.folder, showWarnings = FALSE, recursive = TRUE)
# }  

gt.parser.cur <- argo:::gt.parser.pub.api

state_data <- load_reg_data(gt.folder=gt.folder,
                            ili.folder=ili.folder,
                            population.file=population.file,
                            gft.file="data/GFT.txt",
                            gt.parser = gt.parser.cur)
print("Data input done.")

GT_national <- state_data$GT_national
GT_regional <- state_data$GT_regional
GT_state <- state_data$GT_state
ili_national <- state_data$ili_national
ili_regional <- state_data$ili_regional
ili_state <- state_data$ili_state

## flusurv
flusurv <- read.csv("./data/flusurv20250708.csv", skip = 2, header = TRUE)
View(flusurv)

filtered <- flusurv %>% 
  filter(AGE.CATEGORY == "Overall", SEX.CATEGORY == "Overall",
         RACE.CATEGORY == "Overall", VIRUS.TYPE.CATEGORY == "Overall") %>%
  mutate(rate = WEEKLY.RATE,
         date = MMWRweek2Date(MMWRyear = YEAR.1, MMWRweek = WEEK, MMWRday = 7)) %>%
  select(rate, date)

filtered$rate <- as.numeric(filtered$rate)
filtered$date <- as.Date(filtered$date)
filtered <- xts(filtered[, 1], order.by = filtered$date)
View(filtered)


## nhsn
hosp_national <- read.csv("./data/nhsn20250707.csv")
hosp_national <- hosp_national[hosp_national$location == "US", ]
hosp_national$weekly_rate <- as.numeric(hosp_national$weekly_rate)
hosp_national <- hosp_national[, -c(2,3,4)]
hosp_national <- xts(hosp_national[, -1], order.by =  as.Date(hosp_national$date))
View(hosp_national)


## ili processing
# start from start of imputation (best mse 1.15)
# start <- min(index(filtered))
# ili_nat2 <- ili_national[index(ili_national) >= start]

ili_nat2 <- bn(ili_national)
hosp_nat2 <- bn(hosp_national)

ili_orq <- orq(ili_national)
hosp_orq <- orq(hosp_national)


#### plotting ####
## ili and hosp rate
idx <- index(hosp_national)
df <- data.frame(ili = ili_national[idx],
                 hosp = hosp_national[idx],
                 logit_ili = transY(ili_national[idx]),
                 logit_hosp = transY(hosp_national[idx]),
                 log_hosp = log(hosp_national[idx] + 1),
                 log_ili = log(ili_national[idx] + 1),
                 bn_ili = ili_nat2[idx],
                 bn_hosp = hosp_nat2[idx],
                 orq_ili = ili_orq[idx],
                 orq_hosp = hosp_orq[idx]) # bn = bestNormalize; orq = ordered quantile norm


trans_ili <- c("ili", "logit_ili", "log_ili", "bn_ili", "orq_ili")
trans_hosp <- c("hosp", "logit_hosp", "log_hosp", "bn_hosp", "orq_hosp")

graph <- expand.grid(trans_ili, trans_hosp)

plots <- list()
for(i in 1:nrow(graph)) {
  dat_ili <- toString(graph[i, 1])
  dat_hosp <- toString(graph[i, 2])
  title <- paste0(dat_ili, " + ", dat_hosp)
  plt <- ggplot(data = df, aes(x = .data[[dat_ili]], y = .data[[dat_hosp]])) + 
    geom_point(size = 0.3) + ggtitle(title)
  plots[[i]] <- plt
}

combined_plot <- wrap_plots(plots, ncol = 5)
combined_plot

file <- "./results/Sunny/transformations.pdf"
ggsave(file, combined_plot)

## nhsn and flusurv
idx <- index(merge(filtered, hosp_national, all = FALSE))
df <- data.frame(flusurv = filtered[idx], nhsn = hosp_national[idx])
ggplot(data = df, aes(x = flusurv, y = nhsn)) + geom_point(size = 0.4) +
  ggtitle("flusurv v.s. nhsn")

#### cross validation ####
## splitting data set
hosp_rate <- data.frame(date = index(hosp_national), rate = as.numeric(hosp_national[,1]))
hosp_rate$year <- format(hosp_rate$date, "%Y")

## function for training one model
onemodel <- function(dat_train, dat_test, delta){
  ## fitting model 
  nhsn_train <- log(dat_train + delta)
  ili_train <- ili_nat2[index(dat_train)]
  model <- lm(as.numeric(nhsn_train) ~ as.numeric(ili_train))
  
  ## predict
  pred <- predict(model, 
                  newdata = data.frame(ili_train = ili_nat2[index(dat_test)]))
  pred <- exp(pred) - delta
  
  ## coerce 0 when negative
  pzero <- sum(pred < 0)/length(pred)
  pred[pred<0] <- 0

  mse <- mean((dat_test - pred)^2)
  
  c(mse, pzero)
}

## function for training two models
twomodel <- function(dat_train, dat_test, delta){
  week <- MMWRweek(index(dat_train))
  off_idx <- index(dat_train)[week$MMWRweek <= 39 & week$MMWRweek >= 18]
  flu_idx <- index(dat_train)[!index(dat_train) %in% off_idx]
  
  ## flu season
  nhsn_train <- log(dat_train[flu_idx] + delta)
  flusurv_train <- log(filtered[flu_idx] + delta)
  ili_train <- ili_nat2[flu_idx]
  
  flu_model <- lm(as.numeric(nhsn_train) ~ as.numeric(ili_train) + 
                    as.numeric(flusurv_train))
  
  ## off season 
  nhsn_train <- log(dat_train[off_idx] + delta)
  ili_train <- ili_nat2[off_idx]
  
  off_model <- lm(as.numeric(nhsn_train) ~ as.numeric(ili_train))
  
  ## predict
  week <- MMWRweek(index(dat_test))
  off_idx <- index(dat_test)[week$MMWRweek <= 39 & week$MMWRweek >= 18]
  flu_idx <- index(dat_test)[!index(dat_test) %in% off_idx]
  
  ## flu season
  flusurv_pred <- log(filtered[flu_idx] + delta)
  ili_pred <- ili_nat2[flu_idx]
  flu_pred <- predict(flu_model, newdata = data.frame(ili_train = ili_pred,
                                                      flusurv_train = flusurv_pred))
  flu_pred <- exp(flu_pred) - delta
  
  ## off season
  ili_pred <- ili_nat2[off_idx]
  off_pred <- predict(off_model, newdata = data.frame(ili_train = ili_pred))
  off_pred <- exp(off_pred) - delta 
  
  pred <- xts(matrix(nrow = length(index(dat_test)), ncol = 1), 
              order.by = index(dat_test))
  
  pred[off_idx] <- off_pred
  pred[flu_idx] <- flu_pred
  
  ## coerce 0 when negative 
  pzero <- sum(pred < 0)/length(pred)
  pred[pred <0] <- 0
  
  mse <- mean((dat_test - pred)^2)
  
  c(mse, pzero)
}

## two v.s. one model
years <- c(2022, 2023, 2024, 2025)
deltas <- c(0.01, 0.05, 0.1, 0.5, 1, 1.5, 2, 5, 7,5, 10, 20) # min hosp_national is 0.06
models <- c(0, 1) # 0 for single model; 1 for mixed season
cv <- expand.grid(models, deltas)


result <- matrix(nrow = nrow(cv), ncol = 4)
colnames(result) <- c("model", "delta", "mse", "% negative")

for(i in 1:nrow(cv)){
  run <- cv[i, ]
  print(run)
  model <- run[,1]
  delta <- run[,2]
  sum_mse <- 0
  sum_pzero <- 0
  for(testyear in years){
    print(testyear)
    train <- hosp_rate[as.numeric(hosp_rate$year) != testyear, ]
    test <- hosp_rate[as.numeric(hosp_rate$year) == testyear, ]
    xts_train <- xts(train$rate, order.by = as.Date(train$date))
    xts_test <- xts(test$rate, order.by = as.Date(test$date))
    
    if(model == 0){
      res <- onemodel(xts_train, xts_test, delta)
      mse <- res[1]
      pzero <- res[2]
    }else{
      res <- twomodel(xts_train, xts_test, delta)
      mse <- res[1]
      pzero <- res[2]
    }
    
    sum_mse <- sum_mse + mse
    sum_pzero <- sum_pzero + pzero
    
  }
  avg_mse <- sum_mse/4
  avg_pzero <- sum_pzero/4
  
  result[i, ] <- c(model, delta, avg_mse, avg_pzero)
}

kable(result[order(result[,3]),])

## CV selected two-model
## cv of different transformations 
twopart <- function(dat_train, dat_test, dat_ili, dat_fs, t_hosp){
  
  week <- MMWRweek(index(dat_train))
  off_idx <- index(dat_train)[week$MMWRweek <= 39 & week$MMWRweek >= 18]
  flu_idx <- index(dat_train)[!index(dat_train) %in% off_idx]
  
  dat_train <- t_hosp(dat_train)
  ## flu season
  nhsn_train <- dat_train[flu_idx]
  flusurv_train <- dat_fs[flu_idx]
  ili_train <- dat_ili[flu_idx]
  
  flu_model <- lm(as.numeric(nhsn_train) ~ as.numeric(ili_train) + 
                    as.numeric(flusurv_train))
  
  ## off season 
  nhsn_train <- dat_train[off_idx]
  ili_train <- dat_ili[off_idx]
  
  off_model <- lm(as.numeric(nhsn_train) ~ as.numeric(ili_train))
  
  ## predict
  week <- MMWRweek(index(dat_test))
  off_idx <- index(dat_test)[week$MMWRweek <= 39 & week$MMWRweek >= 18]
  flu_idx <- index(dat_test)[!index(dat_test) %in% off_idx]
  
  ## flu season
  flusurv_pred <- dat_fs[flu_idx]
  ili_pred <- dat_ili[flu_idx]
  flu_pred <- predict(flu_model, newdata = data.frame(ili_train = ili_pred,
                                                      flusurv_train = flusurv_pred))

  ## off season
  ili_pred <- dat_ili[off_idx]
  off_pred <- predict(off_model, newdata = data.frame(ili_train = ili_pred))

  pred <- xts(matrix(nrow = length(index(dat_test)), ncol = 1), 
              order.by = index(dat_test))
  
  pred[off_idx] <- off_pred
  pred[flu_idx] <- flu_pred
  
  pred
}

years <- c(2022, 2023, 2024, 2025)
funcs <- c("org", "transY", "log_1", "bn", "orq")
cv_func <- expand.grid(funcs, funcs, stringsAsFactors = FALSE)

trans_result <- matrix(nrow = nrow(cv_func), ncol = 4)
trans_result <- data.frame(trans_result)
colnames(trans_result) <- c("ili transformation", "hosp transformation", "mse", "% negative")
pred_result <- list()

for(i in 1:nrow(cv_func)){
  print(cv_func[i,])
  trans_ili <- get(cv_func[i,1])
  trans_hosp <- get(cv_func[i,2])

  dat_y <- hosp_national
  dat_x <- trans_ili(ili_national)
  fs <- trans_hosp(filtered) ## transform flusurv similar to hosp_national
  
  sum_mse <- 0
  sum_pzero <- 0
  
  for(testyear in years){
    print(testyear)
    train <- dat_y[as.numeric(hosp_rate$year) != testyear, ]
    test <- dat_y[as.numeric(hosp_rate$year) == testyear, ]
    truth <- hosp_national[as.numeric(hosp_rate$year) == testyear, ]
    
    
    res <- twopart(train, test, dat_x, fs, trans_hosp)
    
    invfunc <- paste0("inv_", cv_func[i,2])
    if(cv_func[i,2] == "bn" | cv_func[i,2] == "orq") {
      inv <- get(invfunc)
      pred <- inv(train, res)
    }else{
      inv <- get(invfunc)
      pred <- inv(res)
    }
    print(invfunc)
    
    ## coerce 0 when negative 
    pzero <- sum(pred < 0)/length(pred)
    pred[pred <0] <- 0
    
    mse <- mean((truth - pred)^2)
    
    sum_mse <- sum_mse + mse
    sum_pzero <- sum_pzero + pzero
    
  }
  avg_mse <- sum_mse/4
  avg_pzero <- sum_pzero/4
  
  trans_result[i, ] <- c(cv_func[i,1], cv_func[i,2], avg_mse, avg_pzero)
}

order <- trans_result[order(trans_result$mse), ]
cv_trans <- data.frame(order)
cv_trans$`ili+hosp` <- paste0(cv_trans$ili.transformation, "+", cv_trans$hosp.transformation)
cv_trans <- data.frame(`ili+hosp` = cv_trans$`ili+hosp`, 
                       cv_mse = cv_trans$mse,
                       cv_pnegative = cv_trans$X..negative)
file_path <- "./results/Sunny/cv_transformations.Rdata"
save(cv_trans, file = file_path)

file <- "./results/Sunny/cv_transformations.pdf"
pdf(file, width = 8, height = 10) 
grid.table(order)                              
dev.off()

## cv of deltas 
years <- c(2022, 2023, 2024, 2025)
ili_funcs <- c("org", "transY", "log_1", "bn", "orq")
deltas <- c(0.01, 0.05, 0.1, 0.5, 1, 3, 5, 10, 50, 100, 1000)
cv_delta <- expand.grid(ili_funcs, deltas)

delta_result <- matrix(nrow = nrow(cv_delta), ncol = 4)
delta_result <- data.frame(delta_result)
colnames(delta_result) <- c("ili_transformation", "delta", "mse", "% negative")

for(i in 1:nrow(cv_delta)){
  print(cv_delta[i,])
  trans_ili <- get(toString(cv_delta[i,1]))
  delta <- as.numeric(cv_delta[i, 2])
  dat_y <- log(hosp_national + delta)
  dat_x <- trans_ili(ili_national)
  fs <- log(filtered + delta) ## transform flusurv similar to hosp_national
  
  sum_mse <- 0
  sum_pzero <- 0
  
  for(testyear in years){
    print(testyear)
    train <- dat_y[as.numeric(hosp_rate$year) != testyear, ]
    test <- dat_y[as.numeric(hosp_rate$year) == testyear, ]
    truth <- hosp_national[as.numeric(hosp_rate$year) == testyear, ]
    
    
    res <- twopart(train, test, dat_x, fs)
    
    pred <- exp(res) - delta
    
    ## coerce 0 when negative 
    pzero <- sum(pred < 0)/length(pred)
    pred[pred <0] <- 0
    
    mse <- mean((truth - pred)^2)
    
    sum_mse <- sum_mse + mse
    sum_pzero <- sum_pzero + pzero
    
  }
  avg_mse <- sum_mse/4
  avg_pzero <- sum_pzero/4
  
  delta_result[i, ] <- c(toString(cv_delta[i,1]), cv_delta[i,2], avg_mse, avg_pzero)
}

delta_res <- delta_result[order(delta_result$mse), ]
file <- "./results/Sunny/cv_deltas.pdf"
pdf(file, width = 8, height = 20) 
grid.table(delta_res)                              
dev.off()

