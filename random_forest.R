### 환경설정
# required_pkg <- c("ipred", "gbm", "randomForest", "readxl")
# installed_pkg <- rownames(installed.packages())
# pkg_to_install <- required_pkg[!(required_pkg %in% installed_pkg)]
# if (length(pkg_to_install) > 0) install.packages(pkg_to_install)

library(ipred)
library(gbm)
library(randomForest)
library(readxl)
library(dplyr)


########################################################
### 테스트 셋 만들기
########################################################

# data <- read.csv(file = "data_mart_final_200814.csv")
 
##########################################
# Training set은 2016, 2017, 2018 데이터
# Validation set은 2019 데이터
# Test set은 2020 데이터
##########################################

team <- unique(data$T_ID) # team 순서
# [1] HH HT KT LG LT NC OB SK SS WO


train   <- list(HH=data.frame(), HT=data.frame(), KT=data.frame(), LG=data.frame(), LT=data.frame(), NC=data.frame(), OB=data.frame(), SK=data.frame(), SS=data.frame(), WO=data.frame())
val     <- list(HH=data.frame(), HT=data.frame(), KT=data.frame(), LG=data.frame(), LT=data.frame(), NC=data.frame(), OB=data.frame(), SK=data.frame(), SS=data.frame(), WO=data.frame())
test    <- list(HH=data.frame(), HT=data.frame(), KT=data.frame(), LG=data.frame(), LT=data.frame(), NC=data.frame(), OB=data.frame(), SK=data.frame(), SS=data.frame(), WO=data.frame())

for (i in 1:10){
  train[[i]]  <- data %>% select(T_ID, W, YEAR, P:S) %>% filter(YEAR %in% 2016:2018) %>% filter(T_ID==team[i])
  val[[i]]    <- data %>% select(T_ID, W, YEAR, P:S) %>% filter(YEAR == 2019) %>% filter(T_ID==team[i])
  test[[i]]   <- data %>% select(T_ID, W, YEAR, P:S) %>% filter(YEAR == 2020) %>% filter(T_ID==team[i])
}

str(train); str(val); str(test)

# 데이터 형식은 아래와 같음
# $ SS:'data.frame':	65 obs. of  8 variables:
#   ..$ T_ID: Factor w/ 10 levels "HH","HT","KT",..: 9 9 9 9 9 9 9 9 9 9 ...
# ..$ W   : Factor w/ 2 levels "LOSE","WIN": 1 1 1 2 2 1 1 2 2 1 ...
# ..$ YEAR: int [1:65] 2020 2020 2020 2020 2020 2020 2020 2020 2020 2020 ...
# ..$ P   : num [1:65] 0.683 0.609 0.535 0.745 0.602 ...
# ..$ D   : num [1:65] 0.667 0.667 0.667 0.667 0.833 ...
# ..$ B   : num [1:65] 0.337 0.478 0.459 0.486 0.534 ...
# ..$ C   : num [1:65] 0.117 0.405 0.167 0.316 0.503 ...
# ..$ S   : num [1:65] 0 0.318 0.318 0.267 0.35 ...

###########################################################
# x 값 : train[[i]][,4:8],  val[[i]][,4:8], test[[i]][,4:8]
# y 값 : train[[i]][,2],    val[[i]][,2],   test[[i]][,2]
###########################################################

############################
#### Bagging
############################


fit.bagg      <- list(HH=NULL, HT=NULL, KT=NULL, LG=NULL, LT=NULL, NC=NULL, OB=NULL, SK=NULL, SS=NULL, WO=NULL)
pred.bagg     <- list(HH=NULL, HT=NULL, KT=NULL, LG=NULL, LT=NULL, NC=NULL, OB=NULL, SK=NULL, SS=NULL, WO=NULL)
pred_p.bagg   <- list(HH=NULL, HT=NULL, KT=NULL, LG=NULL, LT=NULL, NC=NULL, OB=NULL, SK=NULL, SS=NULL, WO=NULL)

for (i in 1:10){
  # 모형적합 -----------
  fit.bagg[[i]] <- ipredbagg(y=train[[i]][,2], X=train[[i]][,4:8], nbagg=1000)
  fit.bagg[[i]]
  # 그룹예측 ---------------
  pred.bagg[[i]] <- predict(fit.bagg[[i]], newdata = val[[i]][,4:8])
  print(table(pred.bagg[[i]], val[[i]][,2]))
  # 오분류율 ---------------
  print(paste(team[[i]], ":: 오분류율 = ", mean(pred.bagg[[i]]!=val[[i]][,2])*100, "%"))
  # 확률예측 ---------------
  pred_p.bagg[[i]] <- predict(fit.bagg[[i]], val[[i]][,4:8], type="prob")
  head(pred_p.bagg[[i]])
}

##############################
#### Boosting
##############################

fit.boosting      <- list(HH=NULL, HT=NULL, KT=NULL, LG=NULL, LT=NULL, NC=NULL, OB=NULL, SK=NULL, SS=NULL, WO=NULL)
pred_p.boosting   <- list(HH=NULL, HT=NULL, KT=NULL, LG=NULL, LT=NULL, NC=NULL, OB=NULL, SK=NULL, SS=NULL, WO=NULL)
pred.boosting     <- list(HH=NULL, HT=NULL, KT=NULL, LG=NULL, LT=NULL, NC=NULL, OB=NULL, SK=NULL, SS=NULL, WO=NULL)

for (i in 1:10){
  # 모형적합 -------
  fit.boosting[[i]] <- gbm(W ~ P+D+B+C+S  , data = train[[i]], distribution = "multinomial", n.trees=500)
  # summary(fit.boosting[[i]])
  
  # 확률예측 -------
  pred_p.boosting[[i]] <- predict(fit.boosting[[i]], val[[i]][,4:8], type="response", n.trees=500)
  pred_p.boosting[[i]] <- matrix(pred_p.boosting[[i]], ncol=2)
  colnames(pred_p.boosting[[i]]) <- levels(val[[i]][,2])
  head(pred_p.boosting[[i]])
  
  # 분류 -----------
  pred.boosting[[i]] <- apply(pred_p.boosting[[i]], 1, which.max)
  pred.boosting[[i]] <- ifelse(pred.boosting[[i]]==1, "LOSE", "WIN")
  print(table(pred.boosting[[i]], val[[i]][,2]))
  
  # 오분류율 -------
  print(paste(team[i], ":: 오분류율 = ", mean(pred.boosting[[i]]!=val[[i]][,2])*100, "%"))
}

###############################################
## Boosting 횟수 정하기

## 함수 정의

find_M <- function(m, t_id){
  i <- which(team==t_id)
  fit <- gbm(W~P+D+B+C+S, data = train[[i]], distribution = "multinomial", n.trees = m)
  pred.prob <- predict(fit, val[[i]][,4:8],type="response", n.trees=m)
  pred.prob <- matrix(pred.prob, ncol=length(fit$classes))
  pred <- fit$classes[apply(pred.prob, 1, which.max)]
  err_rate <- mean(pred!=val[[i]][,2])
  return(err_rate)
}

t_id <- "WO"
for (m in seq(500, 10000, length.out = 100)){
  print(paste(t_id, m, find_M(m, t_id)))
}

##########################################################################################################
#############  함수 정의하여 덮어쓰기
##########################################################################################################
# 위의 사례를 일반화 하여 함수에 담아 보았다.

#######################################################
# train_year   = Training set으로 사용한 데이터
# val_year     = Validation set으로 사용한 데이터
# test_year    = Test set으로 사용한 데이터
######################################################

do_randomForest(train_year = 2016:2018, val_year = 2019, test_year = 2020) ## 2026~2018 훈련, 2019 검증, 2020 테스트
do_randomForest(train_year = 2016:2019, val_year = 2020, test_year = NULL) ## 2016~2019 훈련, 2020 검증, 테스트 없음
do_randomForest(train_year = 2016:2018, val_year = 2020, test_year = NULL) ## 2016~2018 훈련, 2019 훈련에 사용하지 않음


## 함수 정의

do_randomForest <- function(train_year, val_year, test_year=NULL){
  train   <- list(HH=data.frame(), HT=data.frame(), KT=data.frame(), LG=data.frame(), LT=data.frame(), NC=data.frame(), OB=data.frame(), SK=data.frame(), SS=data.frame(), WO=data.frame())
  val     <- list(HH=data.frame(), HT=data.frame(), KT=data.frame(), LG=data.frame(), LT=data.frame(), NC=data.frame(), OB=data.frame(), SK=data.frame(), SS=data.frame(), WO=data.frame())
  test    <- list(HH=data.frame(), HT=data.frame(), KT=data.frame(), LG=data.frame(), LT=data.frame(), NC=data.frame(), OB=data.frame(), SK=data.frame(), SS=data.frame(), WO=data.frame())
  
  for (i in 1:10){
    train[[i]]  <- data %>% select(T_ID, W, YEAR, P:S) %>% filter(YEAR %in% train_year) %>% filter(T_ID==team[i])
    val[[i]]    <- data %>% select(T_ID, W, YEAR, P:S) %>% filter(YEAR %in% val_year) %>% filter(T_ID==team[i])
    if(!is.null(test_year)){
      test[[i]]   <- data %>% select(T_ID, W, YEAR, P:S) %>% filter(YEAR %in% test_year) %>% filter(T_ID==team[i])
    }
  }
  
  print("############################")
  print("## Bagging                ##")
  print("############################")
  
  
  fit.bagg      <- list(HH=NULL, HT=NULL, KT=NULL, LG=NULL, LT=NULL, NC=NULL, OB=NULL, SK=NULL, SS=NULL, WO=NULL)
  pred.bagg     <- list(HH=NULL, HT=NULL, KT=NULL, LG=NULL, LT=NULL, NC=NULL, OB=NULL, SK=NULL, SS=NULL, WO=NULL)
  pred_p.bagg   <- list(HH=NULL, HT=NULL, KT=NULL, LG=NULL, LT=NULL, NC=NULL, OB=NULL, SK=NULL, SS=NULL, WO=NULL)
  
  for (i in 1:10){
    # 모형적합 -----------
    fit.bagg[[i]] <- ipredbagg(y=train[[i]][,2], X=train[[i]][,4:8], nbagg=1000)
    fit.bagg[[i]]
    # 그룹예측 ---------------
    pred.bagg[[i]] <- predict(fit.bagg[[i]], newdata = val[[i]][,4:8])
    print(table(pred.bagg[[i]], val[[i]][,2]))
    # 오분류율 ---------------
    print(paste(team[[i]], ":: 오분류율 = ", mean(pred.bagg[[i]]!=val[[i]][,2])*100, "%"))
    # 확률예측 ---------------
    pred_p.bagg[[i]] <- predict(fit.bagg[[i]], val[[i]][,4:8], type="prob")
    head(pred_p.bagg[[i]])
  }
  
  print("##############################")
  print("## Boosting                 ##")
  print("##############################")
  
  fit.boosting      <- list(HH=NULL, HT=NULL, KT=NULL, LG=NULL, LT=NULL, NC=NULL, OB=NULL, SK=NULL, SS=NULL, WO=NULL)
  pred_p.boosting   <- list(HH=NULL, HT=NULL, KT=NULL, LG=NULL, LT=NULL, NC=NULL, OB=NULL, SK=NULL, SS=NULL, WO=NULL)
  pred.boosting     <- list(HH=NULL, HT=NULL, KT=NULL, LG=NULL, LT=NULL, NC=NULL, OB=NULL, SK=NULL, SS=NULL, WO=NULL)
  
  for (i in 1:10){
    # 모형적합 -------
    fit.boosting[[i]] <- gbm(W ~ P+D+B+C+S  , data = train[[i]], distribution = "multinomial", n.trees=500)
    # summary(fit.boosting[[i]])
    
    # 확률예측 -------
    pred_p.boosting[[i]] <- predict(fit.boosting[[i]], val[[i]][,4:8], type="response", n.trees=500)
    pred_p.boosting[[i]] <- matrix(pred_p.boosting[[i]], ncol=2)
    colnames(pred_p.boosting[[i]]) <- levels(val[[i]][,2])
    head(pred_p.boosting[[i]])
    
    # 분류 -----------
    pred.boosting[[i]] <- apply(pred_p.boosting[[i]], 1, which.max)
    pred.boosting[[i]] <- ifelse(pred.boosting[[i]]==1, "LOSE", "WIN")
    print(table(pred.boosting[[i]], val[[i]][,2]))
    
    # 오분류율 -------
    print(paste(team[i], ":: 오분류율 = ", mean(pred.boosting[[i]]!=val[[i]][,2])*100, "%"))
  }  
}


#############################
## 머신러닝 결과 분석하기

# HT의 모델학습 결과
str(fit.bagg[[2]])
str(fit.boosting[[2]])

# TODO... 어떻게 해석하는가?
# TODO... Random Forest와 Bagging, Boosting의 차이점은??
