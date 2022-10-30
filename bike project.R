#Install Packages
install.packages('dplyr','plyr','caret', 'ROCR')

#Install Library 
#패키지 순서는 plyr 다음 dplyr
library(plyr)
library(dplyr)
library(tidyverse)
library(caret)
library(ROCR)
library(pROC)

#Setting the directory
setwd("C:/Users/Administrator/Desktop/Bike")

#Load the CSV files
fail <- read.csv("서울시 공공자전거 고장신고 내역_22.01-06.csv",header=T, fileEncoding="euc-kr")
record1 <- read.csv("서울특별시 공공자전거 대여이력 정보_22.01.csv",header=T, fileEncoding="euc-kr")
record2 <- read.csv("서울특별시 공공자전거 대여이력 정보_22.02.csv", header=T, fileEncoding="euc-kr")
record3 <- read.csv("서울특별시 공공자전거 대여이력 정보_22.03.csv", header=T, fileEncoding="euc-kr")

#Check the data
head(fail)



#고장난 자전거 모델이 다른 모델과 겹치는지 확인하기
##고장신고에서, 대여이력에서 id col만 빼기
fail_id <- fail[,1]
head(fail_id)

record1_id <- record1[,1]
head(record1_id)

record2_id <- record2[,1]
head(record2_id)

record3_id <- record3[,1]
head(record3_id)

#고장기록과 사용기록1 사이 겹치는 자전거 번호만 고르기
fail$intersect = ifelse(fail$자전거번호 %in% record1$자전거번호, 'yes','no')
record1$intersect = ifelse(record1$자전거번호 %in% fail$자전거번호, 'yes', 'no')

fail_1 <- fail[!(fail$intersect =='no'),]
record1_1 <- record1[!(record1$intersect == 'no'),]

head(fail_1)

fail_1 <- fail_1[order(fail_1$자전거번호),]
record1_1 <- record1_1[order(record1_1$자전거번호),]


#고장기록에서 각각 자전거번호마다 고장 횟수 종합하기
##fail_id는 중복된 자전거 번호를 없앤 리스트
fail_id <- fail_1[,1]
head(fail_id)
fail_id <- unique(fail_id)
head(fail_id)
length(fail_id)
#vector length = 18685

record1_id <- record1_1[,1]
head(record1_id)
record1_id <- unique(record1_id)
length(record1_id)
#vector length = 18685

#두 df 모두 중복된 데이터를 삭제하니 18685개의 데이터만 남으므로 공통된것만 남았다고 확인 가능
#fail은 고장기록원본데이터, fail_1은 사용기록데이터에도 존재하는 자전거 번호만 남긴 데이터, fail_id는 fail_1에서 중복을 제외한 벡터

##fail_id마다 몇번의 고장 신고가 접수됬는지 확인하기
fail_sum <- ddply(fail_1, .(자전거번호), nrow)
fail_sum
#fail_sum애서 횟수 col 이름은 v1임

#우리는 사용기록에서 이용시간과 이용거리만 관심이 있음
##이용시간 총합 구하기
head(record1_1)
record1_time<- record1_1[,c('자전거번호','이용시간')]
head(record1_time)
record1_time <- record1_time[order(record1_time$자전거번호),]
record1_time_sum <- ddply(record1_time, .(자전거번호), summarise,time_sum=sum(이용시간))
head(record1_time_sum)


##이용거리 총합 구하기
record1_distance <- record1_1[,c('자전거번호', '이용거리')]
head(record1_distance)
record1_distance <- record1_distance[order(record1_distance$자전거번호),]
record1_distance_sum <- ddply(record1_distance, .(자전거번호), summarise,distance_sum=sum(이용거리))
head(record1_distance_sum)

#각각 자전거번호마다 고장횟수, 이용거리, 이용시간 활용하여 다중회귀분석 진행하기
## 고장횟수 = 반응변수 / 이용시간, 이용거리 = 독립변수
bike_reg <- cbind(fail_sum, record1_time_sum, record1_distance_sum)
head(bike_reg)
bike_reg <- bike_reg[,c(-3,-5)]
bike_reg <- rename(bike_reg,c("V1" = "fail_freq")) #ERROR , 냅두고 V1을 fail_freq로 진행하기

reg1 <- lm(formula= V1 ~ time_sum + distance_sum, data=bike_reg)
summary(reg1)

#y(고장횟수) = 2.862 + 0.0001945time - 2.329^e-7distance
#하지만 distance가 p-value 충족하지 못함. (null값이 많아서 그런듯함)

#distance null값이 몇개인지 확인해보기


## 고장횟수 = 반응변수 / 이용시간 = 독립변수
reg2 <- lm(formula = V1 ~ time_sum, data=bike_reg)
summary(reg2)

#y(고장횟수) = 2.861 + 0.000175time , intercept와 x1 모두 p-value 만족함
#대략 5991분(100시간)을 사용하면 평균 3회 고장난다는것을 알 수 있음



#고장이 나거나 고장이 안나거나 둘중 하나로 만드려면 다중 로지스틱으로 돌려야함
##fail 데이터와 record1 사이에서 고장이 안난 데이터를 사용하기
###record1_1은 고장났던데이터였음, record1에서 record1_1을 제외한 데이터가 고장 안나고 운행기록만 있는 데이터임

normal <- record1[!(record1$자전거번호 %in% record1_1$자전거번호),] 
head(normal)
nrow(normal)
head(record1_1)

#고장 안남 = 0, 고장남 =1 
logistic_bike <- rbind(normal, record1_1)
nrow(logistic_bike)

logistic_bike <- logistic_bike %>% 
  mutate(failure=ifelse(logistic_bike$intersect=='no', 0,1))

head(logistic_bike)

log1 <- glm(formula = failure~ 이용시간 + 이용거리,family = binomial, data = logistic_bike)
summary(log1)

#intercept, 이용시간, 이용거리 모두 통계적으로 의미있는 결과로 나타남
#로지스틱 회귀 0과 1둘중 


#record1을 가지고 모델을 만들었으니, record2, record3과 비교해보기
##record2 또한 겹치는 데이터셋 생성하기
fail$intersect = ifelse(fail$자전거번호 %in% record2$자전거번호, 'yes','no')
record2$intersect = ifelse(record2$자전거번호 %in% fail$자전거번호, 'yes', 'no')

fail_2 <- fail[!(fail$intersect =='no'),]
record2_1 <- record2[!(record2$intersect == 'no'),]

head(fail_2)

fail_2 <- fail_2[order(fail_2$자전거번호),]
record2_1 <- record2_1[order(record2_1$자전거번호),]

#fail_2는 record2 와 공통된 자전거 번호만 모아놓은 결과, record2_1은 fail_2와 겹치는 데이터만 모아놓은 결과

head(record2_1)
nrow(record2_1)

normal_2 <- record2[!(record2$자전거번호 %in% record2_1$자전거번호),]
nrow(normal_2)

logistic_bike_2 <- rbind(normal_2, record2_1)
nrow(logistic_bike_2)


logistic_bike_2 <- logistic_bike_2 %>% 
  mutate(failure=ifelse(logistic_bike_2$intersect=='no', 0,1))


#confint로 신뢰구간 측정 (시간좀 걸림)
confint(log1)
#신뢰구간 또한 0.05를 만족함

#카이제곱 검정을 사용한 ANOVA Test(이탈도 검정)
anova(log1, test="Chisq")
#Anova Test에서도 이용시간, 이용거리 모두 Pr(>Chi)가 0.05이상으로 나옴 


#record2를 test 셋으로 돌려서 혼동행렬로 Accuracy, ROC 그리기
predict_prob <- predict(log1, newdata = logistic_bike_2, type='response')
predict_failure <- ifelse(predict_prob > 0.5, 1, 0)
predict_result <- data.frame(actual=logistic_bike_2$failure, predicted= predict_failure, pred_prob=predict_prob)
head(predict_result,10)

arrange(predict_result, pred_prob)
confusionMatrix(as.factor(logistic_bike_2$failure), as.factor(predict_result$predicted))

#혼동행렬에서는 Accuracy가 84.61로 나옴 (상당히 괜찮은 예상 모델)

#pROC library를 사용한 ROC와 AUC그래프
head(predict_prob)
predict_prob_num <- as.numeric(predict_prob)
result_validation <- pROC::roc(logistic_bike_2$failure, predict_prob_num)

pROC::plot.roc(result_validation, legacy.axes = TRUE)

result_validation$auc
#AUC값은 0.5065,일반적으로 AUC는 반드시 0.5보다 커야하고 0.7보다 커야만 수용할 수준이 된다. 
#AUC값은 

result_validation_ROCR <- prediction(predict_result$pred_prob, logistic_bike_2$failure)
plot(performance(result_validation_ROCR,"acc",'cutoff'))
#Accuracy와 cutoff를 기준으로 plot하면 cutoff 값은 0.8이하면 accuracy가  0.8이상으로 유지됨 




help(performance)
