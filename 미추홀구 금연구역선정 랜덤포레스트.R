# 데이터 불러오기
data<-read.csv(file.choose())

head(data,1)


# 박스플롯 이용하여 y값 구분
boxplot.stats(data$y)
boxplot(data$y)

#파생변수 Degree(충전소 필요 정도) 생성
data$Degree <- ifelse(data$y < 2.0001, 1, 
                      ifelse(data$y<4.0001, 2, 
                             ifelse(data$y< 9.5001, 3, 4)))


#data의 record 개수 구하기
numrow <- nrow(data)

#시드를 고정
#train set과 test set을 7:3으로 split
set.seed(123)

splt <- sample(1:numrow, size = as.integer(0.7*numrow))

train_set <- data[splt,]
test_set <- data[-splt,]

train_set$Degree <- as.factor(train_set$Degree)
test_set$Degree <- as.factor(test_set$Degree)

train_set <- train_set[,-1]
test_set <- test_set[,-1]


#랜덤포레스트 기법 사용하여 data에 fit model 생성
#그리드 검색을 수행하여 5중 교차검정 진행, 모델의 최적 하이퍼파라미터 도출
library(randomForest)
library(e1071)
library(MLmetrics)

rf_grid <- tune(randomForest, train.y=train_set$Degree, train.x=subset(train_set, select = -Degree),
                data = train_set, ranges = list(mtry = c(3,4,5),
                                                ntree = c(700,800,900,1000),
                                                nodesize = c(1,2)),
                tunecontrol = tune.control(cross = 5))

summary(rf_grid)
best_model <- rf_grid$best.model
summary(best_model)

#변수 중요도 그래프
vari <- varImpPlot(best_model)
print(paste("Variable Importance - Table"))
print(vari)


#model에 train set을 입력하고 target attribute(금연장소가 필요한 정도: 1~4단계) 예측
y_pred_train <- predict(best_model, data=train_set)

#model의 confusion matrix(혼동행렬) 생성
train_conf_mat <- table(train_set$Degree, y_pred_train)

#model의 confusion matrix(혼동행렬) 출력
print(paste("Train Confusion Matrix - Grid Search:"))
print(train_conf_mat)

#model 성능의 평가 척도 중, 정확도(accuracy)를 계산 및 출력
train_acc <- (train_conf_mat[1,1]+train_conf_mat[2,2]+train_conf_mat[3,3]+train_conf_mat[4,4])/sum(train_conf_mat)
print(paste("Train Confusion Matrix - Grid Search Accuracy:", round(train_acc,4)))

#F1-score 계산
F1_Score(train_set$Degree, y_pred_train, positive = NULL)



#---------------------- test set ----------------------

#model에 test set을 입력하고 target attribute(충전소가 필요한 정도: 1~4단계) 예측
y_pred_test <- predict(best_model, newdata = test_set)

#model의 confusion matrix(혼동행렬) 생성
test_conf_mat <- table(test_set$Degree, y_pred_test)

#model의 confusion matrix(혼동행렬) 출력
print(paste("Test Confuision Matrix- Grid Search:"))
print(test_conf_mat)

#model 성능의 평가 척도 중, 정확도(accuracy)를 계산 및 출력
test_acc <- (test_conf_mat[1,1]+test_conf_mat[2,2]+test_conf_mat[3,3]+test_conf_mat[4,4])/sum(test_conf_mat)
print(paste("Test Confusion Matrix - Grid Search Accuracy:", round(test_acc,4)))

#F1-score 계산
F1_Score(test_set$Degree,  y_pred_test, positive = NULL)

plot(data,col=data$Degree)


# 격자 나눈 데이터 예측
data1<-read.csv(file.choose())
pre<-predict(best_model,newdata = data1)
write.csv(pre,"predict1.csv")

table(pre)
