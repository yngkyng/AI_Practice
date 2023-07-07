df <- data.frame(x=seq(-5,5,legth.out=100))
df
df$y <- exp(df$x)/(1+exp(df$x))
plot(df, type='l')

## 오존량예측
df<-read.csv("c:/vscode/data/ozone/ozone2.csv")
library(dplyr)
# 필드 제거
df <- df %>% select(-Ozone,-Month,-Day)
#상관계수 행렬
(corrmatrix <- cor(df))
# 강한 양의 상관관계, 강한 음의 상관관계
corrmatrix[corrmatrix > 0.5 | corrmatrix < -0.5]
library(corrplot)
corrplot(cor(df), method="circle")
#불균형 데이터셋
tbl <- table(df$Result)
tbl
barplot(tbl, beside = TRUE, legend = TRUE, col = rainbow(2))
# 언더샘플링
# install.packages("ROSE")
library(ROSE)
df_samp <- ovun.sample(Result ~ . ,data = df, seed=1, 
                       method = "under", N=144)$data
tbl<-table(df_samp$Result)
tbl
library(caret)
#랜덤 시드 고정
set.seed(123)
idx_train <- createDataPartition(y=df_samp$Result, p=0.8,
                                 list=FALSE)
#학습용
train <- df_samp[idx_train, ]
X_train <- train[, -1]
y_train <- train[, 1]
#검증용
test <- df_samp[-idx_train, ]
X_test <- test[, -1]
y_test <- test[, 1]
#로지스틱 회귀모델 생성
model <- glm(Result ~ ., data=train)
#모델정보 요약
summary(model)
#회귀계수 확인
(coef1 <- coef(model))
#예측값을 0~1 사이로 설정
pred <- predict(model, newdata=X_test)
pred
#0.5보다 크면 1, 아니면 0으로 설정
result <- ifelse(pred>0.5,1,0)
#예측정확도
mean(y_test == result)
#오분류표 출력
table(y_test, result)
# install.packages("ROCR")
library(ROCR)
pr <- prediction(pred, y_test)
pr@predictions #출력값
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
win.graph()
plot(prf, main="ROC Curve")
# AUC (The Area Under an ROC Curve)
auc <- performance(pr, measure = "auc")
auc@y.values
auc@y.values[[1]]



## 붓꽃품종
df <- read.csv('c:/vscode/data/iris/iris.csv')
tail(df)
library(dplyr)
df <- df %>% select(-Name)
(corrmatrix <- cor(df))
library(corrplot)
corrplot(cor(df), method = 'circle')
library(caret)
set.seed(123)
idx_train <- createDataPartition(y=df$Species, p=0.8, list=FALSE)
train <- df[idx_train,]
X_train <- train[, -5]
y_train <- train[, 5]
head(X_train)
head(y_train)
test <- df[-idx_train,]
X_test <- test[, -5]
y_test <- test[,5]
head(X_train)
install.packages('nnet')
library(nnet)
model <- multinom(Species~., data=train)
summary(model)
(coef1 <- coef(model))
pred <- predict(model, newdata=X_test)
pred
mean(y_test == pred)
table(y_test, pred)
pred <- predict(model, newdata=X_test, type='probs')
pred
result <- ifelse(pred>0.5, 1, 0)
head(result)
new_result <- c()
for(i in 1:nrow(result)){
  for(j in 1:ncol(result)){
    if(result[i,j]==1){
      new_result[i] <- j-1 # 품종이 0,1,2 이므로 1을 빼야 함
    }
  }
}
y_test == new_result
mean(y_test == new_result)
table(y_test,new_result)


### 와인품질
df<-read.csv("c:/vscode/data/wine/wine_new.csv")
head(df)
library(dplyr)
df<-df %>% select(-quality)
library(corrplot)
corrplot(cor(df), method="circle")
tbl<-table(df$class)
barplot(tbl, beside = TRUE, legend = TRUE, col = rainbow(2))
library(ROSE)
df_samp <- ovun.sample(class ~ . ,data = df, seed=1, method =
                         "under", N=744*2)$data
tbl<-table(df_samp$class)
library(caret)
set.seed(123)
idx_train <- createDataPartition(y=df_samp$class, p=0.8,
                                 list=FALSE)
#학습용
train <- df_samp[idx_train, ]
X_train <- train[, -12]
y_train <- train[, 12]
#검증용
test <- df_samp[-idx_train, ]
X_test <- test[, -12]
y_test <- test[, 12]
model <- glm(class ~ ., data=train, family=binomial)
pred <- predict(model, newdata=X_test, type='response')
result <- ifelse(pred>0.5,1,0)
mean(y_test == result)
reduced<-step(model, direction='backward')
pred <- predict(reduced, newdata=X_test, type='response')
result <- ifelse(pred>0.5,1,0)
mean(y_test == result)
table(y_test, result)



### 타이타닉 오즈비
df<-read.csv("c:/vscode/data/titanic/train3.csv")
library(ROSE)
df_samp <- ovun.sample(Survived ~ . ,data = df, seed=1, method
                       = "under", N=342*2)$data
tbl<-table(df_samp$Survived)

library(caret)
set.seed(123)
idx_train <- createDataPartition(y=df_samp$Survived, p=0.8,
                                 list=FALSE)
#학습용
train <- df_samp[idx_train, ]
X_train <- train[, -1]
y_train <- train[, 1]
#검증용
test <- df_samp[-idx_train, ]
X_test <- test[, -1]
y_test <- test[, 1]
model <- glm(Survived ~ ., data=train)
pred <- predict(model, newdata=X_test, type='response')
result <- ifelse(pred>0.5,1,0)
mean(y_test == result)
#OR(odds ratio, 오즈비=승산비=교차비)
exp(coef1)
