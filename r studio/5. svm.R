### svm 모형의 개요
set.seed(100)
x <- matrix(rnorm(40), 20, 2)
y <- rep(c(-1,1), c(10,10))
x[y==1, ] <- x[y==1, ]+1
win.graph(); plot(x, col=y+3, pch=19)

library(e1071)
dat <- data.frame(x, y=as.factor(y))
svmfit <- svm(y~., data=dat, kernel='linear', cost=10, scale=F)
svmfit
win.graph(); plot(svmfit, dat)

make.grid <- function(x, n=75){
    grange <- apply(x,2,range)
    x1 <- seq(from=grange[1,1], to=grange[2,1], length=n)
    x2 <- seq(from=grange[1,2], to=grange[2,2], length=n)
    expand.grid(X1=x1, X2=x2)
}
xgrid <- make.grid(x)
xgrid[1:10,]
ygrid <- predict(svmfit, xgrid)

win.graph()
plot(xgrid, col=c('red','blue')[as.numeric(ygrid)], pch=20, cex=.2)
points(x, col=y+3, pch=19)
points(x[svmfit$index,], pch=5, cex=2)



### 콘크리트 분류
df<-read.csv("c:/vscode/data/concrete/concrete.csv")
library(dplyr)
# 필드 제거
df <- df %>% select(-strength)

#상관계수 행렬
head(corrmatrix <- cor(df))
library(corrplot)
corrplot(cor(df), method="circle")

# under sampling
library(ROSE)
df_samp <- ovun.sample(class ~ . ,data = df, seed=1, method ="under", N=507*2)$data
tbl=table(df_samp$class)

library(caret)
set.seed(123)
idx_train <- createDataPartition(y=df_samp$class, p=0.8, list=FALSE)
#학습용
train <- df_samp[idx_train, ]
X_train <- train[, -9]
y_train <- train[, 9]
#검증용
test <- df_samp[-idx_train, ]
X_test <- test[, -9]
y_test <- test[, 9]

library(reshape)
meltData <- melt(X_train)
boxplot(data=meltData, value~variable)

# 정규화된 데이터를 data.frame형태로 변경
X_train_scaled <- as.data.frame(scale(X_train))
X_test_scaled <- as.data.frame(scale(X_test))
# 데이터프레임 연결(가로방향)
train_scaled <- cbind(X_train_scaled, class=y_train)
test_scaled <- cbind(X_test_scaled, class=y_test)

meltData <- melt(X_train_scaled)
boxplot(data=meltData, value~variable)

# 가장 에러율이 적은 cost, gamma value 확인
library("e1071")
set.seed(123)
tune.out <- tune(svm, class ~ ., data = train_scaled, 
range=list(cost=c(0.001, 0.01, 0.1, 1, 10), gamma=c(0.0001, 0.001, 0.01, 0.1)))
summary(tune.out)

bestmodel <- tune.out$best.model
summary(bestmodel)

pred <- predict(bestmodel, X_train_scaled)
result <- ifelse(pred>0.5,1,0)
table(y_train, result)
mean(y_train == result)

pred <- predict(bestmodel, X_test_scaled)
result <- ifelse(pred>0.5,1,0)
table(y_test, result)
mean(y_test == result)



### ksvm 모형
# 교차검증을 지원하는 svm 모형
library(kernlab)
#스팸데이터
data(spam)
#학습용:검증용 5:5로 구분
index <- sample(1:dim(spam)[1])
spamtrain <- spam[index[1:2300], ]
spamtest <- spam[index[2301:4600], ]
#ksvm 모형
model <- ksvm(type~.,data=spamtrain,cross=3)
#분류
mailtype <- predict(model,spamtest[,-58])
#성능평가
table(mailtype,spamtest[,58])
mean(mailtype == spamtest[,58])

#
df<-read.csv("c:/vscode/data/iris/iris.csv")
library(dplyr)
# 필드 제거
df<-df %>% select(-Name)
tbl<-table(df$Species)
#학습용:검증용 8:2로 구분
idx_train <- createDataPartition(y=df$Species, p=0.8,
list=FALSE)
#학습용
train <- df[idx_train, ]
X_train <- train[, -5]
y_train <- train[, 5]
#검증용
test <- df[-idx_train, ]
X_test <- test[, -5]
y_test <- test[, 5]
#ksvm 모형
irismodel<- ksvm(as.factor(y_train)~.,data=X_train,cross=3)
#학습
fitted(irismodel)
pred<-predict(irismodel, X_test)
pred
table(y_test , pred)
mean(y_test == pred)
#svm 그래프
x <- rbind(matrix(rnorm(120),120,2),matrix(rnorm(120,mean=3),120,2))
y <- matrix(c(rep(1,60),rep(-1,60)))
svp <- ksvm(x,y,type="C-svc",cross=3) #분류 svm
win.graph(); plot(svp,data=x)
# svm회귀모형
x <- seq(-20,20,0.1)
y <- sin(x)/x + rnorm(401,sd=0.03)
regm <- ksvm(x,y,cross=3)
win.graph()
plot(x,y,type="l")
lines(x,predict(regm,x),col="red")



### 파라미터 최적화 (회귀)
df <- read.csv("c:/vscode/data/diabetes/data.csv")
library(dplyr)
# 필드 제거
df <- df %>% select(-target)
dim(df)
X <- df[, -11]
y <- df[, 11]
