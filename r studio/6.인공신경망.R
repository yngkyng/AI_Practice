df <- read.csv("c:/vscode/data/iris/iris.csv")
# 필드 제거
library(dplyr)
df <- df %>% select(-Name)
#Species 변수가 int로 되어 있는데 Factor 타입으로 변경해야 함
df$Species <- as.factor(df$Species)
summary(df)
#상관계수 행렬
(corrmatrix <- cor(df[1:4]))
library(corrplot)
corrplot(cor(df[1:4]), method="circle")
library(caret)
#랜덤 시드 고정
set.seed(123)
#학습용:검증용 8:2로 구분
#list=FALSE, 인덱스값들의 리스트를 반환하지 않음
idx_train <- createDataPartition(y=df$Species, p=0.8, list=FALSE)
#학습용
train <- df[idx_train, ]
X_train <- train[, -5]
y_train <- train[, 5]
#검증용
test <- df[-idx_train, ]
X_test <- test[, -5]
y_test <- test[, 5]

# 학습용 데이터를 이용하여 신경망 모형 생성
library(nnet)
# nnet(종속변수 ~ 독립변수)
model <- nnet(Species ~ ., data = train, size = 10)
summary(model)
names(model)
head(model$wts)
library(devtools)
# install.packages ('devtools')
#최적의 가중치 집합
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
plot.nnet(model)

# 신경망 모형의 검증(학습용 데이터)
# predict(신경망모델, 데이터셋)
pred <- predict(model, X_train, type="class")
table(y_train, pred)
mean(y_train == pred)

# 신경망 모형의 검증(검증용 데이터)
pred <- predict(model, X_test, type="class")
# result <- ifelse(pred>0.5,1,0)
table(y_test, pred)
mean(y_test == pred)




### deepnet 패키지 : 은닉층을 2개 이상 만들 수 있음
# install.packages('deepnet')
library(deepnet)
#and 문제
input<-matrix(c(0,0,1,1,
                0,1,0,1),ncol=2) #4행 2열의 행렬
input
output<-matrix(c(0,0,0,1),ncol=1) #4행 1열의 행렬
nn <- nn.train(input, output, hidden=c(2))
nn.predict(nn, input)
#학습이 부족함
#학습횟수 증가
nn <- nn.train(input, output, hidden=c(2), numepochs=1000)
nn.predict(nn, input)
nn <- nn.train(input, output, hidden=c(2), learningrate=10,
numepochs=10000)
nn.predict(nn, input)




#### 2차함수 neuralnet
# install.packages('neuralnet')
library(neuralnet)
set.seed(100)
X <- as.matrix(sample(seq(-2,2,length=50),50, replace=FALSE),
ncol=1)
y <- X^2
win.graph(); plot(y~X)
df<-as.data.frame(cbind(X,y))
colnames(df) <- c("X","y")
df
#신경망 모형
nn <- neuralnet(y~X, data=df, hidden=c(10,10))
win.graph(); plot(nn) #신경망 그래프
test<- as.matrix(sample(seq(-2,2,length=10),10, replace=FALSE),ncol=1)
pred<-predict(nn,test)
test^2 #실제값
pred #예측값
#Mean Squared Error(평균제곱오차)
mean((pred - test^2)^2)
result<- cbind(test, test^2, pred)
colnames(result) <- c("test","test^2","pred")
result



#### 다항분류(neuralnet)
df <- read.csv('c:/vscode/data/iris/iris.csv')
library(dplyr)
df <- df %>% select(-Name)
head(df)
tbl <- table(df$Species)
tbl
library(caret)
set.seed(123)
idx_train <- createDataPartition(y=df$Species, p=0.8, list=FALSE)
train <- df[idx_train, ]
X_train <- train[ , -5]
y_train <- train[ , 5]
test <- df[-idx_train, ]
X_test <- test[,-5]
y_test <- test[,5]

library(neuralnet)
set.seed(123)
model <- neuralnet(as.factor(Species)~., data=train, hidden = 10,
                    threshold = 0.01, linear.output = F)
model$result.matrix
win.graph(); plot(model)
pred <- predict(model, X_test, type='prob')
pred
result <- apply(pred, 1, function(x) which.max(x)-1)
result
table(y_test, result)




### 회귀분석(neuralnet)
df<-read.csv("c:/vscode/data/ozone/ozone2.csv")
library(dplyr)
df<-df %>% select(-Month, -Day, -Result)
library(caret)
set.seed(123)
idx_train <- createDataPartition(y=df$Ozone, p=0.8,list=FALSE)
train <- df[idx_train, ]
X_train <- train[, -4]
y_train <- train[, 4]
#검증용
test <- df[-idx_train, ]
X_test <- test[, -4]
y_test <- test[, 4]

library(neuralnet)
model <- neuralnet(Ozone ~ ., data=train, hidden=10,
                    threshold=0.05, linear.output = T, stepmax =1e7)
pred<-predict(model, X_test)
pred
mean((y_test - pred)^2)





### h2o
# install.packages('RCurl')
# install.packages('h2o')
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jdk1.8.0_361')
library(h2o)
localH2O = h2o.init()

train <- h2o.importFile("c:/vscode/data/iris/iris.csv")
test <- h2o.importFile("c:/vscode/data/iris/iris.csv")
X <- names(train)[1:4]
y <- names(train)[6]
train[,y] <- as.factor(train[,y])
test[,y] <- as.factor(test[,y])
iris_h2o <- as.h2o(iris, destination_frame = "iris_h2o")
h2o.ls()
class(iris_h2o)
head(iris_h2o)
n_rows <- nrow(iris_h2o)
n_cols <- ncol(iris_h2o)
paste("행의 개수 : ", n_rows)
set.seed(123)
train_idx <- sample(1:nrow(iris), size = 0.8 * nrow(iris),
                    replace = FALSE)
train_iris <- iris[train_idx, ]
test_iris <- iris[-train_idx, ]

#품종별 비율
with(train_iris, prop.table(table(Species)))
with(test_iris, prop.table(table(Species)))
#h2o 타입으로 변환
train_iris_h2o <- as.h2o(train_iris, "train_iris_h2o")
test_iris_h2o <- as.h2o(test_iris, "test_iris_h2o")
target <- "Species"
#독립변수들의 이름
features <- names(train_iris)[!names(train_iris) %in% target]
features
#로지스틱 회귀분석
glm_model <- h2o.glm(x = features, y = target, training_frame
                     = train_iris_h2o, model_id = "glm_model", family =
                       "multinomial")
summary(glm_model)
pred_iris_glm <- as.data.frame(h2o.predict(glm_model, newdata
                                           = test_iris_h2o))
#예측값
test_iris$pred_glm <- pred_iris_glm$predict
#오분류표 , dnn(Dimension Names)
# with - 데이터프레임의 필드명으로 직접 접근할 수 있는 함수
with(test_iris, table(Species, pred_glm, dnn = c("Real",
                                                 "Predict")))
#랜덤 포레스트 모형
rf_model <- h2o.randomForest(x = features, y = target,
                             training_frame = train_iris_h2o, model_id = "rf_model", ntrees =
                               100)
pred_iris_rf <- as.data.frame(h2o.predict(rf_model, newdata =
                                            test_iris_h2o))
#예측값
test_iris$pred_rf <- pred_iris_rf$predict
#오분류표
with(test_iris, table(Species, pred_rf, dnn = c("Real",
                                                "Predict")))




#### h2o (회귀)
df <- read.csv('c:/vscode/data/ozone/ozone2.csv')
library(dplyr)
df <- df %>% select(-Result)
library(caret)
set.seed(123)

idx_train <- createDataPartition(y=df$Ozone, p=0.8, list=F)
train <- df[idx_train, ]
X_train <- train[,-4]
y_train <- train[, 4]
#검증용
test <- df[-idx_train, ]
X_test <- test[, -4]
y_test <- test[, 4]

library(h2o)
h2o.init()
set.seed(123)
tr_data <- as.h2o(train)
te_data <- as.h2o(test)
target <- 'Ozone'

features <- names(train)[2:4]
features
model <- h2o.deeplearning(x=features, y=target,
                          training_frame = tr_data,
                          ignore_const_cols = FALSE,
                          hidden = c(8,7,5,5))
summary(model)
pred <- h2o.predict(model, te_data)
pred
perf <- h2o.performance(model, newdata=te_data)
perf
h2o.mse(perf)




#### 파라미터 최적화
library(nnet)
library(dplyr)
df<-read.csv("c:/vscode/data/ozone/ozone2.csv")
df <- df %>% select(-Ozone)
library(caret)
set.seed(123)
idx_train <- createDataPartition(y =df$Result, p=0.8, list=F)
train <- df[idx_train, ]
X_train <- train[, -1]
y_train <- train[, 1]
#검증용
test <- df[-idx_train, ]
X_test <- test[, -1]
y_test <- test[, 1]

test.rate <- function(h.size){
  model <- nnet(as.factor(Result)~., data = train, size=h.size)
  pred <- predict(model, X_test, type='class')
  rate <- mean(y_test==pred)
  c(h.size, rate)
}
sapply(1:5, FUN=test.rate)
out <- t(sapply(10:50, FUN=test.rate))
out
out[which.max(out[,2]),]
win.graph();plot(out, type = "b", xlab = "hidden nodes",
     ylab = "accuracy")
library('e1071')
set.seed(123)
tmodel <- tune.nnet(as.factor(Result)~., data=train, size=10:50)
summary(tmodel)
bestmodel <- tmodel$best.model
summary(bestmodel)
pred <- predict(bestmodel, X_train)
result <- ifelse(pred>0.5, 1,0)
table(y_train, result)
mean(y_train == result)

my.grid <- expand.grid(.decay=c(0.3), .size=10:50)
fit <- train(as.factor(Result)~., data=train, method='nnet',
             tuneGrid=my.grid, trace=F)
fit
pred <- predict(fit, newdata=test)
pred
table(y_test, pred)
mean(y_test == pred)
