### 거리계산
#학생 5명의 국어,영어 점수
s1 <- c(70, 65)
s2 <- c(90, 95)
s3 <- c(65, 60)
s4 <- c(85, 90)
s5 <- c(60, 75)
s_all <- rbind(s1, s2, s3, s4, s5)
s_all

#맨해튼 거리
dist1 <- as.matrix(dist(s_all, method = 'manhattan'))
dist1

#유클리드 거리
dist2 <- as.matrix(dist(s_all, method = 'euclidean'))
dist2

library(proxy)
#코사인 거리
dist3 <- as.matrix(dist(s_all, method = 'cosine'))
dist3


a <- c(1, 0, 5)
b <- c(4, 7, 3)
c <- c(40, 70, 30)
items <- rbind(a,b,c)
colnames(items) <- c('v1','v2','v3')
items

# 코사인 거리 계산 (1-코사인유사도)
dist4 <- as.matrix(dist(items, method = 'cosine'))
dist4
# b,c처럼 두 벡터의 값이 같은 방향, 같은 배수로 증가할 경우
# : 코사인 거리 0, 코사인 유사도 1



### 최적의 k 계산
df <- read.csv("c:/vscode/data/ozone/ozone2.csv")
library(dplyr)
# 필드 제거
df <- df %>% select(-Ozone, -Month, -Day)

# under sampling
library(ROSE)
df_samp <- ovun.sample(Result ~ ., data = df, seed = 1, 
                        method = 'under', N = 72*2)$data
tb1 <- table(df_samp$Result)
tb1

library(caret)
set.seed(123)
#학습용:검증용 8:2로 구분
idx_train <- createDataPartition(y=df_samp$Result, p=0.8, list = FALSE)
#학습용
train <- df_samp[idx_train,]
X_train <- train[, -1]
y_train <- train[, 1]
#검증용
test <- df_samp[-idx_train, ]
X_test <- test[, -1]
y_test <- test[, 1]

library(e1071)
#최적의 k값을 찾는 함수, 10회 교차검증
tune.out <- tune.knn(x = X_train, y = as.factor(y_train), k = 1:10)
tune.out    # best k = 3
win.graph(); plot(tune.out)

library(class)
pred <- knn(X_train, X_test, y_train, k = 3)
tbl <- table(real = y_test, predict = pred)
tbl

(tbl[1,1]+tbl[2,2])/sum(tbl) #정확도

install.packages('gmodels')
library(gmodels)
#정오분류표에 카이제곱검정값을 세부적으로 출력하는 함수
CrossTable(y_test, pred)
install.packages('Epi')
library(Epi)
win.graph(); ROC(test=pred, stat=y_test, plot="ROC", AUC=T, main="KNN")



### 최근접 이웃 회귀모형
df <- read.csv("c:/vscode/data/ozone/ozone2.csv")
# 필드 제거
df<-df %>% select(-Month,-Day,-Result)
#학습용:검증용 8:2로 구분
idx_train <- createDataPartition(y=df$Ozone, p=0.8, list=FALSE)
#학습용
train <- df[idx_train, ]
X_train <- train[, -1]
y_train <- train[, 1]
#검증용
test <- df[-idx_train, ]
X_test <- test[, -1]
y_test <- test[, 1]

install.packages('FNN')
library(FNN)
#분류 정확도를 저장할 비어있는 벡터
acc <- NULL
for(i in c(1:10)){
    set.seed(123) #재현성을 위해 설정
    model<- knn.reg(X_train, X_test, y_train, k=i)
    m<-mean((model$pred - y_test)^2)
    acc <- c(acc, m)
}
#차트를 그리기 위해 데이터프레임으로 변환
df <- data.frame(k=c(1:10), mse=acc)
# k에 따른 분류 정확도 그래프 그리기
win.graph(); plot(mse ~ k, data = df, type = "o", pch = 20, main = "최적의 k값", col="red")
# 그래프에 k 라벨링 하기
with(df, text(mse ~ k, labels = c(1:10), pos = 1, cex = 0.7))

n <- min(df[df$mse %in% min(acc), "k"])
df[n,]

model <- knn.reg(X_train, X_test, y_train, k=10)
model$pred #예측값
#Mean Squared Error(평균제곱오차)
mean((model$pred - y_test)^2)
