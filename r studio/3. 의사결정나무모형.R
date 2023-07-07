
## C5.0 모형
df <- read.csv("c:/vscode/data/ozone/ozone2.csv")

library(dplyr)
df <- df %>% select(-Ozone, -Month, -Day)

# 불균형 데이터셋
tbl<-table(df$Result)
win.graph(); barplot(tbl, beside = T, legend = T, col =
                       rainbow(2))
# under sampling
library(ROSE)
df_samp <- ovun.sample(Result ~ . ,data = df, seed=1, method =
                         "under", N=72*2)$data
tbl<-table(df_samp$Result)

library(caret)
#랜덤 시드 고정
set.seed(123)

#학습용:검증용 8:2로 구분
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
head(X_train)
head(y_train)

# install.packages('C50')
library(C50)
y_train<-as.factor(y_train)

# trials 최대 10개의 모형을 생성
model <- C5.0(y_train ~ ., data=X_train, trials=10)
summary(model)

# 트리 그래프
plot(model, uniform=T, main="Tree")
text(model, use.n=T, all=T, cex=.8)

# predict(트리모델, 학습용데이터셋)
pred <- predict(model,newdata=X_test)
pred

#분류 모델 평가를 위해 오분류표(confusion matrix) 출력
table(y_test, pred)
mean(y_test == pred)

y_test_f<-as.factor(y_test)
confusionMatrix(y_test_f, pred)



### ctree 모형
df <- read.csv("c:/vscode/data/ozone/ozone2.csv")
library(dplyr)
df <- df %>% select(-Ozone)



### 교차검증
# 데이터셋 구조
names(iris)
attributes(iris)
str(iris)

# iris 데이터셋을 8:2 비율로 구분
set.seed(123)
dim(iris)
idx <- sample(1:nrow(iris), nrow(iris)*0.8)
tr <- iris[idx,]
te <- iris[-idx,]
dim(tr)

# K겹 교차검정 데이터셋 생성
name<-c('김철수','이미영','홍상수','이찬수','송희연','최승희','박만수','민지영','정선호','한상수')
score <- c(90,85,70,85,60,66,77,88,99,100)
df <- data.frame(Name=name, Score=score)
df

install.packages('cvTools')
set.seed(123)
library(cvTools)

cross <- cvFolds(n=10, K=5, type="random")
cross
cross$subsets[cross$which==1,1] # K=1인 경우
cross$subsets[cross$which==2,1] # K=2인 경우
cross$subsets[cross$which==3,1] # K=3인 경우
cross$subsets[cross$which==4,1] # K=4인 경우
cross$subsets[cross$which==5,1] # K=5인 경우
str(cross)

# 연속된 데이터를 순서대로 검증용 데이터로 사용하는 방식
cross <- cvFolds(n=10, K=5, type="consecutive")
cross

#연속된 데이터를 차례로 서로 다른 k의 검증 데이터로 할당하는 방식
cross <- cvFolds(n=10, K=5, type="interleaved")
cross

for(k in 1:5){
    idx <- cross$subsets[cross$which==k ,1]
    cat('\nk=',k,'학습용\n')
    print(df[-idx, ])
    cat('\nk=',k,'검증용\n')
    print(df[idx, ])
}


install.packages('e1071')
set.seed(123)
library(caret)
library(e1071)
#method='cv' 교차검증, number=5 5회
trControl <- trainControl(method = "cv", number = 5)
#tuneGrid k값을 1~10까지 테스트
fit <- train(Species ~ .,
            method = "knn",
            tuneGrid = expand.grid(k = 1:10),
            trControl = trControl,
            metric = "Accuracy",
            data = iris)
fit

set.seed(123)
library("class")
#구간 설정
group1 <- cut(seq(1,50),breaks=5,labels=F)
group2 <- cut(seq(51,100),breaks=5,labels=F)
group3 <- cut(seq(101,150),breaks=5,labels=F)
fold <- c(group1, group2, group3)
acc <- c()
#실험 5회
for (i in 1:5){
    ds.tr <- iris[fold != i, 1:4] #학습용
    ds.te <- iris[fold == i, 1:4] #검증용
    cl.tr <- factor(iris[fold != i, 5]) #품종필드
    cl.te <- factor(iris[fold == i, 5])
    #knn 모형 knn(학습용,검증용,라벨,이웃의수)
    pred <- knn(ds.tr, ds.te, cl.tr, k = 7)
    #정확도
    acc[i] <- round(mean(pred==cl.te) * 100,1)
}
acc
mean(acc)



### GridSearchCV 파라미터 최적화
#pima indian 당뇨병 데이터셋
df<-read.csv("c:/vscode/data/pima/data.csv")
#불균형 데이터셋
tbl<-table(df$outcome)
tbl
win.graph(); barplot(tbl, beside = TRUE, legend = TRUE, col = rainbow(2))

# under sampling
library(ROSE)
# method: under,over,both N: 샘플링 후의 샘플 개수(적은 쪽 x 2) 또는 p=0.5 50:50으로 선택
df_samp <- ovun.sample(outcome ~ . ,data = df, seed=1, method = "under", N=268*2)$data
tbl<-table(df_samp$outcome)
tbl

X<-df_samp[, -9]
y<-df_samp[, 9]

#랜덤 포레스트 모형
install.packages('randomForest')
library(randomForest)
library(superml)
# XGBTrainer, RFTrainer, NBTrainer
# 클래스이름$new() : 인스턴스 생성하는 문법
rf <- RFTrainer$new()
gst <-GridSearchCV$new(trainer = rf,
                    parameters = list(n_estimators = c(10,50,100),
                    max_depth = c(2,5,10)),
                    n_folds = 3,
                    scoring = c('accuracy','auc'))
gst$fit(df_samp,'outcome')
gst$best_iteration()
#트리개수 100, max_depth 2, 정확도 75%, auc: 0.75