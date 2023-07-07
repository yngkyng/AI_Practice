### 1. 고객이탈
df <- read.csv("c:/vscode/data/customer/customer.csv")
head(df)
dim(df)
summary(df)

#상관계수 행렬
head(corrmatrix <- cor(df))
library(corrplot)
corrplot(cor(df), method="circle")

#install.packages("reshape")
library(reshape)
meltData <- melt(df[,-17])
boxplot(data=meltData, value~variable)

# 데이터셋에서 특성별 최소값 계산
min_on_train = min(df[,-17])
# 데이터셋에서 특성별 (최대값 - 최소값) 범위 계산
range_on_train = max(df[,-17] - min_on_train)
# 데이터셋에서 최소값을 빼고 범위로 나누면
# 각 특성에 대해 최소값은 0 최대값은 1로 조정됨
X = (df[,-17] - min_on_train) / range_on_train
df_scaled = cbind(X, Result=df[,17])
head(df_scaled)
meltData <- melt(df_scaled[,-17])
boxplot(data=meltData, value~variable)

# 1. 최적의 군집수(k)를 설정하는 방법(엘보우 포인트)
result<-NULL
for (k in 1:10){
  result[[k]]<-kmeans(df_scaled[,-17],k)
}
wss <- numeric(10)
for(k in 1:10){
  wss[k]<-result[[k]]$tot.withinss
}
plot(wss,type="l")
#엘보우 포인트 : 2 

# 2. 최적의 군집수(k)를 설정하는 방법(실루엣 포인트)
library("cluster")
avgsil<-numeric(10)
for (k in 2:10){
  si<-
    summary(silhouette(result[[k]]$cluster,dist(df_scaled[,-17])))
  avgsil[k]<-si$avg.width
}
max(avgsil)
plot(avgsil,type="l")
# 실루엣 포인트가 최대가 되는 값 : k=2

# factoextra 패키지를 이용하여 적절한 클러스터 개수를 결정하고 시각화
#install.packages("factoextra")
library(factoextra)
fviz_nbclust(df_scaled[,-17], FUN=kmeans, method = "wss")
fviz_nbclust(df_scaled[,-17], FUN=kmeans, method =
               "silhouette")
#산점도 행렬(클러스터가 2개일 경우)
plot(df_scaled[,-17], pch=result[[2]]$cluster,
     col=result[[2]]$cluster)
set.seed(123)
# centers=군집수
model <- kmeans(df_scaled[,-17], centers = 2)
model
model$centers #중심좌표
#군집 분석 결과를 데이터프레임에 추가
#integer를 factor로 변환
df$cluster <- as.factor(model$cluster - 1)
#실제값과 클러스터링 결과를 확인해 보면 군집화는 대체로 잘 되
었으나
# 0,1이 1,0으로 처리됨
# 답을 미리 알려주지 않는 비지도학습이기에 label이 중요하지 않
음
# 1,0을 0,1로 변경하는 함수
head(df)
convert <- function(i){
  if( i == 0){
    return("1")
  }else if (i==1){
    return("0")
  }
}
#각 샘플에 convert() 함수를 적용
result<-sapply(df$cluster, convert)
head(result)
#오분류표
table(df$Churn, result)
#예측정확도
mean(result == df$Churn)



### 2. 붓꽃품종
df<-read.csv("c:/vscode/data/iris/iris.csv")
# 필드 제거
library(dplyr)
df<-df %>% select(-Name)
#Species 변수가 int로 되어 있는데 Factor 타입으로 변경해야 함
str(df)
df$Species <- as.factor(df$Species)
                  