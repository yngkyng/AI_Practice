x<-c(0,1,4,9)
y<-c(1,2,3,4)
z<-c(0,5,7,9)
mean(x)
mean(y)
mean(z)

cor(x,y,method="pearson")   #기본값
cor(x,y,method="spearman")

cor(y,z,method="pearson")
cor(y,z,method="spearman")

cor(x,z,method="pearson")
cor(x,z,method="spearman")


# 상관계수
x<-c(70,72,62,64,71,76,0,65,74,72)
y<-c(70,74,65,68,72,74,61,66,76,75)
cor.test(x,y,method="pearson")


## 공분산과 상관계수
df<-read.csv("c:/vscode/data/rides/rides.csv")
head(df)

plot(df$overall~df$rides) #산점도 # y ~ X
# rides와 overall은 양의 상관관계가 있는 것으로 보임
cov(df$overall, df$rides) #공분산

cov(1:5, 2:6)
cov(1:5, rep(3,5))
cov(1:5, 5:1) 
cov(c(10,20,30,40,50), 5:1)

#피어슨 상관계수
cor(df$overall, df$rides, method='pearson')
cor(df$overall, df$rides, use='complete.obs', method='pearson')
cor.test(df$overall, df$rides, method = "pearson",conf.level = 0.95) 

head(df[,4:8])
plot(df[,4:8])
pairs(df[,4:8], panel=panel.smooth)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(df[,4:8], histogram=TRUE, pch=19)

cor(df[,4:8])
install.packages('corrplot')
library(corrplot)
X<-cor(df[,4:8])
corrplot(X)
corrplot(X, method="number")
corrplot.mixed(X, lower='ellipse',upper='circle') 
# method: circle,square,ellipse,number,shade,color,pie
corrplot(X,order="hclust",addrect=3)



### 회귀분석

df<-read.csv("c:/vscode/data/ozone/ozone.csv")
head(df)


is.na(df)             #결측값 여부 확인
df[is.na(df$Ozone),]  # Ozone 필드에 결측값이 있는 행
sum(is.na(df))        #결측값의 개수
sum(is.na(df$Ozone))  #특정 필드의 결측값 개수
complete.cases(df)    #각 샘플의 모든 필드가 NA가 아닐 때 TRUE
                      #샘플에 결측값이 하나라도 있으면 FALSE
df[complete.cases(df),]#결측값이 없는 샘플 출력
df[!complete.cases(df),]#결측값이 있는 샘플 출력


mean(df$Ozone, na.rm=T) #결측값을 제외하고 계산
mapply(median, df[1:2], na.rm=T)#1~2번 필드의 중위수 계산


df2<-na.omit(df)  #결측값 제외 데이터
df3<-df
df3[is.na(df)]<-0 #결측값을 0으로 대체
df4<-df
df4$Ozone[is.na(df4$Ozone)]<-0 #특정한 필드 결측값만 0으로 대체
df5<-df
m1<-mean(df[,1], na.rm=T)
m2<-mean(df[,2], na.rm=T)
df5[,1][is.na(df[,1])]<-m1
df5[,2][is.na(df[,2])]<-m2  #결측값을 평균값으로 대체


# 결측값 시각화 패키지
install.packages('VIM')
install.packages('mice')
library(VIM)
library(mice)

win.graph()
md.pattern(df)  #결측값이 있는 필드 표시
aggr(df, prop = F, numbers = T) #결측값의 개수 표시
matrixplot(df)  # 결측값의 위치를 시각적으로 표현


## 스케일링
df<-read.csv("c:/vscode/data/rides/rides.csv")
df
#범주형 변수는 팩터 자료형으로 변환 후 스케일링 수행
# factor로 바꾸면 숫자로 따로 안 바꿔도 계산됨.
df$weekend <- as.factor(df$weekend)

install.packages("reshape")
library(reshape)

#????  melt() 필드 1개를 variable,value 로 여러 행으로 만드는 함수(차원변경)
meltData <- melt(df[2:7])
boxplot(data=meltData, value~variable)

#스케일링과 센터링
df_scaled <- as.data.frame(scale(df[2:7]))
meltData <- melt(df_scaled)
boxplot(data=meltData, value~variable)


#caret 패키지
install.packages('caret')
library(caret)
df<-read.csv("d:/data/rides/rides.csv")
meltData <- melt(df[2:7])
boxplot(data=meltData, value~variable)

#평균 0, 표준편차 1로 스케일링
prep <- preProcess(df[2:7], c('center','scale'))
df_scaled2 <- predict(prep, df[2:7])
meltData <- melt(df_scaled2)
boxplot(data=meltData, value~variable)

#range: 0~1 정규화 
prep <- preProcess(df[2:7], c("range"))
df_scaled3 <- predict(prep, df[2:7])
meltData <- melt(df_scaled3)
boxplot(data=meltData, value~variable)



## 이상치 처리
install.packages('car')
library(car)
df<-read.csv("d:/data/rides/rides.csv")

#회귀분석 모형
model<-lm(overall~num.child + distance + rides + 
            games + wait + clean, data=df)
summary(model)    #설명력 68.27%

# 1. 아웃라이어 
outlierTest(model) # 이상치 데이터 발견 - 184번 샘플
# 184번 샘플을 제거한 모형
model2<-lm(overall~num.child + distance + rides + games +
             wait + clean, data=df[-184,])
summary(model2)   #설명력이 68.27% => 68.76%로 개선됨

#2. 영향 관측치(influential observation) 
# 영향 관측치를 제거하면 더 좋은 모형이 될 수 있음
influencePlot(model)
model3=lm(overall~num.child + distance + rides + games +
            wait + clean, data=df[c(-184,-103,-367,-373),])
summary(model3)
# 설명력 69.12%



## 단순회귀분석
#20명의 신장과 체중 데이터
height <- c(179,166,175,172,173,167,169,172,172,179,161,174,166,176,182,175,177,167,176,177)
weight <- c(113,84,99,103,102,83,85,113,84,99,51,90,77,112,150,128,133,85,112,85)
plot(height,weight)

#상관계수 계산
cor(height,weight)

#기울기와 절편
slope <- cor(height, weight) * (sd(weight) / sd(height))
intercept <- mean(weight) - (slope * mean(height))
slope
intercept

#단순회귀분석 모델 생성
#체중 = 기울기x신장 + 절편
df <- data.frame(height, weight)
model <- lm(weight ~ height, data=df)
model
#절편(Intercept) -478.816   #기울기 3.347

#키가 180인 사람의 체중 예측
model$coefficients[[2]]*180 + model$coefficients[[1]]

summary(model)
plot(height,weight)
abline(model,col='red')

pred<-model$coefficients[[2]]*height + model$coefficients[[1]]
pred
err<-(weight-pred)^2  #오차의 제곱
sum(err) #오차의 제곱합
sum(err/length(weight)) #평균제곱오차

#최적의 가중치(기울기)를 구하기 위한 계산(경사하강법, Gradient Descent)
#여기서는 전체의 값이 아닌 1개의 값만 계산
x<-height[1]
y<-weight[1]
w<-seq(-1,2.3,by=0.0001) #가중치, by 간격
pred<-x*w #예측값
err<-(y-pred)^2 #제곱오차
plot(err)
#기울기가 0에 가까운 값이 최적의 기울기
min(err) #최소오차
i<-which.min(err)
paste('최적의 기울기=',w[i])

#최적의 편향(절편)을 구하기 위한 계산
x<-height[1]
y<-weight[1]
w<-0.6313 #가중치
b<-seq(-3.2,3.2,by=0.0001) #편향
#b<-seq(-1,3.2,by=0.1) #편향
pred<-x*w + b #예측값
err<-(y-pred)^2 #제곱오차
plot(err)
#기울기가 증가하면 오차가 증가하고 기울기가 감소하면 오차가 감소한다
#기울기가 0에 가까운 값이 최적의 기울기가 된다.
min(err) #최소오차
i<-which.min(err) # 최소인 인덱스값/ 파이썬에서는 np.argmin 
paste('최적의 편향=',b[i])

#위의 계산을 통해 얻은 최적의 w,b를 적용한 회귀식
x<-height[1]
y<-weight[1]
w<- 0.6313
b<- -0.00269999999999992
pred<-x*w + b
y
pred      # 최적의 w,b를 넣은거라서 y값=pred값



## 단순회귀분석2
regression<-read.csv("c:/vscode/data/regression.csv", 
                     fileEncoding='utf-8')

summary(regression)
hist(regression$height)
hist(regression$weight)

cor(regression$height, regression$weight)

# lm(y ~ x) : x 독립변수, y 종속변수(x가 한단위 증가할 때 y에게 미치는 영향)
r <- lm(regression$weight ~ regression$height)
plot(regression$weight ~ regression$height, 
     main="평균키와 몸무게", xlab="Height", ylab="Weight")
abline(r,col='red')

#키가 180인 사람의 체중 예측
r$coefficients[[2]]*180 + r$coefficients[[1]]
r
summary(r)



## 다중회귀분석
# attitude data  다중회귀분석 모델 생성
model <- lm(rating ~., data = attitude)
summary(model)  # complaints와 learning 항목만이 유의

#기여도가 낮은 항목을 제거함으로써 의미있는 회귀식을 구성하는 과정
reduced <- step(model, direction="backward")
summary(reduced)



## 다중공선성
# 독립변수끼리 강한 상관관계를가지는 현상
# VIF
library(car)
df <- MplsDemo
plot(df[,-1])
cor(df[,2:7])   #독립변수들의 상관계수

install.packages('corrplot')
library(corrplot) 
corrplot(cor(df[,2:7]), method="number")

model1<-lm(collegeGrad~.-neighborhood,data=df)
summary(model1)
#설명력 81.86%
#But, black(흑인비율), foreignBorn(외국태생) 변수의 회귀계수가 양수로 출력됨
#실제 현상을 잘 설명하지 못하는 모형

#white 변수를 제거한 모형
model2<-lm(collegeGrad~.-neighborhood-white,data=df)
summary(model2)
#설명력은 다소 떨어졌지만 회귀계수가 실제 현상을 잘 설명하는 것으로 보임
#black(흑인비율)이 음수로 바뀌었음, foreignBorn(외국태생) 변수는 양수이지만 유의하지 않음


# 변수별 VIF 구하기
model<-lm(population~.-collegeGrad-neighborhood,data=df)
print(paste("population의 VIF : ",(1-summary(model)$r.squared)^{-1}))

model<-lm(white~.-collegeGrad-neighborhood,data=df)
print(paste("white의 VIF : ",(1-summary(model)$r.squared)^{-1}))
  #다중공선성이 매우 높은 변수

model<-lm(black~.-collegeGrad-neighborhood,data=df)
print(paste("black의 VIF : ",(1-summary(model)$r.squared)^{-1}))

model<-lm(foreignBorn~.-collegeGrad-neighborhood,data=df)
print(paste("foreinBorn의 VIF : ",(1-summary(model)$r.squared)^{-1}))

model<-lm(hhIncome~.-collegeGrad-neighborhood,data=df)
print(paste("hhIncome의 VIF : ",(1-summary(model)$r.squared)^{-1}))

model<-lm(poverty~.-collegeGrad-neighborhood,data=df)
print(paste("poverty의 VIF : ",(1-summary(model)$r.squared)^{-1}))

#다중공선성을 계산해주는 함수
vif(model1)
  #다중공선성이 높은 white 변수 제거
model2<-lm(collegeGrad~.-neighborhood-white,data=df)
summary(model2)
vif(model2)



## 보스턴주택가격
library(MASS)
head(Boston)
dim(Boston)
summary(Boston)

pairs(Boston) #산점도 행렬
plot(medv~crim, data=Boston, main="범죄율과 주택가격과의 관계", xlab="범죄율", ylab="주택가격")

#범죄율과의 상관계수 행렬
(corrmatrix <- cor(Boston)[1,])
#강한 양의 상관관계, 강한 음의 상관관계
corrmatrix[corrmatrix > 0.5 | corrmatrix < -0.5]
#세율과의 상관계수 행렬
(corrmatrix <- cor(Boston)[10,]) 

#최고가로 팔린 주택들
(seltown <- Boston[Boston$medv == max(Boston$medv),])

#다중회귀분석 모델 생성
(model<-lm(medv ~ . , data=Boston))
summary(model)
reduced<-step(model, direction="backward")
summary(reduced)



## 회귀분석 모형 저장, 불러오기
df<-read.csv("c:/vscode/data/rides/rides.csv")
#모델 만들기
model<-lm(overall~num.child + distance + rides + games +
            wait + clean, data=df)
summary(model)
#저장
save(model, file="c:/vscode/data/r_rides_regress.model")
rm(list=ls()) #현재 작업중인 모든 변수들을 제거
#불러오기
load("c:/vscode/data/r_rides_regress.model")
ls()
summary(model)
