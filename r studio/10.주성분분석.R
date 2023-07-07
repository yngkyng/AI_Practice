### 요인 분석
data1<-c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
data2<-c(1,2,1,1,1,1,1,1,1,1,3,4,3,3,3,4,5,6)
data3<-c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
data4<-c(3,4,3,3,3,1,2,1,1,1,1,1,1,1,1,5,4,5)
data5<-c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
data6<-c(1,2,1,1,1,3,3,3,2,3,1,1,1,1,1,5,4,5)
#가로방향(열방향)으로 벡터 결합
testData<-cbind(data1,data2,data3,data4,data5,data6)
head(testData)

#상관변수 행렬 구하기
data<-cor(testData)
data
eigen(data)

#3개의 값이 유의하다고 보여지므로 요인분석 실시
#데이터를 3개로 요약
factanal(testData, factors=3)



### 주성분 분석
#iris 데이터
cor(iris[1:4])
log.ir<-log(iris[,1:4])
ir.species<-iris[,5]

#주성분 분석 함수
ir.pca<-prcomp(log.ir, center=T, scale=T)
ir.pca

########



### 라면 데이터
data<-read.table("c:/vscode/data/noodle/noodle.txt",header=T)
data
summary(data)
cor(data)

#주성분 분석
p1<-prcomp(data,scale=T) # scale=T 데이터 표준화 처리 포함
p1
plot(p1, type="l")
summary(p1)
predict(p1)
biplot(p1)
