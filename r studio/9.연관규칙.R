### 맥주,빵,콜라,기저귀,달걀,우유
x <- data.frame(beer=c(0,1,1,1,0),bread=c(1,1,0,1,1),cola=c(0,0,1,0,1),
             diapers=c(0,1,1,1,1),eggs=c(0,1,0,0,0),milk=c(1,0,1,1,1))
x
class(x)

#install.packages('arules')
library(arules)
#arules에서 사용하는 transaction 타입으로 변환
trans<-as.matrix(x,"Transaction")
trans
class(trans)

rules1 <- apriori(trans,parameter=list(supp=0.2,conf=0.6,target='rules'))
rules1
summary(rules1)
#상위 3개의 룰
as(head(sort(rules1,by=c('confidence','support')),n=3),'data.frame')
#발견된 연관규칙을 확인
inspect(sort(rules1))

#install.packages('arulesViz')
library(arulesViz)
#물품들 간의 연관성 그래프
plot(rules1, method = "graph")

# 기저귀를 살 때 같이 사는 물품들
rules2 <- subset(rules1, rhs %in% 'diapers')
rules2
inspect(rules2)
plot(rules2, method="graph")

# 왼쪽 item이 beer or cola인 규칙만 서브셋으로 작성
rules3 <- subset(rules1, lhs %in% c('beer', 'cola'))
rules3
inspect(sort(rules3, decreasing=T, by="count"))
plot(rules3, method="graph")



### dvd
dvd <- read.csv("c:/vscode/data/basket/dvdtrans.csv")

# DVD별로 하나의 레코드로 기록된 구조를 
# 거래 ID별로 하나의 레코드가 되도록 변환
dvd.list <- split(dvd$Item, dvd$ID)
dvd.list
library(arules)
#arules에서 사용하는 transaction 타입으로 변환
dvd.trans <- as(dvd.list, "transactions")
summary(dvd.trans)
image(dvd.trans)

dvd.rules <- apriori(dvd.trans)
#상위 3개의 룰
as(head(sort(dvd.rules,by=c('confidence','support')),n=3),'data.frame')
# count 기준 내림차순 정렬 상위 6개 출력
inspect(head(sort(dvd.rules, decreasing=T, by="count")))
summary(dvd.rules)
inspect(dvd.rules)

#최소 지지도를 0.2로 설정한 모형(2번 이상 거래에 나타나는 연관규칙)
#신뢰도는 0.6으로 설정
dvd.rules <- apriori(dvd.trans, parameter = list(support = 0.2,
                                                 confidence = 0.6))
summary(dvd.rules)
inspect(dvd.rules)

#연관규칙의 시각화
#install.packages("arulesViz")
library(arulesViz)
plot(dvd.rules, method = "graph")

# 특정 상품[item] 서브셋 작성과 시각화
rule1 <- subset(dvd.rules, rhs %in% 'Gladiator')
rule1
inspect(rule1)
plot(rule1, method="graph")

# 왼쪽 item이 Gladiator or Patriot인 규칙만 서브셋으로 작성
rule2 <- subset(dvd.rules, lhs %in% c('Gladiator', 'Patriot'))
rule2
inspect(rule2)
plot(rule2, method="graph")
