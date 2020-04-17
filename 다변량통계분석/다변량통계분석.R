
######################################################
######################################################
crime <- read.csv("crime.csv")
str(crime)
crime$인구 <-as.numeric(crime$인구)
crime$인구밀도 <- as.numeric(crime$인구밀도)
crime$기초생활수급자인원 <-as.numeric(crime$기초생활수급자인원)
crime$기타주점업<-as.numeric(crime$기타주점업)


########## 탐데분 ############
## 3년간 범죄 발생 추이
#비율
scalecrime<-crime[,c(1,3:7)]
colnames(scalecrime)[1]<-c("year")

library(reshape)
crime.melt <-melt(scalecrime, id.vars = c("year"),
                  measure.vars= c("절도","살인","강도","성폭력","폭행"))
newcrime<-aggregate(crime.melt[3],
                    by=list(year=crime.melt$year,variable=crime.melt$variable),sum)

library(ggplot2)
ggplot(newcrime, aes(x=year, y=value,group=variable, fill=variable))+
  geom_area(position="fill")+
  scale_fill_brewer(palette = "Blues", name="범죄 유형" )+
  ggtitle("5대범죄 발생 추이 (2014 ~ 2016)")+
  ylab("연도별 범죄발생비율")+
  xlab("연도")

# 건수
a <- tapply(crime$절도,crime$기간,sum)
b <- tapply(crime$살인,crime$기간,sum)
c <- tapply(crime$강도,crime$기간,sum)
d <- tapply(crime$성폭력,crime$기간,sum)
e <- tapply(crime$폭행,crime$기간,sum)

par(mfcol=c(3,1))
barplot(a,horiz=T,main ="절도",col="light pink",xlim=c(0,65000))
barplot(b,horiz=T,main="살인",col="orange",xlim=c(0,200))
barplot(c,horiz=T,main="강도",col="light green",xlim=c(0,400))
par(mfcol=c(2,1))
barplot(d,horiz=T,main="성폭력",col="violet",xlim=c(0,10000))
barplot(e,horiz=T,main="폭행",col="skyblue",xlim=c(0,45000))


## 구별 범죄 비율
a<-crime[51:75,8:16]
a1<-matrix(c(a$경찰서,a$지구대파출소치안센터,a$기타주점업,a$슈퍼마켓,a$노래연습장.운영업,
             a$체인화.편의점,a$치킨.전문점,a$당구장.운영업,a$컴퓨터게임방.운영업),ncol=9)
rownames(a1)<-levels(crime$자치구)
colnames(a1)<-colnames(crime)[8:16]
colnames(a1)[2]<-c("지구대")
colnames(a1)[3]<-c("주점업")
colnames(a1)[9]<-c("PC방")

mosaicplot(a1,main= "범죄",
           color=brewer.pal(8,"Set3"))


b<-matrix(c(place$절도,place$살인,place$강도,place$성폭력,place$폭행),ncol=5)
colnames(b)<-c("절도","살인","강도","성폭력","폭행")
rownames(b)<-levels(crime$자치구)

mosaicplot(t(b),main= "범죄",
           color=brewer.pal(3,"Set3"))

mosaicplot(b,main= "서울시 구별 5대범죄 (2014~2016)",
           color=brewer.pal(5,"Set3"))


## 구별 3년간 5대 범죄 합
#절도, 살인, 강도, 성폭력, 폭행  
library(dplyr)
plot.1 <- crime %>% group_by(자치구) %>% summarise(절도=sum(절도)) %>%
  ggplot(aes(x=자치구, y=절도)) +
  geom_bar(stat="identity", fill="lightpink") +
  ggtitle("구별 3년간 절도 합") +
  theme(plot.title=element_text(face="bold", size=30, vjust=2, hjust=0.5, color="black"))
plot.1

plot.2 <- crime %>% group_by(자치구) %>% summarise(살인=sum(살인)) %>%
  ggplot(aes(x=자치구, y=살인)) +
  geom_bar(stat="identity", fill="orange") +
  ggtitle("구별 3년간 살인 합") +
  theme(plot.title=element_text(face="bold", size=30, vjust=2, hjust=0.5, color="black"))
plot.2

plot.3 <- crime %>% group_by(자치구) %>% summarise(강도=sum(강도)) %>%
  ggplot(aes(x=자치구, y=강도)) +
  geom_bar(stat="identity", fill="lightgreen") +
  ggtitle("구별 3년간 강도 합") +
  theme(plot.title=element_text(face="bold", size=30, vjust=2, hjust=0.5, color="black"))
plot.3

plot.4 <- crime %>% group_by(자치구) %>% summarise(성폭력=sum(성폭력)) %>%
  ggplot(aes(x=자치구, y=성폭력)) +
  geom_bar(stat="identity", fill="violet") +
  ggtitle("구별 3년간 성폭력 합") +
  theme(plot.title=element_text(face="bold", size=30, vjust=2, hjust=0.5, color="black"))
plot.4

plot.5 <- crime %>% group_by(자치구) %>% summarise(폭행=sum(폭행)) %>%
  ggplot(aes(x=자치구, y=폭행)) +
  geom_bar(stat="identity", fill="skyblue") +
  ggtitle("구별 3년간 폭행 합") +
  theme(plot.title=element_text(face="bold", size=30, vjust=2, hjust=0.5, color="black"))
plot.5


## 주성분분석(PCA) ##


## 변수별 상관계수 ##
library(psych)
pairs.panels(crime[,-c(4,5,6,7)])

library(corrgram)
corrgram(cor(crime[,-c(2,4,5,6,7)]),type="corr",upper.panel=panel.conf)


############### 다변량분석 ###############
## 선형회귀 ##
library(car)

crime1 <- crime
str(crime1)
crime1 <- crime[,-c(1,2,14)]
test <- crime1[-c(1:50),]

#절도
lm.theft <- lm(절도~.-살인-강도-성폭력-폭행, data=crime1)
summary(lm.theft)
vif(lm.theft)

for.theft=step(lm(절도~1, data=crime1), 
          scope=formula(lm(절도~.-살인-강도-성폭력-폭행+
                               (경찰서+지구대파출소치안센터+기타주점업+슈퍼마켓+CCTV+
                                     노래연습장.운영업+면적+인구밀도+기초생활수급자인원+
                                     기초생활수급자비율++X1인세대비율)^2, data=crime1)), 
          direction="forward")
lm.theft.total <- lm(formula(for.theft), data=crime1)
summary(lm.theft.total) #0.9705

lm.theft2 <- lm(formula(for.theft), data=crime1[-c(51:75),])

pred.theft <- predict(lm.theft2, test)
R2(test$절도, pred.theft) # 0.887485


#강도
lm.robber <- lm(강도~.-살인-절도-성폭력-폭행, data=crime1)
summary(lm.robber)
for.robber=step(lm(강도~1, data=crime1), 
                scope=formula(lm(강도~.-살인-폭행-성폭력-절도+
                                     (경찰서+지구대파출소치안센터+기타주점업+슈퍼마켓+CCTV+노래연습장.운영업+
                                           면적+인구밀도+기초생활수급자인원+기초생활수급자비율+
                                           +X1인세대비율)^2
                                   , data=crime1)), direction="forward")
lm.robber.total <- lm(formula(for.robber), data=crime1)
summary(lm.robber.total) #0.6519 
lm.robber2 <- lm(formula(for.robber), data=crime1[-c(51:75),])

pred.robber <- predict(lm.robber2, test)
R2(test$강도, pred.robber) #0.779845


#성폭력
lm.sex <- lm(성폭력~.-살인-절도-폭행-강도, data=crime1)
summary(lm.sex)
for.sex=step(lm(성폭력~1, data=crime1), 
                scope=formula(lm(성폭력~.-살인-강도-폭행-절도+
                                     (경찰서+지구대파출소치안센터+기타주점업+슈퍼마켓+CCTV+노래연습장.운영업+
                                           면적+인구밀도+기초생활수급자인원+기초생활수급자비율+
                                           +X1인세대비율)^2
                                   , data=crime1)), direction="forward")
lm.sex.total <- lm(formula(for.sex), data=crime1)
summary(lm.sex.total) # 0.6109 
lm.sex2 <- lm(formula(for.sex), data=crime1[-c(51:75),])

pred.sex <- predict(lm.sex2, test)
R2(test$성폭력, pred.sex) #0.7605988


#폭행
lm.attack <- lm(폭행~.-살인-절도-강도-성폭력, data=crime1)
summary(lm.attack)
vif(lm.attack)

for.attack=step(lm(폭행~1, data=crime1), 
               scope=formula(lm(폭행~.-살인-강도-성폭력-절도+
                                    (경찰서+지구대파출소치안센터+기타주점업+슈퍼마켓+CCTV+노래연습장.운영업+
                                          면적+인구밀도+기초생활수급자인원+기초생활수급자비율+
                                          +X1인세대비율)^2
                                  , data=crime1)), direction="forward")
lm.attack.total <- lm(formula(for.attack), data=crime1)
summary(lm.attack.total) #0.8801 
lm.attack2 <- lm(formula(for.attack), data=crime1[-c(51:75),])
summary(lm.attack2)

pred.attack <- predict(lm.attack2, test)
R2(test$폭행, pred.attack) #0.7978782


#살인
lm.murder <- lm(살인~.-폭행-절도-강도-성폭력, data=crime1)
summary(lm.murder)
vif(lm.murder)

for.murder=step(lm(살인~1, data=crime1), 
                scope=formula(lm(살인~.-폭행-강도-성폭력-절도+
                                     (경찰서+지구대파출소치안센터+기타주점업+슈퍼마켓+CCTV+노래연습장.운영업+
                                           면적+인구밀도+기초생활수급자인원+기초생활수급자비율+
                                           +X1인세대비율)^2
                                   , data=crime1)), direction="forward")
lm.murder.total <- lm(formula(for.murder), data=crime1)
summary(lm.murder.total) #0.6136 
lm.murder2 <- lm(formula(for.murder), data=crime1[-c(51:75),])
summary(lm.murder2)

pred.murder <- predict(lm.murder2, test)
R2(test$살인, pred.murder) #0.5505212


## mclust ##
head(crime)
crime2 <-crime[1:25,3:7]
crime3<-crime[26:50,3:7]
crime4<-crime[51:75,3:7]

cr2014<-matrix(c(crime2$절도,crime2$살인,crime2$강도,crime2$성폭력,crime2$폭행),ncol=5)
cr2015<-matrix(c(crime3$절도,crime3$살인,crime3$강도,crime3$성폭력,crime3$폭행),ncol=5)
cr2016<-matrix(c(crime4$절도,crime4$살인,crime4$강도,crime4$성폭력,crime4$폭행),ncol=5)

rownames(cr2014)<-levels(crime$자치구)
colnames(cr2014)<-c("절도","살인","강도","성폭력","폭행")

rownames(cr2015)<-levels(crime$자치구)
colnames(cr2015)<-c("절도","살인","강도","성폭력","폭행")

rownames(cr2016)<-levels(crime$자치구)
colnames(cr2016)<-c("절도","살인","강도","성폭력","폭행")

cr_all<-cr2014+cr2015+cr2016

library(mclust)
cr_all=Mclust(cr_all)
summary(cr_all)
cr_all$classification
plot(cr_all)





######################### 계층적 군집분석
library(dplyr)
a <- crime %>% group_by(자치구) %>% 
  summarise(절도=sum(절도),살인=sum(살인),강도=sum(강도),성폭력=sum(성폭력),폭행=sum(폭행))
a <- as.data.frame(a)
rownames(a) <- a[,1]
a <- a[,-1]

hc1 <- hclust(d,method="single")
hc2 <- hclust(d,method="complete")
hc3 <- hclust(d,method="average")
hc4 <- hclust(d,method="ward.D2")

plot(hc1)
plot(hc2)
plot(hc3)
plot(hc4)

cluster <- cutree(hc2,5)
plot(a$절도,a$강도,col=cluster,pch=cluster,main="절도vs강도 (hclust)",xlab="절도",ylab="강도")
text(a$절도,a$강도,labels=rownames(a),col=cluster,pch=cluster,pos=4)

plot(a$절도,a$성폭력,col=cluster,pch=cluster,main="절도vs성폭력 (hclust)",xlab="절도",ylab="성폭력")
text(a$절도,a$성폭력,labels=rownames(a),col=cluster,pch=cluster,pos=4)

plot(a$강도,a$폭행,col=cluster,pch=cluster,main="강도vs폭행 (hclust)",xlab="강도",ylab="폭행")
text(a$강도,a$폭행,labels=rownames(a),col=cluster,pch=cluster,pos=4)

##################### 비계층적 군집분석
km <- kmeans(a,centers=5,nstart=10)
table(km$cluster)

wss = c()
for(k in 1:20){
  km_k <- kmeans(a,center=k,nstart=10)
  wss[k] <- km_k$tot.withinss
}
plot(1:20, wss, type="b")

plot(a$절도,a$강도,pch=km$cluster, col=km$cluster,main="절도vs강도 (kmeans)",xlab="절도",ylab="강도")
text(a$절도,a$강도,labels=rownames(a),pch=km$cluster, col=km$cluster,pos=4)

plot(a$절도,a$성폭력,pch=km$cluster, col=km$cluster,main="절도vs성폭력 (kmeans)",xlab="절도",ylab="성폭력")
text(a$절도,a$성폭력,labels=rownames(a),pch=km$cluster, col=km$cluster,pos=4)

plot(a$강도,a$폭행,pch=km$cluster, col=km$cluster,main="강도vs폭행 (kmeans)",xlab="강도",ylab="폭행")
text(a$강도,a$폭행,labels=rownames(a),pch=km$cluster, col=km$cluster,pos=4)





