# 데이터마이닝 2018 기말 프로젝트 제출 파일
#
# - 아래 파일의 내용을 고쳐 최종 예측을 하는 코드를 작성할 것
# - 이 파일의 파일명은 "이름.R" 형태로 제출
# - 예측 파일은 제출하지 않음
# - 작업 디렉토리 설정 후 전체 코드를 실행했을 때 에러 없이 예측 파일이 생성되어야 함
# - 다른 사람의 코드 또는 아이디어를를 베끼면 양측 모두 0점 처리


# 이름 (자기 이름으로 고쳐 넣을 것)
name = '김민채'

# 데이터 불러오기 (수정하지 말것)
df <- read.csv('apt.csv')  # 학습용 데이터
test <- read.csv('mid_X.csv')  # 테스트용 데이터(실제 테스트는 중간테스트 데이터와 파일명은 같고 내용은 다름)

############# 데이터 전처리#############
df[which(df$building_coverage_ratio >= 600),]$building_coverage_ratio <- NA
df[which(df$floor_area_ratio >= 100000),]$floor_area_ratio <- NA #20042~20050, 26941~26943
df[which(df$floor_max >= 1000),]$floor_max <- NA #25539~25543, 25609~25617

# which(colnames(df)=="floor_min") #32
df <- df[,-c(1, 32)]
str(df)

##위도, 경도
library(NISTunits)
apt.commute <- data.frame(lat=NA, lng=NA)
apt.commute[1,] <- c(37.5779636,126.9004205) #dmc
apt.commute[2,] <- c(37.48089,126.88257350000003) #가산
apt.commute[3,] <- c(37.49794199999999,127.02762099999995) #강남
apt.commute[4,] <- c(37.5031784,126.88203670000007) #구로
apt.commute[5,] <- c(37.56025,126.82466399999998) #마곡
apt.commute[6,] <- c(37.5609892,126.98618680000004) #명동
apt.commute[7,] <- c(37.384931,127.12324899999999) #서현
apt.commute[8,] <- c(37.544569,127.05597399999999) #성수
apt.commute[9,] <- c(37.429714,126.65448500000002) #송도
apt.commute[10,] <- c(37.2664398,126.9994077) #수원
apt.commute[11,] <- c(37.327006,126.78874799999994) #안산
apt.commute[12,] <- c(37.484102,127.03436899999997) #양재
apt.commute[13,] <- c(37.5215695,126.92431149999993) #여의도
apt.commute[14,] <- c(37.50081000000001,127.03693599999997) #역삼
apt.commute[15,] <- c(37.5611284,127.03550500000006) #왕십리
apt.commute[16,] <- c(37.52989,126.96477500000003) #용산
apt.commute[17,] <- c(37.566056,126.982662) #을지로
apt.commute[18,] <- c(37.681997,126.77004) #일산
apt.commute[19,] <- c(37.5132612,127.10013359999994) #잠실
apt.commute[20,] <- c(37.3638388,127.11632069999996) #정자
apt.commute[21,] <- c(37.5609739,126.99352870000007) #충무로
apt.commute[22,]  <- c(37.394798,127.11113499999999) #판교
apt.commute[23,] <- c(37.394346,126.96388000000002) #평촌
apt.commute[24,] <- c(37.557527,126.92446689999997) #홍대

for (i in 1:24){
  df[,i+3] <- 6317*(acos(sin(NISTdegTOradian(df$lat)) 
                         * sin(NISTdegTOradian(apt.commute[i,1]))
                         + cos(NISTdegTOradian(df$lat)) 
                         * cos(NISTdegTOradian(apt.commute[i,1])) 
                         * cos(abs(NISTdegTOradian(df$lng)-NISTdegTOradian(apt.commute[i,2])))))
}
head(df)


########결측값 대치#########
library(mice)
summary(df)

str(df)

imp.m = mice(df[,-47], seed=123,m=5,method = "cart")
df.m1 <-complete(imp.m, 1)
df.m2 <- complete(imp.m, 2)
df.m3 <- complete(imp.m, 3)
df.m4 <- complete(imp.m, 4)
df.m5 <- complete(imp.m, 5)



########훈련 테스트 나누기#########
##train, test
library(caret)

set.seed(1)
idx <- createDataPartition(y = df.m1$price, p=0.7, list=FALSE)
train.1 <- df.m1[idx,]
test.1 <- df.m1[-idx,]
train.2 <- df.m2[idx,]
test.2 <- df.m2[-idx,]
train.3 <- df.m3[idx,]
test.3 <- df.m3[-idx,]
train.4 <- df.m4[idx,]
test.4 <- df.m4[-idx,]
train.5 <- df.m5[idx,]
test.5 <- df.m5[-idx,]


##변수선택
fit=step(lm(price~1, data=df.m1), 
         scope=formula(lm(price~., data=df.m1)), direction="forward") #581046.7
summary(fit) #0.7473
formula(fit)


####################################
##########중간 테스트##########
##전처리
# which(colnames(test)=="floor_min") #31
test <- test[,-31]

summary(test)

# test[which(test$building_coverage_ratio >= 600),]$building_coverage_ratio <- NA
test[which(test$floor_area_ratio >= 100000),]$floor_area_ratio <- NA 
# test[which(test$floor_max >= 1000),]$floor_max <- NA 

for (i in 1:24){
  test[,i+3] <- 6317*(acos(sin(NISTdegTOradian(test$lat)) 
                           * sin(NISTdegTOradian(apt.commute[i,1]))
                           + cos(NISTdegTOradian(test$lat)) 
                           * cos(NISTdegTOradian(apt.commute[i,1])) 
                           * cos(abs(NISTdegTOradian(test$lng)-NISTdegTOradian(apt.commute[i,2])))))
}
summary(test)


##결측값
library(mice)
imp.mid = mice(test[,-47], seed=123,m=5,method = "cart")
mid.1 <- complete(imp.mid, 1)
mid.2 <- complete(imp.mid, 2)
mid.3 <- complete(imp.mid, 3)
mid.4 <- complete(imp.mid, 4)
mid.5 <- complete(imp.mid, 5)



########중간 테스트 훈련###########
##예측
library(caret)
control = trainControl(method='cv', search='grid', number=5)
xgb.grid.pre = expand.grid(
  .nrounds = 200,
  .eta = c(0.1,0.3),
  .gamma = 1,
  .max_depth = c(3,5,7),
  .min_child_weight = 1,
  .colsample_bytree = 1,
  .subsample = 1
)

xgb.model.final1 <- train(
  formula(fit),
  data = df.m1,
  tuneGrid = xgb.grid.pre,
  trControl = control,
  method = 'xgbTree'
)
test.pred.f.xgb1 <- predict(xgb.model.final1, mid.1)


xgb.model.final2 <- train(
  formula(fit),
  data = df.m2,
  tuneGrid = xgb.grid.pre,
  trControl = control,
  method = 'xgbTree'
)
test.pred.f.xgb2 <- predict(xgb.model.final2, mid.2)


xgb.model.final3 <- train(
  formula(fit),
  data = df.m3,
  tuneGrid = xgb.grid.pre,
  trControl = control,
  method = 'xgbTree'
)
test.pred.f.xgb3 <- predict(xgb.model.final3, mid.3)

xgb.model.final4 <- train(
  formula(fit),
  data = df.m4,
  tuneGrid = xgb.grid.pre,
  trControl = control,
  method = 'xgbTree'
)
test.pred.f.xgb4 <- predict(xgb.model.final4, mid.4)

xgb.model.final5 <- train(
  formula(fit),
  data = df.m5,
  tuneGrid = xgb.grid.pre,
  trControl = control,
  method = 'xgbTree'
)
test.pred.f.xgb5 <- predict(xgb.model.final5, mid.5)


xgb.f.df1 <- as.data.frame(test.pred.f.xgb1)
xgb.f.df2 <- as.data.frame(test.pred.f.xgb2)
xgb.f.df3 <- as.data.frame(test.pred.f.xgb3)
xgb.f.df4 <- as.data.frame(test.pred.f.xgb4)
xgb.f.df5 <- as.data.frame(test.pred.f.xgb5)

test.pred.f.xgb <- cbind(xgb.f.df1, xgb.f.df2, xgb.f.df3, xgb.f.df4, xgb.f.df5)
test.pred.f.xgb$mean <- apply(test.pred.f.xgb, 1, mean)
head(test.pred.f.xgb)


#####최종######
price <- as.data.frame(test.pred.f.xgb$mean, ncol=1)
colnames(price) <- "price"

write.csv(data.frame(price=price), paste0(name, '.csv'))



