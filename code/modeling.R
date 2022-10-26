install.packages('data.table')
library(data.table)

setwd('C:/Users/User/Documents/카카오톡 받은 파일')

#Turkey Fence로 이상치 제거한 최종데이터 가져오기 및 데이터 확인
data=fread('최종final이상치제거(Turkey Fences).csv')

data=data[1:14612,]
nrow(data[which(data$month!="2019-11-01"),])
str(data)
head(data,2)
tail(data,2)

colnames(data)


df=data[,-c(1,2,3,4,5,7,8,9,11,12,33,39,40)] #문자형 변수 제거
str(df)

#문자형으로 불러와진 factor형 변수 factor함수써서 factor형으로 바꿔주기
df$area=as.factor(df$area)
df$in_hanriverpark=as.factor(df$in_hanriverpark)
df$n_hanriverpark=as.factor(df$n_hanriverpark)
df$season=as.factor(df$season)

str(df)

# area는 서울시 자치구(25개)로 범주가 너무 많으므로 권역별로 재범주화(5개)
# 도심권 : 종로구, 중구, 용산구
# 서북권 : 은평구, 서대문구, 마포구
# 동북권 : 노원구, 도봉구, 강북구, 성북구, 중랑구, 동대문구, 성동구, 광진구
# 서남권 : 강서구, 양천구, 영등포구, 구로구, 금천구, 동작구, 관악구
# 동남권 : 서초구, 강남구, 송파구, 강동구
table(df$area)
area5 = as.character(df$area)
table(area5)
area5<-replace(area5,c(area5=="종로구"|area5=="중구"|area5=="용산구"),"도심권")
area5<-replace(area5,c(area5=="은평구"|area5=="서대문구"|area5=="마포구"),"서북권")
area5<-replace(area5,c(area5=="노원구"|area5=="도봉구"|area5=="강북구"|area5=="성북구"|area5=="중랑구"|area5=="동대문구"|area5=="성동구"|area5=="광진구"),"동북권")
area5<-replace(area5,c(area5=="강서구"|area5=="양천구"|area5=="영등포구"|area5=="구로구"|area5=="금천구"|area5=="동작구"|area5=="관악구"),"서남권")
area5<-replace(area5,c(area5=="서초구"|area5=="강남구"|area5=="송파구"|area5=="강동구"),"동남권")

table(area5) #확인

df=cbind(area5,df)		
str(df)

df=df[,-c(2)] #area 제거
str(df)
df$area5=as.factor(df$area5)
str(df)

# #####df2생성 : school_num을 school_YN(학교유무), 
# subway_num+ bus_num 합친 traffic변수,
# park_num을 park_YN(공원유무)변수로 만들자.####


#subway+bus 변수 생성 : transfer
str(df)
traffic=df$subway_num+df$busStop_num
length(traffic)

#공원 유무 변수 생성 : park_YN
park_YN=rep(0,nrow(df))
df=cbind(df,park_YN)
str(df)

df[which(df$park_num2>0),'park_YN']=1
df[which(df$park_num2<=0),'park_YN']=0
str(df)

df$park_YN=as.factor(df$park_YN)
str(df)
unique(df$park_YN)

#학교 유무 변수 생성 : school_YN
school_YN=rep(0,nrow(df))
df=cbind(df,school_YN)
str(df)

df[which(df$school_num>0),'school_YN']=1
df[which(df$school_num<=0),'school_YN']=0
str(df)

df$school_YN=as.factor(df$school_YN)
str(df)
unique(df$school_YN)

df2=df[,-c('school_num','subway_num','busStop_num','park_num2')]
str(df2)
df2=cbind(df2,traffic)
str(df2)
colnames(df2)
colnames(data)

#df2_1: user_num=0인 행 제거
df2_1=df2[-which(df2$user_num==0),]
str(df2_1)

#### 1. full model : 이상치 제거만 한 데이터(문자형 지우고 바로 돌린 회귀모델) ####
lmfull=lm(user_num ~ . , data=df2_1)
summary(lmfull)

#full model VIF확인
library(olsrr)

multicor_lmfull=ols_vif_tol(lmfull)
str(multicor_lmfull)
#다중공선성 vif>10 인 변수 찾기
multicor_lmfull[which(multicor_lmfull$VIF>=10),]
#한강유무변수, 한강종류변수 다중공선성 inf값 가짐.
#아마도 한강유무변수랑 한강종류변수가 강한 선형관계를 가지는 것 때문인듯
#oneDay_mem, regular, oneDay_cust 도 다중공선성 가지므로 제거하로 결정.
#AGE_002, AGE_008은 10이랑 가깝고 다른 변수와의 상관관계 가능성이 높은 변수라 일단 놔두자.


#다시 correlation 확인
#1. area
#str(df[i,'area'])
sort_area=sort(unique(df$area))
area_nu=c()
for(i in 1:nrow(df)){
  for(j in 1:length(sort_area)){
    if(as.character(df$area[i])==as.character(sort_area[j])){
      area_nu[i]=j
    }
  }
}
head(area_nu)
tail(area_nu)
head(df$area)

#2. n_hanriverpark
sort_nHanriver=sort(unique(df$n_hanriverpark))
sort_nHanriver
n_hanriverpark_nu=c()
for(i in 1:nrow(df)){
  for(j in 1:length(sort_nHanriver)){
    if(as.character(df$n_hanriverpark[i])==as.character(sort_nHanriver[j])){
      n_hanriverpark_nu[i]=j
    }
  }
}
head(n_hanriverpark_nu)
tail(n_hanriverpark_nu)
head(df$n_hanriverpark)

#3. season
sort_season=c('spring','summer','fall','winter')
sort_season
season_nu=c()
for(i in 1:nrow(df)){
  for(j in 1:length(sort_season)){
    if(as.character(df$season[i])==as.character(sort_season[j])){
      season_nu[i]=j
    }
  }
}
head(season_nu)
tail(season_nu)
head(df$season)

colnames(df2)
cordf=data.frame(df2[,c(23,2:19,26)],
                 area_nu,in_hanriverpark_nu=as.integer(df2$in_hanriverpark),n_hanriverpark_nu,
                 season_nu, park_YN_nu=as.integer(df2$park_YN), school_YN_n=as.integer(df2$school_YN))

str(cordf)



#corr matrix
#install.packages('corrplot')
library(corrplot)

colnames(df)
cor_df=cor(cordf)
cor_df

windows()
corrplot(cor_df,main="최종 데이터 변수들 간 상관관계 확인")

#한강종류변수와 한강유무변수 correlation 확인
cor(cordf$in_hanriverpark_nu,cordf$n_hanriverpark_nu) #약 0.9383

#다중공선성 높음 따라서 둘 중 하나 제거해야함.

###2. 한강종류변수 제거************
#한강종류변수 제거하는 것으로 결정- > 한강종류가 수준의 개수가 많아 해석이 힘들것같다.
str(df2_1)
df3=df2_1[,-c('n_hanriverpark','oneDay_mem','oneDay_cust','regular')]
str(df3)
lm3=lm(user_num ~ . , data= df3)
summary(lm3)
lm.inter.full=lm(user_num ~ .^2, data=df3)
summary(lm.inter.full)

#유의한 교호작용 변수들 : area5*AGE_003 +  number_of_holders*total_store+ number_of_holders*food_num+number_of_holders*work_num+
#number_of_holders*AGE_005 + group*work_num + 
# group*AGE_004  + group*AGE_005 + group*AGE_008 + group*traffic+ total_store*food_num+
#total_store*move_num + food_num:move_num + live_num*move_num 

#유의한 교호작용 변수들만 추가한 모델

lm.inter1=lm(user_num ~ . +  area5*AGE_003 + number_of_holders*total_store+ number_of_holders*food_num+number_of_holders*work_num+
               group*AGE_004  + group*AGE_005 + group*AGE_008 + group*traffic + total_store*food_num +
               total_store*move_num + food_num*move_num +live_num*move_num
               , data=df3) #area5*AGE_003, total_store*food_num, live_num*move_num 추가
summary(lm.inter1)

#다중공선성 확인
multicor_lm.inter1=ols_vif_tol(lm.inter1)
#multicor_lm.inter1
#다중공선성 vif>10 인 변수 찾기
vifover10=multicor_lm.inter1[which(multicor_lm.inter1$VIF>=10),]
vifover10$Variables
#total_store, food_num, AGE_003, number_of_holders:total_store, number_of_holders:food_num, group:AGE_004, group:AGE_005, total_store:move_num
#food_num:move_num가 다중공선성 높은 변수들

#다중공선성 높은 변수 제거
#total_store, food_num 중 더 다중공선성이 높은 total_store를 제거
df3_1=df3[,-c('total_store')]
str(df3_1)
lm.inter2=lm(user_num ~ . +   area5*AGE_003 + number_of_holders*food_num+number_of_holders*work_num+
               group*AGE_004  + group*AGE_005 + group*AGE_008 + group*traffic+
                food_num*move_num +live_num*move_num
             , data=df3_1)
summary(lm.inter2)

multicor_lm.inter2=ols_vif_tol(lm.inter2)
#multicor_lm.inter2
#다중공선성 vif>10 인 변수 찾기
multicor_lm.inter2[which(multicor_lm.inter2$VIF>=10),]
#group*AGE_004, group:AGE_005, number_of_holders:food_num 크게 나옴

#다중공선성 높은 변수 제거2
l=lm(user_num ~ . + area5*AGE_003 +number_of_holders*work_num+
  group*AGE_004  + group*AGE_005 + group*AGE_008 + group*traffic+
  food_num*move_num +live_num*move_num, data=df3_1)
summary(l)

multicor_l=ols_vif_tol(l)
multicor_l
multicor_l[which(multicor_l$VIF>=10),]

df3_2=df3_1[,-'AGE_003']
l2=lm(user_num ~ . +number_of_holders*work_num+
        group*AGE_008 + group*traffic+
       food_num*move_num +live_num*move_num, data=df3_2)
summary(l2)

multicor_l2=ols_vif_tol(l2)
multicor_l2
multicor_l2[which(multicor_l2$VIF>=10),]

formula(l2)


# lm.inter3=lm(user_num ~ . + number_of_holders*work_num+
#                group*AGE_008 + group*traffic + 
#                food_num*move_num 
#              , data=df3_1)
# summary(lm.inter3)
# 
# multicor_lm.inter3=ols_vif_tol(lm.inter3)
# multicor_lm.inter3
# #multicor_lm.inter2
# #다중공선성 vif>10 인 변수 찾기 = 결과 없음 = > vif >10인 변수들 없음
# multicor_lm.inter3[which(multicor_lm.inter3$VIF>=10),]
# #교호작용 추가한 모델 생성 : lm.inter3 = user_num ~ area5 + number_of_holders + group + food_num + live_num + 
# # work_num + move_num + AGE_001 + AGE_002 + AGE_003 + AGE_004 + 
# #   AGE_005 + AGE_006 + AGE_007 + AGE_008 + in_hanriverpark + 
# #   season + park_YN + school_YN + traffic + number_of_holders * 
# #   work_num + group * AGE_008 + group * traffic + food_num * 
# #   move_num
# 
# formula(lm.inter3)
# 
# str(lm.inter3)

#stepwise
library(MASS)
l2step=stepAIC(l2,direction='both')
summary(l2step)
formula(l2step) #work_num, AGE_007, in_hanriverpark 제거
str(l2step)

#최종모델 lmfinal
lmfinal=stepAIC(l2, direction='both')
summary(lmfinal)
formula(lmfinal)

### 최종 회귀 모델 테스트 ###
# lmfinal : 최종 회귀
windows()
par(mfrow=c(2,2))
plot(lmfinal) 
# fitted vs residuals : 모형의 선형성
# Normal Q-Q : 잔차의 정규성
# Scale-Location : 잔차의 등분산성 
# Cook's distance : 극단값

### 정규성 검정 ###
# 잔차가 정규분포 따르는지 확인 

# q-q plot 확인하기 (시각적 방법으로 확인하기)
windows()
par(mfrow=c(2,2))
plot(lmfinal) 

# n>5000이므로 Kolmogorov-Smirnov test, 콜모고로프-스미노프 검정

# 귀무가설 : 정규분포를 따른다, 대립가설 : not 귀무가설
ks.test(lmfinal$residuals, "pnorm", mean=mean(lmfinal$residuals), sd=sd(lmfinal$residuals))
# p-value < 2.2e-16 < 0.05 reject 귀무가설. 정규분포를 따르지 않는다.
windows()
hist(rstandard(lmfinal)) # 표준화 잔차의 분포를 그래프로 그려보기 

### 독립성 검정 ###

# durbinWatsonTest : 잔차끼리 자기상관성 있는지 확인
#install.packages("lmtest")
# 귀무가설 : 잔차끼리 자기상관성을 가지지않는다. (독립)
# 대립가설 : 잔차끼리 자기상관성을 가진다.
library(lmtest)
dwtest(lmfinal)
# p-value < 2.2e-16 < 0.05 reject 귀무가설. 독립성을 따르지 않는다.

### 이상치 확인 ###
library(car)
outlier=outlierTest(lmfinal, n.max=nrow(df3_2))
str(df3_2)
length(outlier$p) #41개
#outlier 행 번호
outlier_row=names(outlier$p)
outlier_row=as.numeric(outlier_row)

df3_3 = df3_2[-c(outlier_row),] # 이상치 제거한 데이터
str(df3_3) #41개 제거

### 이상치 제거한 데이터로 회귀 ###
lmout=lm(formula(lmfinal),data=df3_3)
summary(lmout)
ols_vif_tol(lmout)

lmoutStep=step(lmout,direction="both")
summary(lmoutStep)

windows()
par(mfrow=c(2,2))
plot(lmoutStep)

### 정규성 검정 ###
# 잔차가 정규분포 따르는지 확인 

# n>5000이므로 Kolmogorov-Smirnov test, 콜모고로프-스미노프 검정

# 귀무가설 : 정규분포를 따른다, 대립가설 : not 귀무가설
ks.test(lmoutStep$residuals, "pnorm", mean=mean(lmoutStep$residuals), sd=sd(lmoutStep$residuals))
# p-value < 2.2e-16 < 0.05 reject 귀무가설. 정규분포를 따르지 않는다.
windows()
hist(rstandard(lmoutStep)) # 표준화 잔차의 분포를 그래프로 그려보기 

### 독립성 검정 ###

# durbinWatsonTest : 잔차끼리 자기상관성 있는지 확인
#install.packages("lmtest")
# 귀무가설 : 잔차끼리 자기상관성을 가지지않는다. (독립)
# 대립가설 : 잔차끼리 자기상관성을 가진다.
library(lmtest)
dwtest(lmoutStep)
# p-value < 2.2e-16 < 0.05 reject 귀무가설. 독립성을 따르지 않는다.

# 이상치를 제거했지만, 등분산성, 독립성, 정규성 만족 안함


#################################정규성 해결####################
#1. 영향치 제거 (Cook's distance)

#2. 반응변수의 수학적 변환
##1) log 변환

### 이상치 제거한 데이터로 회귀 ###
formula(lmoutStep)
lmoutLog=lm(log(user_num)~ area5 + number_of_holders + group + food_num + work_num + 
              move_num + AGE_002 + AGE_004 + AGE_005 + AGE_006 + AGE_008 + 
              in_hanriverpark + season + traffic + number_of_holders:work_num + 
              group:AGE_008 + group:traffic + food_num:move_num,data=df3_3)
summary(lmoutLog)
ols_vif_tol(lmoutLog)

lmoutLogStep=step(lmoutLog,direction="both")
summary(lmoutStep)

windows()
par(mfrow=c(2,2))
plot(lmoutLogStep)

### 정규성 검정 ###
# 잔차가 정규분포 따르는지 확인 

# n>5000이므로 Kolmogorov-Smirnov test, 콜모고로프-스미노프 검정

# 귀무가설 : 정규분포를 따른다, 대립가설 : not 귀무가설
ks.test(lmoutLogStep$residuals, "pnorm", mean=mean(lmoutLogStep$residuals), sd=sd(lmoutLogStep$residuals))
# p-value < 2.2e-16 < 0.05 reject 귀무가설. 정규분포를 따르지 않는다.
windows()
hist(rstandard(lmoutLogStep)) # 표준화 잔차의 분포를 그래프로 그려보기 

### 독립성 검정 ###

# durbinWatsonTest : 잔차끼리 자기상관성 있는지 확인
#install.packages("lmtest")
# 귀무가설 : 잔차끼리 자기상관성을 가지지않는다. (독립)
# 대립가설 : 잔차끼리 자기상관성을 가진다.
library(lmtest)
dwtest(lmoutLogStep)
# p-value < 2.2e-16 < 0.05 reject 귀무가설. 독립성을 따르지 않는다.

# 이상치를 제거했지만, 등분산성, 독립성, 정규성 만족 안함

##2) squre route 변환

### ###
formula(lmoutStep)
lmoutsq=lm(sqrt(user_num)~ area5 + number_of_holders + group + food_num + work_num + 
              move_num + AGE_002 + AGE_004 + AGE_005 + AGE_006 + AGE_008 + 
              in_hanriverpark + season + traffic + number_of_holders:work_num + 
              group:AGE_008 + group:traffic + food_num:move_num,data=df3_3)
summary(lmoutsq)
ols_vif_tol(lmoutsq)

lmoutsqStep=step(lmoutsq,direction="both")
summary(lmoutsqStep)

windows()
par(mfrow=c(2,2))
plot(lmoutsqStep)

### 정규성 검정 ###
# 잔차가 정규분포 따르는지 확인 

# n>5000이므로 Kolmogorov-Smirnov test, 콜모고로프-스미노프 검정

# 귀무가설 : 정규분포를 따른다, 대립가설 : not 귀무가설
ks.test(lmoutsqStep$residuals, "pnorm", mean=mean(lmoutsqStep$residuals), sd=sd(lmoutsqStep$residuals))
# p-value < 2.2e-16 < 0.05 reject 귀무가설. 정규분포를 따르지 않는다.
windows()
hist(rstandard(lmoutsqStep)) # 표준화 잔차의 분포를 그래프로 그려보기 

### 독립성 검정 ###

# durbinWatsonTest : 잔차끼리 자기상관성 있는지 확인
#install.packages("lmtest")
# 귀무가설 : 잔차끼리 자기상관성을 가지지않는다. (독립)
# 대립가설 : 잔차끼리 자기상관성을 가진다.
library(lmtest)
dwtest(lmoutLogStep)
# p-value < 2.2e-16 < 0.05 reject 귀무가설. 독립성을 따르지 않는다.

# 이상치를 제거했지만, 등분산성, 독립성, 정규성 만족 안함

#lmoutSTep에서 유의하지 않는 변수들 제거
lm(user_num ~ area5 + number_of_holders + group + food_num + 
      move_num + AGE_002 + AGE_004 + AGE_005 + AGE_006 + 
     AGE_008 + in_hanriverpark + season  + 
     group:AGE_008 + group:traffic + food_num:move_num, data = df3_3)

#cook's distance 영향치 제거!
n=nrow(df3_3)
cooksd<-cooks.distance(lmout)
windows()
plot(cooksd,pch="*",cex=2)
abline(h=4/n,col="red")
i=subset(cooksd,cooksd>4/n)
cooksd_row=names(i)
cooksd_row=as.numeric(cooksd_row)
length(cooksd_row)
df4 = df3_3[-c(cooksd_row),]
nrow(df4)

#영향치 제거한 데이터 넣고 돌린 회귀모델
lmf=lm(user_num ~ area5 + number_of_holders + group + food_num + work_num + 
          move_num + AGE_001 + AGE_002 + AGE_004 + AGE_005 + AGE_006 + 
          AGE_008 + in_hanriverpark + season + traffic + number_of_holders:work_num + 
          group:AGE_008 + group:traffic + food_num:move_num, data=df4)
summary(lmf)

lmfStep=stepAIC(lmf,direction='both')
summary(lmfStep)

ols_vif_tol(lmfStep)

windows()
par(mfrow=c(2,2))
plot(lmfStep)

### 정규성 검정 ###
# 잔차가 정규분포 따르는지 확인 

# n>5000이므로 Kolmogorov-Smirnov test, 콜모고로프-스미노프 검정

# 귀무가설 : 정규분포를 따른다, 대립가설 : not 귀무가설
ks.test(lmfStep$residuals,"pnorm", mean=mean(lmfStep$residuals), sd=sd(lmfStep$residuals))

windows()
head(rstandard(lmf))
hist(rstandard(lmfStep))
lines(dnorm(1000000,0,1),col='red')


dwtest(lmfStep,alternative='two.sided')


library(car)
durbinWatsonTest(lmfStep)
formula(lmfStep)

#최종모델(lmfStep)의 QQPlot 확인 QQplot은 일직선을 따름.
#따라서 검정결과는 정규성을 만족한다고 하지 않지만, QQ-plot을 통해 정규성을 만족한다고 결론 내릴 수 있음.
windows()
qqPlot(lmfStep,id.method="identify",simulate=TRUE,main="Q-Q PLOT")

#############독립성 확인#################
windows()
plot(1:nrow(df4),residuals(lmfStep),main="잔차 vs 순서",xlab='Observation Order',ylab='residuals')
abline(h=1,col='red')

############################ K-fold method #########################################
library(caret) # createFolds
#최종 데이터 : df4
set.seed(1234)
folds <- createFolds(df4$user_num, k=10)
R2s <- sapply(folds, function (idx) {
  df.train.i <- df4[-idx, ]
  df.test.i <- df4[idx, ]
  model.i <- lm(formula(lmfStep), data=df.train.i)
  y.i <- df.test.i$user_num
  y.hat.i <- predict(model.i, newdata=df.test.i)
  TSS.i <- sum((y.i - mean(y.i))^2)
  RSS.i <- sum((y.hat.i - y.i)^2)
  adjR2.i <- 1 - (RSS.i/(length(y.hat.i)-16))/(TSS.i/(length(y.hat.i)-1))
  aic.i <- nrow(df.test.i)*log((sum(y.i-y.hat.i)^2)/nrow(df.test.i))+2*16
  bic.i <-nrow(df.test.i)*log((sum(y.i-y.hat.i)^2)/nrow(df.test.i))+16*log(nrow(df.test.i))
  return(list(adjR2.i,aic.i,bic.i))
})
str(folds)
#10-folds 한 모델 adjR2.i값 확인 

R2s
str(R2s)
res=t(R2s)
res # list 형태
str(res)

# list -> matrix
rnames=c("Fold01","Fold02","Fold03","Fold04","Fold05","Fold06","Fold07","Fold08","Fold09","Fold10")
cnames=c("adjR2","aic","bic")
rcnames=list(rnames,cnames)
res_mat=matrix(rep(0,30),nrow=10,ncol=3,byrow=FALSE,dimnames=rcnames)
str(res_mat)

for (i in 1:30){
  if (i %in% 1:10){
    for (j in 1:10){
      if (i==j){
        res_mat[j,1]<-res[[i]]}}}
  if (i %in% 11:20){
    for (j in 1:10){
      if (i-10==j){
        res_mat[j,2]<-res[[i]]}}}
  if (i %in% 21:30){
    for (j in 1:10){
      if (i-20==j){
        res_mat[j,3]<-res[[i]]}}}
}
res_mat

#10개의 모델의 평균 성능과 성능 분산 비교
mean(res_mat[,1])
mean(res_mat[,2])
mean(res_mat[,3])
sd(res_mat[,1])  
sd(res_mat[,2]) 
sd(res_mat[,3]) 

  #예측치 확인
  
  setwd('D:/회귀분석/회귀분석_프로젝트/data')
  user_m11=fread('new_user_m11.csv')
  str(user_m11)
  user_m11=user_m11[,-1]
  str(user_m11)
  
  str(df4)
  head(df4)
  
  unique(area5)

  df4$area5[1541]
  
  setwd('C:/Users/User/Documents/카카오톡 받은 파일')
  
  
  final=fread('finalfinalfinal(201812추가)_최종.csv')
  str(final)
  
  
  #문자형으로 불러와진 factor형 변수 factor함수써서 factor형으로 바꿔주기
  final$area=as.factor(final$area)
  final$in_hanriverpark=as.factor(final$in_hanriverpark)
  final$n_hanriverpark=as.factor(final$n_hanriverpark)
  final$season=as.factor(final$season)
  
  str(df)
  
  # area는 서울시 자치구(25개)로 범주가 너무 많으므로 권역별로 재범주화(5개)
  # 도심권 : 종로구, 중구, 용산구
  # 서북권 : 은평구, 서대문구, 마포구
  # 동북권 : 노원구, 도봉구, 강북구, 성북구, 중랑구, 동대문구, 성동구, 광진구
  # 서남권 : 강서구, 양천구, 영등포구, 구로구, 금천구, 동작구, 관악구
  # 동남권 : 서초구, 강남구, 송파구, 강동구
  table(final$area)
  area5 = as.character(final$area)
  table(area5)
  area5<-replace(area5,c(area5=="종로구"|area5=="중구"|area5=="용산구"),"도심권")
  area5<-replace(area5,c(area5=="은평구"|area5=="서대문구"|area5=="마포구"),"서북권")
  area5<-replace(area5,c(area5=="노원구"|area5=="도봉구"|area5=="강북구"|area5=="성북구"|area5=="중랑구"|area5=="동대문구"|area5=="성동구"|area5=="광진구"),"동북권")
  area5<-replace(area5,c(area5=="강서구"|area5=="양천구"|area5=="영등포구"|area5=="구로구"|area5=="금천구"|area5=="동작구"|area5=="관악구"),"서남권")
  area5<-replace(area5,c(area5=="서초구"|area5=="강남구"|area5=="송파구"|area5=="강동구"),"동남권")
  
  table(area5) #확인
  
  final=cbind(area5,final)		
  str(final)
  
  colnames(df)
  area5_n=final$area5[1:1540]; length(area5_n)
  number_of_holders_n=final$number_of_holders[1:1540]; length(number_of_holders_n)
  
  newdata=final[1:1540,]
  str(newdata)
  colnames(newdata)
  newdata=newdata[,-c(2:5,6,7:9,11:16,25:32,33)]
  str(newdata)
  newdata=cbind(newdata,user_m11)
  str(newdata)
  
  #######################################
  #####회귀 변수중요도 (수치형변수만 가능)######
  #######################################

  relweights <- function(fit,...){
    R <- cor(fit$model)
    nvar <- ncol(R)
    rxx <- R[2:nvar, 2:nvar]
    rxy <- R[2:nvar, 1]
    svd <- eigen(rxx)
    evec <- svd$vectors
    ev <- svd$values
    delta <- diag(sqrt(ev))
    lambda <- evec %*% delta %*% t(evec)
    lambdasq <- lambda ^ 2
    beta <- solve(lambda) %*% rxy
    rsquare <- colSums(beta ^ 2)
    rawwgt <- lambdasq %*% beta ^ 2
    import <- (rawwgt / rsquare) * 100
    import <- as.data.frame(import)
    row.names(import) <- names(fit$model[2:nvar])
    names(import) <- "Weights"
    import <- import[order(import),1, drop=FALSE]
    dotchart(import$Weights, labels=row.names(import),
             xlab="% of R-Square", pch=19,
             main="Relative Importance of Predictor Variables",
             sub=paste("Total R-Square=", round(rsquare, digits=3)),
             ...)
    return(import)
  }
  
  formula(lmfStep)
  summary(lmfStep)
  lmweight <- lm(user_num ~  number_of_holders + group + food_num + move_num + 
                 AGE_002 + AGE_004 + AGE_005 + AGE_006 + AGE_008 +  
                 traffic + group:AGE_008 + group:traffic + food_num:move_num, data=df4)
  windows()
  relweights(lmweight, col="blue")

  
  #ggplot
  library(ggplot2)
  plotRelWeights=function(fit){
    data<-relweights(fit)
    data$Predictors<-rownames(data)
    p <- ggplot(data=data,aes(x=reorder(Predictors,Weights),y=Weights,fill=Predictors))+ 
      geom_bar(stat="identity",width=0.5)+
      ggtitle("Relative Importance of Predictor Variables")+
      ylab(paste0("% of R-square \n(Total R-Square=",attr(data,"R-square"),")"))+
      geom_text(aes(y=Weights-0.1,label=paste(round(Weights,1),"%")),hjust=1)+
      guides(fill=FALSE)+
      coord_flip()
    p
  }
  
  windows()
  plotRelWeights(lmweight)
  
  #최종회귀모델의 신뢰구간그래프
  
  #일단 y와 y_hat의 scatter plot확인하고 회귀모델 직선과 비교
  
  pred=predict(lmfStep)
  str(pred)
  
  windows()
  plot(df4$user_num, pred)
  abline(0,1,col='red')
  
  summary(lmfStep)
  
  
  
