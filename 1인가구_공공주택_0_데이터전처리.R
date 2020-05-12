library(tidyverse)

df<-read.table("C:/Users/user/Documents/카카오톡 받은 파일/Koweps_h13_2018_beta1.csv",header=T,sep=",")
dim(df)

##1인 가구 +다인 가구
house<-ifelse(df$h1306_3==1,1,0) #1:자가 0:자가 X
table(y)
one<-ifelse(df$h1301_1==1,1,0) #1인가구 추출 

#경제적 변수
attach(df)
income<-cbind(h1308_114,h1308_122,h1308_aq2,h1308_160,h1308_166,h1308_172,h1308_aq9, h1308_aq10, h1308_aq11, h1308_116, h1308_174, h1308_176, h1308_178, h1308_4aq106, h1308_4aq108, h1308_4aq110, h1308_4aq112, h1308_4aq114, h1308_116, h1308_118, h1308_120, h1308_3aq2, h1308_4aq70, h1308_4aq72, h1308_4aq74, h1308_4aq76, h1308_124, h1308_126, h1308_128, h1308_130, h1308_4aq78, h1308_4aq80, h1308_4aq82, h1308_4aq84, h1308_aq4, h1308_aq6, h1308_aq8, h1308_4aq96, h1308_4aq98, h1308_4aq100, h1308_4aq102,h1308_4aq104,h1308_11aq3,h1308_11aq2,h1308_aq25,h1308_7aq8,h1308_7aq6,h1308_7aq4,h1308_11aq1,h1308_5aq1,h1308_4aq117,h1308_4aq116,h1308_aq24,h1308_aq23,h1308_aq22,h1308_5aq3,h1308_aq21,h1308_aq20,h1308_aq19,h1308_10aq1,h1308_10aq1,h1308_aq16,h1308_aq9,h1308_aq10
)
#설문조사 답변 중 9개의 소득()을 더해서 하나의 income변수로 나타냄
#설문조사 특성 상 자신에게 해당하는 소득만을 체크하므로 NA가 무조건 존재

clean.income<-apply(income, 1, sum ,na.rm=T) 
#그러므로 NA를 무시하고 모든 income을 더해서 clean.income을 만듦
hist(clean.income) #right-skewed한 분포
log.income<-log(clean.income);hist(log.income)
t_root.income<-(clean.income)^(1/3);hist(t_root.income)
#얘가 제일 정규분포 모양에 가까우므로 채택
log.clean.income<-ifelse(clean.income==0,0,log(clean.income))

asset<- cbind(h1310_aq1,h1310_aq2,h1310_aq3,h1310_aq4,h1310_aq5,h1310_aq6,h1310_aq7,h1310_aq8,h1310_aq9,h1310_aq10,h1310_aq11,h1310_aq12,h1310_aq13,h1310_aq14,h1310_aq15,h1310_aq16,h1310_aq17,h1310_aq18,h1310_aq19,h1310_aq20,h1310_27,h1310_aq23,h1310_aq24,h1310_aq25,h1310_aq26)
clean.asset<-apply(asset, 1, sum ,na.rm=T)
table(clean.asset) #0인 것이 1102개임  
hist(clean.asset)
log.asset<-log(clean.asset) 
hist(log.asset)
log.clean.asset<-ifelse(clean.asset==0,0,log(clean.asset))
hist(log.clean.asset)

#사회적 인구 통계학적 변수
age<-(2019-df$h1301_5)
age.c <- cut(age, breaks=c(seq(20, 100,20)),labels=c("20-40","40-60","60-80","80-100"))
table(age.c)

sex<-df$h1301_4 #남:1 여:0
table(sex)
sex.d<-ifelse(df$h1301_4==1,1,0) 
table(sex.d)
edu<-df$h1301_6
table(edu)
edu.d<-ifelse(df$h1301_6>=7,1,0) 


#지역적 변수
seoul<-df$h13_reg7 #더미(서울:1,비서울:0)
seoul.d<-ifelse(df$h13_reg7==1,1,0) 
seoul.d
table(seoul.d)
dim(df)
