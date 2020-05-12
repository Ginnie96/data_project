#반응변수 1인가구 여부(one)을 두고 randomForest 적합  
library(randomForest)
a<-data.frame(house,one,t_root.income,log.clean.asset,age.c,sex.d,edu.d,seoul.d)
a$one<-as.factor(ifelse(a$one==1,"Yes","No"))
rf.fit1 = randomForest(one~.
                       , data=train.data.c, mtry = floor(sqrt(7)), ntree = 500, importance = T)

importance(rf.fit1)
varImpPlot(rf.fit1)
#income,sex,age,asset이 1인가구여부에 유의한 영향을 미침을 확인 
#본 팀은 자력으로 주거공간을 꾸릴 수 없는 저소득층을 전제로 함
#age에 집중하기로하

##자가소유 여부(house)을 두고 randomForest 적합  
rf.fit2 = randomForest(house~.
                       , data=train.data.c, mtry = floor(sqrt(7)), ntree = 500, importance = T)

importance(rf.fit2)
varImpPlot(rf.fit2)

##자가소유 여부 반응변수로 둔 로지스틱 회귀 분석 (경향성 파악)
full.logit.house<-glm(house~.,family = binomial(link="logit"),data=a)
summary(full.logit.house)
#1인 가구일 수록 다인 가구에 비해 자가가 비교적 없다는 경향성을 파악했다 
table(a$house,a$one)

