library("tidyverse")
seoul_ft<-read.csv("//dongjak_foot_traffic.csv")
#head(seoul_ft)
#dim(seoul_ft)
#dj.ft<-seoul_ft[grep("노량진중앙시장|본동인정시장|신노량진시장|강남시장_동작|상도전통시장|영도시장|성대시장|상도4동도깨비골목시장|흑석시장|남성사계시장|사당시장|남성역골목시장|신대방삼거리역|노량진역|국사봉2길|노량진1동_1|동작구 총신대입구역_1|보라매공원|사당역_2|동작구|노량진동|대방동|동작동|본동|사당동|상도1동|상도동|신대방동|흑석동|노량진로|노들로|여의대방로|상도로|보라매로|알마타길|등용로|동작대로|매봉로|현충로|솥밭로|사당로|남부순환로|양녕로|상도로|강남초등|국사봉|만양로|성대로|장승배기로|신대방길|신대방|서달로|흑석로",seoul_ft$상권_코드_명),]
#dj.ft<-dj.ft[!(dongjak.ft$상권_코드==1001412),]
#write.csv(dj.ft,"C:/temp/dongjak/dongjak.ft.2.csv")
#dim()

#####동작구 내 상권만 추출한 뒤, 기존에 있던 dj.ratio.csv 파일에 덮어씌웠다#####
dj.total.ratio<-read.csv("C:/temp/dongjak/dj.ratio.csv",encoding = 'UTF-8')
new_cluster2 <- read.csv("C:/temp/dongjak/new/new_cluster2.csv")
new_cluster2 <- new_cluster2[,1:4]
dj.total.ratio <- dj.total.ratio %>% 
  filter(X.U.FEFF.street.code %in% unlist(new_cluster2[,1])) 

write.csv(dj.total.ratio, "C:/temp/dongjak/new/dj.ratio.csv")

dj.total.ratio<-read.csv("C:/temp/dongjak/new/dj.ratio.csv")
dj.total.ratio<-rename(dj.total.ratio,street.code=X.U.FEFF.street.code)
names(dj.total.ratio)


#####표준화#####
names(dj.total.ratio)
dj.test<- dj.total.ratio[,c(2,5,8:13)]
dj.test.st<-apply(dj.test[,2:8], 2, scale)

#####클러스트갯수#####
library(NbClust)

nc.1 <- NbClust(dj.test.st, min.nc = 2, max.nc = 15, method = "kmeans")
#시각화 
par(mfrow=c(1,1))
barplot(table(nc.1$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")
#다른방법 sse합계 함수정의
wssplot <- function(data, nc = 15, seed = 1234) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab = "Number of Clusters",
       ylab = "Within groups sum of squares")}
#시각화
wssplot(dj.test.st)

#2개가 가장 적합하지만.. 일단 4개로 해보겠음
#혹시몰라서 표준화안한거
NbClust(dj.test[2:8], min.nc = 2, max.nc = 15, method = "kmeans") ###########이 부분 에러가 뜨는데 해결방법을 모르겠음########
wssplot(dj.test[2:8])
#이거도 두개가 가장 적합할거래 ㅎㅎ 

#######클러스터링######
#일단 상권 유동인구만 가지고 해보겠다 
#ft.k4<-kmeans(dj.test.st,4)
#ft.k4$cluster
#c4<-data.frame(ft.k4$cluster)
#dj.test.c<-cbind(dj.test,c4)


#############################
########매출데이터#######

seoul.sell.new<-read.csv("C:/temp/dongjak/new/seoul_selling_new.csv")
names(seoul.sell.new)
dj.selling <- seoul.sell.new %>% 
  filter(癤퓋treet.name %in% unlist(new_cluster2[,1])) 
names(dj.selling)
dj.selling<-rename(dj.selling,street.code=癤퓋treet.name)


#merge
dj.merge<-merge(dj.selling,dj.total.ratio,x.all=T,by=c("street.code"))
names(dj.merge)

apply(apply(dj.merge,2,is.na),2,sum)

dj.merge<-dj.merge%>%select(street.name,1:39)
dj.merge<-dj.merge%>%select(-23,-24)
############dj.merge로 클러스터링을 합시다#############
  
#write.csv(dj.merge,"C:/temp/dongjak/new/dongjak__merge_final.csv")


#######시각화 ######
#클러스터별 연 매출 횟수#
ggplot(dj.final.c,aes(factor(c1),month.count))+
  geom_boxplot(aes(color=factor(c1)))+
  labs(x="cluster type", y="montly count")+scale_color_discrete(name="cluster type")+
  ggtitle("Number of Clusters = 4") + theme(plot.title = element_text(face = "bold", hjust = 0.5))


#클러스터별 연 매출# 
ggplot(dj.final.c,aes(factor(c1),month.amt))+
  geom_boxplot(aes(color=factor(c1)))+
  labs(x="cluster type", y="montly amount")+scale_color_discrete(name="cluster type")+
  ggtitle("Number of Clusters = 4") + theme(plot.title = element_text(face = "bold", hjust = 0.5))

#매출은 3번 클러스터가 가장 높은 것으로 보임 . 

#####클러스터별 매출량 차이가 유의한지 ANOVA #####일단안함 
#summary(aov(month.amt ~ factor(c1), data = dj.final.c))
#p통계량을 보니 유의하지 않은 것으로 보임... 매출을 넣고 다시 클러스터링 해보자. 


#######매출 데이터를 가지고 클러스터링#########

#표준화

test.st<-apply(dj.merge[2:37],2,scale)
test.st<-data.frame(test.st)
names(test.st)
#k개수 정하기 
nc <- NbClust(test.st[,c(2:3,7,22,25:30)], min.nc = 2, max.nc = 15, method = "kmeans")
#시각화 
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")
wssplot(test.st[,c(2:3,7,22,25:30)])


#5개로 해보자 
result.k2<-kmeans(test.st[,c(2:3,7,22,25:30)],2)
result.k2

result.k6<-kmeans(test.st[,c(2:3,7,22,25:30)],6)
result.k6
#3개로 해보자 
result.k3<-kmeans(test.st[,c(2:3,7,22,25:30)],3)
result.k3

k2<-data.frame(result.k2$cluster)
k3<-data.frame(result.k3$cluster)
k6<-data.frame(result.k6$cluster)
k3
names(dj.merge)
clustering <- cbind(k2,k3,k6,dj.merge)
names(clustering)
clustering_final<-clustering%>%select(street.code,street.name,1:40)
clustering_final

write.csv(clustering_final,"C:/temp/dongjak/new/real_final_new.csv")
final<-rename(clustering_final,k2=result.k2.cluster,k3=result.k3.cluster,k6=result.k6.cluster)
final

#######ANOVA#######
summary(aov(mon_selng_amt ~ factor(k3), data = final))
summary(aov(mon_selng_co ~ factor(k3), data = final))
#3개로 클러스터링을 진행했을 때, cluster별 매출양이 차이가 남 

summary(aov(mon_selng_amt ~ factor(k6), data = final))
summary(aov(mon_selng_co ~ factor(k6), data = final))
#6개로 클러스터링을 진행했을 때, cluster별 매출양이 차이가 남 

#######시각화########
p.k3<-ggplot(final,aes(x=factor(k3),color=factor(k3)))
p.k3+geom_boxplot(aes(y=mon_selng_amt))
p.k3+geom_boxplot(aes(y=mon_selng_co))

p.k6<-ggplot(final,aes(x=factor(k6),color=factor(k6)))
p.k6+geom_boxplot(aes(y=mon_selng_amt))
p.k6+geom_boxplot(aes(y=mon_selng_co))


########시각화2############
names(final)
final.tb<-tibble(final)
final.tb<-final.tb %>% 
  mutate(age10=as.numeric(age10),age20=as.numeric(age20),age30=as.numeric(age30),age40=as.numeric(age40),age50=as.numeric(age50),age60=as.numeric(age60)) 


final.tb1<-final.tb%>%
  gather(age10,age20,age30,age40,age50,age60,key=age,value=age_ft)%>%
  gather(agrde_10_amt,agrde_20_amt,agrde_30_amt,agrde_40_amt,agrde_50_amt,agrde_60_amt,key=selling_age,value=age_sell_amt)

final.tb.g1<-final.tb1%>%
  group_by(k6,age)%>% summarise(ft_mean=mean(age_ft))

final.tb.g2<-final.tb1%>%
  group_by(k6,selling_age)%>% summarise(sell_avg=mean(age_sell_amt))

final.tb.g1
final.tb.g2

###클러스터별 나이대별 평균 유동인구###
ggplot(final.tb.g1,aes(age,ft_mean))+
  geom_bar(stat="identity",aes(fill=factor(age)))+facet_wrap(~factor(k6))+
  scale_fill_brewer(palette = "YlGnBu")+
  theme(legend.position = "none")+
  ggtitle('나이대별 평균 유동인구')+
  labs(y="",fill="age")+
  scale_x_discrete(breaks=NULL)+
  theme_minimal() 

###클러스터별 나이대별 평균 매출###
ggplot(final.tb.g2,aes(selling_age,sell_avg))+
  geom_bar(stat="identity",aes(fill=factor(selling_age)))+facet_wrap(~factor(k6))+
  scale_fill_brewer(palette = "YlGnBu")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
  ggtitle('나이대별 평균 매출')+
  labs(y="평균 매출",fill="age")+
  labs(y="",fill="age")+
  scale_x_discrete(breaks=NULL)+
  theme_minimal() 



