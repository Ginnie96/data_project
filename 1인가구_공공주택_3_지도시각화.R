#패키지 설치 
#install.packages('sf')
library('sf')
library('tidyverse')
library('ggplot2')

library(scales)
library(RColorBrewer)

#자료불러오기 st_read

map.seoul <- st_read('C:/temp/TL_SCCO_SIG.shp') 

#한글로 된 변수가 깨지지 않도록 인코딩 (SIG_KOR_NM) '
map.seoul$SIG_KOR_NM <- iconv(map.seoul$SIG_KOR_NM, from = "CP949", to = "UTF-8", sub = NA, mark = TRUE, toRaw = FALSE)
#head(map.seoul) 
#class(map.seoul)  

#서울시 데이터만 뽑기 (지역번호 11로 시작 )
map.seoul %>% 
  filter(substr(SIG_CD, start = 1, stop = 2) == "11") -> seoul.map

#지역특성 변수 
data<-read.table("C:/temp/teamdata/finalvar.txt",header=T,sep='\t')
data

#데이터 결합 
names(data)[names(data) == "region"] <- c("SIG_KOR_NM") 
seoul.map.final<- merge(seoul.map,data,by="SIG_KOR_NM",all.x=TRUE)

###시각화 

#solo.old
#지역별 독거노인 면적 당 거주 비율 시각화

older.map.ratio<-ggplot(seoul.map.final)+geom_sf(aes(fill=solo_old))
older.map.ratio+
  theme(axis.text=element_blank(),axis.ticks = element_blank(),panel.background = element_rect(fill="white"),legend.title=element_blank())


#acess
#교통 접근성 시각화 
traffic.map<-ggplot(seoul.map.final) + geom_sf(aes(fill = acess))
traffic.map+scale_fill_gradient(low='dark olive green', high='white')+
  theme(axis.text=element_blank(),axis.ticks = element_blank(),panel.background = element_rect(fill="white"),legend.title=element_blank())

#facility
#근린 생활시설 데이터 시각화 
facility.map<-ggplot(seoul.map.final) + geom_sf(aes(fill = facility))
facility.map+
  scale_fill_gradient(low='slate blue', high='lavender')+
  theme(axis.text=element_blank(),axis.ticks = element_blank(),panel.background = element_rect(fill="white"),legend.title=element_blank())

#socialplace
#노인복지시설, 교육시설 등 밀집도 데이터 시각화 
social.map <- ggplot(seoul.map.final) + geom_sf(aes(fill = socialplace))
social.map+scale_fill_gradient(low='gray25', high='white')+
  theme(axis.text=element_blank(),axis.ticks = element_blank(),panel.background = element_rect(fill="white"),legend.title=element_blank())

#clustering 결과를 자료와 결합  

c.result<-data.frame(SIG_KOR_NM=c("종로구","용산구", "성동구" ,"광진구", "동대문구", "중랑구", "성북구","강북구" ,"도봉구",  "서대문구", "마포구","양천구" ,"구로구","금천구","영등포구", "동작구" ,"관악구","서초구","강동구", "중구","노원구","강서구","송파구","은평구","강남구"),cluster=rep(c("1","2","3"),c(19,1,5)),
                     prefer.c=rep(c(0.033876,0.099515,0.048338),c(19,1,5)),c.final=rep(c(0.0374952,0.034943,0.0523876),c(19,1,5)))
c.result
seoul.map.c <- merge(seoul.map.final,c.result,by="SIG_KOR_NM",all.x=TRUE)
seoul.map.c
#clustering 시각화 
c.map <- ggplot(seoul.map.c) + geom_sf(aes(fill = cluster))
c.map+
  theme(axis.text=element_blank(),axis.ticks = element_blank(),panel.background = element_rect(fill="white"),legend.title=element_blank())+
  scale_fill_brewer(palette = "Pastel1")

#노인의 주거 선호도로 시각화 
old.prefer.map <- ggplot(seoul.map.c) + geom_sf(aes(fill = prefer.c))
old.prefer.map+
  theme(axis.text=element_blank(),axis.ticks = element_blank(),panel.background = element_rect(fill="white"),legend.title=element_blank())

#주거선호도 + 독거노인밀집도(최종) 
0.033876 * 0.2 + 0.0384 * 0.8
0.099515 * 0.2 + 0.0188 * 0.8
0.048338 * 0.2 + 0.0534 * 0.8 
final.cluster.map <- ggplot(seoul.map.c) + geom_sf(aes(fill = c.final))
final.cluster.map+
  scale_fill_gradient(low="gray32",high="light gray")+
  theme(axis.text=element_blank(),axis.ticks = element_blank(),panel.background = element_rect(fill="white"),legend.title=element_blank())
