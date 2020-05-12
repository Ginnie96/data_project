#hclust
data<-read.table("C:/Users/user/Documents/finalvar.txt",header=T,sep='\t')
data$region<-as.factor(data$region)

x<-data[,-1]
dist_mat <- dist(x, method = 'euclidean')
hclust_avg <- hclust(x, method = 'average')
plot(hclust_avg)
summary(hclust_avg)
groups$region<-data$region[order.dendrogram(as.dendrogram(hclust_avg))]
hclust_avg$region<-data$region[order.dendrogram(as.dendrogram(hclust_avg))]
as.dendrogram(hclust_avg)
groups <- cutree(hclust_avg, k=3)
split(data$region,groups)
rect.hclust(hclust_avg,k=3,border=("red"))

a<-cbind(data$region,groups)
a[groups==1,1]
a[groups==2,1]
a[groups==3,1]

split(data$region,hclust_avg)

?tolower
rownames(data)=tolower(rownames(data))
data
x.scaled=scale(x)
install.packages("NbClust")
library(NbClust)
devAskNewPage(ask=TRUE)
nc=NbClust(x.scaled,distance="euclidean",min.nc=2,max.nc=15,
           method="average")

devAskNewPage(ask=FALSE)

table(nc$Best.n[1,])
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),xlab="Number of Clusters",ylab="Number of Criteria",
        main="Number of Clusters Chosen")
#최종적으로 3개의 클러스터 선택
#최종결과획득
table(groups)
aggregate(x,by=list(cluster=groups),median)