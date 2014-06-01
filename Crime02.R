library(sqldf)
library(reshape)
library(plotrix)


##2010-2013
CrimesAllDate<-read.csv("F:\\RCrime\\crimeAll.csv",header=TRUE)
CrimeData<-na.omit(CrimesAllDate)
rm(CrimesAllDate)

##most frequent crime type
crimeblockDis<-sqldf("select count(*) as crimeCount,Block,PrimaryType from CrimeData group by Block,PrimaryType order by crimeCount desc")
crimeblockDisType<-sqldf("select sum(crimeCount) as crimeTotalCount,PrimaryType from crimeblockDis group by PrimaryType order by crimeTotalCount desc limit 6")
PrimaryType<-crimeblockDisType$PrimaryType

##reshape
md<-melt(crimeblockDis,id=(c("Block","PrimaryType")))
newcrimeblockDis<-cast(md,Block~PrimaryType)

##set NA to 0
newcrimeblockDis[is.na(newcrimeblockDis)==TRUE]<-0

##cluster dataset preparation
clustercrimeblockDis<-newcrimeblockDis
clustercrimeblockDis$Block=NULL
clusterdata<-clustercrimeblockDis[PrimaryType]

##kmeans
kc<-kmeans(clusterdata,6)

##cluster rst analysis
table(newcrimeblockDis$Block,kc$cluster)
blockCluster<-as.data.frame(kc$cluster)
##head(blockCluster)
##str(blockCluster)

cellcluster<-data.frame(Block=newcrimeblockDis$Block,cluster=blockCluster[[1]])
head(cellcluster)

cluster1<-as.character(cellcluster$Block[cellcluster$cluster==1])
lencluster1<-length(cluster1)
cluster1str<-cluster1[1]
for(iLenhour in 2:lencluster1){
  cluster1str<-paste(cluster1str,cluster1[iLenhour],sep=",")
}
cluster1str

cluster2<-as.character(cellcluster$Block[cellcluster$cluster==2])
lencluster2<-length(cluster2)
cluster2str<-cluster2[2]
for(iLenhour in 2:lencluster2){
  cluster2str<-paste(cluster2str,cluster2[iLenhour],sep=",")
}
cluster2str


cluster3<-as.character(cellcluster$Block[cellcluster$cluster==3])
lencluster3<-length(cluster3)
cluster3str<-cluster3[3]
for(iLenhour in 2:lencluster3){
  cluster3str<-paste(cluster3str,cluster3[iLenhour],sep=",")
}
cluster3str


cluster4<-as.character(cellcluster$Block[cellcluster$cluster==4])
lencluster4<-length(cluster4)
cluster4str<-cluster4[4]
for(iLenhour in 2:lencluster4){
  cluster4str<-paste(cluster4str,cluster4[iLenhour],sep=",")
}
cluster4str


cluster5<-as.character(cellcluster$Block[cellcluster$cluster==5])
lencluster5<-length(cluster5)
cluster5str<-cluster5[5]
for(iLenhour in 2:lencluster5){
  cluster5str<-paste(cluster5str,cluster5[iLenhour],sep=",")
}
cluster5str


cluster6<-as.character(cellcluster$Block[cellcluster$cluster==6])
lencluster6<-length(cluster6)
cluster6str<-cluster6[6]
for(iLenhour in 2:lencluster6){
  cluster6str<-paste(cluster6str,cluster6[iLenhour],sep=",")
}
cluster6str


clusterinfo<-data.frame(cluster1=cluster1str,cluster2=cluster2str,cluster3=cluster3str,cluster4=cluster4str,cluster5=cluster5str,cluster6=cluster6str)
write.table(clusterinfo, file = "F:\\RCrime\\clusterinfo.csv", sep = ",")

##cluster center
centers<-as.data.frame(kc$centers)
names(centers)<-PrimaryType
centers

##cell number of cluster
mytable<-table(kc$cluster)
mytable

##pie show
#pie3D(mytable,main="Cluster quantity chart")
#hc1 <- hclust(dist(clusterdata),method = "ave")
#plot(hc1 )
