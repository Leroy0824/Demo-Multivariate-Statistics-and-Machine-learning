# Problem 6.1
library("mlbench")
data(Zoo)
#str(Zoo)
dim(Zoo)

Xmatrix<-Zoo[,1:16]
Ymatrix<-factor(Zoo[,17])
#str(Xmatrix);str(Ymatrix)

colnames(Xmatrix)
levels(Ymatrix)

summary(Ymatrix)

distance<-dist(Xmatrix)
hc1<-hclust(distance,method="ward.D2")
hc2<-hclust(distance,method="average")
hc3<-hclust(distance,method="complete")
hc4<-hclust(distance,method="single")
#str(hc1);str(hc2);str(hc3);str(hc4)

par(mfrow=c(2,2))
plot(hc1,label=FALSE)
plot(hc2,label=FALSE)
plot(hc3,label=FALSE)
plot(hc4,label=FALSE)

tester<-function(h1=NULL,k1=NULL){
    par(mfrow=c(2,2))
    plot(hc1,label=FALSE)
    rect.hclust(tree=hc1,k=k1,h=h1)
    plot(hc2,label=FALSE)
    rect.hclust(tree=hc2,k=k1,h=h1)
    plot(hc3,label=FALSE)
    rect.hclust(tree=hc3,k=k1,h=h1)
    plot(hc4,label=FALSE)
    rect.hclust(tree=hc4,k=k1,h=h1)
    par(mfrow=c(1,1))
}
tester(k1=10)

tester2<-function(k1=NULL,h1=NULL){
    temp1<-cutree(hc1,k=k1,h=h1)
    temp2<-cutree(hc2,k=k1,h=h1)
    temp3<-cutree(hc3,k=k1,h=h1)
    temp4<-cutree(hc4,k=k1,h=h1)
    print(table(temp1));print(table(temp2))
    print(table(temp3));print(table(temp4))
    par(mfrow=c(2,2))
    plot(table(temp1));plot(table(temp2))
    plot(table(temp3));plot(table(temp4))
}
tester2(k1=10)

# Problem 6.2
set.seed(1)
library("mnormt")
n1<-300
mu1<-c(5,5)
sigma1<-matrix(c(1,0,0,1),nrow=2,ncol=2)
x1<-rmnorm(n1,mean=mu1,varcov=sigma1)

n2<-500
mu2<-c(8,3)
sigma1<-matrix(c(4,0,0,9),nrow=2,ncol=2)
x2<-rmnorm(n2,mean=mu2,varcov=sigma1)

n3<-900
mu3<-c(12,9)
sigma1<-matrix(c(8,0,0,3),nrow=2,ncol=2)
x3<-rmnorm(n3,mean=mu3,varcov=sigma1)

Xmatrix<-rbind(x1,x2,x3)
Ymatrix<-factor(c(rep("Class A",n1),rep("Class B",n2),rep("Class C",n3)))
head(Ymatrix)

temp<-kmeans(Xmatrix,centers=3)
names(temp)
temp$centers #mean value of each cluster
temp$size #number of observations in each cluster
temp$totss #SSE across all observations
temp$tot.withinss #SSE across all observations in each cluster
temp$iter #Number of iterations used

center<-temp$centers
plot(Xmatrix,col=as.integer(Ymatrix),pch=1)
points(center,pch=16,col="purple")
legend("bottomright",legend=c(levels(Ymatrix),"center"),col=c(1,2,3,"purple"),pch=c(1,1,1,16))
center

temp2<-as.matrix(temp$centers)
temp3<-t(temp2)
temp4<-matrix(0,nrow=nrow(temp3),ncol=ncol(temp3))
temp5<-temp4
x<-order(temp3[1,])
for (i in 1:3){
    temp4[2,x[i]]<-temp3[2,i]
    temp4[1,x[i]]<-temp3[1,i]
}
print(temp4)
temp5<-cbind(mu1,mu2,mu3)
temp5
temp5-temp4

table(Ymatrix,temp$cluster)
names(temp)
temp6<-matrix(0,nrow=10,ncol=2)
for (i in 1:10){
    temp<-kmeans(Xmatrix,centers=i)
    temp6[i,1]<-temp$tot.withinss
    temp6[i,2]<-temp$betweenss
}
colnames(temp6)<-c("tot.withinss","betweenss")
print(temp6)
plot(1:10,temp6[,1],xlab="# of classes",ylab="tot.withinss",type="s")
plot(1:10,temp6[,2],xlab="# of classes",ylab="betweenss",type="s")

# Problem 6.3
# We want to apply the same analytical methods as in Problem 6.2, but to real-world data rather than generated data.
set.seed(1)
data(iris)
str(iris)
names(iris)
Xmatrix<-scale((iris[,1:4]),scale=TRUE)
Ymatrix<-iris[,5]
table(Ymatrix)
pairs(Xmatrix,col=as.integer(Ymatrix),pch=as.integer(Ymatrix))
temp<-kmeans(Xmatrix,centers=3)
names(temp)
temp$centers #mean value of each cluster
temp$size #number of observations in each cluster
temp$totss #SSE across all observations
temp$tot.withinss #SSE across all observations in each cluster
temp$iter #Number of iterations used

center<-temp$centers
plot(Xmatrix,col=as.integer(Ymatrix),pch=1)
points(center,pch=16,col="purple")
legend("bottomright",legend=c(levels(Ymatrix),"center"),col=c(1,2,3,"purple"),pch=c(1,1,1,16))
center

temp2<-as.matrix(temp$centers)
temp3<-t(temp2)
temp4<-matrix(0,nrow=nrow(temp3),ncol=ncol(temp3))
temp5<-temp4
x<-order(temp3[1,])
for (i in 1:3){
    temp4[2,x[i]]<-temp3[2,i]
    temp4[1,x[i]]<-temp3[1,i]
}
print(temp4)
for(i in 1:3){
    temp5[,i]<-colMeans(Xmatrix[Ymatrix==levels(Ymatrix)[i],])
}
temp5
colnames(temp5)<-levels(Ymatrix)
temp5-temp4

table(Ymatrix,temp$cluster)
names(temp)
temp6<-matrix(0,nrow=10,ncol=2)
for (i in 1:10){
    temp<-kmeans(Xmatrix,centers=i)
    temp6[i,1]<-temp$tot.withinss
    temp6[i,2]<-temp$betweenss
}
colnames(temp6)<-c("tot.withinss","betweenss")
print(temp6)
plot(1:10,temp6[,1],xlab="# of classes",ylab="tot.withinss",type="s")
plot(1:10,temp6[,2],xlab="# of classes",ylab="betweenss",type="s")