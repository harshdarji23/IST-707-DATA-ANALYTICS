
install.packages("rJava",type = "source")

install.packages("RWeka")

library(RWeka)

data<-read.csv("C:/Users/Harsh Darji/Desktop/iris.csv")

head(data)

str(data)

model_rweka<-SimpleKMeans(data,control=Weka_control(N=7,I=500,S=100))

model_rweka

# Using R

mydata<-data[,c(1:4)]

model_r<-kmeans(mydata,7)

model_r

#Print centroids

model_r$centers

cluster_Assignment<-data.frame(data,model_r$cluster)

head(cluster_Assignment)
#Outlier
#Setosa is being grouped into cluster no 1 as well as 3 but it should be grouped into 1

#visualization

install.packages("cluster")

library(cluster)

clusplot(mydata,model_r$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)

#RAC

d=dist(as.matrix(mydata))

hc=hclust(d)

plot(hc)


