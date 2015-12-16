setwd("C:/Users/Dmitriy/Desktop/HU/Seminar IS/GIT/IS_refugees/Data")

data_pooled<-read.csv('Data_refugees.csv')
head(data_pooled)
data<-data_pooled[,!names(data_pooled)%in%c('image_url')]
data<-data[!is.na(data$longitude),]

set.seed(200)
splited<-data[sample(nrow(data)*.2),c('link','id')]
x<-split(splited, 1:3)
liza<-data.frame(x[1])
dima<-data.frame(x[2])
lesha<-data.frame(x[3])
