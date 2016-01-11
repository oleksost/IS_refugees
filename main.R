setwd("~/Documents/HU Berlin/WI 1516/refugeestest/IS refugees")
source("instaAuth.R")
source("searchForHashtag.R")
source("Utils.R")

#client id and secret can be interchanged over private comunication and not in gitHub

#load private file with client id + client_secret
source("../credentials_Insta.R")

#install.packages("devtools")
#devtools::install_github("hadley/httr")
#require("httr") 
#library("httr")
#HEAD("https://api.instagram.com/oauth")
#handle_find("http://google.com/")
#For Error handling
#http://recology.info/2014/12/multi-handle/


my_oauth <-instaOAuth(client_id,client_secret)
#saves my_pauth doc in the prior folder not to push it into the git
save(my_oauth, file="../my_oauth")
load("../my_oauth")
##################################################################################
#Loading Data
data.refugees <- searchInstagram(tag="refugees", token=my_oauth, n = 200000)

write.csv(data.refugees, file = "Data/Data_refugees.csv")

#tagsToLoad<-c("refugeesnotwelcome","norefugees","flüchtling","überfremdung", "wakeupeurope", "norefugees","refugeecrisis","nomorerefugees","syrianrefugees")
tagsToLoad<-c("norefugees","refugeecrisis","nomorerefugees","syrianrefugees")
for (i in 1:length(tagsToLoad)){
  print(tagsToLoad[i])
  data.on.Tag <- searchInstagram(tag=tagsToLoad[i], token=my_oauth, n = 200000)
  data.refugees <- rbind(data.refugees ,data.on.Tag)
  #remove dublicates
  data.refugees<-data.refugees[!duplicated(data.refugees[,"id"]),]
  write.csv(data.refugees, file = "Data/Data_refugees.csv")
}
write.csv(data.refugees, file = "Data/Data_refugees.csv")
##################################################################################
data.with.GeoLocation<-data.refugees[!is.na(data.refugees$longitude),]

#random data selecting
install.packages("caret",dep=TRUE)
require("caret")

idizies<-createDataPartition(y = data.with.GeoLocation$X, p=0.2, list=FALSE)
random.data.twentypercent<-data.with.GeoLocation[idizies,]
write.csv(data.refugees, file = "Random 20 percent.csv")

##################################################################################
#Hashtags reorganization/separation - tag / line
##################################################################################
#read data
data.refugees<-read.csv(file ="Data/Data_refugees_nachgeladen040116.csv")
data.refugees[,1]<-NULL

#take only id and tags
data_if_tag<-data.refugees[,c(13,17)]

#list of list of tags
tags<-apply(data_if_tag,1, function(x) sapply(strsplit(as.character(x[1])," "),function(y) unlist(lapply(y, function(a) unlist(a))) ) )
#tags2<-apply(data_if_tag,1, function(x) sapply(split(as.character(x[1])," "),function(y) unlist(lapply(y, function(a) unlist(a))) ) )
#replace NULLs with NAs
for(i in 1:length(tags)){
  tags[i][sapply(tags[[i]], is.null)]<-NA
}
id<-data_if_tag[,2]
ids<-unlist(sapply(1:length(id),function(i) rep(as.character(id[[i]]),length(tags[[i]])) ))
tagss<-unlist(tags)
id_tag_pro_line<-data.frame(ids, tagss)

write.csv(id_tag_pro_line, file = "Data/Data_refugees_tag_pro_line.csv")


##################################################################################
#Frequency Analysis
ftable(data)
xtabs(formula = ~ y , data)
tab<- table(data$y)
freq<-sort(tab, decreasing = TRUE)

#data<-data[-which(is.na(data$y)),]
positiv_id<-data[data$y=="refugeeswelcome",]$x
negativ_id<-data[data$y=="refugeesnotwelcome",]$x
positiv_id<-positiv_id[-which(is.na(positiv_id))]
negativ_id<-negativ_id[-which(is.na(negativ_id$y)),]

data$x[positiv_id]

freq_positiv<-sort(table(positiv$y), decreasing = TRUE)
freq_negativ<-sort(table(negativ), decreasing = TRUE)




