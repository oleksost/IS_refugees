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
data.refugees <- searchInstagram(tag="refugees", token=my_oauth, n = 2000000)

write.csv(data.refugees, file = "Data/Data_refugees.csv")
#read data
#data.refugees<-read.csv(file ="Data/Data_refugees.csv")
data.refugees[,1]<-NULL

tagsToLoad<-c("refugeesnotwelcome","norefugees","flüchtling","überfremdung","wakeupeurope", "norefugees","refugeecrisis","nomorerefugees","syrianrefugees")
# tagsToLoad<-c("norefugees","refugeecrisis","nomorerefugees","syrianrefugees")
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




