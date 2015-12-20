setwd("~/Documents/HU Berlin/WI 1516/refugeestest/IS refugees")
#reading data
data.refugees<-read.csv(file ="Data/Data_refugees.csv")

#Find seperate words for santiment
##################################################################################
caption<-data.refugees$caption
a<-strsplit(as.character(caption[1:5]),"(?<=[#])",perl = TRUE)
#List pf Wordlist
splitwords<-function(x){
  w<-unlist(strsplit(x,"#| |\n"))
  words<- unlist(w[which(!w=="")])
  return(words)
}
words<-sapply(a,function(x) x<-splitwords(x))
#extract hashtags - NOT NEEDED ANYMORE
##################################################################################
install.packages("stringr",dep=TRUE)
require("stringr")
 devidedByHashTag<- strsplit(as.character(caption), "(?<=[#])", perl = TRUE)
 getHashTags<-function(x){
   tags<- x[(which(grepl("#$", x)))+1]
   x<-word(string = x ,1, sep = " |#|\n|\\.|\\,")
   return(x)
 }
 #List of Hashtag-Arrays
 HashTags<-sapply(devidedByHashTag, function(x) x<-getHashTags(x))
 
 stringsOfHashTags<-sapply(HashTags, function(x) paste(x, collapse=" "))
 
 data.refugees$HasgTags<-stringsOfHashTags
 write.csv(data.refugees, file = "Data/Data_refugees_tags.csv")
##################################################################################

#HashTags<-data.refugees$tags
#refs<-sapply(HashTags[1:10], function(x) sum(grepl("refugees", strsplit(x = as.character(x), split = " ", perl = TRUE))))
#refs.welcome<-sapply(HashTags, function(x) sum(grepl("regugeeswelcome", strsplit(x = as.character(x), split = " ", perl = TRUE))))
#refs.not.welcome<-sapply(HashTags, function(x) sum(grepl("refugeesnotwelcome|norefugees|überfremdung|wakeupeurope|nomorerefugees", strsplit(x = as.character(x), split = " ", perl = TRUE))))













