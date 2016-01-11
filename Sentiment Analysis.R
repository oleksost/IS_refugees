setwd("~/Documents/HU Berlin/WI 1516/refugeestest/IS refugees")
#reading data
data.refugees<-read.csv(file ="Data/Data_refugees.csv")
#Find seperate words for santiment

score.sentiment = function(sentences, pos.words, neg.words, exc.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # exclude stop words
    check <- match(words,exc.words)
    exc.list <-!is.na(check)
    words <-words[!exc.list]
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}



data.refugees<-read.csv(file ="Data/Data_refugees_nachgeladen040116.csv")
colnames(data.refugees)
data_for_sentiment<-data.refugees[,c(13,18)]
#remove 
data_for_sentiment_cleaned<-gsub("#|@|\n|/|❤|[^[:alnum:][:blank:]+\\]"," ",data_for_sentiment$caption)

positive_words<-scan("positive_word_bank.txt", what = "character", comment.char=";")
negative_words<-scan("negative_word_bank.txt", what = "character", comment.char=";")
#exc.words - stop words
analysis<-score.sentiment(data_for_sentiment_cleaned,positive_words,negative_words, exc.words=NULL)
table(analysis$score)
hist(analysis$score)

##################################################################################
library(httr)
#API Sentiment Analysis
text<-data_for_sentiment_cleaned[13]
url <- "https://community-sentiment.p.mashape.com/text/"
r <- POST(url, add_headers("X-Mashape-Key:"="b046W1AuLimshPLIBGkMKeHGwYw0p1RCYLJjsnyHjhot6bIwCy", 
                          "Content-Type:"="application/x-www-form-urlencoded",
                          "Accept:"="application/json"), query=list("txt"="Hallo bad bad bad"), accept_json())


install.packages("JSON")
http_status(r)
str(content(r))
content(r, "text")









# End Sentiment Analysis
##################################################################################


#######some other shit


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

##################################################################################
















