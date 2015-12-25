###Twitter
setwd("~/Documents/HU Berlin/WI 1516/refugeestest/IS refugees")


install.packages("twitteR", dependencies = T)
install.packages("twitteR")
install.packages("ROAuth")
library("twitteR")
library("ROAuth")
library(plyr)


#getTwitterOAuth(api_key, api_secret)

source("../Twitter cridentials.R")


# Download "cacert.pem" file
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

#create an object "cred" that will save the authenticated object that we can use for later sessions
cred <- OAuthFactory$new(consumerKey=api_key,
                         consumerSecret=api_secret,
                         requestURL="https://api.twitter.com/oauth/request_token",
                         accessURL="https://api.twitter.com/oauth/access_token",
                         authURL="https://api.twitter.com/oauth/authorize")

# Executing the next step generates an output --> To enable the connection, please direct your web browser to: <hyperlink> . Note:  You only need to do this part once
cred$handshake(cainfo="cacert.pem")

#save for later use for Windows
save(cred, file="../twitter authentication.Rdata")

load("../twitter authentication.Rdata")
registerTwitterOAuth(cred)

tweets<- searchTwitter("refugees", n=100000, cainfo="cacert.pem")

length(tweets)

