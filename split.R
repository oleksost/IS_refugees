setwd("C:/Users/Dmitriy/Desktop/HU/Seminar IS/GIT/IS_refugees/Data")
#setwd("~/Documents/HU Berlin/WI 1516/refugeestest/IS refugees")

data_pooled<-read.csv('Data/Data_refugees.csv')
head(data_pooled)
data<-data_pooled[,!names(data_pooled)%in%c('image_url')]
data<-data[!is.na(data$longitude),]

set.seed(200)

# error, only takes first 20% of the data raws
splited<-data[sample(nrow(data)*.2),c('link','id')]
x<-split(splited, 1:3)
liza<-data.frame(x[1])
dima<-data.frame(x[2])
lesha<-data.frame(x[3])

##################################################################################
data_with_geoLov<-data.refugees[!is.na(data.refugees$latitude),]
HashTags<-data_with_geoLov$tags
tags<-strsplit(as.character(HashTags),split = " ", perl = TRUE)

refs<-sapply(tags, function(x) sum(grepl("\\<refugees\\>", x)))
refs.welcome<-sapply(tags, function(x) sum(grepl("\\<refugeeswelcome\\>", x)))
refs.not.welcome<-sapply(tags, function(x) sum(grepl("\\<refugeesnotwelcome\\>|\\<norefugees\\>|\\<überfremdung\\>|\\<wakeupeurope\\>|\\<nomorerefugees\\>", x)))
negative.Indicies<-which(refs.not.welcome>0)
welcome.indicies<-which(refs.welcome>0)
#schnittmenge?
#intersect(negative.Indicies,  welcome.indicies)
negative.Indicies.without.intersect<-setdiff(negative.Indicies,intersect(negative.Indicies,  welcome.indicies))
#out of all media with geolocation only 96 can be defined as truly negative based on hashtags
# -> to few truly negative media for ng/positiv analysis

##################################################################################
#split
#indicies
indicies.refs<-which(refs>=0)
indicies.refs.wichtout.negative<-setdiff(indicies.refs,negative.Indicies.without.intersect)

set.seed(200)
rand.indicies<-sample(indicies.refs.wichtout.negative, length(indicies.refs.wichtout.negative)*.2)
#true negatives can be added to the data sample that will be reviewed manualy, since there are only a few of true negatives
rand.indizies.with.true.negatives<-append(rand.indicies, negative.Indicies.without.intersect)

x<-split(rand.indizies.with.true.negatives, 1:3)
liza.indizies<-x[1]
dima.indizies<-x[2]
lesha.indizies<-x[3]

liza.data<-data_with_geoLov[unlist(liza.indizies),c('link','id')]
dima.data<-data_with_geoLov[unlist(dima.indizies),c('link','id')]
lioscha.data<-data_with_geoLov[unlist(lesha.indizies),c('link','id')]
##########
write.csv(liza.data, file = "Data/lizas.csv")
write.csv(dima.data, file = "Data/dima.csv")
write.csv(lioscha.data, file = "Data/lioscha.csv")

