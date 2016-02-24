setwd("~/Documents/HU_Berlin/WI_1516/refugeestest/IS refugees")
data.refugees<-read.csv(file ="../Data/Data_refugees_nachgeladen040116.csv")


data.refugees_positive<-read.csv(file ="../Data/positiv_set.csv")
data.refugees_negativ<-read.csv(file ="../Data/negatieve.csv")
data.refugees_negativ[,1]<-NULL
data.refugees_negativ[2,]

colnames(data.refugees_negativ)
data.refugees_negativ_kleine<-data.refugees_negativ[,c(9,17)]
write.csv(data.refugees_negativ_kleine, file = "../Data/negative1.csv")

#############################################################################################

refs_tag_pro_line<-read.csv(file ="../Data/Data_refugees_tag_pro_line.csv")
refs_tag_pro_line2<-refs_tag_pro_line
refs_tag_pro_line2$tagss<-sapply(refs_tag_pro_line, function(x) tolower(x))
#refs_tag_pro_line2[,1]<-NULL
dat<-refs_tag_pro_line
negative_ids<-dat[which(dat$tagss=="refugeesnotwelcome"|dat$tagss=="nomorerefugees"|dat$tagss=="fuckrefugees"),]
negative_ids_cleanet<-negative_ids[!duplicated(negative_ids[,2]),]

positive_ids<-dat[which(dat$tagss=="refugeeswelcome"),]
positive_id_cleaned<-positive_ids[!duplicated(positive_ids[,2]),]

seeker_ids<-dat[which(dat$tagss=="amazing"|dat$tagss=="follow4follow"|dat$tagss=="like4like"
                      |dat$tagss=="look"|
                        dat$tagss=="instalike"|
                        dat$tagss=="swag"|
                        dat$tagss=="instafollow"|
                        dat$tagss=="art"
),]
seeker_ids_cleaned<-seeker_ids[!duplicated(seeker_ids[,2]),]


negative_ids_cleanet_ids<-negative_ids_cleanet[,2]
positive_id_cleaned_id<-positive_id_cleaned[,2]
seeker_ids_cleaned_id<-seeker_ids_cleaned[,2]

negative_ids_cleanet_ids_cleaned<-negative_ids_cleanet_ids[!(negative_ids_cleanet_ids %in% positive_id_cleaned_id)]
positive_id_cleaned_id_cleaned<-positive_id_cleaned_id[!(positive_id_cleaned_id %in% negative_ids_cleanet_ids)]
positive_id_cleaned_id_cleaned<-positive_id_cleaned_id[!(positive_id_cleaned_id_cleaned %in% seeker_ids_cleaned_id)]

negative_set<-data.refugees[data.refugees$id %in% negative_ids_cleanet_ids_cleaned,]
positive_set<-data.refugees[data.refugees$id %in% positive_id_cleaned_id_cleaned,]




#colnames(negative_set)
#data.refugees_negativ_kleine<-data.refugees_negativ[,c(9,17)]
#write.csv(data.refugees_negativ_kleine, file = "../Data/negative1.csv")
#write.csv(negative_set, file = "../Data/neative_set.csv")
#write.csv(positive_set, file = "../Data/positive_set.csv")

colnames(negative_set)
#make subset as long as negative data set
positive_subset<-positive_set[sample(nrow(positive_set), length(negative_set[,1])), ]

#1 - positive, 0 - negativ
#positive_subset$class<-"1"
#negative_set$class<-"0"

#merge
#notoversamples_set<-rbind(positive_subset,negative_set)

#unlist(notoversamples_set[2,]$caption)

#delete "#"
#notoversamples_set[,12] <- sapply(notoversamples_set[,12], function(x) gsub("#"," ",x))

#write
#write.csv(notoversamples_set[,c(12,13,17,18)], file = "../Data/weka_analysis/set1.csv")
#write.csv(negative_set[,c(12,13,17)], file = "../Data/weka_analysis/neative.csv")
#write.csv(positive_subset[,c(12,13,17)], file = "../Data/weka_analysis/positive.csv")


#write files to category folders
sapply(1:length(negative_set[,1]), function(x) write( gsub("#refugeesnotwelcome|#nomorerefugees|#fuckrefugees|refugeesnotwelcome|nomorerefugees|fuckrefugees|#" ," ",paste0(negative_set[x,13]," ",negative_set[x,14])), file=paste0("../Data/weka_analysis_onlycomment/neg/",as.character(negative_set$id[x]),".txt")))
sapply(1:length(positive_subset[,1]), function(x) write( gsub("#refugeeswelcome|refugeeswelcome|[[:punct:]]"," ",paste0(positive_subset[x,13]," ",positive_subset[x,14])), file=paste0("../Data/weka_analysis_onlycomment/pos/",as.character(positive_subset$id[x]),".txt")))

#############################################################################################
#oversample
###################

#prepare test data
data_gelocated<-data.refugees[!is.na(data.refugees$longitude),]
colnames(data_gelocated)
data_gelocated_prepared<-data_gelocated[,c(13,14,18)]
data_gelocated_prepared$feature<-paste0(data_gelocated_prepared[,1]," ",data_gelocated_prepared[,2])
#data_gelocated_prepared$caption<-NULL
#data_gelocated_prepared$tags<-NULL
head(data_gelocated_prepared)
data_gelocated_prepared$feature<-sapply(data_gelocated_prepared$feature, function(x) gsub("[[:punct:]]","
#write                                                                                          ",x))
sapply(1:length(data_gelocated_prepared[,1]), 
       function(x) 
         write( data_gelocated_prepared$feature[x], 
         file=paste0("../Data/geolocated_test_data/",as.character(data_gelocated_prepared$id[x]),".txt")))


#make subset as long as negative data set
positive_subset<-positive_set[sample(nrow(positive_set), 0.2567*length(data_gelocated[,1])/2), ]

negative_set_oversampled<-negative_set
#oversample negative set
for (i in 1:(round(length(positive_subset[,1])/length(negative_set[,1]))-1)){
  negative_set_oversampled<-rbind(negative_set_oversampled,negative_set)
}
colnames(negative_set_oversampled)
colnames(positive_set)
negative_set_oversampled[,1]<-NULL
positive_set[,1]<-NULL

#write
sapply(1:length(negative_set_oversampled[,1]), function(x) write( gsub("[[:punct:]]"," ",paste0(negative_set_oversampled[x,12], negative_set_oversampled[x,13])), file=paste0("../Data/weka_oversampled/neg/",as.character(negative_set_oversampled$id[x]),"_",x,".txt")))
sapply(1:length(positive_subset[,1]), function(x) write( gsub("[[:punct:]]"," ",paste0(positive_subset[x,12], positive_subset[x,13])), file=paste0("../Data/weka_oversampled/pos/",as.character(positive_subset$id[x]),".txt")))

#############################################################################################
#analysis of the prediction
###################
predicted_sentiment_geolocated<-read.csv(file ="../Data/geolocated_classification.csv")
install.packages("stringr")
library(stringr)
a<-str_split_fixed(predicted_sentiment_geolocated$X1000009679659754714_508051176.pos, ";", 2)
#a<-rbind(a, c("1000009679659754714_508051176","pos"))
predicted_sentiment_geolocated<-a
#write.csv(predicted_sentiment_geolocated, file ="../Data/geolocated_classification.csv")
colnames(data_gelocated)
for (x in 1:length(data_gelocated[,1])){
  print(x)
data_gelocated[x,"class"] <-predicted_sentiment_geolocated[which(predicted_sentiment_geolocated[,1]==data_gelocated[x,18]),2]
}
data_gelocated$class
write.csv(data_gelocated, file ="../Data/data_geolocated_with_class.csv")

############################
#present on a map
data_gelocated<-read.csv(file ="../Data/data_geolocated_with_class.csv")
positives<-data_gelocated[data_gelocated$class=="pos",]
negatives<-data_gelocated[data_gelocated$class=="neg",]

install.packages("ggmap")
require(ggmap)
library(ggmap)
library(ggplot2)
map <- get_map(location = 'America', zoom = 3)
mapPoints <- ggmap(map) + geom_point(aes(x = longitude, y = latitude),data = negatives, alpha = .5, color = "red")
mapPoints

negatives[negatives$latitude]


install.packages("doBy")
install.packages("maptools")
library(doBy)
library(maptools) # read shapefiles (.shp)
# convert shp data to data frame for ggplot
eurMap
eurMap <- readShapePoly(fn="NUTS_2010_60M_SH/data/NUTS_RG_60M_2010")
plot(eurMap)

eurMapDf <- fortify(eurMap, region='NUTS_ID')

# read downloaded data (change folder appropriately)
eurEdu <- read.csv("educ_thexp_1_Data.csv", stringsAsFactors = F)
eurEdu$Value <- as.double(eurEdu$Value) #format as numeric


map <- get_map(location = 'Europe', zoom = 4)
m0 <- ggmap(map)
m1 <- m0 + geom_polygon(aes(x = longitude, y = latitude, group=group, fill=likes_count), data=negatives, alpha=.9)
m2 <- m1 + geom_path(aes(x = longitude, y = latitude, group=group  ), data=negatives, color='black')

txtVal <- summaryBy(longitude + latitude + likes_count ~ id, data=negatives, FUN=mean, keep.names=T)
m3 <- m2 + geom_text(aes(x = longitude, y = latitude, label=likes_count), data=txtVal, col="yellow", cex=3)
m

require("caret")
control<-trainControl(method="cv", number = 5, repeats=5)
gbm.model<-train(churn~.,data=tr.data, trConrol=control, method="gbm")
