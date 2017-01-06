## retrieve stats for each player on pga tour
## requires jsonlite, rlist, gtools, splitstackshape, dplyr, plyr

library(plyr)
library(jsonlite)
library(rlist)
library(gtools)
library(splitstackshape)
library(dplyr)
## this is the url from 2016 masters, but has the data for all the player id's needed to form further urls
url<-"http://www.pgatour.com/data/r/stats/current/02671.json"

## create player, pid dataframe
players<-fromJSON(url)
players<-players$tours$years[[1]]
players<-players$stats[[1]]$details[[1]]
players<-players[,1:2]
players$playerName<-paste(players$plrName$first,players$plrName$last, sep=" ")
players<-players[,-2]

## create a folder in which to put seperate files for each player (if it isn't there yet)
if(!dir.exists("player_stats")) dir.create("player_stats")

setwd("player_stats")

## this is the function that will scrape the stats for each player on tour
retrieve_stats<-function (x) {
  pid<-x[1]
  name<-x[2]
  url2<-paste("http://www.pgatour.com/data/players/",pid,"/2016stat.json", sep="") 
  temp<-fromJSON(url2)
  filename<-paste(name, ".rds", sep="")
  list.save(temp, filename)
}
## run through all the players and get their stats
## following loop commented out so i don't continuously scrape the web and get flagged
apply(players, 1, retrieve_stats)


##function for processing data into single data set
build_data<-function (x) {
##unload all the lists into their own readable frames
  temp_list<-list.load(x)
  temp1<-as.data.frame(temp_list$plrs$years)
  temp2<-as.data.frame(temp1$tours)
  temp3<-as.data.frame(temp2$statCats)
  
  temp4<-temp3$stats
  rm(list=c("temp1", "temp2", "temp3", "temp_list"))
  temp4a<-as.data.frame(temp4[6])
  temp4a<-rbind(temp4a, as.data.frame(temp4[5]))
  temp4a<-rbind(temp4a, as.data.frame(temp4[4]))
  temp4a<-rbind(temp4a, as.data.frame(temp4[3]))
  temp4a<-rbind(temp4a, as.data.frame(temp4[2]))
  temp4a<-rbind(temp4a, as.data.frame(temp4[1]))
  temp4a<-rbind(temp4a, as.data.frame(temp4[7]))
  
  ## turns out additional information gave insight into how stats gathered, but isn't needed for now. 
  ## Leaving code so I don't forget how to do it.
  ## this gets the last additional data from key/value pairs into a dataframe 
  ## test4b<-as.data.frame(temp4a$additionals)
  ## key<-which(odd(1:length(test4b)))
  ## val<-which(even(1:length(test4b)))
  ## extra<-data.frame(key=as.character(test4b[,key]), value=as.character(test4b[,val]), stringsAsFactors = F)
  ## extra<-cSplit(extra, c("key", "value"), ",", "long")
  ## extra <- data.frame(lapply(extra, as.character), stringsAsFactors=FALSE)
  ## extra$key<-gsub("^c\\(\"", "", extra$key)
  ## extra$value<-gsub("^c\\(\"", "", extra$value)
  ## extra$key<-gsub("\\)","" , extra$key)
  ## extra$value<-gsub("\\)","" , extra$value)
  ## extra$key<-gsub("\"", "", extra$key)
  ## extra$value<-gsub("\"", "", extra$value)
  ## extra<-na.omit(extra)
  ## extra<-subset(extra, extra$key!="")
  ## rm(test4b)
  temp4a$additionals<-NULL
  ## data1<-merge(temp4a, extra, by.x=c("name", "value"), by.y=c("key", "value"), all=TRUE)
  ## rm(temp4a)
  ## rm(extra)
  data1<-arrange(temp4a, name)
  data1$statID<-NULL
  data1$rank<-NULL
  data1$projRank<-NULL
  
  rm(temp4a)
  ## remove duplicate data that has lost it's significance at this point
  ## keepers<-which(duplicated(data1$name)==FALSE)
  ## data1<-data1[keepers,]
  ## convert to wide with names as colnams instead
  data1<-t(data1)
  data1<-as.data.frame(data1, stringsAsFactors = FALSE)
  colnames(data1)<-data1[1,]
  data1<-data1[-1,]
  ## data1<-data1[-c(2:4),]
  x1<-gsub(".rds", "", x)
  data1$playerName<-x1
  write.csv(data1, paste(x1, ".csv", sep=""))
}

## create a list of all the csv's


## get file list
filelist<-list.files(pattern="rds")
## run through list to build data base
lapply(filelist, build_data)


files2 <- list.files(pattern="csv")
datalist = lapply(files2, function(x){read.csv(file=x)})
golf_stats<-ldply(datalist, data.frame)
golf_stats<-golf_stats[,-1]
clnum<-which(colnames(golf_stats)=="playerName")
stopping<-clnum-1
starting<-clnum+1
ending<-ncol(golf_stats) 
golf_stats<-golf_stats[,c(clnum, 1:stopping, starting:ending)]



## return working directory back to what it was so no one gets confused
setwd("../")
write.csv(golf_stats, file="golf_stats.csv")