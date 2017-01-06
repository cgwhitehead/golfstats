## retrieve scorecards for each player on pga tour
## requires jsonlite, rlist, gtools, splitstackshape, dplyr, plyr

library(plyr)
library(jsonlite)
library(rlist)
library(gtools)
library(splitstackshape)
library(dplyr)
library(RCurl)
## this is the url from 2016 masters, but has the data for all the player id's needed to form further urls
url<-"http://www.pgatour.com/data/r/current/schedule.json"

## create df of tourney info
tourneys<-fromJSON(url)
tourneys_set<-tourneys$tours$trns[[1]][5]
tourneys_names<-tourneys$tours$trns[[1]][16]$trnName$short
tourneys_date<-tourneys$tours$trns[[1]][17]$date
tourneys<-data.frame(num=as.character(tourneys_set$permNum), name=tourneys_names, date=as.POSIXct(tourneys_date$start))
rm(list=c("tourneys_names", "tourneys_set", "tourneys_date"))

## create a folder in which to put seperate files for each player (if it isn't there yet)
if(!dir.exists("tn_stats")) dir.create("tn_stats")
setwd("tn_stats")

##unpack the lists and put them into data frames
unpack<-function(tName, pName, listObject) {
  if(names(listObject[[1]])[1]!="version"){
    rounds<-listObject[[1]][[3]][[1]]
  }else{
    rounds<-listObject[[2]]$rnds
  }
  if(1 %in% rounds$n){
    round1<-as.data.frame(rounds$holes[[1]])
    round1$round<-rep(1, length(round1$n))
  }
  if(2 %in% rounds$n){
    round2<-as.data.frame(rounds$holes[[2]])
    round2$round<-rep(2, length(round2$n))
  }
  if(3 %in% rounds$n){
    round3<-as.data.frame(rounds$holes[[3]])
    round3$round<-rep(3, length(round3$n))
  }
  if(4 %in% rounds$n){
    round4<-as.data.frame(rounds$holes[[4]])
    round4$round<-rep(4, length(round4$n))
    
  }
  rm(rounds)
  roundObj<-objects(pattern = "round")
  data<-do.call(rbind.fill, mget(roundObj))
  data$shots<-NULL
  data$player<-rep(pName, length(data$n))
  data$tournament<-rep(tName, length(data$n))
  filename<-paste(tName,"_", pName,".csv", sep="")
  write.csv(data, filename)
}

## this is the function that will scrape the stats for each player on tour
retrieve_stats_2<-function (x) {
  for(i in 1:length(players$pid)){
  tryCatch({
  url2<-paste("http://www.pgatour.com/data/r/",x,"/scorecards/",players$pid[i],".json", sep="") 
    ##url2<-"http://www.pgatour.com/data/r/002/scorecards/23541.json"
  if(url.exists(url2)){
  con <- url(url2)
  temp<-stream_in(con)
  data<-unpack(x, players$pid[i], temp)
    }
  }, error=function(e){
    sink(file="log.txt", append=T)
    cat(as.character(e), append=T)
    cat(players$pid[i], append=T)
    cat(as.character(x), append=T)
    sink()
})
  }
  
}
## run through all the players and get their stats
## following loop commented out so i don't continuously scrape the web and get flagged
data_list<-list(c(""))
file.create("log.txt")
lapply(tourneys$num, retrieve_stats_2)

files3 <- list.files(pattern="csv")
datalist = lapply(files3, function(x){read.csv(file=x)})
scorecard_stats<-ldply(datalist, data.frame)
scorecard_stats$tournament<-sprintf("%03d", scorecard_stats$tournament)

## I'm going to need to get par from this

getPar<-function(x){
  
  url<-paste("http://www.pgatour.com/data/r/",x,"/coursestat.json", sep="")
  if(url.exists(url)){
  course<-fromJSON(url)
  course2<-course$courses$holes[[1]]
  course2$stats<-NULL
  course2$tNum<-rep(x, length(course2$holeNum))
  filename<-paste(x, "par.csv", sep = "")
  write.csv(course2, file = filename)
  }else{
    sink(file="log.txt", append=T)
    cat(x, append=T)
    cat("\n", append=T)
    sink()
  }
}
setwd("../")
if(!dir.exists("tn_par")) dir.create("tn_par")
setwd("tn_par")
lapply(tourneys$num, getPar)

files4 <- list.files(pattern="csv")
datalist = lapply(files4, function(x){read.csv(file=x)})
parValues<-ldply(datalist, data.frame)
parValues$tNum<-sprintf("%03d", parValues$tNum)
setwd("../")
## http://www.pgatour.com/data/r/475/coursestat.json is the url i used to format urls
for(i in 1:length(scorecard_stats[,1])){
  whichRow<-which(parValues$holeNum==scorecard_stats[i,3]&parValues$tNum==scorecard_stats[i,9])
  if(length(whichRow)>0){
    scorecard_stats$par[i]<-(parValues$par[whichRow])
  }else{
    scorecard_stats$par[i]<-NA
  }
  
  
}

scores<-na.omit(tbl_df(scorecard_stats))
rm(list=c("scorecard_stats", "parValues"))


