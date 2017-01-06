library(plyr)
library(jsonlite)
library(rlist)
library(gtools)
library(splitstackshape)
library(dplyr)
#setwd("/Users/Rebel7/Dropbox/golf/golf")


  scorecard_stats<-read.csv("scorecards.csv")
  scorecard_stats$X.1<-NULL
  players<-read.csv('players.csv')
  parValues<-read.csv('parValues.csv')
  parValues$X.1<-NULL
  
  for(i in 1:length(scorecard_stats[,1])){
    whichRow<-which(parValues$holeNum==scorecard_stats[i,3]&parValues$tNum==scorecard_stats[i,9])
    if(length(whichRow)>0){
      scorecard_stats$par[i]<-(parValues$par[whichRow])
    }else{
      scorecard_stats$par[i]<-NA
    }
    
    
  }
  rm(i)
  rm(whichRow)
  scores<-na.omit(tbl_df(scorecard_stats))
  ##rm(list=c("scorecard_stats", "parValues"))
  golf_stats<-read.csv('golf_stats.csv')
  
  ##error, but works
  scores<-subset(scores, , -c(X,n,pDay,pTot))
  scores$score<-scores$par-scores$sc
  scores<-group_by(scores, player, tournament, round)
  scorePerRound<-summarize(scores, sum(sc))
  

  
  
  