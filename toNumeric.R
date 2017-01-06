## convert feet' inches"
## this is the columns
golfstats<-read.csv("golf_stats.csv", stringsAsFactors = F)
theRow<-golfstats[1,]
feet<-grep("\"", theRow)
percent<-grep("%", theRow)

for(i in feet){
golfstats[,i]<-sapply(strsplit(as.character(golfstats[,i]),"'|\""), function(x){12*as.numeric(x[1]) + as.numeric(x[2])})
}
for(i in percent){
golfstats[,i]<-sapply(golfstats[,i], function(x){as.numeric(sub("%", "", x))})
golfstats[,i]<-golfstats[,i]/100
}

##remove player row
##players<-golfstats[,1:2]
##golfstats<-golfstats[,-2]

##remove the last two columns. There are so many NA's it's not significant and is blocking the lm function
golfstats$Web.com.Tour.Regular.Season.Money.List<-NULL
golfstats$Scoring.Average.Final.Round<-NULL
golfstats$Scoring.Average..Actual..1<-NULL
golfstats$Consecutive.Cuts<-NULL
golfstats$Money.Leaders<-NULL


##need all everything as numeric
for(i in 3:140){
  golfstats[,i]<-as.numeric(golfstats[,i])
}

rm(list=c("theRow", "feet", "i", "percent"))

## add a row from prior tournaments for comparison
