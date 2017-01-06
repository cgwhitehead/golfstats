for(i in 1:length(players[,2])){
  whereName<-grep(players[i,2], masters$Player)
  if(length(whereName)>0){
  players$R1[i]<-(masters$R1[whereName])
  }else{
    players$R1[i]<-NA
  }
}
for(i in 1:length(players[,2])){
  whereName<-grep(players[i,2], masters$Player)
  if(length(whereName)>0){
    players$R2[i]<-(masters$R2[whereName])
  }else{
    players$R2[i]<-NA
  }
}
for(i in 1:length(players[,2])){
  whereName<-grep(players[i,2], masters$Player)
  if(length(whereName)>0){
    players$R3[i]<-(masters$R3[whereName])
  }else{
    players$R3[i]<-NA
  }
}
for(i in 1:length(players[,2])){
  whereName<-grep(players[i,2], masters$Player)
  if(length(whereName)>0){
    players$R4[i]<-(masters$R4[whereName])
  }else{
    players$R4[i]<-NA
  }
}