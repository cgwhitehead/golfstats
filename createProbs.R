
scores$score<-scores$par-scores$sc
scores<-group_by(scores, player, par, score)
new_scores<-summarise(scores, score_sum = sum(score), par_sum=sum(par))
new_scores$par_sum<-new_scores$par_sum/new_scores$par
new_scores$score_sum<-new_scores$score_sum/new_scores$score
new_scores[is.na(new_scores)]<-0
parTotals<-summarize(new_scores, sum(par_sum))
new_scores<-merge(new_scores, parTotals)
rm(parTotals)
new_scores$prob<-new_scores$par_sum/new_scores$`sum(par_sum)`

#merge to see player names (make sure players is loaded)
new_players<-merge(new_scores, players, by.x="player", by.y="pid")
new_players$X<-NULL
rm(new_scores)

##breakdown by par and stuff
test<-split(new_players, as.factor(new_players$par))
par3<-test[[1]]
par3<-subset(par3, score>=1)
par3<-group_by(par3, player, pn)
par3<-summarize(par3, sum(prob))

tourn_scores<-group_by(tbl_df(scores), tournament, player, round)
tourn_scores<-summarize(tourn_scores, sum(sc))
tourn_scores<-split(tourn_scores, as.factor(tourn_scores$tournament))
#tourn1<-tourn_scores[[1]]
#tourn1<-merge(tourn1, par4_good_prob)



