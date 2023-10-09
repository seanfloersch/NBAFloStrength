################################################################################
################################################################################
################################################################################
#Complete Yesterday
yestgameID <- getyestDate(x)
NBAResults <- comyest(yestgameID) %>% na.omit
write_csv(NBAResults, "/Users/seanfloersch/FloStrength/NBAFloStrength/ComNBAResults")
NBAModel <- comModDF(NBAResults)
write_csv(NBAModel, "/Users/seanfloersch/FloStrength/NBAFloStrength/NBAModel")
################################################################################
################################################################################
################################################################################
#Read In Pred Data
predteam23<- read.csv("~/FloStrength/NBAFloStrength/predteam23")
playpred23<- read.csv("~/FloStrength/NBAFloStrength/playpred23")
################################################################################
################################################################################
################################################################################
#Get data for year
player23 <- getNBAplayers(2023)
team23 <- getNBATeam(2023)
sched23 <- getNBAsched(team23,NBAResults)
lately23 <- getStreaks(team23, NBAResults)
PowerRanking <- getNBArank(player23, team23, sched23, predteam23,lately23)
################################################################################
################################################################################
################################################################################
#Predict today
todgameID <- getDate(x)
predToday <- predictGames(PowerRanking, todgameID)
write_csv(predToday, "/Users/seanfloersch/FloStrength/NBAFloStrength/yestmodpred")
################################################################################
