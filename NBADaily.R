##########################################################################################################
#################################################
# Completing yesterday's odds
#################################################
toddate <- getDate(x)
yestdate <- getyestDate(x)
todgameID <- getGameID(x)
yestgameID <- getyestGameID(x)
NBACompleteOddsPO <- CompleteTheOddsNBA(yestdate, yestgameID)

write_csv(NBACompleteOddsPO, "/Users/seanfloersch/FloStrength/NBAFloStrength/2022NBAOddsPlayOffs")
#################################################
# Add yesterday games to NHL Game Dataset
#################################################
NBAGames <- AddYestfun(yestdate)

write_csv(NBAGames, "/Users/seanfloersch/FloStrength/NBAFloStrength/NBAGames2122")
#################################################
# Team Stats
#################################################
yestteam <- teamyestfun(yestdate)
#FloPlayer <- NBAPlayerFun(x)
NBAMaster <- NBAMasterfun(NBAGames, FloPlayer)
#################################################
# Daily DataFrame 
#################################################
NBAOdds <- DailyNBA(toddate, NBAMaster, yestteam, todgameID)
NBACompleteOdds <- read.csv("~/FloStrength/NBAFloStrength/NBACompleteOdds2.0") 

linmod<- LinMod(NBACompleteOdds)
dfy <- WinAnalysis(NBACompleteOdds)
NBAOddsNew <- UpdDailyOdds(NBAOdds, linmod, dfy)
write_csv(NBAOddsNew, "/Users/seanfloersch/FloStrength/NBAFloStrength/NBAOddsPlayOffsyest")

linmod
mean(dfy$WinCorrect)
mean(dfy$BookCorrect)
mean(dfy$SpreadCorrect)
mean(NBACompleteOdds$FloCorrect)
mean(NBACompleteOdds$BookCorrect)
mean(NBACompleteOddsPO$FloSpreadCorrect)
############################################################################################################
