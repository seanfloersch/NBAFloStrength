library(rvest)
library(tidyverse)
library(optmatch)
library(stringr)
library(e1071)
library(rpart)
rm(list=ls(all=TRUE)) # clear local memory
graphics.off() # close figures
cat("\014") # clear console
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
## Complete yesterday
comyest <- function(yestgameID){
  df <- read.csv("~/FloStrength/NBAFloStrength/yestmodpred") %>%
    select(AwayTeam,HomeTeam, OU,Spread,gameID, AOdd, HOdd, PredWinner, BookWin)
  h <- read_html(str_c("https://www.espn.com/nba/scoreboard/_/date/",yestgameID))
  AwayTeam <- html_nodes(h, ".ScoreboardScoreCell__Item--away .db") %>% html_text
  HomeTeam <- html_nodes(h, ".ScoreboardScoreCell__Item--home .db") %>% html_text
  teams <- c("Timberwolves" ="MIN", "Grizzlies"="MEM", "Bucks"="MIL", "Hornets"="CHA", "Suns"="PHO", "Hawks"="ATL", "Jazz"="UTA", "Spurs"="SAS", "Nets"="BRK", "Nuggets"="DEN", "Lakers"="LAL", "Celtics"="BOS", "Bulls"="CHI", "Pacers"="IND", "Warriors"="GSW", "Kings"="SAC", "Heat"="MIA", "76ers"="PHI", "Rockets"="HOU", "Raptors"="TOR", "Pelicans"="NOP", "Wizards"="WAS", "Clippers"="LAC", "Mavericks"="DAL", "Cavaliers"="CLE", "Knicks"="NYK", "Trail Blazers"="POR", "Pistons"="DET", "Magic"="ORL", "Thunder"="OKC")
  AwayTeam<- as.character(teams[AwayTeam])
  HomeTeam<- as.character(teams[HomeTeam])
  AwayScore <- html_nodes(h, ".ScoreboardScoreCell__Item--away .ScoreCell_Score--scoreboard") %>% html_text %>% as.numeric
  HomeScore <- html_nodes(h, ".ScoreboardScoreCell__Item--home .ScoreCell_Score--scoreboard") %>% html_text%>% as.numeric
  yestdf <- data.frame(AwayTeam, HomeTeam, AwayScore, HomeScore) %>%
    mutate(gameID = str_c(AwayTeam, HomeTeam, yestgameID)) %>%
    mutate(PD = AwayScore - HomeScore) %>%
    mutate(WTeam = AwayTeam )
  ind <- which(yestdf$PD < 0)
  yestdf$WTeam[ind] = yestdf$HomeTeam[ind]
  yestdf <- yestdf %>% select(-AwayTeam, -HomeTeam)
  df <- left_join(df, yestdf, by = "gameID")
  dfcom<- read.csv("~/FloStrength/NBAFloStrength/ComNBAResults")
  df<- rbind(df, dfcom)
  return(df)
}
comModDF <- function(NBAResults){
  df <- read.csv("~/FloStrength/NBAFloStrength/yestmodpred")
  res <- NBAResults %>%
    select(gameID, AwayScore,HomeScore, PD, WTeam)
  df <- left_join(df, res, by = "gameID")%>%
    mutate(BookCorr = ifelse(BookWin == WTeam, 1,0)) %>%
    mutate(FloCorr = ifelse(PredWinner == WTeam, 1,0)) %>%
    mutate(AWin = ifelse(WTeam==AwayTeam,1,0))
  dfcom<- read.csv("~/FloStrength/NBAFloStrength/NBAModel")
  df<- rbind(df, dfcom)
  return(df)
}
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
## predict today
predtoday <- function(todgameID, predteam23){
  h <- read_html(str_c("https://www.cbssports.com/nba/scoreboard/",todgameID))
  AwayTeam <- html_nodes(h, "tr:nth-child(1) a+ a") %>% html_text
  HomeTeam <- html_nodes(h, "tr+ tr a+ a") %>% html_text
  OU <- html_nodes(h, ".in-progress-odds-away") %>% html_text %>% str_remove("o") %>% as.numeric()
  Spread <- html_nodes(h, ".in-progress-odds-home") %>% html_text
  ind <- which(Spread == "PK")
  Spread[ind] = 0
  Spread <- Spread %>% as.numeric()
  df <- data.frame(AwayTeam, HomeTeam, OU, Spread)
  h <- read_html("https://www.sportsline.com/nba/odds/money-line/")
  AwayTeam <- html_nodes(h, ".away-team .cfYQTQ") %>% html_text
  ovr <-length(df$AwayTeam)
  AwayTeam<- AwayTeam[1:ovr]
  HomeTeam <- html_nodes(h, ".home-team .cfYQTQ") %>% html_text
  HomeTeam<- HomeTeam[1:ovr]
  AOdd <- html_nodes(h, ".away-team .projected-score+ td .primary") %>% html_text%>% str_remove("\\+") %>% as.numeric()
  AOdd<- AOdd[1:ovr]
  HOdd <- html_nodes(h, ".home-team .projected-score+ td .primary") %>% html_text%>% str_remove("\\+") %>% as.numeric()
  HOdd<- HOdd[1:ovr]
  BookOdd <- data.frame(AwayTeam,HomeTeam,AOdd,HOdd)
  teams <- c("Timberwolves" ="MIN", "Grizzlies"="MEM", "Bucks"="MIL", "Hornets"="CHA", "Suns"="PHO", "Hawks"="ATL", "Jazz"="UTA", "Spurs"="SAS", "Nets"="BRK", "Nuggets"="DEN", "Lakers"="LAL", "Celtics"="BOS", "Bulls"="CHI", "Pacers"="IND", "Warriors"="GSW", "Kings"="SAC", "Heat"="MIA", "76ers"="PHI", "Rockets"="HOU", "Raptors"="TOR", "Pelicans"="NOP", "Wizards"="WAS", "Clippers"="LAC", "Mavericks"="DAL", "Cavaliers"="CLE", "Knicks"="NYK", "Trail Blazers"="POR", "Pistons"="DET", "Magic"="ORL", "Thunder"="OKC")
  df$AwayTeam<- as.character(teams[df$AwayTeam])
  df$HomeTeam<- as.character(teams[df$HomeTeam])
  BookOdd$AwayTeam<- as.character(teams[BookOdd$AwayTeam])
  BookOdd$HomeTeam<- as.character(teams[BookOdd$HomeTeam])
  df <- df %>% 
    mutate(gameID = str_c(AwayTeam, HomeTeam, todgameID))
  BookOdd <- BookOdd %>% 
    mutate(gameID = str_c(AwayTeam, HomeTeam, todgameID)) %>%
    select(-AwayTeam, -HomeTeam)
  df <- left_join(df, BookOdd, by = "gameID")
  ateam <- predteam23 %>%
    ungroup() %>%
    select("AwayTeam" = "Tm", "AwayFSPred" = "predWP")
  hteam <- predteam23 %>%
    ungroup() %>%
    select("HomeTeam" = "Tm", "HomeFSPred" = "predWP")
  df <- left_join(df, ateam, by = "AwayTeam")
  df <- left_join(df, hteam, by = "HomeTeam")
  df <- df %>% 
    mutate(FloDiff = AwayFSPred - HomeFSPred) %>%
    mutate(predPD = -3 + FloDiff*75) %>%
    mutate(predper = (AwayFSPred / (AwayFSPred + HomeFSPred))- .5) %>%
    mutate(predper = .4 + predper) %>%
    mutate(PredWinner = HomeTeam) %>%
    mutate(BookWin = HomeTeam)
  ind <- which(df$predPD > 0)
  df$PredWinner[ind] = df$AwayTeam[ind]
  ind <- which(df$Spread > 0)
  df$BookWin[ind] = df$AwayTeam[ind]
  return(df)
}
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
## get day ID
getDate <- function(x){
  x <- Sys.Date()
  x <- str_remove_all(x, "-")
  return(x)
}
getyestDate <- function(x){
  x <- Sys.Date() -1
  x <- str_remove_all(x, "-")
  return(x)
}
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
### get data
getNBAplayers <- function(year) {
  url <- str_c("https://www.basketball-reference.com/leagues/NBA_", year,"_totals.html")
  h <- read_html(url) 
  len <- length(html_nodes(h, ".full_table th") %>% html_text %>% as.numeric)
  stats <- html_nodes(h, ".full_table .center , .full_table .left , .full_table .right") %>% html_text
  df<- data.frame()
  for (i in c(1:len)) {
    for (j in c(1:30)){
      marker = j + (i -1)* 30
      df[i,j]<- stats[marker]
    }
  }
  
  for (i in c(4, 6:30)){
    df[i] <- as.numeric(unlist(df[i]))
  }
  df <- df %>%
    select(-V1)
  col <-html_nodes(h, ".center+ .poptip") %>% html_text
  colnames(df) <- col
  url <- str_c("https://www.basketball-reference.com/leagues/NBA_", year,"_advanced.html")
  h <- read_html(url) 
  stats <- html_nodes(h, ".full_table td") %>% html_text
  dfadv<- data.frame()
  for (i in c(1:len)) {
    for (j in c(1:28)){
      marker = j + (i -1)* 28
      dfadv[i,j]<- stats[marker]
    }
  }
  for (i in c(3, 5:28)){
    dfadv[i] <- as.numeric(unlist(dfadv[i]))
  }
  col <-html_nodes(h, ".center+ .poptip") %>% html_text
  colnames(dfadv) <- col
  dfadv <- dfadv[, c(1,4,7:18, 20:23,25:28)]
  dfcomp <- left_join(df, dfadv, by = c("Player", "Tm"))%>%
    mutate(yearID = year)
  dfcomp <- dfcomp  %>%
    mutate(MPG = MP / G) %>%
    filter(MPG > 12.9 & G > 0) %>%
    mutate(DWS = DWS / MP) %>%
    mutate(Pos = str_extract(Pos, "[A-Z]{1,2}")) %>%
    rename("FG2"="2P","FG2A"="2PA","FG3"="3P","FG3A"="3PA") %>%
    select(-`eFG%`, -VORP, -`AST%`,-`TOV%`, -`WS/48`, -FTr, -`3PAr`, -DWS, -OWS, -WS)%>%
    mutate(Scoring = ((FG2*FG2 / (FG2A +.00001)) + 1.5 *(FG3 * FG3 /(.00001 + FG3A)) + .5 * (FT * FT / (FTA + .00001))) / MP) %>%
    mutate(Passing = (AST/MP) - 2 * (TOV/MP)) %>%
    mutate(Rebounding = 2.5 * (ORB/MP) + (DRB/MP)) %>%
    mutate(Defense = ((STL) + (BLK) + (G/2) * DBPM)/MP) %>%
    group_by(yearID) %>%
    mutate(Scoring = (Scoring - mean(Scoring)) / sd(Scoring))%>%
    mutate(Passing = (Passing - mean(Passing)) / sd(Passing))%>%
    mutate(Rebounding = (Rebounding - mean(Rebounding)) / sd(Rebounding)) %>%
    ungroup() %>%  
    group_by(yearID, Pos) %>%
    mutate(Defense = (Defense - mean(Defense)) / sd(Defense))%>%
    ungroup() %>%  
    group_by(yearID) %>%
    mutate(Defense = (Defense - mean(Defense)) / sd(Defense)) %>%
    ungroup %>%
    mutate(FloStrength = .23* Scoring + .20*Defense + .15 * Passing + .03 * Rebounding) %>%
    group_by(yearID) %>%
    mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
    ungroup %>%
    mutate(Value = FloStrength * MP / 820) %>%
    arrange(-Value)
  return(dfcomp)
}
getNBATeam <- function(year){
  url <- str_c("https://www.basketball-reference.com/leagues/NBA_", year,".html")
  h <- read_html(url) 
  stats <- html_nodes(h, "#per_game-team tbody td") %>% html_text
  df<- data.frame()
  for (i in c(1:30)) {
    for (j in c(1:24)){
      marker = j + (i -1)* 24
      df[i,j]<- stats[marker]
    }
  }
  names4cols <- html_nodes(h, "#per_game-team thead .center+ .center") %>% html_text
  colnames(df)<- names4cols
  oppstats <- html_nodes(h, "#per_game-opponent tbody td") %>% html_text
  oppodf<- data.frame()
  for (i in c(1:30)) {
    for (j in c(1:24)){
      marker = j + (i -1)* 24
      oppodf[i,j]<- oppstats[marker]
    }
  }
  names4cols <- html_nodes(h, "#per_game-team thead .center+ .center") %>% html_text
  for (i in c(1:length(names4cols))){
    names4cols[i] = str_c("opp", names4cols[i])
    if (i == 1) {
      names4cols[i] <- "Team"
    }
  }
  colnames(oppodf)<- names4cols
  teamstats <- left_join(df, oppodf, by = "Team") %>%
    mutate(yearID = year)
  szn <- html_nodes(h, "#advanced-team tbody th+ .left , #advanced-team tbody .right:nth-child(5) , #advanced-team tbody .right:nth-child(4)") %>% html_text
  oppodf<- data.frame()
  for (i in c(1:30)) {
    for (j in c(1:3)){
      marker = j + (i -1)* 3
      oppodf[i,j]<- szn[marker]
    }
  }
  colnames(oppodf)<- c("Team", "W", "L")
  teamstats <- left_join(teamstats, oppodf, by = "Team")%>%
    mutate(Team = str_remove(Team, "\\*")) %>%
    na.omit
  for (i in c(2:50)) {
    teamstats[i] <- as.numeric(unlist(teamstats[i]))
  }
  teamstats <- teamstats %>%
    mutate(Wpct = W / G) %>%
    mutate(yearID = 2023) %>%
    mutate(MPG = MP / 5) %>%
    mutate(Playmaking = (AST - 2* TOV - oppBLK)- (oppAST - 2* oppTOV - BLK)) %>%
    mutate(Rebounding1 = (DRB -2* oppORB)) %>%
    mutate(Rebounding2 = (oppDRB -2* ORB)) %>%
    mutate(Rebounding = Rebounding1 / Rebounding2) %>%
    mutate(Defense = oppPTS) %>%
    mutate(Shooting = (`2P` * `2P%` + 1.5 *`3P`* `3P%`+ .5 *FT*`FT%`)/(`opp2P` * `opp2P%` + 1.5 *`opp3P`* `opp3P%`+ .5 *oppFT*`oppFT%`))%>%
    group_by(yearID) %>%
    mutate(Defense = (Defense - mean(Defense)) / sd(Defense)) %>%
    mutate(Shooting = (Shooting - mean(Shooting)) / sd(Shooting)) %>%
    mutate(Rebounding = (Rebounding - mean(Rebounding)) / sd(Rebounding)) %>%
    mutate(Playmaking = (Playmaking - mean(Playmaking)) / sd(Playmaking)) %>%
    ungroup() %>%
    mutate(TeamStrength = 0.499851+0.027328* Playmaking+0.006801*Rebounding-0.008344*Defense+0.123311*Shooting)%>%
    select(Team,G,MP,Wpct,Playmaking,Rebounding, Defense, Shooting, TeamStrength)
  return(teamstats)
}
getNBAsched <- function(team23,NBAResults){
  sched <- NBAResults %>% 
    select(AwayTeam,HomeTeam, PD, WTeam)
  teams <- c("Minnesota Timberwolves" ="MIN", "Memphis Grizzlies"="MEM", "Milwaukee Bucks"="MIL", "Charlotte Hornets"="CHA", "Phoenix Suns"="PHO", "Atlanta Hawks"="ATL", "Utah Jazz"="UTA", "San Antonio Spurs"="SAS", "Brooklyn Nets"="BRK", "Denver Nuggets"="DEN", "Los Angeles Lakers"="LAL", "Boston Celtics"="BOS", "Chicago Bulls"="CHI", "Indiana Pacers"="IND", "Golden State Warriors"="GSW", "Sacramento Kings"="SAC", "Miami Heat"="MIA", "Philadelphia 76ers"="PHI", "Houston Rockets"="HOU", "Toronto Raptors"="TOR", "New Orleans Pelicans"="NOP", "Washington Wizards"="WAS", "Los Angeles Clippers"="LAC", "Dallas Mavericks"="DAL", "Cleveland Cavaliers"="CLE", "New York Knicks"="NYK", "Portland Trail Blazers"="POR", "Detroit Pistons"="DET", "Orlando Magic"="ORL", "Oklahoma City Thunder"="OKC")
  team23$Team<- as.character(teams[team23$Team])
  Ateam <- team23 %>%
    select("AwayTeam"=Team, "AwayFS" =TeamStrength)
  Hteam <- team23 %>%
    select("HomeTeam"=Team,"HomeFS"= TeamStrength)
  sched <- left_join(sched, Ateam, by = "AwayTeam")
  sched<- left_join(sched, Hteam, by = "HomeTeam") %>%
    mutate(ASched = 0) %>%
    mutate(HSched = 0)
  ind <- which(sched$PD > 0)
  sched$ASched[ind] = sched$PD[ind] * sched$HomeFS[ind]
  sched$HSched[ind] = -1 * sched$PD[ind] * (1-sched$AwayFS[ind])
  ind <- which(sched$PD < 0)
  sched$HSched[ind] = -1 *sched$PD[ind] * sched$AwayFS[ind]
  sched$ASched[ind] =  sched$PD[ind] * (1-sched$HomeFS[ind])
  AFS <- sched %>%
    group_by(AwayTeam) %>%
    mutate(ASched = sum(ASched)) %>%
    mutate(AG = length(ASched)) %>%
    slice(1) %>%
    ungroup()%>%
    select("Team" = AwayTeam, ASched, AG)
  HFS <- sched %>%
    group_by(HomeTeam) %>%
    mutate(HSched = sum(HSched)) %>%
    mutate(HG = length(HSched)) %>%
    slice(1) %>%
    ungroup() %>%
    select("Team" = HomeTeam, HSched, HG)
  FSSc <- left_join(AFS, HFS, by = "Team") %>%
    mutate(SchedRating = (ASched + HSched)/ (AG+HG)) %>%
    mutate(yr = 1) %>%
    group_by(yr) %>%
    mutate(SchedRating = ((SchedRating-mean(SchedRating))/ sd(SchedRating))/6) %>%
    mutate(SchedRating = SchedRating + .5) %>%
    ungroup()%>%
    select(Team, SchedRating)
  return(FSSc)
}
getStreaks <- function(team23,NBAResults){
  sched <- NBAResults%>%
    mutate(date = str_extract(gameID, "\\d{1,8}$")) %>% 
    select(AwayTeam,HomeTeam, PD, WTeam,date) %>%
    ungroup() %>% 
    mutate(Date = NA)
  for (i in c(1:length(unique(sched$date)))) {
    i = i
    x <- unique(sched$date)[i]
    ind <- which(sched$date == x)
    sched$Date[ind] <- i
  }
  sched <- sched %>%
    select(-date)
  teams <- c("Minnesota Timberwolves" ="MIN", "Memphis Grizzlies"="MEM", "Milwaukee Bucks"="MIL", "Charlotte Hornets"="CHA", "Phoenix Suns"="PHO", "Atlanta Hawks"="ATL", "Utah Jazz"="UTA", "San Antonio Spurs"="SAS", "Brooklyn Nets"="BRK", "Denver Nuggets"="DEN", "Los Angeles Lakers"="LAL", "Boston Celtics"="BOS", "Chicago Bulls"="CHI", "Indiana Pacers"="IND", "Golden State Warriors"="GSW", "Sacramento Kings"="SAC", "Miami Heat"="MIA", "Philadelphia 76ers"="PHI", "Houston Rockets"="HOU", "Toronto Raptors"="TOR", "New Orleans Pelicans"="NOP", "Washington Wizards"="WAS", "Los Angeles Clippers"="LAC", "Dallas Mavericks"="DAL", "Cleveland Cavaliers"="CLE", "New York Knicks"="NYK", "Portland Trail Blazers"="POR", "Detroit Pistons"="DET", "Orlando Magic"="ORL", "Oklahoma City Thunder"="OKC")
  team23$Team<- as.character(teams[team23$Team])
  Ateam <- team23 %>%
    select("AwayTeam"=Team, "AwayFS" =TeamStrength)
  Hteam <- team23 %>%
    select("HomeTeam"=Team,"HomeFS"= TeamStrength)
  sched <- left_join(sched, Ateam, by = "AwayTeam")
  sched<- left_join(sched, Hteam, by = "HomeTeam") %>%
    mutate(ASched = 0) %>%
    mutate(HSched = 0)
  ind <- which(sched$PD > 0)
  sched$ASched[ind] = sched$PD[ind] * sched$HomeFS[ind]
  sched$HSched[ind] = -1 * sched$PD[ind] * (1-sched$AwayFS[ind])
  ind <- which(sched$PD < 0)
  sched$HSched[ind] = -1 *sched$PD[ind] * sched$AwayFS[ind]
  sched$ASched[ind] =  sched$PD[ind] * (1-sched$HomeFS[ind])
  away <- sched %>%
    select("Team" = AwayTeam,"Sched"= ASched, Date)
  home <- sched %>%
    select("Team" = HomeTeam, "Sched" =HSched,Date)
  sched3 <- rbind(away,home) %>%
    arrange(Date) %>%
    group_by(Team) %>%
    slice(1:3) %>%
    mutate(last3games = sum(Sched)/3) %>%
    slice(1) %>%
    ungroup()%>%
    mutate(yr = 1) %>%
    group_by(yr) %>%
    mutate(last3games = ((last3games-mean(last3games))/ sd(last3games))/6) %>%
    mutate(last3games = last3games + .5) %>%
    ungroup()%>%
    select(Team, last3games)
  sched5 <- rbind(away,home) %>%
    arrange(Date) %>%
    group_by(Team) %>%
    slice(1:5) %>%
    mutate(last5games = sum(Sched)/5) %>%
    slice(1) %>%
    ungroup()%>%
    mutate(yr = 1) %>%
    group_by(yr) %>%
    mutate(last5games = ((last5games-mean(last5games))/ sd(last5games))/6) %>%
    mutate(last5games = last5games + .5) %>%
    ungroup()%>%
    select(Team, last5games)
  sched7 <- rbind(away,home) %>%
    arrange(Date) %>%
    group_by(Team) %>%
    slice(1:7) %>%
    mutate(last7games = sum(Sched)/7) %>%
    slice(1) %>%
    ungroup()%>%
    mutate(yr = 1) %>%
    group_by(yr) %>%
    mutate(last7games = ((last7games-mean(last7games))/ sd(last7games))/6) %>%
    mutate(last7games = last7games + .5) %>%
    ungroup()%>%
    select(Team, last7games)
  sched10 <- rbind(away,home) %>%
    arrange(Date) %>%
    group_by(Team) %>%
    slice(1:10) %>%
    mutate(last10games = sum(Sched)/10) %>%
    slice(1) %>%
    ungroup()%>%
    mutate(yr = 1) %>%
    group_by(yr) %>%
    mutate(last10games = ((last10games-mean(last10games))/ sd(last10games))/6) %>%
    mutate(last10games = last10games + .5) %>%
    ungroup()%>%
    select(Team, last10games)
  lastx <- left_join(sched3,sched5, by = "Team")
  lastx <- left_join(lastx,sched7, by = "Team")
  lastx <- left_join(lastx,sched10, by = "Team")
  return(lastx)
}
getNBArank <- function(player23, team23, sched23, predteam23,lately23){
  getInjuries <- function(x){
    h <- read_html("https://www.espn.com/nba/injuries") 
    inj <- html_nodes(h, "#fittPageContainer .AnchorLink") %>% html_text
    status <-html_nodes(h, ".plain") %>% html_text
    len <- length(status)
    inj <- inj[1:len]
    injuries <- data.frame(inj, status) %>%
      filter(status == "Out")
    inj <- injuries$inj
    return(inj)
  }
  injured <- getInjuries(x)
  currentplay <- player23 %>%
    filter(!Player %in%injured) %>%
    group_by(Tm) %>%
    mutate(propmin = MPG / sum(MPG)) %>%
    mutate(wfs = propmin*FloStrength) %>%
    mutate(PlayerStrength = sum(wfs)) %>%
    slice(1) %>%
    ungroup()%>%
    mutate(yr = 1) %>%
    group_by(yr) %>%
    mutate(PlayerStrength = ((PlayerStrength-mean(PlayerStrength))/ sd(PlayerStrength))/6) %>%
    mutate(PlayerStrength = PlayerStrength + .5) %>%
    ungroup() %>%
    select("Team" = Tm, PlayerStrength)
  ind <- which(currentplay$Team == "CHO")
  currentplay$Team[ind] = "CHA"
  currteam <- team23 %>%
    select(Team, TeamStrength)
  teams <- c("Minnesota Timberwolves" ="MIN", "Memphis Grizzlies"="MEM", "Milwaukee Bucks"="MIL", "Charlotte Hornets"="CHA", "Phoenix Suns"="PHO", "Atlanta Hawks"="ATL", "Utah Jazz"="UTA", "San Antonio Spurs"="SAS", "Brooklyn Nets"="BRK", "Denver Nuggets"="DEN", "Los Angeles Lakers"="LAL", "Boston Celtics"="BOS", "Chicago Bulls"="CHI", "Indiana Pacers"="IND", "Golden State Warriors"="GSW", "Sacramento Kings"="SAC", "Miami Heat"="MIA", "Philadelphia 76ers"="PHI", "Houston Rockets"="HOU", "Toronto Raptors"="TOR", "New Orleans Pelicans"="NOP", "Washington Wizards"="WAS", "Los Angeles Clippers"="LAC", "Dallas Mavericks"="DAL", "Cleveland Cavaliers"="CLE", "New York Knicks"="NYK", "Portland Trail Blazers"="POR", "Detroit Pistons"="DET", "Orlando Magic"="ORL", "Oklahoma City Thunder"="OKC")
  currteam$Team<- as.character(teams[currteam$Team])
  currpred <- predteam23 %>%
    select("PredRating"= "predWP", "Team" = "Tm")
  prdf <- left_join(currteam, currentplay, by = "Team")
  prdf <- left_join(prdf, currpred, by = "Team")
  prdf <- left_join(prdf, lately23, by = "Team")
  prdf <- left_join(prdf, sched23, by = "Team") %>%
    mutate(Rank = (1*TeamStrength+1.2*PlayerStrength+.5*SchedRating+.2*last3games+.16*last5games+.14*last7games+.1*last10games+PredRating)/ 4.3) %>% 
    arrange(-Rank)
  return(prdf)
}
predictGames <- function(PowerRanking,todgameID){
  h <- read_html(str_c("https://www.cbssports.com/nba/scoreboard/",todgameID))
  AwayTeam <- html_nodes(h, "tr:nth-child(1) a+ a") %>% html_text
  HomeTeam <- html_nodes(h, "tr+ tr a+ a") %>% html_text
  OU <- html_nodes(h, ".in-progress-odds-away") %>% html_text %>% str_remove("o") %>% as.numeric()
  Spread <- html_nodes(h, ".in-progress-odds-home") %>% html_text
  ind <- which(Spread == "PK")
  Spread[ind] = 0
  Spread <- Spread %>% as.numeric()
  df <- data.frame(AwayTeam, HomeTeam, OU, Spread)
  h <- read_html("https://www.sportsline.com/nba/odds/money-line/")
  AwayTeam <- html_nodes(h, ".away-team .cfYQTQ") %>% html_text
  ovr <-length(df$AwayTeam)
  AwayTeam<- AwayTeam[1:ovr]
  HomeTeam <- html_nodes(h, ".home-team .cfYQTQ") %>% html_text
  HomeTeam<- HomeTeam[1:ovr]
  AOdd <- html_nodes(h, ".away-team .projected-score+ td .primary") %>% html_text%>% str_remove("\\+") %>% as.numeric()
  AOdd<- AOdd[1:ovr]
  HOdd <- html_nodes(h, ".home-team .projected-score+ td .primary") %>% html_text%>% str_remove("\\+") %>% as.numeric()
  HOdd<- HOdd[1:ovr]
  BookOdd <- data.frame(AwayTeam,HomeTeam,AOdd,HOdd)
  teams <- c("Timberwolves" ="MIN", "Grizzlies"="MEM", "Bucks"="MIL", "Hornets"="CHA", "Suns"="PHO", "Hawks"="ATL", "Jazz"="UTA", "Spurs"="SAS", "Nets"="BRK", "Nuggets"="DEN", "Lakers"="LAL", "Celtics"="BOS", "Bulls"="CHI", "Pacers"="IND", "Warriors"="GSW", "Kings"="SAC", "Heat"="MIA", "76ers"="PHI", "Rockets"="HOU", "Raptors"="TOR", "Pelicans"="NOP", "Wizards"="WAS", "Clippers"="LAC", "Mavericks"="DAL", "Cavaliers"="CLE", "Knicks"="NYK", "Trail Blazers"="POR", "Pistons"="DET", "Magic"="ORL", "Thunder"="OKC")
  df$AwayTeam<- as.character(teams[df$AwayTeam])
  df$HomeTeam<- as.character(teams[df$HomeTeam])
  BookOdd$AwayTeam<- as.character(teams[BookOdd$AwayTeam])
  BookOdd$HomeTeam<- as.character(teams[BookOdd$HomeTeam])
  df <- df %>% 
    mutate(gameID = str_c(AwayTeam, HomeTeam, todgameID))
  BookOdd <- BookOdd %>% 
    mutate(gameID = str_c(AwayTeam, HomeTeam, todgameID)) %>%
    select(-AwayTeam, -HomeTeam)
  predAct <- left_join(BookOdd, df, by ="gameID") %>% 
    mutate(BookWin = ifelse(Spread>0,AwayTeam, HomeTeam)) %>%
    rename("Team"="AwayTeam")
  predAct <- left_join(predAct,PowerRanking, by = "Team") %>%
    rename("AwayTeam" ="Team", "Team" = "HomeTeam", "AwayPlayer"= "PlayerStrength", "AwayTeamStr"= "TeamStrength", "AwaySched"= "SchedRating", "AwayPred"= "PredRating", "AwayRank"= "Rank", "Awaylast3games"= "last3games", "Awaylast5games"= "last5games", "Awaylast7games"= "last7games", "Awaylast10games"= "last10games")
  predAct <- left_join(predAct,PowerRanking, by = "Team") %>%
    rename("HomeTeam" ="Team",  "HomePlayer"= "PlayerStrength", "HomeTeamStr"= "TeamStrength", "HomeSched"= "SchedRating", "HomePred"= "PredRating", "HomeRank"= "Rank", "Homelast3games"= "last3games", "Homelast5games"= "last5games", "Homelast7games"= "last7games", "Homelast10games"= "last10games") %>%
    mutate(PlayerDiff = AwayPlayer - HomePlayer) %>%
    mutate(TeamDiff = AwayTeamStr- HomeTeamStr) %>%
    mutate(SchedDiff = AwaySched - HomeSched) %>%
    mutate(PredDiff = AwayPred - HomePred) %>%
    mutate(last3Diff = Awaylast3games - Homelast3games) %>%
    mutate(last5Diff = Awaylast5games - Homelast5games) %>%
    mutate(last7Diff = Awaylast7games - Homelast7games) %>%
    mutate(last10Diff = Awaylast10games - Homelast10games) %>%
    mutate(OvrDiff = AwayRank - HomeRank) %>%
    mutate(WinProb = .45 + OvrDiff) %>%
    mutate(PredWinner = ifelse(WinProb>.5,AwayTeam,HomeTeam)) %>%
    relocate(BookWin, PredWinner)
  modeldf<- read.csv("~/FloStrength/NBAFloStrength/NBAModel")
  logmod <- glm(AWin~PlayerDiff+TeamDiff+SchedDiff+PredDiff+last3Diff+last5Diff+last7Diff+last10Diff, data = modeldf,family = binomial)
  logpred1 <- predict(logmod, newdata = predAct, type = "response")
  linmod <- lm(PD~PlayerDiff+TeamDiff+SchedDiff+PredDiff+last3Diff+last5Diff+last7Diff+last10Diff, data = modeldf)
  linpred1 <- predict(linmod, newdata = predAct)
  logpred <- ifelse(logpred1>.5, 1,0)
  linpred <- ifelse(linpred1>0, 1,0)
  dtmod <- rpart(AWin~PlayerDiff+TeamDiff+SchedDiff+PredDiff+last3Diff+last5Diff+last7Diff+last10Diff, data = modeldf, method = 'class')
  svmmod = svm(formula = AWin~PlayerDiff+TeamDiff+SchedDiff+PredDiff+last3Diff+last5Diff+last7Diff+last10Diff, data = modeldf, type = 'C-classification', kernel = 'linear')
  svmpred <- as.vector(predict(svmmod, newdata = predAct, type = "response"))
  dtpred = as.numeric(as.vector(predict(dtmod, newdata = predAct, type = 'class')))
  predAct <- predAct %>%
    mutate(linpred = linpred) %>%
    mutate(predPD = linpred1) %>%
    mutate(logpred = logpred) %>%
    mutate(logprob = logpred1) %>%
    mutate(svmpred = svmpred) %>%
    mutate(dtpred = dtpred)
  return(predAct)
}
modupdate <- function(NBAModel){
  NBAModel <- NBAModel %>% na.omit()
  num <- length(NBAModel$gameID)
  a <- round(length(which(NBAModel$linpred == NBAModel$AWin)) /num, 3)
  b <- round(length(which(NBAModel$logpred == NBAModel$AWin))/num,3)
  c <- round(length(which(NBAModel$svmpred == NBAModel$AWin))/num,3)
  d<- round(length(which(NBAModel$dtpred == NBAModel$AWin))/num,3)
  e <- round(length(which(NBAModel$BookCorr == 1))/num,3)
  return(str_c("Linear: ",a,"   Log: ",b,"   SVM: ",c,"   DT: ",d,"   Book: ",e))
}
