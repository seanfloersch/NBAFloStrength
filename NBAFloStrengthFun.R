library(rvest)
library(tidyverse)
library(optmatch)
library(stringr)
rm(list=ls(all=TRUE)) # clear local memory
graphics.off() # close figures
cat("\014") # clear console
##########################################
#DATE AND ID FUNCTIONS
##########################################
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
getGameID <- function(x){
  x <- Sys.Date()
  x <- str_remove_all(x, "-")
  year <- x %>% str_extract("\\d{4}") %>% str_extract("\\d{2}$")
  month <-x %>% str_extract("\\d{6}") %>% str_extract("\\d{2}$")
  day <-x %>% str_extract("\\d{2}$")
  x <- str_c(month, day, year)
  return(x)
}
getyestGameID <- function(x){
  x <- Sys.Date() -1
  x <- str_remove_all(x, "-")
  year <- x %>% str_extract("\\d{4}") %>% str_extract("\\d{2}$")
  month <-x %>% str_extract("\\d{6}") %>% str_extract("\\d{2}$")
  day <-x %>% str_extract("\\d{2}$")
  x <- str_c(month, day, year)
  return(x)
}
weekschedfun <- function(url)  {
  h <- read_html(url) 
  date <- url %>% str_remove("https://www.espn.com/nba/scoreboard/_/date/")
  ATeam <- html_nodes(h, ".ScoreboardScoreCell__Item--away .db") %>% html_text
  HTeam <- html_nodes(h, ".ScoreboardScoreCell__Item--home .db") %>% html_text
  Scores <- html_nodes(h, ".ScoreCell_Score--scoreboard") %>% html_text
  AScore <- c()
  for (i in c(1:(length(Scores)/2))){
    y <- 2 *i -1
    AScore[i] <- Scores[y]
  }
  HScore <- c()
  for (i in c(1:(length(Scores)/2))){
    y <- 2 *i
    HScore[i] <- Scores[y]
  }
  if (length(HScore) != length(HTeam)){
    x <- length(HScore)
    HTeam <- HTeam[1:x]
    ATeam <- ATeam[1:x]
  }
  df <- data.frame(ATeam, HTeam, AScore, HScore) %>%
    mutate(AScore = as.numeric(AScore))%>%
    mutate(HScore = as.numeric(HScore)) %>%
    mutate(PD = AScore - HScore) %>%
    mutate(gameID = str_c(date, c(1:length(AScore)), by = ""))
  return(df)
}

###################################################
#ADD YESTERDAY'S DATA FUNCTIONS
###################################################
CompleteTheOddsNBA <- function(yestdate, yestgameID){
  
  NBAOddsyesterday <- read.csv("~/FloStrength/NBAFloStrength/NBAOddsPlayOffsyest")
  
  nbayesterday <- weekschedfun(str_c("https://www.espn.com/nba/scoreboard/_/date/", yestdate, sep = "") )
  
  HWinners <- nbayesterday %>% 
    filter(HScore >AScore) %>%
    select(ATeam, "WTeam" = HTeam, -AScore, "WScore"=HScore) %>%
    mutate(HTeam = WTeam)
  AWinners <- nbayesterday %>% 
    filter(AScore >HScore) %>%
    select(HTeam, "WTeam" = ATeam, -HScore, "WScore"=AScore)%>%
    mutate(ATeam = WTeam)
  Winners <- rbind(HWinners, AWinners)
  HLosers <- nbayesterday %>% 
    filter(HScore <AScore) %>%
    select(ATeam, "LTeam" = HTeam, "LScore"=HScore,-AScore, -PD)%>%
    mutate(HTeam = LTeam)
  ALosers <- nbayesterday %>% 
    filter(AScore <HScore) %>%
    select(HTeam, "LTeam" = ATeam, "LScore" = AScore, -HScore, -PD)%>%
    mutate(ATeam = LTeam)
  Losers <- rbind(HLosers, ALosers)
  
  NBAGames <- merge(Winners, Losers, by="ATeam") %>%
    select(WTeam, WScore, LTeam, LScore, ATeam, HTeam = "HTeam.x") %>%
    mutate(PD = WScore - LScore) 
  
  Teams <- c("Hawks" = "ATL","Nets" = "BKN","Celtics" = "BOS","Hornets" = "CHA","Bulls" = "CHI","Cavaliers" = "CLE","Mavericks" = "DAL","Nuggets" = "DEN","Pistons" = "DET","Warriors" = "GS","Rockets" = "HOU","Pacers" = "IND","Clippers" = "LAC","Lakers" = "LAL","Grizzlies" = "MEM","Heat" = "MIA","Bucks" = "MIL","Timberwolves" = "MIN","Pelicans" = "NO","Knicks" = "NY","Thunder" = "OKC","Magic" = "ORL","76ers" = "PHI","Suns" = "PHO","Trail Blazers" = "POR","Kings" = "SAC","Spurs" = "SA","Raptors" = "TOR","Jazz" = "UTA","Wizards" = "WAS")
  NBAGames$WTeam <-as.character(Teams[NBAGames$WTeam])
  NBAGames$LTeam <-as.character(Teams[NBAGames$LTeam])
  NBAGames$ATeam <-as.character(Teams[NBAGames$ATeam])
  NBAGames$HTeam <-as.character(Teams[NBAGames$HTeam])
  NBAGames <-NBAGames %>%
    mutate(gameID = str_c(yestgameID, ATeam, HTeam, sep = "")) %>%
    select(-WScore, -LScore, -ATeam, -HTeam, -LTeam)
  
  NBANewOdds <- merge(NBAOddsyesterday, NBAGames, by ="gameID")%>%
    mutate(FloCorrect = NA)%>%
    mutate(BookCorrect = NA)%>%
    mutate(FloSpreadCorrect = NA) %>%
    mutate(HomeWin = NA) %>%
    mutate(SpreadWinnings = NA) %>%
    mutate(MLWinnings = NA)
  ind <- which(NBANewOdds$FloWin == NBANewOdds$HTeam)
  NBANewOdds$FreshWin[ind] = -1 *NBANewOdds$FreshWin[ind] 
  NBACompleteOdds <- read.csv("~/FloStrength/NBAFloStrength/2022NBAOddsPlayOffs") 
  
  NBACompleteOdds <- rbind(NBACompleteOdds, NBANewOdds)
  NBACompleteOdds <- NBACompleteOdds %>%
    mutate(FloCorrect = NA)%>%
    mutate(BookCorrect = NA)%>%
    mutate(FloSpreadCorrect = NA)%>%
    mutate(HomeWin = NA)
  
  ind <- which(NBACompleteOdds$WTeam == NBACompleteOdds$FloWin)
  NBACompleteOdds$FloCorrect[ind] <- 1
  ind <- which(NBACompleteOdds$WTeam != NBACompleteOdds$FloWin)
  NBACompleteOdds$FloCorrect[ind] <- 0
  
  ind <- which(NBACompleteOdds$WTeam == NBACompleteOdds$BookWin)
  NBACompleteOdds$BookCorrect[ind] <- 1
  ind <- which(NBACompleteOdds$WTeam != NBACompleteOdds$BookWin)
  NBACompleteOdds$BookCorrect[ind] <- 0
  
  ind <- which((NBACompleteOdds$WTeam == NBACompleteOdds$FloWin) & (NBACompleteOdds$BookSpread < NBACompleteOdds$gr)& (NBACompleteOdds$BookSpread < NBACompleteOdds$PD))
  NBACompleteOdds$FloSpreadCorrect[ind] <- 1
  ind <- which((NBACompleteOdds$WTeam == NBACompleteOdds$FloWin) & (NBACompleteOdds$WTeam != NBACompleteOdds$BookWin))
  NBACompleteOdds$FloSpreadCorrect[ind] <- 1
  ind <- which((NBACompleteOdds$BookWin != NBACompleteOdds$FloWin)&(NBACompleteOdds$WTeam != NBACompleteOdds$FloWin) & (NBACompleteOdds$PD < NBACompleteOdds$BookSpread))
  NBACompleteOdds$FloSpreadCorrect[ind] <- 1
  ind <- which((NBACompleteOdds$PD < NBACompleteOdds$BookSpread)&(NBACompleteOdds$PD < NBACompleteOdds$gr))
  NBACompleteOdds$FloSpreadCorrect[ind] <- 1
  ind <- which((NBACompleteOdds$gr < NBACompleteOdds$BookSpread)&(NBACompleteOdds$PD < NBACompleteOdds$BookSpread))
  NBACompleteOdds$FloSpreadCorrect[ind] <- 1
  ind <- which((NBACompleteOdds$WTeam != NBACompleteOdds$BookWin)&(NBACompleteOdds$gr < NBACompleteOdds$BookSpread))
  NBACompleteOdds$FloSpreadCorrect[ind] <- 1
  ind <- which(is.na(NBACompleteOdds$FloSpreadCorrect))
  NBACompleteOdds$FloSpreadCorrect[ind] <- 0
  ind <- which((NBACompleteOdds$WTeam == NBACompleteOdds$FloWin)&(NBACompleteOdds$PD == -1 * NBACompleteOdds$FloSpread))
  NBACompleteOdds$FloSpreadCorrect[ind] <- NA
  
  ind <- which((NBACompleteOdds$FloSpreadCorrect == 0))
  NBACompleteOdds$SpreadWinnings[ind] <- -1 
  ind <- which((NBACompleteOdds$FloSpreadCorrect == 1))
  NBACompleteOdds$SpreadWinnings[ind] <- .91
  ind <- which((NBACompleteOdds$FloCorrect == 0))
  NBACompleteOdds$MLWinnings[ind] <- -1 
  
  ind <- which((NBACompleteOdds$HTeam == NBACompleteOdds$FloWin))
  NBACompleteOdds$HomeWin[ind] <- 1
  ind <- which((is.na(NBACompleteOdds$HomeWin)))
  NBACompleteOdds$HomeWin[ind] <- 0
  
  return(NBACompleteOdds)
}


AddYestfun <- function(yestdate){
  
  NBAGames2122<- read.csv("~/FloStrength/NBAFloStrength/NBAGames2122")
  
  nbayesterday <- weekschedfun(str_c("https://www.espn.com/nba/scoreboard/_/date/", yestdate, sep = "") )
  team1 <- nbayesterday$ATeam
  team2 <- nbayesterday$HTeam
  
  HWinners <- nbayesterday %>% 
    filter(HScore >AScore) %>%
    select(-ATeam, "WTeam" = HTeam, -AScore, "WScore"=HScore)
  AWinners <- nbayesterday %>% 
    filter(AScore >HScore) %>%
    select(-HTeam, "WTeam" = ATeam, -HScore, "WScore"=AScore)
  Winners <- rbind(HWinners, AWinners)
  HLosers <- nbayesterday %>% 
    filter(HScore <AScore) %>%
    select(-ATeam, "LTeam" = HTeam, "LScore"=HScore,-AScore, -PD)
  ALosers <- nbayesterday %>% 
    filter(AScore <HScore) %>%
    select(-HTeam, "LTeam" = ATeam, "LScore" = AScore, -HScore, -PD)
  Losers <- rbind(HLosers, ALosers)
  
  NBAGames <- merge(Winners, Losers, by="gameID") %>%
    select(WTeam, WScore, LTeam, LScore, gameID) %>%
    mutate(PD = WScore - LScore)
  
  Teams <- c("Hawks" = "ATL","Nets" = "BKN","Celtics" = "BOS","Hornets" = "CHA","Bulls" = "CHI","Cavaliers" = "CLE","Mavericks" = "DAL","Nuggets" = "DEN","Pistons" = "DET","Warriors" = "GS","Rockets" = "HOU","Pacers" = "IND","Clippers" = "LAC","Lakers" = "LAL","Grizzlies" = "MEM","Heat" = "MIA","Bucks" = "MIL","Timberwolves" = "MIN","Pelicans" = "NO","Knicks" = "NY","Thunder" = "OKC","Magic" = "ORL","76ers" = "PHI","Suns" = "PHO","Trail Blazers" = "POR","Kings" = "SAC","Spurs" = "SA","Raptors" = "TOR","Jazz" = "UTA","Wizards" = "WAS")
  NBAGames$WTeam <-as.character(Teams[NBAGames$WTeam])
  NBAGames$LTeam <-as.character(Teams[NBAGames$LTeam])
  
  NBAGames <-NBAGames %>%
    mutate(Win = 1) %>%
    mutate(GP = 1) %>%
    mutate(Loss = 0) 
  
  NBAGames<- rbind(NBAGames2122,NBAGames)
  return(NBAGames)
}

####################################################
#NBA MASTER DATAFRAME FUNCTIONS
####################################################


NBAPlayerFun <- function(x){
  urls <- c()
  teams <- c("ATL", "BKN", "BOS", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GS",  "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN",
             "NO",  "NY",  "OKC", "ORL", "PHI", "PHO", "POR", "SA",  "SAC", "TOR", "UTAH", "WAS")
  for(i in c(1:30)){
    team = teams[i]
    team <- tolower(team)
    urls[i] <- str_c("https://www.espn.com/nba/team/stats/_/name/",team, "/season/2022/seasontype/2",sep="")
  }
  #urls[16] <-"https://www.espn.com/nba/team/stats/_/name/MIA"
  playerNBAfun<- function(url){
    h <- read_html(url) 
    Name <- html_nodes(h, ".remove_capitalize+ .remove_capitalize .Table__TD .AnchorLink") %>% html_text 
    Pos <- html_nodes(h, ".ResponsiveWrapper+ .remove_capitalize .font10") %>% html_text %>% str_trim 
    GP <- html_nodes(h, ".ResponsiveWrapper+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(1) span") %>% html_text %>% as.numeric
    GP <- GP[1:length(Name)]
    GS <- html_nodes(h, ".ResponsiveWrapper+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(2) span") %>% html_text %>% as.numeric
    GS <- GS[1:length(Name)]
    MIN <- html_nodes(h, ".ResponsiveWrapper+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(3) span") %>% html_text %>% as.numeric
    MIN <- MIN[1:length(Name)]
    PTS <- html_nodes(h, ".ResponsiveWrapper+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(4) span") %>% html_text %>% as.numeric
    PTS <- PTS[1:length(Name)]
    OR <- html_nodes(h, ".ResponsiveWrapper+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(5) span") %>% html_text %>% as.numeric
    OR <- OR[1:length(Name)]
    DR <- html_nodes(h, ".ResponsiveWrapper+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(6) span") %>% html_text %>% as.numeric
    DR <- DR[1:length(Name)]
    REB <- html_nodes(h, ".ResponsiveWrapper+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(7) span") %>% html_text %>% as.numeric
    REB <- REB[1:length(Name)]
    AST <- html_nodes(h, ".ResponsiveWrapper+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(8) span") %>% html_text %>% as.numeric
    AST <- AST[1:length(Name)]
    STL <- html_nodes(h, ".ResponsiveWrapper+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(9) span") %>% html_text %>% as.numeric
    STL <- STL[1:length(Name)]
    BLK <- html_nodes(h, ".ResponsiveWrapper+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(10) span") %>% html_text %>% as.numeric
    BLK <- BLK[1:length(Name)]
    TO <- html_nodes(h, ".ResponsiveWrapper+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(11) span") %>% html_text %>% as.numeric
    TO <- TO[1:length(Name)]
    PF <- html_nodes(h, ".ResponsiveWrapper+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(12) span") %>% html_text %>% as.numeric
    PF <- PF[1:length(Name)]
    TPM <- html_nodes(h, ".remove_capitalize+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(4) span") %>% html_text %>% as.numeric
    TPM <- TPM[1:length(Name)]
    TPA <- html_nodes(h, ".remove_capitalize+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(5) span") %>% html_text %>% as.numeric
    TPA <- TPA[1:length(Name)]
    FTM <- html_nodes(h, ".remove_capitalize+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(7) span") %>% html_text %>% as.numeric
    FTM <- FTM[1:length(Name)]
    FTA <- html_nodes(h, ".remove_capitalize+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(8) span") %>% html_text %>% as.numeric
    FTA <- FTA[1:length(Name)]
    TWOM <- html_nodes(h, ".remove_capitalize+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(10) span") %>% html_text %>% as.numeric
    TWOM <- TWOM[1:length(Name)]
    TWOA <- html_nodes(h, ".remove_capitalize+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(11) span") %>% html_text %>% as.numeric
    TWOA <- TWOA[1:length(Name)]
    df <- data.frame(Name, Pos, GP, GS, MIN, PTS, OR, DR, REB, AST, STL, BLK, TO, PF, TWOM, TWOA, TPM, TPA, FTM, FTA)
    
    if (url =="https://www.espn.com/nba/team/stats/_/name/MIA"){
     url <-"https://www.espn.com/nba/team/stats/_/name/mia"
    }
    team <- url %>% str_remove("https://www.espn.com/nba/team/stats/_/name/") %>% str_extract("[a-z]{1,3}")
    team <- toupper(team)
    df <- df %>%
      mutate(Team = team)  %>%
      filter(GP > 8 & MIN > 19.9)
    return(df)
  }
  playerNBAfun1<- function(url){
    h <- read_html(url) 
    Name <- html_nodes(h, ".stats-header+ .remove_capitalize .Table__TD .AnchorLink") %>% html_text 
    Pos <- html_nodes(h, ".stats-header+ .remove_capitalize .font10") %>% html_text %>% str_trim 
    GP <- html_nodes(h, ".stats-header+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(1) span") %>% html_text %>% as.numeric
    GP <- GP[1:length(Name)]
    GS <- html_nodes(h, ".stats-header+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(2) span") %>% html_text %>% as.numeric
    GS <- GS[1:length(Name)]
    MIN <- html_nodes(h, ".stats-header+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(3) span") %>% html_text %>% as.numeric
    MIN <- MIN[1:length(Name)]
    PTS <- html_nodes(h, ".stats-header+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(4) span") %>% html_text %>% as.numeric
    PTS <- PTS[1:length(Name)]
    OR <- html_nodes(h, ".stats-header+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(5) span") %>% html_text %>% as.numeric
    OR <- OR[1:length(Name)]
    DR <- html_nodes(h, ".stats-header+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(6) span") %>% html_text %>% as.numeric
    DR <- DR[1:length(Name)]
    REB <- html_nodes(h, ".stats-header+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(7) span") %>% html_text %>% as.numeric
    REB <- REB[1:length(Name)]
    AST <- html_nodes(h, ".stats-header+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(8) span") %>% html_text %>% as.numeric
    AST <- AST[1:length(Name)]
    STL <- html_nodes(h, ".stats-header+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(9) span") %>% html_text %>% as.numeric
    STL <- STL[1:length(Name)]
    BLK <- html_nodes(h, ".stats-header+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(10) span") %>% html_text %>% as.numeric
    BLK <- BLK[1:length(Name)]
    TO <- html_nodes(h, ".stats-header+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(11) span") %>% html_text %>% as.numeric
    TO <- TO[1:length(Name)]
    PF <- html_nodes(h, ".stats-header+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(12) span") %>% html_text %>% as.numeric
    PF <- PF[1:length(Name)]
    TPM <- html_nodes(h, ".remove_capitalize+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(4) span") %>% html_text %>% as.numeric
    TPM <- TPM[1:length(Name)]
    TPA <- html_nodes(h, ".remove_capitalize+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(5) span") %>% html_text %>% as.numeric
    TPA <- TPA[1:length(Name)]
    FTM <- html_nodes(h, ".remove_capitalize+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(7) span") %>% html_text %>% as.numeric
    FTM <- FTM[1:length(Name)]
    FTA <- html_nodes(h, ".remove_capitalize+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(8) span") %>% html_text %>% as.numeric
    FTA <- FTA[1:length(Name)]
    TWOM <- html_nodes(h, ".remove_capitalize+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(10) span") %>% html_text %>% as.numeric
    TWOM <- TWOM[1:length(Name)]
    TWOA <- html_nodes(h, ".remove_capitalize+ .remove_capitalize .Table__Scroller .Table__TD:nth-child(11) span") %>% html_text %>% as.numeric
    TWOA <- TWOA[1:length(Name)]
    df <- data.frame(Name, Pos, GP, GS, MIN, PTS, OR, DR, REB, AST, STL, BLK, TO, PF, TWOM, TWOA, TPM, TPA, FTM, FTA)
    
    if (url =="https://www.espn.com/nba/team/stats/_/name/MIA"){
      url <-"https://www.espn.com/nba/team/stats/_/name/mia"
    }
    team <- url %>% str_remove("https://www.espn.com/nba/team/stats/_/name/") %>% str_extract("[a-z]{1,3}")
    team <- toupper(team)
    df <- df %>%
      mutate(Team = team)  %>%
      filter(GP > 8 & MIN > 19.9)
    return(df)
  }
  players <- map_df(.x= urls, .f=playerNBAfun)  

  ###defense from "https://github.com/fivethirtyeight/data/tree/master/nba-raptor"
  NBARaptor <- read.csv("~/Downloads/latest_RAPTOR_by_playermarch22.csv") %>% rename("Name" = "player_name") %>% filter(raptor_defense < 7)
  
  players <- left_join(players, NBARaptor, by = "Name")
  
  FloPlayer <- players %>%
    mutate(Defense = raptor_defense + 20 * (STL/MIN) + 10 * (BLK/MIN)) %>%
    filter(Name != "Frank Kaminsky" & Name != "Kris Dunn") %>%
    filter(Name != "Brook Lopez") %>%
    mutate(Defense = (Defense - mean(Defense)) / sd(Defense)) %>%
    mutate(Scoring = (TWOM*TWOM / (TWOA +.00001)) + 1.5 *(TPM * TPM /(.00001 + TPA)) + .5 * (FTM * FTM / (FTA + .00001))) %>%
    mutate(Rebounding = 2.5 * (OR/MIN) + (DR/MIN)) %>%
    mutate(Scoring = (Scoring - mean(Scoring)) / sd(Scoring)) %>%
    mutate(Passing = (AST/MIN) - 2 * (TO/MIN)) %>%
    mutate(Passing = (Passing - mean(Passing)) / sd(Passing)) %>%
    mutate(Rebounding = (Rebounding - mean(Rebounding)) / sd(Rebounding)) %>%
    select(Name, Pos, "Team" = Team, GP, MIN, Scoring, Rebounding, Passing, Defense) %>%
    mutate(FloStrength = 5.619 * Scoring +  3.454* Defense + 2.729 * Rebounding + 4.268 * Passing) %>%
    mutate(FloStrength = -1 *min(FloStrength) + FloStrength) 
  
  #######Traded
  ind <- which(FloPlayer$Name == "Buddy Hield")
  FloPlayer$Team[ind] <- "IND"
  ind <- which(FloPlayer$Name == "Caris LeVert")
  FloPlayer$Team[ind] <- "CLE"
  ind <- which(FloPlayer$Name == "Norman Powell")
  FloPlayer$Team[ind] <- "LAC"
  ind <- which(FloPlayer$Name == "Robert Covington")
  FloPlayer$Team[ind] <- "LAC"
  ind <- which(FloPlayer$Name == "Eric Bledsoe")
  FloPlayer$Team[ind] <- "POR"
  ind <- which(FloPlayer$Name == "Justise Winslow")
  FloPlayer$Team[ind] <- "POR"
  ind <- which(FloPlayer$Name == "Keon Johnson")
  FloPlayer$Team[ind] <- "POR"
  ind <- which(FloPlayer$Name == "Tyrese Haliburton")
  FloPlayer$Team[ind] <- "IND"
  ind <- which(FloPlayer$Name == "Tristan Thompson")
  FloPlayer$Team[ind] <- "IND"
  ind <- which(FloPlayer$Name == "Domantas Sabonis")
  FloPlayer$Team[ind] <- "SAC"
  ind <- which(FloPlayer$Name == "Justin Holiday")
  FloPlayer$Team[ind] <- "SAC"
  ind <- which(FloPlayer$Name == "Jeremy Lamb")
  FloPlayer$Team[ind] <- "SAC"
  ind <- which(FloPlayer$Name == "CJ McCollum")
  FloPlayer$Team[ind] <- "NO"
  ind <- which(FloPlayer$Name == "Larry Nance Jr.")
  FloPlayer$Team[ind] <- "NO"
  ind <- which(FloPlayer$Name == "Tony Snell")
  FloPlayer$Team[ind] <- "NO"
  ind <- which(FloPlayer$Name == "Josh Hart")
  FloPlayer$Team[ind] <- "POR"
  ind <- which(FloPlayer$Name == "Tomas Satoransky")
  FloPlayer$Team[ind] <- "SA"
  ind <- which(FloPlayer$Name == "Nickeil ALexander-Walker")
  FloPlayer$Team[ind] <- "UTA"
  ind <- which(FloPlayer$Name == "Didi Louzada")
  FloPlayer$Team[ind] <- "POR"
  ind <- which(FloPlayer$Name == "Cam Reddish")
  FloPlayer$Team[ind] <- "NY"
  ind <- which(FloPlayer$Name == "KZ Okpala")
  FloPlayer$Team[ind] <- "OKC"
  ind <- which(FloPlayer$Name == "Juancho Hernangomez")
  FloPlayer$Team[ind] <- "UTA"
  ind <- which(FloPlayer$Name == "Elijah Hughes")
  FloPlayer$Team[ind] <- "POR"
  ind <- which(FloPlayer$Name == "Bol Bol")
  FloPlayer$Team[ind] <- "ORL"
  ind <- which(FloPlayer$Name == "PJ Dozier")
  FloPlayer$Team[ind] <- "ORL"
  ind <- which(FloPlayer$Name == "Goran Dragic")
  FloPlayer$Team[ind] <- "SA"
  ind <- which(FloPlayer$Name == "Thaddeus Young")
  FloPlayer$Team[ind] <- "TOR"
  ind <- which(FloPlayer$Name == "Drew Eubanks")
  FloPlayer$Team[ind] <- "TOR"
  ind <- which(FloPlayer$Name == "Aaron Holiday")
  FloPlayer$Team[ind] <- "PHO"
  ind <- which(FloPlayer$Name == "Torrey Craig")
  FloPlayer$Team[ind] <- "PHO"
  ind <- which(FloPlayer$Name == "Jalen Smith")
  FloPlayer$Team[ind] <- "IND"
  ind <- which(FloPlayer$Name == "Montrezl Harrell")
  FloPlayer$Team[ind] <- "CHA"
  ind <- which(FloPlayer$Name == "Vernon Carey Jr.")
  FloPlayer$Team[ind] <- "WAS"
  ind <- which(FloPlayer$Name == "Ish Smith")
  FloPlayer$Team[ind] <- "WAS"
  ind <- which(FloPlayer$Name == "Serge Ibaka")
  FloPlayer$Team[ind] <- "MIL"
  ind <- which(FloPlayer$Name == "Rodney Hood")
  FloPlayer$Team[ind] <- "LAC"
  ind <- which(FloPlayer$Name == "Semi Ojelye")
  FloPlayer$Team[ind] <- "LAC"
  ind <- which(FloPlayer$Name == "Donte DiVincenzo")
  FloPlayer$Team[ind] <- "SAC"
  ind <- which(FloPlayer$Name == "Josh Jackson")
  FloPlayer$Team[ind] <- "SAC"
  ind <- which(FloPlayer$Name == "Trey Lyles")
  FloPlayer$Team[ind] <- "SAC"
  ind <- which(FloPlayer$Name == "Marvin Bagley III")
  FloPlayer$Team[ind] <- "DET"
  ind <- which(FloPlayer$Name == "Daniel Theis")
  FloPlayer$Team[ind] <- "BOS"
  ind <- which(FloPlayer$Name == "Dennis Schroder")
  FloPlayer$Team[ind] <- "HOU"
  ind <- which(FloPlayer$Name == "Enes Freedom")
  FloPlayer$Team[ind] <- "HOU"
  ind <- which(FloPlayer$Name == "Bruno Fernando")
  FloPlayer$Team[ind] <- "HOU"
  ind <- which(FloPlayer$Name == "Derrick White")
  FloPlayer$Team[ind] <- "BOS"
  ind <- which(FloPlayer$Name == "Josh Richardson")
  FloPlayer$Team[ind] <- "SA"
  ind <- which(FloPlayer$Name == "Romeo Langford")
  FloPlayer$Team[ind] <- "SA"
  ind <- which(FloPlayer$Name == "Kristaps Porzingis")
  FloPlayer$Team[ind] <- "WAS"
  ind <- which(FloPlayer$Name == "Spencer Dinwiddie")
  FloPlayer$Team[ind] <- "DAL"
  ind <- which(FloPlayer$Name == "Davis Bertans")
  FloPlayer$Team[ind] <- "DAL"
  ind <- which(FloPlayer$Name == "Seth Curry")
  FloPlayer$Team[ind] <- "BKN"
  ind <- which(FloPlayer$Name == "James Harden")
  FloPlayer$Team[ind] <- "PHI"
  ind <- which(FloPlayer$Name == "Ben Simmons")
  FloPlayer$Team[ind] <- "BKN"
  ind <- which(FloPlayer$Name == "Andre Drummond")
  FloPlayer$Team[ind] <- "BKN"
  
  
  FloPlayer <- FloPlayer %>% distinct(Name, .keep_all = TRUE)
  
  return(FloPlayer)
}

NBAMasterfun <- function(NBAGames, FloPlayer){
  
  NBAGames <- NBAGames
  FloPlayer <- FloPlayer
  
  ### Team Stats
  h <- read_html("https://www.espn.com/nba/stats/team/_/season/2022/seasontype/2/table/offensive/sort/avgPoints/dir/desc") 
  Team <- html_nodes(h, ".TeamLink__Logo+ .AnchorLink") %>% html_text
  GP <- html_nodes(h, ".Table__Scroller .Table__TD:nth-child(1) div") %>% html_text%>% as.numeric
  PTS <- html_nodes(h, ".Table__Scroller .Table__TD:nth-child(2) div") %>% html_text %>% as.numeric
  FGM <- html_nodes(h, ".Table__TD:nth-child(3) div") %>% html_text %>% str_remove(",")%>% as.numeric
  FGA <- html_nodes(h, ".Table__TD:nth-child(4) div") %>% html_text %>% str_remove(",")%>% as.numeric
  TPM <- html_nodes(h, ".Table__TD:nth-child(6) div") %>% html_text %>% str_remove(",")%>% as.numeric
  TPA <- html_nodes(h, ".Table__TD:nth-child(7) div") %>% html_text %>% str_remove(",")%>% as.numeric
  FTM <- html_nodes(h, ".Table__TD:nth-child(9) div") %>% html_text %>% str_remove(",")%>% as.numeric
  FTA <- html_nodes(h, ".Table__TD:nth-child(10) div") %>% html_text %>% str_remove(",")%>% as.numeric
  OR <- html_nodes(h, ".Table__TD:nth-child(12) div") %>% html_text %>% str_remove(",")%>% as.numeric
  DR <- html_nodes(h, ".Table__TD:nth-child(13) div") %>% html_text %>% str_remove(",")%>% as.numeric
  AST <- html_nodes(h, ".Table__TD:nth-child(15) div") %>% html_text %>% str_remove(",")%>% as.numeric
  STL <- html_nodes(h, ".Table__TD:nth-child(16) div") %>% html_text %>% str_remove(",")%>% as.numeric
  BLK <- html_nodes(h, ".Table__TD:nth-child(17) div") %>% html_text %>% str_remove(",")%>% as.numeric
  TO <- html_nodes(h, ".Table__TD:nth-child(18) div") %>% html_text %>% str_remove(",")%>% as.numeric
  PF <- html_nodes(h, ".Table__TD:nth-child(19) div") %>% html_text %>% str_remove(",")%>% as.numeric
  
  dft <- data.frame(Team, GP, PTS, FGM, FGA, TPM, TPA, FTM, FTA, OR, DR, AST, STL, BLK, TO, PF)
  
  ### Opponent Stats
  h <- read_html("https://www.espn.com/nba/stats/team/_/view/opponent/season/2022/seasontype/2/table/offensive/sort/avgPoints/dir/asc") 
  Team <- html_nodes(h, ".TeamLink__Logo+ .AnchorLink") %>% html_text
  GP <- html_nodes(h, ".Table__Scroller .Table__TD:nth-child(1) div") %>% html_text%>% as.numeric
  OPTS <- html_nodes(h, ".Table__Scroller .Table__TD:nth-child(2) div") %>% html_text %>% as.numeric
  OFGM <- html_nodes(h, ".Table__TD:nth-child(3) div") %>% html_text %>% str_remove(",")%>% as.numeric
  OFGA <- html_nodes(h, ".Table__TD:nth-child(4) div") %>% html_text %>% str_remove(",")%>% as.numeric
  OTPM <- html_nodes(h, ".Table__TD:nth-child(6) div") %>% html_text %>% str_remove(",")%>% as.numeric
  OTPA <- html_nodes(h, ".Table__TD:nth-child(7) div") %>% html_text %>% str_remove(",")%>% as.numeric
  OFTM <- html_nodes(h, ".Table__TD:nth-child(9) div") %>% html_text %>% str_remove(",")%>% as.numeric
  OFTA <- html_nodes(h, ".Table__TD:nth-child(10) div") %>% html_text %>% str_remove(",")%>% as.numeric
  OOR <- html_nodes(h, ".Table__TD:nth-child(12) div") %>% html_text %>% str_remove(",")%>% as.numeric
  ODR <- html_nodes(h, ".Table__TD:nth-child(13) div") %>% html_text %>% str_remove(",")%>% as.numeric
  OAST <- html_nodes(h, ".Table__TD:nth-child(15) div") %>% html_text %>% str_remove(",")%>% as.numeric
  OSTL <- html_nodes(h, ".Table__TD:nth-child(16) div") %>% html_text %>% str_remove(",")%>% as.numeric
  OBLK <- html_nodes(h, ".Table__TD:nth-child(17) div") %>% html_text %>% str_remove(",")%>% as.numeric
  OTO <- html_nodes(h, ".Table__TD:nth-child(18) div") %>% html_text %>% str_remove(",")%>% as.numeric
  OPF <- html_nodes(h, ".Table__TD:nth-child(19) div") %>% html_text %>% str_remove(",")%>% as.numeric
  
  dfo <- data.frame(Team, OPTS, OFGM, OFGA, OTPM, OTPA, OFTM, OFTA, OOR, ODR, OAST, OSTL, OBLK, OTO, OPF)
  
  TeamStats <- merge(dft, dfo, by = "Team")
  Teams <- c("Atlanta Hawks" = "ATL","Brooklyn Nets" = "BKN","Boston Celtics" = "BOS","Charlotte Hornets" = "CHA","Chicago Bulls" = "CHI","Cleveland Cavaliers" = "CLE","Dallas Mavericks" = "DAL","Denver Nuggets" = "DEN","Detroit Pistons" = "DET","Golden State Warriors" = "GS","Houston Rockets" = "HOU","Indiana Pacers" = "IND","LA Clippers" = "LAC","Los Angeles Lakers" = "LAL","Memphis Grizzlies" = "MEM","Miami Heat" = "MIA","Milwaukee Bucks" = "MIL","Minnesota Timberwolves" = "MIN","New Orleans Pelicans" = "NO","New York Knicks" = "NY","Oklahoma City Thunder" = "OKC","Orlando Magic" = "ORL","Philadelphia 76ers" = "PHI","Phoenix Suns" = "PHO","Portland Trail Blazers" = "POR","Sacramento Kings" = "SAC","San Antonio Spurs" = "SA","Toronto Raptors" = "TOR","Utah Jazz" = "UTA","Washington Wizards" = "WAS")
  TeamStats$Team <-as.character(Teams[TeamStats$Team])
  standw <- NBAGames %>%
    group_by(WTeam) %>%
    mutate(gp = sum(GP)) %>%
    mutate(wins = sum(Win)) %>%
    slice(1)%>%
    rename("Team" = "WTeam") %>%
    select(Team, gp, wins)
  standl <- NBAGames %>%
    group_by(LTeam) %>%
    mutate(gp = sum(GP)) %>%
    mutate(loss = sum(Loss)) %>%
    slice(1) %>%
    rename("Team" = "LTeam") %>%
    select(Team, gp, loss)
  
  standings <- merge(standw, standl, by ="Team" ) %>%
    mutate(wpct = wins / (gp.x + gp.y)) %>%
    select(Team, wpct)
  
  TeamStats <- merge(TeamStats, standings, by = "Team")
  
  NBAMaster <- TeamStats %>%
    mutate(Pts = PTS / OPTS) %>%
    mutate(Two = FGM - TPM) %>%
    mutate(TwoA = FGA - TPA) %>%
    mutate(EFG = (Two*Two / TwoA) + 1.5 *(TPM * TPM /TPA) + .5 * (FTM * FTM / FTA))%>%
    mutate(Two = OFGM - OTPM) %>%
    mutate(TwoA = OFGA - OTPA) %>%
    mutate(OEFG = (Two*Two / TwoA) + 1.5 *(OTPM * OTPM /OTPA) + .5 * (OFTM * OFTM / OFTA)) %>%
    mutate(Scoring = EFG) %>%
    mutate(Passing = (AST -TO) / (OAST - OTO)) %>%
    mutate(Rebounding = (DR + 2*OR) / (ODR + 2*OOR)) %>%
    mutate(Defense = (BLK + 1.2 * STL) / (OBLK + 1.2 *OSTL)) %>%
    mutate(Scoring = (Scoring - mean(Scoring)) / sd(Scoring)) %>%
    mutate(Passing = (Passing - mean(Passing)) / sd(Passing)) %>%
    mutate(Rebounding = (Rebounding - mean(Rebounding)) / sd(Rebounding)) %>%
    mutate(Defense = (Defense - mean(Defense)) / sd(Defense))
  NBAMaster <- NBAMaster %>%
    mutate(FloStrength = .5 + .05619 * Scoring + .04268* Passing + 0.03454 *Defense + 0.02729 * Rebounding)
  
  ######injured
  h <- read_html("https://www.espn.com/nba/injuries") 
  Injured <- html_nodes(h, ".Table__TD .AnchorLink") %>% html_text 
  x <- setdiff(FloPlayer$Name, Injured)
  FloPlayer1 <- FloPlayer %>%
    filter(Name %in% x)
  
  FloPlay <- FloPlayer1 %>%
    mutate(Ratio = MIN / 48) %>%
    mutate(FloStrengthProp = Ratio *FloStrength) %>%
    group_by(Team) %>%
    arrange(-FloStrengthProp) %>%
    slice(1:8) %>%
    mutate(FloStrength.player = sum(FloStrengthProp))%>%
    slice(1) %>%
    ungroup %>%
    mutate(FloStrength.player = (FloStrength.player - mean(FloStrength.player)) / sd(FloStrength.player)) %>%
    mutate(FloStrength.player = -1 * min(FloStrength.player) + FloStrength.player) %>%
    mutate(FloStrength.player = (1 + FloStrength.player) / (2 + mean(FloStrength.player))) %>%
    mutate(FloStrength.player = FloStrength.player / max(FloStrength.player)) %>%
    mutate(FloStrength.player = .5 - mean(FloStrength.player) + FloStrength.player) %>%
    select(Team, FloStrength.player)
  
  OVRPlay <- FloPlayer %>%
    mutate(Ratio = MIN / 48) %>%
    mutate(FloStrengthProp = Ratio *FloStrength) %>%
    group_by(Team) %>%
    mutate(FloStrength.player = mean(FloStrengthProp))%>%
    slice(1) %>%
    ungroup %>%
    mutate(FloStrength.player = (FloStrength.player - mean(FloStrength.player)) / sd(FloStrength.player)) %>%
    mutate(FloStrength.player = -1 * min(FloStrength.player) + FloStrength.player) %>%
    mutate(FloStrength.player = (1 + FloStrength.player) / (2 + mean(FloStrength.player))) %>%
    mutate(FloStrength.player = FloStrength.player / max(FloStrength.player))%>%
    mutate(FloStrength.player = .5 - mean(FloStrength.player) + FloStrength.player) %>%
    select(Team, FloStrength.player)
  NBAMaster1 <-NBAMaster
  NBAMaster <- merge(NBAMaster, FloPlay, by = "Team")
  NBAMaster <- NBAMaster %>% 
    mutate(FloStrengthTeam = FloStrength) %>%
    mutate(FloStrength = (2*FloStrength.player + FloStrength)/3)%>%
    mutate(FloStrength = (.5 - mean(FloStrength)) + FloStrength) %>%
    select(Team, GP, FloStrength, FloStrengthTeam, FloStrength.player, PTS, OPTS)
  
  ##### Massey Method
  MasseyFS <- merge(NBAMaster1, OVRPlay, by = "Team") %>%
    select(Team, FloStrength, FloStrength.player)%>% 
    mutate(FloStrength = (FloStrength.player + FloStrength)/2)%>%
    mutate(FloStrength = (.5 - mean(FloStrength)) + FloStrength) 
  
  findTeam <- function(teams, t){
    for(i in c(1:30)){
      if(t == teams[i]){
        return(i)
      }
    }
    print(t)
    print("not found")
  }
  
  total <- c(rep(0,30))
  colley <- matrix(0, ncol=30, nrow = 30)
  teams <- MasseyFS$Team
  a <- length(NBAGames$WScore)
  b = a-120
  c = a -360
  for(i in c(c:b-1)){
    x = findTeam(teams, NBAGames[i,1])
    y = findTeam(teams, NBAGames[i,3])
    colley[x,y] = colley[x,y] + MasseyFS$FloStrength[y] * 1.2 * NBAGames[i, 5]
    colley[y,x] = colley[y,x] - (1-MasseyFS$FloStrength[x]) *1.2*NBAGames[i,5]
    total[x] = total[x] + NBAGames[i, 5]
    total[y] = total[y] - NBAGames[i,5]
  }
  for(i in c(1:30)){
    colley[i,i] = 0
  }
  
  for(i in c(b:a)){
    x = findTeam(teams, NBAGames[i,1])
    y = findTeam(teams, NBAGames[i,3])
    colley[x,y] = colley[x,y] + MasseyFS$FloStrength[y] * 5 * NBAGames[i, 5]
    colley[y,x] = colley[y,x] - (1-MasseyFS$FloStrength[x]) *5*NBAGames[i,5]
    total[x] = total[x] + NBAGames[i, 5]
    total[y] = total[y] - NBAGames[i,5]
  }
  for(i in c(1:30)){
    colley[i,i] = 0
  }
  
  sumss <- c(rep(1,30))
  x <- colley %*% sumss
  x <- t(x) %>% as.vector
  NBAMaster <- NBAMaster %>%
    mutate(fixFlo = x) %>%
    mutate(fixFlo = (fixFlo - mean(fixFlo)) / sd(fixFlo)) %>%
    mutate(fixFlo = -1 * min(fixFlo) + fixFlo) %>%
    mutate(fixFlo = (1 + fixFlo) / (2 +  mean(fixFlo))) %>%
    mutate(fixFlo = fixFlo / max(fixFlo))%>%
    mutate(fixFlo = (.5 - mean(fixFlo)) + fixFlo) %>%
    mutate(CurrentFS = (7* fixFlo + 21*  FloStrengthTeam + 9 * FloStrength.player) / 37)%>%
    mutate(CurrentFS = -1 * min(CurrentFS) + CurrentFS)%>%
    mutate(CurrentFS = (1 + CurrentFS) / (2 +  mean(CurrentFS)))%>%
    mutate(CurrentFS = (.5 - mean(CurrentFS)) + CurrentFS) 
  return(NBAMaster)
}
#####################################################
#DAILY NBA ODDS FUNCTION
#####################################################
teamyestfun <- function(yestdate){
  
  weekschedfun <- function(url)  {
    h <- read_html(url) 
    date <- url %>% str_remove("https://www.espn.com/nba/scoreboard/_/date/")
    ATeam <- html_nodes(h, ".ScoreboardScoreCell__Item--away .db") %>% html_text
    HTeam <- html_nodes(h, ".ScoreboardScoreCell__Item--home .db") %>% html_text
    Scores <- html_nodes(h, ".ScoreCell_Score--scoreboard") %>% html_text
    AScore <- c()
    for (i in c(1:(length(Scores)/2))){
      y <- 2 *i -1
      AScore[i] <- Scores[y]
    }
    HScore <- c()
    for (i in c(1:(length(Scores)/2))){
      y <- 2 *i
      HScore[i] <- Scores[y]
    }
    if (length(HScore) != length(HTeam)){
      x <- length(HScore)
      HTeam <- HTeam[1:x]
      ATeam <- ATeam[1:x]
    }
    df <- data.frame(ATeam, HTeam, AScore, HScore) %>%
      mutate(AScore = as.numeric(AScore))%>%
      mutate(HScore = as.numeric(HScore)) %>%
      mutate(PD = AScore - HScore) %>%
      mutate(gameID = str_c(date, c(1:length(AScore)), by = ""))
    return(df)
  }
  nbayesterday <- weekschedfun(str_c("https://www.espn.com/nba/scoreboard/_/date/", yestdate, sep = "") )
  team1 <- nbayesterday$ATeam
  team2 <- nbayesterday$HTeam
  yestteam <- c(team1, team2)
  Teams <- c("Hawks" = "ATL","Nets" = "BKN","Celtics" = "BOS","Hornets" = "CHA","Bulls" = "CHI","Cavaliers" = "CLE","Mavericks" = "DAL","Nuggets" = "DEN","Pistons" = "DET","Warriors" = "GS","Rockets" = "HOU","Pacers" = "IND","Clippers" = "LAC","Lakers" = "LAL","Grizzlies" = "MEM","Heat" = "MIA","Bucks" = "MIL","Timberwolves" = "MIN","Pelicans" = "NO","Knicks" = "NY","Thunder" = "OKC","Magic" = "ORL","76ers" = "PHI","Suns" = "PHO","Trail Blazers" = "POR","Kings" = "SAC","Spurs" = "SA","Raptors" = "TOR","Jazz" = "UTA","Wizards" = "WAS")
  yestteam <-as.character(Teams[yestteam])
  return(yestteam)
}
DailyNBA <- function(Day, NBAMaster, yestteam, gameIDdate){
  yestteam <- yestteam
  NBAMaster <- NBAMaster
  x = Day
  h <- read_html(str_c("https://scores.nbcsports.com/nba/scoreboard.asp?day=", x, "&meta=true"))
  Teams <- c("Atlanta" = "ATL","Brooklyn" = "BKN","Boston" = "BOS","Charlotte" = "CHA","Chicago" = "CHI","Cleveland" = "CLE","Dallas" = "DAL","Denver" = "DEN","Detroit" = "DET","Golden State" = "GS","Houston" = "HOU","Indiana" = "IND","LA Clippers" = "LAC","LA Lakers" = "LAL","Memphis" = "MEM","Miami" = "MIA","Milwaukee" = "MIL","Minnesota" = "MIN","New Orleans" = "NO","New York" = "NY","Oklahoma City" = "OKC","Orlando" = "ORL","Philadelphia" = "PHI","Phoenix" = "PHO","Portland" = "POR","Sacramento" = "SAC","San Antonio" = "SA","Toronto" = "TOR","Utah" = "UTA","Washington" = "WAS")
  
  AwayTeam <- html_nodes(h, ".shsTableTtlRow+ tr a") %>% html_text
  HomeTeam <- html_nodes(h, ".shsTableTtlRow~ tr+ tr a") %>% html_text
  
  Team<- as.data.frame(as.character(Teams[AwayTeam]))
  colnames(Team)<- "Team"
  ATeam <-inner_join(Team, NBAMaster, by = "Team") %>%
    select(Team, fixFlo, PTS, OPTS,CurrentFS, FloStrengthTeam, FloStrength.player) %>%
    mutate(gamenumber = c(1:length(Team))) %>%
    rename("ATeam" = "Team")%>%
    mutate(Fresh = 0)
  freshATeam <- setdiff(ATeam$ATeam, yestteam)
  
  ind <- which(ATeam$ATeam%in%freshATeam)
  ATeam$Fresh[ind] <- .03
  
  ATeam <- ATeam %>%
    mutate(CurrentFS= CurrentFS + Fresh)
  
  Team<- as.data.frame(as.character(Teams[HomeTeam]))
  colnames(Team)<- "Team"
  HTeam <-inner_join(Team, NBAMaster, by = "Team") %>%
    select(Team, fixFlo, PTS, OPTS,CurrentFS, FloStrengthTeam, FloStrength.player) %>%
    mutate(gamenumber = c(1:length(Team))) %>%
    rename("HTeam" = "Team")%>%
    mutate(Fresh = 0)
  
  freshHTeam <- setdiff(HTeam$HTeam, yestteam)
  ind <- which(HTeam$HTeam%in%freshHTeam)
  HTeam$Fresh[ind] <- .03
  HTeam <- HTeam %>%
    mutate(CurrentFS= CurrentFS + Fresh)
  

  df <- right_join(ATeam, HTeam, by = "gamenumber") %>%
    mutate(CurrentFS.y = CurrentFS.y + .01) %>%
    mutate(FloProb = (.5 + CurrentFS.x) / (1 +(CurrentFS.x + CurrentFS.y))) %>%
    mutate(OverUnder = ((PTS.x + OPTS.y) /2) + ((PTS.x + OPTS.y) /2)) %>% 
    mutate(AwayScore =216 * (FloProb)) %>%
    mutate(HomeScore =216 * (1- FloProb)) %>%
    mutate(PD = (AwayScore - HomeScore)) %>%
    mutate(FloProb = (CurrentFS.x) / ((CurrentFS.x + CurrentFS.y))) %>%
    mutate(FloProbTeam = (FloStrengthTeam.x) / ((FloStrengthTeam.x + FloStrengthTeam.y))) %>%
    mutate(FloProbPlayer = (FloStrength.player.x) / ((FloStrength.player.x + FloStrength.player.y))) %>%
    mutate(FloProbSched = (fixFlo.x) / ((fixFlo.x + fixFlo.y))) %>%
    mutate(gameID = str_c(gameIDdate, ATeam, HTeam, sep = ""))
  
  
  Hwinners <- df %>%
    filter(FloProb < .5) %>%
    select(- ATeam) %>%
    mutate(PD = -1 * PD) %>%
    mutate(FloProb = 1 - FloProb) %>%
    mutate(FloProbTeam = 1 - FloProbTeam) %>%
    mutate(FloProbPlayer = 1 - FloProbPlayer) %>%
    mutate(FloProbSched = 1 - FloProbSched) %>%
    rename("Team" = "HTeam")%>%
    mutate(PD1.5 =  PD)%>%
    mutate(pdm10 = round(PD-3,2)) %>%
    mutate(pdp10 = round(PD+3, 2)) %>%
    mutate(PD = round(PD,2)) %>%
    mutate(PDCI = str_c("(", pdm10, ",", PD, ",", pdp10, ")", sep="" ))
  Awinners <- df %>%
    filter(FloProb > .5) %>%
    select(- HTeam) %>%
    rename("Team" = "ATeam")%>%
    mutate(PD1.5 = 1 * PD) %>%
    mutate(pdm10 = round(PD-3,2)) %>%
    mutate(pdp10 = round(PD+3, 2)) %>%
    mutate(PD = round(PD,2)) %>%
    mutate(PDCI = str_c("(", pdm10, ",", PD, ",", pdp10, ")", sep="" ))
  
  dfeasy <- rbind(Hwinners, Awinners) %>% select(Team, FloProb,FloProbTeam, FloProbPlayer, FloProbSched, OverUnder, HomeScore, AwayScore, PD, PD1.5, gameID,Fresh.x, Fresh.y)
  
  
  DailyOdds<- function(x) {
      x = x
      h <- read_html("https://www.sportsline.com/nba/odds/money-line/") 
      BovOddAway <- html_nodes(h, ".away-team td:nth-child(6) .primary") %>% html_text
      BovOddAway <- str_replace_all(BovOddAway, "\\+", "") %>% as.numeric
      BovOddHome <- html_nodes(h, ".home-team td:nth-child(6) .primary") %>% html_text
      BovOddHome <- str_replace_all(BovOddHome, "\\+", "") %>% as.numeric
      Team <- html_nodes(h, ".away-team h4") %>% html_text
      Opponent <- html_nodes(h, ".home-team h4") %>% html_text
      det <- html_nodes(h, ".game-details") %>% html_text
      day <- str_extract(det, "\\d{1,2}") %>% as.numeric
    
      h <- read_html("https://www.sportsline.com/nba/odds/picks-against-the-spread/") 
      SpreadAway <- html_nodes(h, ".away-team td:nth-child(6) .primary") %>% html_text
      SpreadAway <- str_replace_all(SpreadAway, "\\+", "") %>% as.numeric
      SpreadHome <- html_nodes(h, ".home-team td:nth-child(6) .primary") %>% html_text
      SpreadHome <- str_replace_all(SpreadHome, "\\+", "") %>% as.numeric
      SpreadAwayOdd<- html_nodes(h, ".away-team td:nth-child(6) .secondary") %>% html_text
      SpreadAwayOdd <- str_replace_all(SpreadAwayOdd, "\\+", "") %>% as.numeric
      SpreadHomeOdd<- html_nodes(h, ".home-team td:nth-child(6) .secondary") %>% html_text
      SpreadHomeOdd <- str_replace_all(SpreadHomeOdd, "\\+", "") %>% as.numeric
      
      BookOdds <- data.frame(Team[1:length(SpreadHome)],BovOddAway[1:length(SpreadHome)], Opponent[1:length(SpreadHome)],BovOddHome[1:length(SpreadHome)] ,SpreadAway, SpreadAwayOdd, SpreadHome, SpreadHomeOdd)
      colnames(BookOdds) <- c("ATeam", "Aodd", "HTeam", "Hodd","ASpread", "SpreadAwayOdd", "HSpread", "SpreadHomeOdd")
      
      BookOdds <-BookOdds %>%
        mutate(BookWin = NA) %>%
        mutate(BookSpread = NA)
      
      Teams <- c("Hawks" = "ATL","Nets" = "BKN","Celtics" = "BOS","Hornets" = "CHA","Bulls" = "CHI","Cavaliers" = "CLE","Mavericks" = "DAL","Nuggets" = "DEN","Pistons" = "DET","Warriors" = "GS","Rockets" = "HOU","Pacers" = "IND","Clippers" = "LAC","Lakers" = "LAL","Grizzlies" = "MEM","Heat" = "MIA","Bucks" = "MIL","Timberwolves" = "MIN","Pelicans" = "NO","Knicks" = "NY","Thunder" = "OKC","Magic" = "ORL","76ers" = "PHI","Suns" = "PHO","Trail Blazers" = "POR","Kings" = "SAC","Spurs" = "SA","Raptors" = "TOR","Jazz" = "UTA","Wizards" = "WAS")
      BookOdds$ATeam <-as.character(Teams[BookOdds$ATeam])
      BookOdds$HTeam <-as.character(Teams[BookOdds$HTeam])
      ind <- which(BookOdds$ASpread < 0)
      BookOdds$BookWin[ind] <- BookOdds$ATeam[ind]
      ind <- which(BookOdds$ASpread > 0)
      BookOdds$BookWin[ind] <- BookOdds$HTeam[ind]
      ind <- which(BookOdds$ASpread < 0)
      BookOdds$BookSpread[ind] <- BookOdds$ASpread[ind]
      ind <- which(BookOdds$ASpread > 0)
      BookOdds$BookSpread[ind] <- -1 * BookOdds$ASpread[ind]
      BookOdds <- BookOdds %>%
        mutate(gameID = str_c(gameIDdate, ATeam, HTeam, sep = ""))
      return(BookOdds)
  }
  BookOdds <- DailyOdds(x)

  NBAOdds <- merge(BookOdds, dfeasy, by = "gameID")
  NBAOdds <- NBAOdds %>%
    select(-HomeScore, -AwayScore, -OverUnder, -PD) %>%
    rename("FloWin" = "Team") %>%
    mutate(FloSpread = NA) %>%
    mutate(PD1.5 = -1* PD1.5) %>%
    mutate(SpreadWinnings = .91)%>%
    mutate(MLWinnings = NA)
  
  ind <- which((NBAOdds$PD1.5 < NBAOdds$BookSpread) & (NBAOdds$BookWin == NBAOdds$FloWin))
  NBAOdds$FloSpread[ind] <- NBAOdds$BookSpread[ind]
  ind <- which((NBAOdds$PD1.5 > NBAOdds$BookSpread) & (NBAOdds$BookWin == NBAOdds$FloWin))
  NBAOdds$FloSpread[ind] <- -1 * NBAOdds$BookSpread[ind]
  ind <- which((NBAOdds$BookWin != NBAOdds$FloWin))
  NBAOdds$FloSpread[ind] <- -1 * NBAOdds$BookSpread[ind]
  ind <- which((NBAOdds$FloWin == NBAOdds$ATeam))
  NBAOdds$MLWinnings[ind] <- NBAOdds$Aodd[ind]
  ind <- which((NBAOdds$FloWin == NBAOdds$HTeam))
  NBAOdds$MLWinnings[ind] <- NBAOdds$Hodd[ind]
  
  ind <- which(NBAOdds$MLWinnings > 0)
  NBAOdds$MLWinnings[ind] <- NBAOdds$MLWinnings[ind] / 100
  ind <- which(NBAOdds$MLWinnings < 0)
  NBAOdds$MLWinnings[ind] <- 100 / (-1 * NBAOdds$MLWinnings[ind])
  
  
  NBAOdds <- NBAOdds %>%
    select(gameID, ATeam, HTeam, BookWin, BookSpread, FloWin, FloSpread, "PredPD" = PD1.5,FloProb, FloProbTeam, FloProbPlayer, FloProbSched, MLWinnings, SpreadWinnings, Fresh.x, Fresh.y)
  y <- NBAOdds %>%
    mutate(FD = NA) %>%
    mutate(SpreadRank = NA)%>%
    mutate(WinRank = NA)
  
  ind <- which(y$BookWin == y$FloWin & y$FloSpread <0)
  y$FD[ind] <- (y$PredPD[ind] - y$BookSpread[ind])*-1
  
  ind <- which(y$BookWin == y$FloWin & y$FloSpread >0)
  y$FD[ind] <-  (-1* y$BookSpread[ind] - y$PredPD[ind])*-1
  
  ind <- which(y$BookWin != y$FloWin & y$FloSpread >0)
  y$FD[ind] <- -1*(y$BookSpread[ind] +  y$PredPD[ind])
  
  ind <- which(y$FD <1)
  y$SpreadRank[ind] <- 1
  ind <- which(y$FD >= 1 & y$FD < 3)
  y$SpreadRank[ind] <- 2
  ind <- which(y$FD >= 3 & y$FD < 5)
  y$SpreadRank[ind] <- 3
  ind <- which(y$FD >= 5 & y$FD < 8)
  y$SpreadRank[ind] <- 4
  ind <- which(y$FD >=8)
  y$SpreadRank[ind] <- 5
  
  ind <- which(y$FloProb <.51)
  y$WinRank[ind] <- 1
  ind <- which(y$FloProb >= .51 & y$FloProb < .53)
  y$WinRank[ind] <- 2
  ind <- which(y$FloProb >= .53 & y$FloProb < .56)
  y$WinRank[ind] <- 3
  ind <- which(y$FloProb >= .56 & y$FloProb < .6)
  y$WinRank[ind] <- 4
  ind <- which(y$FloProb >=.6)
  y$WinRank[ind] <- 5
  
  NBAOdds<-y %>%
    select(-FD)
  return(NBAOdds)
}


##################################################
#EXPERIMENTATION FUNCTIONS
##################################################

LinMod <- function(NBACompleteOdds){
  dfan <- NBACompleteOdds
  dfan <- dfan %>% 
    select(gameID, FloWin, WTeam, ATeam, HTeam, HomeWin, FreshWin, FloProbTeam, FloProbPlayer, FloProbSched, PD, BookSpread, BookWin, BookCorrect)%>% na.omit %>%
    mutate(FloProbPlayer = 2* (FloProbPlayer - .5)) %>%
    mutate(FloProbTeam = 2* (FloProbTeam - .5)) %>%
    mutate(FloProbSched = 2* (FloProbSched - .5)) %>%
    mutate(HomeWin = 1)
  ind <- which(dfan$FloWin == dfan$ATeam & dfan$HomeWin ==1)
  dfan$HomeWin[ind] = -1
  ind <- which(dfan$FloWin != dfan$WTeam)
  dfan$PD[ind] = -1 * dfan$PD[ind]
  dfan2 <- dfan %>%
    mutate(PD = -1 *PD)%>%
    mutate(FloProbPlayer = -1 *FloProbPlayer)%>%
    mutate(FloProbTeam = -1 *FloProbTeam)%>%
    mutate(FloProbSched = -1 *FloProbSched)%>%
    mutate(HomeWin = -1 *HomeWin)%>%
    mutate(FreshWin = -1 *FreshWin)
  dfx <- rbind(dfan, dfan2) 
  a <- lm(PD~FloProbTeam+FloProbPlayer+HomeWin+FreshWin+FloProbSched, data = dfx)
  return(a)
}

WinAnalysis <- function(NBACompleteOdds){
  dfan <- NBACompleteOdds
  dfan <- dfan %>% 
    select(gameID, FloWin, WTeam, ATeam, HTeam, HomeWin, FreshWin, FloProbTeam, FloProbPlayer, FloProbSched, PD, BookSpread, BookWin, BookCorrect)%>% na.omit %>%
    mutate(FloProbPlayer = 2* (FloProbPlayer - .5)) %>%
    mutate(FloProbTeam = 2* (FloProbTeam - .5)) %>%
    mutate(FloProbSched = 2* (FloProbSched - .5)) %>%
    mutate(HomeWin = 1)
  ind <- which(dfan$FloWin == dfan$ATeam & dfan$HomeWin ==1)
  dfan$HomeWin[ind] = -1
  ind <- which(dfan$FloWin != dfan$WTeam)
  dfan$PD[ind] = -1 * dfan$PD[ind]
  dfan2 <- dfan %>%
    mutate(PD = -1 *PD)%>%
    mutate(FloProbPlayer = -1 *FloProbPlayer)%>%
    mutate(FloProbTeam = -1 *FloProbTeam)%>%
    mutate(FloProbSched = -1 *FloProbSched)%>%
    mutate(HomeWin = -1 *HomeWin)%>%
    mutate(FreshWin = -1 *FreshWin)
  dfx <- rbind(dfan, dfan2) 
  a <- lm(PD~FloProbTeam+FloProbPlayer+HomeWin+FreshWin+FloProbSched, data = dfx)
  dfx <- dfx %>%
    mutate(gr =a$coefficients[2] * FloProbTeam+ a$coefficients[3] * FloProbPlayer+  a$coefficients[6]*FloProbSched+ a$coefficients[4] *HomeWin+ a$coefficients[5] * FreshWin) %>% 
    mutate(diff = abs(gr - PD))
  
  dfy <- dfx %>%
    group_by(gameID) %>%
    slice(1) %>%
    ungroup %>%
    mutate(PredWinner = NA)
  ind <- which(dfy$gr > 0)
  dfy$PredWinner[ind] <- dfy$FloWin[ind]
  ind <- which(dfy$gr <0 & dfy$FloWin != dfy$ATeam)
  dfy$PredWinner[ind] <- dfy$ATeam[ind]
  ind <- which(dfy$gr <0 & dfy$FloWin != dfy$HTeam)
  dfy$PredWinner[ind] <- dfy$HTeam[ind]
  ind <- which(dfy$gr <0)
  dfy$gr[ind] <- -1*dfy$gr[ind]
  dfy$PD[ind] <- -1*dfy$PD[ind]
  dfy$FloProbSched[ind] <- -1*dfy$FloProbSched[ind]
  dfy$FloProbPlayer[ind] <- -1*dfy$FloProbPlayer[ind]
  dfy$FloProbTeam[ind] <- -1*dfy$FloProbTeam[ind]
  dfy$HomeWin[ind] <- -1*dfy$HomeWin[ind]
  dfy$FreshWin[ind] <- -1*dfy$FreshWin[ind]
  
  
  dfy <- dfy %>%
    select(gameID, PredWinner, BookWin, WTeam, BookSpread, PD, gr, FloProbPlayer, FloProbSched, FloProbTeam, HomeWin, FreshWin, BookCorrect, ATeam, HTeam) %>%
    mutate(BookSpread = -1 * BookSpread) %>%
    mutate(SpreadCorrect = NA) %>%
    mutate(WinCorrect = NA) %>%
    mutate(diff = gr - PD) %>%
    mutate(SpreadProb = NA) %>%
    mutate(WinProb = NA)
  sdx <- sd(dfy$diff)
  ind <- which(dfy$gr > dfy$BookSpread & dfy$PD > dfy$BookSpread & dfy$BookWin == dfy$WTeam & dfy$BookWin== dfy$PredWinner)
  dfy$SpreadCorrect[ind] <- 1
  ind <- which(dfy$gr < dfy$BookSpread & dfy$BookWin != dfy$WTeam & dfy$BookWin== dfy$PredWinner)
  dfy$SpreadCorrect[ind] <- 1
  ind <- which(dfy$gr < dfy$BookSpread & dfy$PD < dfy$BookSpread & dfy$PredWinner == dfy$WTeam)
  dfy$SpreadCorrect[ind] <- 1
  ind <- which(dfy$BookWin != dfy$WTeam & dfy$PredWinner == dfy$WTeam )
  dfy$SpreadCorrect[ind] <- 1
  ind <- which(is.na(dfy$SpreadCorrect))
  dfy$SpreadCorrect[ind] <- 0
  ind <- which(dfy$PD == dfy$BookSpread)
  dfy$SpreadCorrect[ind] <- NA
  dfy$SpreadCorrect[ind] <- mean(na.omit(dfy$SpreadCorrect))
  ind <- which(dfy$PredWinner == dfy$WTeam )
  dfy$WinCorrect[ind] <- 1
  ind <- which(is.na(dfy$WinCorrect))
  dfy$WinCorrect[ind] <- 0
  
  ind <- which(dfy$BookWin == dfy$PredWinner & dfy$BookSpread > dfy$gr)
  dfy$SpreadProb[ind] = pnorm((dfy$BookSpread[ind]-dfy$gr[ind]), mean = 0, sd = sdx)
  ind <- which(dfy$BookWin == dfy$PredWinner & dfy$BookSpread < dfy$gr)
  dfy$SpreadProb[ind] = pnorm((dfy$BookSpread[ind]-dfy$gr[ind]), mean = 0, sd = sdx, lower.tail = FALSE)
  ind <- which(dfy$BookWin != dfy$PredWinner)
  dfy$SpreadProb[ind] = pnorm((-1 *(dfy$BookSpread[ind]+dfy$gr[ind])), mean = 0, sd = sdx, lower.tail = FALSE)
  
  ind <- which(dfy$gr > 0)
  dfy$WinProb[ind] = pnorm((dfy$gr[ind]), mean = 0, sd = sdx)
  return(dfy)
}

UpdDailyOdds <- function(Odds, linmod, dfy){
  dfz <- Odds %>%
    mutate(Fresh = Fresh.y - Fresh.x) %>%
    mutate(Home = 1) %>%
    mutate(FloProbPlayer = 2* (FloProbPlayer - .5)) %>%
    mutate(FloProbTeam = 2* (FloProbTeam - .5)) %>%
    mutate(FloProbSched = 2* (FloProbSched - .5))
  ind <- which(dfz$FloWin != dfz$HTeam)
  dfz$Home[ind] <- -1 *dfz$Home[ind]
  dfz$Fresh[ind] <- -1 *dfz$Fresh[ind]
  a <- linmod
  dfz <- dfz %>%
    mutate(gr =round(a$coefficients[2] * FloProbTeam+ a$coefficients[3] * FloProbPlayer+  a$coefficients[6]*FloProbSched+ a$coefficients[4] *Home+ a$coefficients[5] * Fresh, 6)) 
  ind <- which(dfz$gr > 0 & dfz$Home == -1)
  dfz$FloWin[ind] = dfz$ATeam[ind]
  ind <- which(dfz$gr > 0 & dfz$Home == 1)
  dfz$FloWin[ind] = dfz$HTeam[ind]
  ind <- which(dfz$gr < 0 & dfz$Home == -1)
  dfz$FloWin[ind] = dfz$HTeam[ind]
  ind <- which(dfz$gr < 0 & dfz$Home == 1)
  dfz$FloWin[ind] = dfz$ATeam[ind]
  ind <- which(dfz$gr <0)
  dfz$gr[ind] <- -1*dfz$gr[ind]
  dfz <- dfz %>%
    select(gameID, ATeam, HTeam, BookWin, FloWin, gr, BookSpread) %>%
    mutate(BookSpread = -1 * BookSpread) %>%
    mutate(SpreadProb = NA)%>% 
    mutate(WinProb = NA)
  dfy <- dfy  %>% mutate(diff = PD - gr)
  sdx <- sd(dfy$diff)
  ind <- which(dfz$BookWin == dfz$FloWin & dfz$BookSpread > dfz$gr)
  dfz$SpreadProb[ind] = pnorm((dfz$BookSpread[ind]-dfz$gr[ind]), mean = 0, sd = sdx)
  ind <- which(dfz$BookWin == dfz$FloWin & dfz$BookSpread < dfz$gr)
  dfz$SpreadProb[ind] = pnorm((dfz$BookSpread[ind]-dfz$gr[ind]), mean = 0, sd = sdx, lower.tail = FALSE)
  ind <- which(dfz$BookWin != dfz$FloWin)
  dfz$SpreadProb[ind] = pnorm((-1 *(dfz$BookSpread[ind]+dfz$gr[ind])), mean = 0, sd = sdx, lower.tail = FALSE)
  ind <- which(dfz$gr > 0)
  dfz$WinProb[ind] = pnorm((dfz$gr[ind]), mean = 0, sd = sdx)
  
  
  ateam <- dfy %>% group_by(ATeam) %>%mutate(SpreadCorrect = sum(as.numeric(SpreadCorrect))) %>% mutate(count = 1) %>% mutate(WinCorrect = sum(WinCorrect)) %>% mutate(count = sum(count))%>% slice(1) %>% select("Team" = ATeam, WinCorrect, SpreadCorrect, count)
  hteam <- dfy %>% group_by(HTeam) %>%mutate(SpreadCorrect = sum(as.numeric(SpreadCorrect))) %>% mutate(count = 1) %>% mutate(WinCorrect = sum(WinCorrect)) %>% mutate(count = sum(count))%>% slice(1) %>% select("Team" = HTeam, WinCorrect, SpreadCorrect, count)
  check <- merge(ateam, hteam, by = "Team")
  check <- check %>% mutate(SpreadCorrect = SpreadCorrect.x +SpreadCorrect.y) %>% mutate(Count = count.x + count.y) %>% mutate(WinCorrect = WinCorrect.x+WinCorrect.y) %>% select(Team, WinCorrect, SpreadCorrect, Count) %>% mutate(WinCorrect = WinCorrect / Count)%>% mutate(SpreadCorrect = SpreadCorrect / Count)
  checka <- check %>% rename("ATeam" = Team)
  checkh <- check %>% rename("HTeam" = Team)
  
  lol <- merge(dfy, checka, by = "ATeam")
  lol <- merge(lol, checkh, by ="HTeam") 
  lolol <- lol %>% mutate(SpreadTeam = (SpreadCorrect + SpreadCorrect.y)/2) %>% mutate(WinTeam = (WinCorrect + WinCorrect.y)/2) %>% mutate(NewWinProb = (WinTeam + WinProb)/2)%>% mutate(NewSpreadProb = (SpreadTeam + SpreadProb)/2)
  
  ind <- which(lolol$NewSpreadProb < .5 & lolol$SpreadCorrect.x ==0)
  lolol$SpreadCorrect.x[ind] = 1.1
  ind <- which(lolol$NewSpreadProb < .5 & lolol$SpreadCorrect.x ==1)
  lolol$SpreadCorrect.x[ind] = 0
  ind <- which(lolol$SpreadCorrect.x ==1.1)
  lolol$SpreadCorrect.x[ind] = 1
  
  ind <- which(lolol$NewSpreadProb > .5 & lolol$SpreadCorrect.x ==0)
  lolol$SpreadCorrect.x[ind] = 0
  ind <- which(lolol$NewSpreadProb > .5 & lolol$SpreadCorrect.x ==1)
  lolol$SpreadCorrect.x[ind] = 1
  mean(as.numeric(lolol$SpreadCorrect.x))
  ind <- which(lolol$NewSpreadProb < .5)
  lolol$NewSpreadProb[ind] = 1- lolol$NewSpreadProb[ind]
  
  lol <- merge(dfz, checka, by = "ATeam")
  lol <- merge(lol, checkh, by ="HTeam") 
  lolol <- lol %>% mutate(SpreadTeam = (SpreadCorrect.x + SpreadCorrect.y)/2) %>% mutate(WinTeam = (WinCorrect.x + WinCorrect.y)/2) %>% mutate(NewWinProb = (WinTeam + WinProb)/2)%>% mutate(NewSpreadProb = (SpreadTeam + SpreadProb)/2)
  ind <- which(lolol$NewSpreadProb < .5 & lolol$SpreadCorrect.x ==0)
  lolol$SpreadCorrect.x[ind] = 1.1
  ind <- which(lolol$NewSpreadProb < .5 & lolol$SpreadCorrect.x ==1)
  lolol$SpreadCorrect.x[ind] = 0
  ind <- which(lolol$SpreadCorrect.x ==1.1)
  lolol$SpreadCorrect.x[ind] = 1
  
  ind <- which(lolol$NewSpreadProb > .5 & lolol$SpreadCorrect.x ==0)
  lolol$SpreadCorrect.x[ind] = 0
  ind <- which(lolol$NewSpreadProb > .5 & lolol$SpreadCorrect.x ==1)
  lolol$SpreadCorrect.x[ind] = 1
  ind <- which(lolol$NewSpreadProb < .5)
  lolol$NewSpreadProb[ind] = 1- lolol$NewSpreadProb[ind]
  dfz <- lolol %>% select(gameID, ATeam, HTeam, BookWin, FloWin, BookSpread, gr, WinProb, WinTeam, NewWinProb, SpreadProb, SpreadTeam, NewSpreadProb)
  return(dfz)
}


