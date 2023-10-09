library(rvest)
library(tidyverse)
library(optmatch)
library(stringr)
rm(list=ls(all=TRUE)) # clear local memory
graphics.off() # close figures
cat("\014") # clear console

weekschedfun <- function(url)  {
  h <- read_html(url) 
  date <- url %>% str_remove("https://www.espn.com/nba/scoreboard/_/date/")
  date <- date %>% str_remove("/")
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


octurls <- c()
for (i in c(1:13)){
  octurls[i] <- str_c("https://www.espn.com/nba/scoreboard/_/date/202110", i + 18, "/", sep = "")  
}
oct <- map_df(.x= octurls, .f=weekschedfun)  

novurls <- c()
novdates <- c("01", "02", "03","04", "05", "06", "07", "08","09", 10:24, 26:30)
for (i in novdates){
  novurls[i] <- str_c("https://www.espn.com/nba/scoreboard/_/date/202111", i, "/", sep = "")  
}
nov <- map_df(.x= novurls, .f=weekschedfun)  

decurls <- c()
decdates <- c("01", "02", "03","04", "05", "06", "07", "08","09", 10:23, 25:31)
for (i in decdates){
  decurls[i] <- str_c("https://www.espn.com/nba/scoreboard/_/date/202112", i, "/", sep = "")  
}
dec <- map_df(.x= decurls, .f=weekschedfun)  

janurls <- c()
jandates <- c("01", "02", "03","04", "05", "06", "07", "08","09", 10:31)
for (i in jandates){
  janurls[i] <- str_c("https://www.espn.com/nba/scoreboard/_/date/202201", i, "/", sep = "")  
}
jan <- map_df(.x= janurls, .f=weekschedfun)  

feburls <- c()
febdates <- c("01", "02", "03", "04", "05", "06", "07", "08","09", 10:17)
for (i in febdates){
  feburls[i] <- str_c("https://www.espn.com/nba/scoreboard/_/date/202202", i, sep = "")     
}
feb <- map_df(.x= feburls, .f=weekschedfun)  

test<- rbind(oct, nov, dec, jan, feb)

NBAGames<- test %>% mutate(Win = 1)%>% mutate(Loss = 0)%>% mutate(GP = 1)

HWinners <- NBAGames %>% 
  filter(HScore >AScore) %>%
  select(-ATeam, "WTeam" = HTeam, -AScore, "WScore"=HScore)
AWinners <- NBAGames %>% 
  filter(AScore >HScore) %>%
  select(-HTeam, "WTeam" = ATeam, -HScore, "WScore"=AScore)
Winners <- rbind(HWinners, AWinners)
HLosers <- NBAGames %>% 
  filter(HScore <AScore) %>%
  select(-ATeam, "LTeam" = HTeam, "LScore"=HScore,-AScore, -GP, -Win, -Loss, -PD)
ALosers <- NBAGames %>% 
  filter(AScore <HScore) %>%
  select(-HTeam, "LTeam" = ATeam, "LScore" = AScore, -HScore, -GP, -Win, -Loss, -PD)
Losers <- rbind(HLosers, ALosers)

NBAGames <- merge(Winners, Losers, by="gameID") %>%
  select(WTeam, WScore, LTeam, LScore, Win, Loss, GP, gameID) %>%
  mutate(PD = WScore - LScore)

Teams <- c("Hawks" = "ATL","Nets" = "BKN","Celtics" = "BOS","Hornets" = "CHA","Bulls" = "CHI","Cavaliers" = "CLE","Mavericks" = "DAL","Nuggets" = "DEN","Pistons" = "DET","Warriors" = "GS","Rockets" = "HOU","Pacers" = "IND","Clippers" = "LAC","Lakers" = "LAL","Grizzlies" = "MEM","Heat" = "MIA","Bucks" = "MIL","Timberwolves" = "MIN","Pelicans" = "NO","Knicks" = "NY","Thunder" = "OKC","Magic" = "ORL","76ers" = "PHI","Suns" = "PHO","Trail Blazers" = "POR","Kings" = "SAC","Spurs" = "SA","Raptors" = "TOR","Jazz" = "UTA","Wizards" = "WAS")
NBAGames$WTeam <-as.character(Teams[NBAGames$WTeam])
NBAGames$LTeam <-as.character(Teams[NBAGames$LTeam])

write_csv(NBAGames, "/Users/seanfloersch/FloStrength/NBAFloStrength/NBAGames2122")

