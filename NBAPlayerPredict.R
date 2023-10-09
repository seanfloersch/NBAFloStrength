library(rvest)
library(tidyverse)
library(optmatch)
library(stringr)
rm(list=ls(all=TRUE)) # clear local memory
graphics.off() # close figures
cat("\014") # clear console

nbaplayers<-read.csv("/Users/seanfloersch/FloStrength/NBAFloStrength/NBAPlayersAT")%>%
  mutate(birth = yearID-Age)%>%
  arrange(yearID) %>%
  group_by(birth,Name) %>%
  mutate(carval = sum(Value)) %>%
  mutate(Experiance = c(1:length(Value))) %>%
  ungroup() %>%
  filter(birth >1955)

current <- nbaplayers %>%
  filter(yearID == 1982) %>%
  select(Name,yearID,Tm,Pos,Age,Experiance,carval,MP,FloStrength,Value,birth)
lastyear <- nbaplayers %>%
  filter(yearID == 1981)%>%
  select(Name,MP,FloStrength,Value,birth)
twoyear <- nbaplayers %>%
  filter(yearID == 1980)%>%
  select(Name,MP,FloStrength,Value,birth)
preddf<- left_join(current,lastyear,by=c("Name","birth"))
preddf<- left_join(preddf,twoyear,by="Name") %>%
  select(Name,yearID,Tm,Age,Experiance,carval,Pos,"PredMP"= "MP.x","PredFS"= "FloStrength.x","PredVal"= "Value.x","LYMP"= "MP.y","LYFS"= "FloStrength.y","LYVal"= "Value.y","TYMP"= "MP","TYFS"= "FloStrength","TYVal"= "Value")

for ( i in c(1983:2022)){
  current <- nbaplayers %>%
    filter(yearID == i) %>%
    select(Name,yearID,Tm,Pos,Age,Experiance,carval,MP,FloStrength,Value,birth)
  lastyear <- nbaplayers %>%
    filter(yearID == (i-1))%>%
    select(Name,MP,FloStrength,Value,birth)
  twoyear <- nbaplayers %>%
    filter(yearID == (i-2))%>%
    select(Name,MP,FloStrength,Value,birth)
  preddf1<- left_join(current,lastyear,by=c("Name","birth"))
  preddf1<- left_join(preddf1,twoyear,by="Name") %>%
    select(Name,yearID,Tm,Age,Experiance,carval,Pos,"PredMP"= "MP.x","PredFS"= "FloStrength.x","PredVal"= "Value.x","LYMP"= "MP.y","LYFS"= "FloStrength.y","LYVal"= "Value.y","TYMP"= "MP","TYFS"= "FloStrength","TYVal"= "Value")
  preddf<- rbind(preddf,preddf1)
}
preddata<- preddf %>%
  mutate(Change21 = LYVal-TYVal) %>%
  mutate(Change10= PredVal - LYVal)%>%
  filter(yearID>2000)%>%
  select(Name,Tm,yearID,Age,PredMP,PredFS,LYMP,LYFS,TYMP,TYFS)

comppred <- preddata %>%
  filter(is.na(LYMP)==F & is.na(TYMP)==F)
lincomFS= lm(PredFS~LYFS+TYFS+Age, data = comppred)
lincomMP= lm(PredMP~LYMP+TYMP+Age, data = comppred)
predMP<-predict(lincomMP,newdata = comppred)
predFS<-predict(lincomFS,newdata = comppred)
comppred<-comppred%>%
  mutate(MPPredict=predMP)%>%
  mutate(FSPredict=predFS)

missedlast <- preddata %>%
  filter(is.na(LYMP)==T & is.na(TYMP)==F)
linmlFS= lm(PredFS~TYFS+Age, data = missedlast)
linmlMP= lm(PredMP~TYMP+Age, data = missedlast)
predMP<-predict(linmlMP,newdata = missedlast)
predFS<-predict(linmlFS,newdata = missedlast)
missedlast<-missedlast%>%
  mutate(MPPredict=predMP)%>%
  mutate(FSPredict=predFS)

secyr <- preddata %>%
  filter(is.na(LYMP)==F & is.na(TYMP)==T)
linsyFS= lm(PredFS~LYFS+Age, data = secyr)
linsyMP= lm(PredMP~LYMP+Age, data = secyr)
predMP<-predict(linsyMP,newdata = secyr)
predFS<-predict(linsyFS,newdata = secyr)
secyr<-secyr%>%
  mutate(MPPredict=predMP)%>%
  mutate(FSPredict=predFS)


firyr <- preddata %>%
  filter(is.na(LYMP)==T & is.na(TYMP)==T)
linrookFS<-mean(firyr$PredFS)
linrookMP<-mean(firyr$PredMP)
firyr<-firyr%>%
  mutate(MPPredict=linrookMP)%>%
  mutate(FSPredict=linrookFS)
PredPlayDF<-rbind(firyr,secyr)
PredPlayDF<-rbind(PredPlayDF,missedlast)
PredPlayDF<-rbind(PredPlayDF,comppred)%>%
  select(Name,yearID,Tm,FSPredict,MPPredict)
PredTeamPlay<-PredPlayDF%>%
  group_by(Tm,yearID)%>%
  arrange(-MPPredict)%>%
  slice(1:8)%>%
  mutate(WFS=FSPredict*(MPPredict/sum(MPPredict)))%>%
  mutate(WFS=sum(WFS))%>%
  slice(1)%>%
  ungroup()%>%
  group_by(yearID)%>%
  mutate(PlayPred=(WFS-mean(WFS))/sd(WFS))%>%
  ungroup()%>%
  select("Team"="Tm",yearID,PlayPred)

teampred<-read.csv("/Users/seanfloersch/FloStrength/NBAFloStrength/NBATeamsAT") %>%
  select(Team,yearID, Wpct, FloStrength,POSuc)
teams <- c("Minnesota Timberwolves" ="MIN", "Memphis Grizzlies"="MEM", "Milwaukee Bucks"="MIL", "Charlotte Hornets"="CHO", "Phoenix Suns"="PHO", "Atlanta Hawks"="ATL", "Utah Jazz"="UTA", "San Antonio Spurs"="SAS", "Brooklyn Nets"="BRK", "Denver Nuggets"="DEN", "Los Angeles Lakers"="LAL", "Boston Celtics"="BOS", "Chicago Bulls"="CHI", "Indiana Pacers"="IND", "Golden State Warriors"="GSW", "Sacramento Kings"="SAC", "Miami Heat"="MIA", "Philadelphia 76ers"="PHI", "Houston Rockets"="HOU", "Toronto Raptors"="TOR", "New Orleans Pelicans"="NOP", "Washington Wizards"="WAS", "Los Angeles Clippers"="LAC", "Dallas Mavericks"="DAL", "Cleveland Cavaliers"="CLE", "New York Knicks"="NYK", "Portland Trail Blazers"="POR", "Detroit Pistons"="DET", "Orlando Magic"="ORL", "Oklahoma City Thunder"="OKC", "New Jersey Nets"="BRK", "Seattle SuperSonics"="OKC", "San Diego Clippers"="LAC", "Washington Bullets"="WAS", "Kansas City Kings"="SAC", "Vancouver Grizzlies"="MEM","New Orleans Hornets"="NOP", "Charlotte Bobcats"="CHA", "New Orleans/Oklahoma City Hornets"="NOP")
teampred$Team<- as.character(teams[teampred$Team])
lagyear <- teampred %>%
  mutate(yearID= yearID +1) %>%
  select(Team, yearID,"LYWpct"=Wpct, "LYFS"=FloStrength, "LYPO"=POSuc)
teampred<-left_join(teampred,lagyear,by=c("Team","yearID"))%>%
  filter(yearID>2000)
teampred<-left_join(teampred,PredTeamPlay,by=c("Team","yearID"))%>%na.omit()
linmod<-lm(Wpct~LYFS+LYPO+PlayPred,data = teampred)
##Current season
allnbaplayersdf <- read.csv("~/FloStrength/FloStrengthHistoryApp/NBAPlayersAT") %>%
  mutate(Player=Name)
getRoster <- function(team){
  url <- str_c("https://www.basketball-reference.com/teams/",team,"/2024.html")
  h <- read_html(url) 
  Player <- html_nodes(h, ".iz+ .left a") %>% html_text 
  df <- data.frame(Player) %>%
    mutate(Team = team) %>%
    mutate(yearID = 2023)
  return(df)
}
teams <- allnbaplayersdf %>% filter(yearID == 2022)%>%
  filter(Tm!="TOT")
teams <- unique(teams$Tm)
roster1 <- map_df(.x= teams[1:15], .f=getRoster)
roster2 <- map_df(.x= teams[16:30], .f=getRoster)
lillard=data.frame(Player="Damian Lillard","Team"="MIL","yearID"=2023)
roster<-rbind(roster1,roster2)
roster<-rbind(roster,lillard)

playly <- allnbaplayersdf %>% filter(yearID == 2023) %>%
  select(Player,Age,"LYMP"=MP, "LYFS"="FloStrength")
playty <- allnbaplayersdf %>% filter(yearID == 2022) %>%
  select(Player,"AgeforML"=Age,"TYMP"=MP, "TYFS"="FloStrength")
PlayerPred<- left_join(roster, playly, by = "Player")
PlayerPred<- left_join(PlayerPred, playty, by = "Player")

comp <- PlayerPred %>%
  filter(is.na(LYMP)==F & is.na(TYMP)==F)
predMP<-predict(lincomMP,newdata = comp)
predFS<-predict(lincomFS,newdata = comp)
comp<-comp%>%
  mutate(FSPredict=predFS)%>%
  mutate(MPPredict=predMP)

misslast <- PlayerPred %>%
  filter(is.na(LYMP)==T & is.na(TYMP)==F)%>%
  mutate(Age=AgeforML+1)
predMP<-predict(linmlMP,newdata = misslast)
predFS<-predict(linmlFS,newdata = misslast)
misslast<-misslast%>%
  mutate(FSPredict=predFS)%>%
  mutate(MPPredict=predMP)

secyr <- PlayerPred %>%
  filter(is.na(LYMP)==F & is.na(TYMP)==T)
predMP<-predict(linsyMP,newdata = secyr)
predFS<-predict(linsyFS,newdata = secyr)
secyr<-secyr%>%
  mutate(FSPredict=predFS)%>%
  mutate(MPPredict=predMP)

rook <- PlayerPred %>%
  filter(is.na(LYMP)==T & is.na(TYMP)==T)%>%
  mutate(FSPredict=linrookFS)%>%
  mutate(MPPredict=linrookMP)

PlayPred<-rbind(comp,misslast,rook,secyr)%>%
  mutate(ValuePredict=(MPPredict/400)*FSPredict)

PlayPredTeam <- PlayPred %>%
  group_by(MPPredict,Team)%>%
  slice(1:2)%>%
  ungroup()%>%
  group_by(Team, yearID) %>%
  arrange(-MPPredict) %>%
  slice(1:8) %>%
  mutate(WFS = FSPredict * (MPPredict / sum(MPPredict))) %>%
  ungroup() %>%
  group_by(Team, yearID) %>%
  mutate(PlayRating = sum(WFS)) %>%
  slice(1) %>%
  select("Tm"=Team, yearID, PlayRating) %>%
  ungroup %>%
  group_by(yearID) %>%
  mutate(PlayPred = (PlayRating - mean(PlayRating)) / sd(PlayRating))%>%
  ungroup()%>%
  select("Team"=Tm,PlayPred)

teampred<-read.csv("/Users/seanfloersch/FloStrength/NBAFloStrength/NBATeamsAT") %>%
  select(Team,yearID, Wpct, FloStrength,POSuc)
teams <- c("Minnesota Timberwolves" ="MIN", "Memphis Grizzlies"="MEM", "Milwaukee Bucks"="MIL", "Charlotte Hornets"="CHO", "Phoenix Suns"="PHO", "Atlanta Hawks"="ATL", "Utah Jazz"="UTA", "San Antonio Spurs"="SAS", "Brooklyn Nets"="BRK", "Denver Nuggets"="DEN", "Los Angeles Lakers"="LAL", "Boston Celtics"="BOS", "Chicago Bulls"="CHI", "Indiana Pacers"="IND", "Golden State Warriors"="GSW", "Sacramento Kings"="SAC", "Miami Heat"="MIA", "Philadelphia 76ers"="PHI", "Houston Rockets"="HOU", "Toronto Raptors"="TOR", "New Orleans Pelicans"="NOP", "Washington Wizards"="WAS", "Los Angeles Clippers"="LAC", "Dallas Mavericks"="DAL", "Cleveland Cavaliers"="CLE", "New York Knicks"="NYK", "Portland Trail Blazers"="POR", "Detroit Pistons"="DET", "Orlando Magic"="ORL", "Oklahoma City Thunder"="OKC", "New Jersey Nets"="BRK", "Seattle SuperSonics"="OKC", "San Diego Clippers"="LAC", "Washington Bullets"="WAS", "Kansas City Kings"="SAC", "Vancouver Grizzlies"="MEM","New Orleans Hornets"="NOP", "Charlotte Bobcats"="CHA", "New Orleans/Oklahoma City Hornets"="NOP")
teampred$Team<- as.character(teams[teampred$Team])
lagyear <- teampred %>%
  filter(yearID==2023)%>%
  select(Team, yearID,"LYWpct"=Wpct, "LYFS"=FloStrength, "LYPO"=POSuc)

teampred<-left_join(lagyear,PlayPredTeam,by=c("Team"))%>%na.omit()%>%
  arrange(Team)
predTeam<-predict(linmod,newdata = teampred)
teampred<-teampred%>%
  mutate(WpctPredict=predTeam)%>%
  mutate(WinsPred=round(82*WpctPredict,2))%>%
  mutate(FSPredict=round(WpctPredict,3))%>%
  select(Team,FSPredict,WinsPred)
