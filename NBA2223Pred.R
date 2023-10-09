library(rvest)
library(tidyverse)
library(optmatch)
library(stringr)
allnbaplayersdf <- read.csv("~/FloStrength/FloStrengthHistoryApp/NBAPlayersAT") %>%
  mutate(MPG=MP/G)%>%
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
teams[4]<- "CHO"
roster <- map_df(.x= teams, .f=getRoster)
play22 <- allnbaplayers %>% filter(yearID == 2023) %>%
  select(Player,Age,MPG, "FloStrength1"="FloStrength", "Value1"=Value)%>%
  mutate(YFP = 25 - Age) %>%
  select(-Age)
play21 <- allnbaplayers %>% filter(yearID == 2022) %>%
  select(Player,Age,MPG, "FloStrength2"="FloStrength", "Value2"=Value) %>%
  mutate(YFP = 24 - Age) %>%
  select(-Age)


pred23 <- left_join(roster, play22, by = "Player")
pred23 <- left_join(pred23, play21, by = "Player")
known23 <- pred23%>%
  na.omit %>%
  mutate(FSPred = 0.04676+ 0.56379 * FloStrength1 + 0.31595 * FloStrength2 + 0.03714*YFP.y)%>%
  mutate(MPGPred = 5.5109+ 0.6436 * MPG.x + 0.1647 * MPG.y + 0.3627*YFP.y) %>%
  select(Player, Team, FSPred, MPGPred)
unknown23 <- pred23 %>%
  filter(is.na(FloStrength1) == TRUE & is.na(FloStrength2) == TRUE) %>%
  mutate(MPGPred = 20.59895) %>%
  mutate(FSPred =-0.4116054)%>%
  select(Player, Team, FSPred, MPGPred)
missedlast23 <- pred23 %>%
  filter(is.na(FloStrength1) == TRUE & is.na(FloStrength2) == FALSE) %>%
  mutate(MPGPred =10.1550 +0.5021 *MPG.y + 0.2804*YFP.y) %>%
  mutate(FSPred =-0.08219+0.68796*FloStrength2+0.03453*YFP.y) %>%
  select(Player, Team, FSPred, MPGPred)
secyr23 <- pred23 %>%
  filter(is.na(FloStrength1) == FALSE & is.na(FloStrength2) == TRUE) %>%
  mutate(MPGPred =8.9250 +0.6675 *MPG.x + 0.4789*YFP.x) %>%
  mutate(FSPred =-0.01067+0.71587*FloStrength1+0.03743*YFP.x) %>%
  select(Player, Team, FSPred, MPGPred)
playpred23<- rbind(known23, unknown23)  
playpred23<- rbind(playpred23, missedlast23)  
playpred23<- rbind(playpred23, secyr23) %>%
  mutate(yearID = 2023)
predteam23 <- playpred23 %>%
  filter(MPGPred != 20.59895) %>%
  group_by(Team, yearID) %>%
  arrange(-FSPred) %>%
  slice(1:12) %>%
  mutate(WFS = FSPred * (MPGPred / sum(MPGPred))) %>%
  ungroup() %>%
  group_by(Team, yearID) %>%
  mutate(FSPlayPred = sum(WFS)) %>%
  slice(1) %>%
  select("Tm"=Team, yearID, FSPlayPred) %>%
  ungroup %>%
  group_by(yearID) %>%
  mutate(FSPlayPred = (FSPlayPred - mean(FSPlayPred)) / sd(FSPlayPred))
yr22 <- teamFlostats %>%
  filter(yearID == 2022) %>%
  mutate(yearID = 2023) %>%
  select(yearID, Tm,"LYFS"= FloStrength,"LYWP" = Wpct) %>%
  group_by(yearID) %>%
  arrange(LYWP) %>%
  mutate(Draft = c(1:length(Tm)))%>%
  ungroup
ind <- which(predteam23$Tm == "CHO")
predteam23$Tm[ind] = "CHA"
linmod
predteam23 <- left_join(predteam23, yr22, by = c("Tm","yearID")) %>%
  mutate(predWP = linmod$coefficients[1]+ linmod$coefficients[2] *LYFS + linmod$coefficients[3]* FSPlayPred+ linmod$coefficients[4]* Draft)
write_csv(predteam23, "/Users/seanfloersch/FloStrength/NBAFloStrength/predteam23")
write_csv(playpred23, "/Users/seanfloersch/FloStrength/NBAFloStrength/playpred23")

rm(allnbaplayersdf, known23, lastyr, missedlast23, nbaplayers, oldnbaplayers, oldplayers, play21, play22, playerteam, pred23, roster, secyr23, teamstats, unknown23, x, yr22, teams, ind,players, linmod)
