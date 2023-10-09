library(rvest)
library(tidyverse)
library(optmatch)
library(stringr)
rm(list=ls(all=TRUE)) # clear local memory
graphics.off() # close figures
cat("\014") # clear console
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
    filter(MP > 250) %>%
    mutate(yearID = year)
  return(dfcomp)
}
players1 <- map_df(.x= (1996:2005), .f=getNBAplayers)
players2<- map_df(.x= (2006:2015), .f=getNBAplayers)
players3 <- map_df(.x= (2016:2023), .f=getNBAplayers)
players <- rbind(players1,players2,players3)

write_csv(players, "/Users/seanfloersch/FloStrength/NBAFloStrength/nbaplayerdf")

nbaplayers <- players  %>%
  mutate(MPG = MP / G) %>%
  filter(MPG > 12.9 & G > 19) %>%
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
  mutate(Value = FloStrength * MP) %>%
  group_by(yearID) %>%
  mutate(Value = 2*((Value - mean(Value)) / sd(Value))) %>%
  ungroup
  
oldplayers <- map_df(.x= (1976:1995), .f=getNBAplayers)
write_csv(oldplayers, "/Users/seanfloersch/FloStrength/NBAFloStrength/oldnbaplayerdf")

oldnbaplayers <- oldplayers  %>%
  mutate(MPG = MP / G) %>%
  filter(MPG > 12.9 & G > 19) %>%
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
  mutate(Value = FloStrength * MP) %>%
  group_by(yearID) %>%
  mutate(Value = 2*((Value - mean(Value)) / sd(Value))) %>%
  ungroup
allnbaplayers <- rbind(oldnbaplayers, nbaplayers) %>%
  filter(yearID > 1979)
write_csv(allnbaplayers, "/Users/seanfloersch/FloStrength/NBAFloStrength/allnbaplayersdf")

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
    
  return(teamstats)
}

teamFlostats <- teamstats %>%
  mutate(Wpct = W / G) %>%
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
  ungroup %>%
  select(-W, -L, -`FG%`,-`3P%`,-`2P%`,-`FT%`, -`oppFG%`,-`opp3P%`,-`opp2P%`,-`oppFT%`, - oppG, -oppMP, -Rebounding1, -Rebounding2) 
x <- lm(Wpct~Playmaking+Rebounding+Defense+Shooting, data = teamFlostats)
teamFlostats <- teamFlostats %>%
  mutate(FloTeam = Playmaking*.19+Rebounding*.11+Scoring*.25+Shooting*.43) %>%
  group_by(yearID) %>%
  mutate(FloTeam = (FloTeam - mean(FloTeam))/ sd(FloTeam)) %>%
  ungroup %>%
  mutate(Tm = toupper(str_c(str_extract(Team, "[A-Z]"), str_extract(Team, "[a-z]{1,2}"))))
ind <- which(teamFlostats$Tm == "GOL")
teamFlostats$Tm[ind] ="GSW"
ind <- which(teamFlostats$Team == "New Jersey Nets")
teamFlostats$Tm[ind] = "NJN"
ind <- which(teamFlostats$Team == "New York Knicks")
teamFlostats$Tm[ind] = "NYK"
ind <- which(teamFlostats$Team == "New Orleans Hornets")
teamFlostats$Tm[ind] = "NOH"
ind <- which(teamFlostats$Team == "New Orleans Pelicans")
teamFlostats$Tm[ind] = "NOP"
ind <- which(teamFlostats$Team == "Los Angeles Lakers")
teamFlostats$Tm[ind] = "LAL"
ind <- which(teamFlostats$Team == "Los Angeles Clippers")
teamFlostats$Tm[ind] = "LAC"
ind <- which(teamFlostats$Tm == "SAN")
teamFlostats$Tm[ind] = "SAS"
ind <- which(teamFlostats$Tm == "KAN")
teamFlostats$Tm[ind] = "KCK"
ind <- which(teamFlostats$Tm == "BRO")
teamFlostats$Tm[ind] = "BRK"
ind <- which(teamFlostats$Team == "San Diego Clippers")
teamFlostats$Tm[ind] = "SDC"
ind <- which(teamFlostats$Team == "Washington Bullets")
teamFlostats$Tm[ind] = "WSB"
ind <- which(teamFlostats$Team == "Oklahoma City Thunder")
teamFlostats$Tm[ind] = "OKC"
ind <- which(teamFlostats$Team == "New Orleans/Oklahoma City Hornets")
teamFlostats$Tm[ind] = "NOK"
playerteam <- allnbaplayers %>%
  filter(Tm != "TOT") %>%
  group_by(Tm, yearID) %>%
  mutate(FloPlayer = sum(Value)) %>%
  slice(1) %>%
  select(Tm, yearID, FloPlayer) %>%
  ungroup %>%
  group_by(yearID) %>%
  mutate(FloPlayer = (FloPlayer - mean(FloPlayer))/ sd(FloPlayer))
ind <- which(playerteam$Tm == "CHH")
playerteam$Tm[ind] = "CHA"
ind <- which(playerteam$Tm == "CHO")
playerteam$Tm[ind] = "CHA"
teamFlostats <- left_join(teamFlostats, playerteam, by = c("Tm", "yearID"))

x <- lm(Wpct~FloPlayer+FloTeam, data = teamFlostats)
teamFlostats <- teamFlostats %>%
  mutate(FloStrength = x$coefficients[1]+x$coefficients[2]*FloPlayer+x$coefficients[3]*FloTeam)














