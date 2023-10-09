oldplay <-read.csv("/Users/seanfloersch/FloStrength/NBAFloStrength/oldnbaplayerdf")
newplay <- read.csv("/Users/seanfloersch/FloStrength/NBAFloStrength/nbaplayerdf")
players <- rbind(oldplay,newplay) %>%
  mutate(MPG = MP / G) %>%
  filter(yearID>1979) %>%
  filter(MPG > 9.9 & G > 19) %>%
  mutate(Pos = str_extract(Pos, "[A-Z]{1,2}")) %>%
  mutate(Name = str_remove(Player, "\\*"))%>%
  mutate(ThPM = ifelse(is.na(X3P)==TRUE,0,X3P))%>%
  mutate(ThPA = ifelse(is.na(X3PA)==TRUE,0,X3PA)) %>%
  mutate(ThPM = ifelse(is.na(X3P)==TRUE,0,X3P))%>%
  mutate(Scoring = (3*(ThPM*(ThPM/(ThPA+0.0001)))+2*(X2P*(X2P/(X2PA+0.0001)))+ (FT*(FT/(FTA+0.0001))))/MP) %>%
  mutate(Passing = (AST/MP) - 2 * (TOV/MP)) %>%
  mutate(Rebounding = sqrt((ORB/MP) + (DRB/MP))) %>%
  mutate(defedit = (MPG-24)/6) %>%
  mutate(Defense = (3*(STL) + (BLK) + .75*G*(DBPM+defedit))/MP) %>%
  group_by(yearID) %>%
  mutate(Scoring = (Scoring - mean(Scoring)) / sd(Scoring))%>%
  mutate(Passing = (Passing - mean(Passing)) / sd(Passing))%>%
  mutate(Rebounding = (Rebounding - mean(Rebounding)) / sd(Rebounding)) %>%
  mutate(Defense = (Defense - mean(Defense)) / sd(Defense)) %>%
  ungroup %>%
  select(Name, Pos, yearID, Age, Tm, MP,G,PTS,TRB,AST,STL,BLK,Scoring,Passing, Rebounding, Defense) 
############################
####Get Teams
############################
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
OldTeams <- map_df(.x= (1980:1999), .f=getNBATeam)
NewTeams <- map_df(.x= (2000:2019), .f=getNBATeam)
NowTeams <- map_df(.x= (2020:2023), .f=getNBATeam)
teams <- rbind(OldTeams, NewTeams, NowTeams) %>%
  mutate(Scoring =(3*(`3P`*`3P%`) +2*(`2P`*`2P%`)+1*(`FT`*`FT%`))/MP) %>%
  mutate(Defense =(3*(`opp3P`*`opp3P%`) +2*(`opp2P`*`opp2P%`)+1*(`oppFT`*`oppFT%`))/MP) %>%
  mutate(Rebounding = (ORB+DRB)-(oppORB+oppDRB)) %>%
  mutate(Passing = (AST-TOV)-(oppAST-oppTOV)) %>%
  mutate(Passing1 = (AST-TOV)) %>%
  mutate(Wpct = W/G) %>%
  group_by(yearID) %>%
  mutate(Scoring = (Scoring -mean(Scoring))/sd(Scoring)) %>%
  mutate(Defense = -1*(Defense -mean(Defense))/sd(Defense))  %>%
  mutate(Rebounding = (Rebounding -mean(Rebounding))/sd(Rebounding))  %>%
  mutate(Passing = (Passing -mean(Passing))/sd(Passing))  %>%
  mutate(Passing1 = (Passing1 -mean(Passing1))/sd(Passing1))  %>%
  mutate(wpctplay = (Wpct -mean(Wpct))/sd(Wpct))  %>%
  ungroup()%>%
  select(Team, G, PTS,"TwPA"="2PA","ThPA"="3PA", Wpct, yearID, Scoring,Defense,Rebounding,Passing,wpctplay,Passing1)
linmod<-lm(Wpct~Scoring+Defense+Rebounding+Passing, data = teams)
summary(linmod)
teams<- teams %>%
  mutate(FloStrength = predict(linmod))
linmod<-lm(wpctplay~Scoring+Defense+Rebounding+Passing1, data = teams)
players <- players %>%
  mutate(FloStrength = linmod$coefficients[2]*Scoring+linmod$coefficients[3]*Defense+linmod$coefficients[4]*Rebounding+linmod$coefficients[5]*Passing) %>%
  mutate(Value = FloStrength * (MP/400))

careers <- players %>%
  mutate(birthyear= yearID-Age) %>%
  filter(yearID>1979) %>%
  group_by(Name, birthyear) %>%
  mutate(Value = round(sum(Value),2))%>%
  mutate(G = sum(G))%>%
  filter(G >70) %>%
  mutate(PTS = sum(PTS))%>%
  mutate(TRB = sum(TRB))%>%
  mutate(AST = sum(AST))%>%
  mutate(STL = sum(STL))%>%
  mutate(BLK = sum(BLK))%>%
  mutate(Defense = round(sum(Defense*MP / sum(MP)),2)) %>%
  mutate(Scoring = round(sum(Scoring*MP / sum(MP)),2)) %>%
  mutate(Passing = round(sum(Passing*MP / sum(MP)),2)) %>%
  mutate(Rebounding = round(sum(Rebounding*MP / sum(MP)),2)) %>%
  mutate(FloStrength = round(sum(FloStrength*MP / sum(MP)),2)) %>%
  mutate(Seasons = length(yearID))%>%
  slice(1) %>%
  ungroup() %>%
  select(Name,Pos, "RookieYr"=yearID,Seasons,G,PTS,TRB,AST,STL,BLK, Value, FloStrength,Defense,Passing, Scoring, Rebounding) %>%
  arrange(-Value)
write_csv(careers, "/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/NBACareers")
write_csv(players, "/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/NBAPlayersAT")
write_csv(teams, "/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/NBATeamsAT")


getSchedules<- function(year){
  
  months<- c("october","november","december","january","february","march","april","may","june")
  if (year %in% c(1980,1981,1983)){
    months<- c("october","november","december","january","february","march","april","may")
  }
  if (year %in% c(1988:1997,2000,2005,2006)){
    months<- c("november","december","january","february","march","april","may","june")
  }
  if (year == 1999){
    months<- c("february","march","april","may","june")
  }
  if (year == 2012){
    months<- c("december","january","february","march","april","may","june")
  }
  if (year == 2020){
    months<- c("october-2019","november","december","january","february","march","july","august","september","october-2020")
  }
  if (year == 2021){
    months<- c("december","january","february","march","april","may","june","july")
  }
  if (year ==2023){
    months<- c("october","november","december","january","february","march","april","may","june")
  }
  i = 1
  url <- str_c("https://www.basketball-reference.com/leagues/NBA_", year,"_games-",months[i],".html")
  h <- read_html(url) 
  columns <- html_nodes(h, "thead .center") %>% html_text
  
  df <- data.frame(matrix(ncol = 4))
  colnames(df)<- c("at","ht","as","hs")
  if ("Start (ET)" %in% columns){
    for (i in c(1:length(months))){
      url <- str_c("https://www.basketball-reference.com/leagues/NBA_", year,"_games-",months[i],".html")
      h <- read_html(url) 
      at <- html_nodes(h, ".left:nth-child(3) a") %>% html_text
      ht <- html_nodes(h, "td.left~ .left a") %>% html_text
      as <- (html_nodes(h, ".right:nth-child(4)") %>% html_text)[1:length(at)]
      hs <- html_nodes(h, ".right:nth-child(6)") %>% html_text
      df1 <- data.frame(at,ht,as,hs)
      df <- rbind(df, df1) %>%
        na.omit()
    }
  } else {
    for (i in c(1:length(months))){
      url <- str_c("https://www.basketball-reference.com/leagues/NBA_", year,"_games-",months[i],".html")
      h <- read_html(url) 
      at <- html_nodes(h, "th+ .left") %>% html_text
      ht <- html_nodes(h, ".left:nth-child(4)") %>% html_text
      as <- (html_nodes(h, ".right:nth-child(3)") %>% html_text)[1:length(at)]
      hs <- html_nodes(h, ".right~ .left+ .right") %>% html_text
      df1 <- data.frame(at,ht,as,hs)
      df <- rbind(df, df1) %>%
        na.omit()
    }
  }
  
  df<- df %>%
    mutate(as = as.numeric(as)) %>%
    mutate(hs = as.numeric(hs)) %>%
    mutate(yearID = year)
  return(df)
}
sched1 <- map_df(.x= (1980:1981), .f=getSchedules)
sched2 <- map_df(.x= (1982:1983), .f=getSchedules)
sched3 <- map_df(.x= (1984:1985), .f=getSchedules)
sched4 <- map_df(.x= (1986:1987), .f=getSchedules)
sched5 <- map_df(.x= (1988:1989), .f=getSchedules)
sched6 <- map_df(.x= (1990:1991), .f=getSchedules)
sched7 <- map_df(.x= (1992:1993), .f=getSchedules)
sched8 <- map_df(.x= (1994:1995), .f=getSchedules)
sched9 <- map_df(.x= (1996:1997), .f=getSchedules)
sched10 <- map_df(.x= (1998:1999), .f=getSchedules)
sched11 <- map_df(.x= (2000:2001), .f=getSchedules)
sched12 <- map_df(.x= (2002:2003), .f=getSchedules)
sched13 <- map_df(.x= (2004:2005), .f=getSchedules)
sched14 <- map_df(.x= (2006:2007), .f=getSchedules)
sched15 <- map_df(.x= (2008:2009), .f=getSchedules)
sched16 <- map_df(.x= (2010:2011), .f=getSchedules)
sched17 <- map_df(.x= (2012:2013), .f=getSchedules)
sched18 <- map_df(.x= (2014:2015), .f=getSchedules)
sched19 <- map_df(.x= (2016:2017), .f=getSchedules)
sched20 <- map_df(.x= (2018:2019), .f=getSchedules)
sched21 <- map_df(.x= (2020:2021), .f=getSchedules)
sched22 <- map_df(.x= (2022:2023), .f=getSchedules)
scheddf <- rbind(sched1,sched2,sched3,sched4,sched5,sched6,sched7,sched8,sched9,sched10,sched11,sched12,sched13,sched14,sched15,sched16,sched17,sched18,sched19,sched20,sched21,sched22)
awayteam<- teams %>%
  select("at"="Team", yearID, "AFS"=FloStrength) 
hometeam<- teams %>%
  select("ht"="Team", yearID, "HFS"=FloStrength)

scheddf <- left_join(scheddf,awayteam, by = c("at","yearID"))
scheddf <- left_join(scheddf,hometeam, by = c("ht","yearID")) 
scheddf <- scheddf %>%
  mutate(PD = as - hs) %>%
  mutate(awaysched = ifelse(PD >0, PD*HFS, PD*(1-HFS))) %>%
  mutate(homesched = ifelse(PD >0, -1*PD*(1-AFS),-1* PD*AFS)) %>%
  group_by(at, yearID) %>%
  mutate(gamenum1 = c(1:length(PD))) %>%
  mutate(marker1 = max(gamenum1)) %>%
  ungroup()  %>%
  group_by(ht, yearID) %>%
  mutate(gamenum2 = c(1:length(PD))) %>%
  mutate(marker2 = max(gamenum2)) %>%
  ungroup() 
schedpo<- scheddf%>%
  group_by(yearID) %>%
  mutate(marker1 = min(marker1)) %>%
  mutate(marker2 = min(marker2)) %>%
  filter(gamenum1>marker1& gamenum2>marker2) %>%
  ungroup() %>%
  select(-marker1,-marker2,-gamenum1,-gamenum2)
scheddf<- scheddf%>%
  group_by(yearID) %>%
  mutate(marker1 = min(marker1)) %>%
  mutate(marker2 = min(marker2)) %>%
  filter(gamenum1<=marker1& gamenum2<= marker2) %>%
  ungroup() %>%
  select(-marker1,-marker2,-gamenum1,-gamenum2)
aschedfs <- scheddf %>%
  group_by(at,yearID) %>%
  mutate(afss = sum(awaysched)) %>%
  mutate(ag = length(awaysched)) %>%
  slice(1) %>%
  ungroup() %>%
  select("Team"= "at", afss, ag, yearID)
hschedfs <- scheddf %>%
  group_by(ht,yearID) %>%
  mutate(hfss = sum(homesched)) %>%
  mutate(hg = length(homesched)) %>%
  slice(1) %>%
  ungroup() %>%
  select("Team"= "ht", hfss, hg, yearID)
schedfs <- left_join(aschedfs, hschedfs, by = c("Team","yearID")) %>%
  mutate(schedfs= (afss+hfss)/ (ag+hg)) %>%
  group_by(yearID) %>%
  mutate(schedfs = (schedfs-mean(schedfs))/ sd(schedfs)) %>%
  mutate(SchedFS = (3+schedfs)/6) %>%
  ungroup() %>%
  select(Team, yearID, SchedFS)
teamdf <- left_join(teams, schedfs, by=c("Team", "yearID")) %>%
  rename("TeamFS"="FloStrength") %>%
  mutate(FloStrength = (TeamFS+SchedFS)/2)

playoff<- schedpo %>%
  filter(yearID == 1980)
at<-last(playoff$at)
ht<-last(playoff$ht)
as<-last(playoff$as)
hs<-last(playoff$hs)
df <- data.frame(at,ht,as,hs) %>%
  mutate(NBAChamp= ifelse(as>hs,at,ht)) %>%
  mutate(ConfChamp= ifelse(as>hs,ht,at)) %>%
  mutate(yearID = 1980) %>%
  select(NBAChamp, ConfChamp,yearID)
x<-playoff %>% filter((at == df$NBAChamp & ht!=df$ConfChamp)|(ht == df$NBAChamp & at!=df$ConfChamp))
at<-last(x$at)
ht<-last(x$ht)
y<-playoff %>% filter((at != df$NBAChamp & ht==df$ConfChamp)|(ht != df$NBAChamp & at==df$ConfChamp))
aty<-last(y$at)
hty<-last(y$ht)
df <- df %>%
  mutate(DivChamp1 = ifelse(at==NBAChamp, ht, at))%>%
  mutate(DivChamp2 = ifelse(aty==ConfChamp, hty, aty))
for (i in c(1981: max(schedpo$yearID))){
  playoff<- schedpo %>%
    filter(yearID == i)
  at<-last(playoff$at)
  ht<-last(playoff$ht)
  as<-last(playoff$as)
  hs<-last(playoff$hs)
  df1 <- data.frame(at,ht,as,hs) %>%
    mutate(NBAChamp= ifelse(as>hs,at,ht)) %>%
    mutate(ConfChamp= ifelse(as>hs,ht,at)) %>%
    mutate(yearID = i) %>%
    select(NBAChamp, ConfChamp,yearID)
  x<-playoff %>% filter((at == df1$NBAChamp & ht!=df1$ConfChamp)|(ht == df1$NBAChamp & at!=df1$ConfChamp))
  at<-last(x$at)
  ht<-last(x$ht)
  y<-playoff %>% filter((at != df1$NBAChamp & ht==df1$ConfChamp)|(ht != df1$NBAChamp & at==df1$ConfChamp))
  aty<-last(y$at)
  hty<-last(y$ht)
  df1 <- df1 %>%
    mutate(DivChamp1 = ifelse(at==NBAChamp, ht, at))%>%
    mutate(DivChamp2 = ifelse(aty==ConfChamp, hty, aty))
  
  df<- rbind(df,df1)
}

poteams1 <- schedpo %>%
  select("Team"="at", yearID)
poteams2 <- schedpo %>%
  select("Team"="ht", yearID)
poteams<- rbind(poteams1,poteams2) %>%
  group_by(Team, yearID) %>%
  slice(1) %>%
  ungroup()
poteams<- left_join(poteams, df, by = "yearID") 
pot<- poteams%>%
  mutate(POSuc = ifelse(Team ==NBAChamp, 4,NA))%>%
  mutate(POSuc = ifelse(Team ==ConfChamp & is.na(POSuc)==TRUE, 3,POSuc))%>%
  mutate(POSuc = ifelse(Team ==DivChamp1 & is.na(POSuc)==TRUE, 2,POSuc))%>%
  mutate(POSuc = ifelse(Team ==DivChamp2 & is.na(POSuc)==TRUE, 2,POSuc))%>%
  mutate(POSuc = ifelse(is.na(POSuc)==TRUE, 1,POSuc)) %>%
  select(yearID,Team,POSuc)
teamdf <- left_join(teamdf, pot, by=c("Team", "yearID"))  %>%
  mutate(POSuc = ifelse(is.na(POSuc)==TRUE, 0,POSuc)) %>%
  mutate(FloStrength = ifelse(FloStrength<0,-1*FloStrength,FloStrength))%>%
  mutate(POSuc = ifelse(yearID==2020&(Wpct<.47&Team!= "Orlando Magic"),0,POSuc))
write_csv(teamdf, "/Users/seanfloersch/FloStrength/NBAFloStrength/NBATeamsAT")
write_csv(scheddf, "/Users/seanfloersch/FloStrength/NBAFloStrength/NBAGamesAT")


playerteam <- players %>%
  group_by(yearID, Tm) %>%
  mutate(PlayerFS = sum(Value))%>%
  slice(1) %>%
  ungroup() %>%
  select(Tm,yearID, PlayerFS) %>%
  filter(Tm !="TOT") %>%
  group_by(yearID) %>%
  mutate(PlayerFS = (PlayerFS - mean(PlayerFS)))%>%
  ungroup()
gamesPY = teamdf %>%
  group_by(yearID) %>%
  slice(1) %>%
  ungroup() %>%
  select(yearID, G)
playerteam<- left_join(playerteam, gamesPY, by = "yearID") %>%
  mutate(PlayerFS = (PlayerFS+(G/2))/G)%>%
  select("Team"="Tm",yearID,PlayerFS)
write_csv(teamdf, "/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/NBATeamsAT")
write_csv(scheddf, "/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/NBAGamesAT")
