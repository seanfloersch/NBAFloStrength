allnbaplayersdf <- read.csv("~/FloStrength/FloStrengthHistoryApp/NBAPlayersAT") %>%
  mutate(MPG=MP/G)%>%
  mutate(Player=Name)
PredPlayers <- function(year1,year2){
  knownplayersfun<- function(year){
    a <- allnbaplayersdf %>%
    filter(yearID == year) %>%
    select(Player,Tm, Age,MPG, FloStrength, Value, yearID)
  b <- allnbaplayersdf %>%
    filter(yearID == year-1) %>%
    select(Player,MPG, "FloStrength1"="FloStrength", "Value1"=Value)
  c <- allnbaplayersdf %>%
    filter(yearID == year-2) %>%
    select(Player,MPG, "FloStrength2"="FloStrength", "Value2"=Value)
  
  df <- left_join(a, b, by = "Player")
  df <- left_join(df, c, by = "Player") %>%
    mutate(YFP = 26 - Age)
  known <- df %>%
    na.omit 
   return(known)
  }
  knownplay <- map_df(.x= (year1:year2), .f=knownplayersfun)
  linmod <- lm(FloStrength~FloStrength1+FloStrength2+YFP, data = knownplay)
  linmod1 <- lm(MPG.x~MPG.y+MPG+YFP, data = knownplay)
  
  knownplay <- knownplay %>%
    mutate(FSPred = linmod$coefficients[1]+ linmod$coefficients[2] * FloStrength1 + linmod$coefficients[3] * FloStrength2 + linmod$coefficients[4]*YFP)%>%
    mutate(MPGPred = linmod1$coefficients[1]+ linmod1$coefficients[2] * MPG.y + linmod1$coefficients[3] * MPG + linmod1$coefficients[4]*YFP)
  
  unknownplayersfun<- function(year){
    a <- allnbaplayersdf %>%
      filter(yearID == year) %>%
      select(Player,Tm, Age,MPG, FloStrength, Value, yearID)
    b <- allnbaplayersdf %>%
      filter(yearID == year-1) %>%
      select(Player,MPG, "FloStrength1"="FloStrength", "Value1"=Value)
    c <- allnbaplayersdf %>%
      filter(yearID == year-2) %>%
      select(Player,MPG, "FloStrength2"="FloStrength", "Value2"=Value)
    
    df <- left_join(a, b, by = "Player")
    df <- left_join(df, c, by = "Player") %>%
      mutate(YFP = 26 - Age)
    unknown <- df %>%
      filter(is.na(FloStrength1) == TRUE & is.na(FloStrength2) == TRUE)
    return(unknown)
  }
  unknownplay <- map_df(.x= (year1:year2), .f=unknownplayersfun)
  unknownplay <- unknownplay %>%
    mutate(FSPred = mean(FloStrength))%>%
    mutate(MPGPred = mean(MPG.x))
  misslastfun<- function(year){
    a <- allnbaplayersdf %>%
      filter(yearID == year) %>%
      select(Player,Tm, Age,MPG, FloStrength, Value, yearID)
    b <- allnbaplayersdf %>%
      filter(yearID == year-1) %>%
      select(Player,MPG, "FloStrength1"="FloStrength", "Value1"=Value)
    c <- allnbaplayersdf %>%
      filter(yearID == year-2) %>%
      select(Player,MPG, "FloStrength2"="FloStrength", "Value2"=Value)
    
    df <- left_join(a, b, by = "Player")
    df <- left_join(df, c, by = "Player") %>%
      mutate(YFP = 26 - Age)
    missedlast <- df %>%
      filter(is.na(FloStrength1) == TRUE & is.na(FloStrength2) == FALSE) 
    return(missedlast)
  }
  missedlast <- map_df(.x= (year1:year2), .f=misslastfun)
  linmod <- lm(FloStrength~FloStrength2+YFP, data = missedlast)
  linmod1 <- lm(MPG.x~MPG+YFP, data = missedlast)
  missedlast <- missedlast %>%
    mutate(FSPred = linmod$coefficients[1]+ linmod$coefficients[2] * FloStrength2 + linmod$coefficients[3] * YFP) %>%
    mutate(MPGPred = linmod1$coefficients[1]+ linmod1$coefficients[2] * MPG + linmod1$coefficients[3] * YFP)
  
  secyearfun<- function(year){
    a <- allnbaplayersdf %>%
      filter(yearID == year) %>%
      select(Player,Tm, Age,MPG, FloStrength, Value, yearID)
    b <- allnbaplayersdf %>%
      filter(yearID == year-1) %>%
      select(Player,MPG, "FloStrength1"="FloStrength", "Value1"=Value)
    c <- allnbaplayersdf %>%
      filter(yearID == year-2) %>%
      select(Player,MPG, "FloStrength2"="FloStrength", "Value2"=Value)
    
    df <- left_join(a, b, by = "Player")
    df <- left_join(df, c, by = "Player") %>%
      mutate(YFP = 26 - Age)
    secondyear <- df %>%
      filter(is.na(FloStrength1) == FALSE & is.na(FloStrength2) == TRUE) 
    return(secondyear)
  }
  secondyear <- map_df(.x= (year1:year2), .f=secyearfun)

  linmod <- lm(FloStrength~FloStrength1+YFP, data = secondyear)
  linmod1 <- lm(MPG.x~MPG.y+YFP, data = secondyear)
  secondyear <- secondyear %>%
    mutate(FSPred = linmod$coefficients[1]+ linmod$coefficients[2] * FloStrength1 + linmod$coefficients[3] * YFP) %>%
    mutate(MPGPred = linmod1$coefficients[1]+ linmod1$coefficients[2] * MPG.y + linmod1$coefficients[3] * YFP)
  preddf <- rbind(knownplay, unknownplay)
  preddf <- rbind(preddf, missedlast)
  preddf <- rbind(preddf, secondyear) %>%
    select(Player,MPG.x, MPGPred,Tm, yearID, FloStrength, FSPred)
  return(preddf)
}
preddf <- PredPlayers(1982,2023)
predteam <- preddf %>%
  filter(Tm != "TOT") %>%
  group_by(Tm, yearID) %>%
  arrange(-FSPred) %>%
  slice(1:12) %>%
  mutate(WFS = FSPred * (MPGPred / sum(MPGPred))) %>%
  ungroup() %>%
  group_by(Tm, yearID) %>%
  mutate(FSPlayPred = sum(WFS)) %>%
  slice(1) %>%
  select(Tm, yearID, FSPlayPred) %>%
  ungroup %>%
  group_by(yearID) %>%
  mutate(FSPlayPred = (FSPlayPred - mean(FSPlayPred)) / sd(FSPlayPred))
ind <- which(predteam$Tm == "CHH")
predteam$Tm[ind] = "CHA"
ind <- which(predteam$Tm == "CHO")
predteam$Tm[ind] = "CHA"
teampred<-read.csv("~/FloStrength/NBAFloStrength/NBATeamPredAT") %>%
  select(yearID, "Tm"="Team", FloStrength, Wpct)
teampred <- left_join(teampred, predteam, c("yearID", "Tm")) %>%
  na.omit
lastyr <- read.csv("~/FloStrength/FloStrengthHistoryApp/NBATeamsAT")  %>%
  rename("Tm"="Team")%>%
  mutate(yearID = yearID + 1) %>%
  select(yearID, Tm,"LYFS"= FloStrength,"LYWP" = Wpct) %>%
  group_by(yearID) %>%
  arrange(LYWP) %>%
  mutate(Draft = c(1:length(Tm)))%>%
  ungroup
  
teampred <- left_join(teampred, lastyr, c("yearID", "Tm")) %>%
  na.omit
teampred<-read.csv("~/FloStrength/NBAFloStrength/NBATeamPredAT")%>%
  group_by(yearID) %>%
  arrange(LYWpct) %>%
  mutate(Draft = c(1:length(Team)))%>%
  ungroup
linmod <- lm(Wpct~LYFS+PlayRating+as.factor(POSuc), data = teampred)
teampred <- teampred %>%
  mutate(Wpct = linmod$coefficients[1]+ linmod$coefficients[2] *LYFS + linmod$coefficients[3]* FSPlayPred)
cor(teampred$predWP, teampred$Wpct)
plot(teampred$predWP, teampred$Wpct)


