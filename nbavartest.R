test <- nbaplayers %>%
  group_by(Tm, yearID) %>%
  mutate(Defense = mean(Defense))%>%
  mutate(Scoring = mean(Scoring))%>%
  mutate(Passing = mean(Passing))%>%
  mutate(Rebounding = mean(Rebounding)) %>%
  slice(1) %>%
  ungroup() %>%
  select(Tm, yearID, Defense, Scoring, Passing, Rebounding)
teams <- c("Atlanta Hawks" = "ATL","San Antonio Spurs" = "SAS","Los Angeles Lakers" = "LAL","Cleveland Cavaliers" = "CLE","New York Knicks" = "NYK","Boston Celtics" = "BOS","Indiana Pacers" = "IND","Phoenix Suns" = "PHO","Houston Rockets" = "HOU","Milwaukee Bucks" = "MIL","Philadelphia 76ers" = "PHI","Detroit Pistons" = "DET","Seattle SuperSonics" = "SEA","New Jersey Nets" = "NJN","Denver Nuggets" = "DEN","Kansas City Kings" = "KCK","San Diego Clippers" = "SDC","Chicago Bulls" = "CHI","Washington Bullets" = "WSB","Golden State Warriors" = "GSW","Portland Trail Blazers" = "POR","Charlotte Hornets" = "CHO","Miami Heat" = "MIA","Orlando Magic" = "ORL","Sacramento Kings" = "SAC","Los Angeles Clippers" = "LAC","Dallas Mavericks" = "DAL","Utah Jazz" = "UTA","Charlotte Bobcats" = "CHA","New Orleans Hornets" = "NOH","Memphis Grizzlies" = "MEM","Washington Wizards" = "WAS","Vancouver Grizzlies" = "VAN","Toronto Raptors" = "TOR","Minnesota Timberwolves" = "MIN","New Orleans/Oklahoma City Hornets" = "NOK","Oklahoma City Thunder" = "OKC","Brooklyn Nets" = "BRK","New Orleans Pelicans" = "NOP")
teamtest <- teamstats %>%
  mutate(wpct = W / (W+L)) %>%
  select("Tm" ="Team", yearID, wpct)
teamtest$Tm <-as.character(teams[teamtest$Tm])
letsee <- left_join(test, teamtest, by = c("Tm", "yearID")) %>% na.omit()
linny <- lm(wpct~Defense+Scoring+Passing+Rebounding, data = letsee)
