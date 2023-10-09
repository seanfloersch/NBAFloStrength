dfan <- NBACompleteOdds
dfan <- dfan %>% select(gameID, FloWin, WTeam, ATeam, HTeam, HomeWin, FreshWin, FloProbTeam, FloProbPlayer, FloProbSched, PD, BookSpread, BookWin, BookCorrect)%>% na.omit %>%
mutate(FloProbPlayer = 2* (FloProbPlayer - .5)) %>%
mutate(FloProbTeam = 2* (FloProbTeam - .5)) %>%
mutate(FloProbSched = 2* (FloProbSched - .5)) 
dfan<- dfan %>% mutate(HomeWin =1)
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
mutate(gr =a$coefficients[2] * FloProbTeam+ a$coefficients[3] * FloProbPlayer+  a$coefficients[6]*FloProbSched+ a$coefficients[4] *HomeWin+ a$coefficients[5] * FreshWin) 

dfx <- dfx %>% mutate(diff = abs(gr - PD))

abcd <- quantile(abs(dfx$gr - dfx$PD))
outlier <- (abcd[4] + 1.5 *(abcd[4]- abcd[2])) %>% as.numeric
dfline <- dfx %>% filter(diff < outlier)
a <- lm(PD~FloProbTeam+FloProbPlayer+HomeWin+FreshWin+FloProbSched, data = dfline)
dfx <- dfx %>%
  mutate(gr =a$coefficients[2] * FloProbTeam+ a$coefficients[3] * FloProbPlayer+  a$coefficients[6]*FloProbSched+ a$coefficients[4] *HomeWin+ a$coefficients[5] * FreshWin) 

ggplot(dfx, aes(x = gr, y = PD)) +
geom_point()+geom_smooth(method = "lm", se = TRUE)
cor(dfx$gr, dfx$PD)


dfy <- dfx %>% filter(gr>0) %>%
mutate(PredWinner = NA)

ind <- which(dfy$PD > 0 & dfy$gr >0)
dfy$PredWinner[ind] <- dfy$WTeam[ind]

ind <- which(dfy$PD <0 & dfy$WTeam != dfy$ATeam)
dfy$PredWinner[ind] <- dfy$ATeam[ind]
ind <- which(dfy$PD <0 & dfy$WTeam != dfy$HTeam)
dfy$PredWinner[ind] <- dfy$HTeam[ind]
dfy <- dfy %>%
select(gameID, PredWinner, BookWin, WTeam, BookSpread, PD, gr, FloProbPlayer, FloProbSched, FloProbTeam, HomeWin, FreshWin, BookCorrect, ATeam, HTeam) %>%
mutate(BookSpread = -1 * BookSpread) %>%
mutate(SpreadCorrect = NA) %>%
mutate(WinCorrect = NA)
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
ind <- which(abs(dfy$PD) == dfy$BookSpread)
dfy$SpreadCorrect[ind] <- mean(dfy$SpreadCorrect)
ind <- which(dfy$PredWinner == dfy$WTeam )
dfy$WinCorrect[ind] <- 1
ind <- which(is.na(dfy$WinCorrect))
dfy$WinCorrect[ind] <- 0

cor(dfy$gr, dfy$PD)
mean(dfy$WinCorrect)
mean(dfy$BookCorrect)

dfy <- dfy %>%
  mutate(WinCorrect = as.character(WinCorrect))

ggplot(dfy, aes(x = gr, y = PD, col = WinCorrect)) +
geom_point() +
scale_color_discrete(name = "Result", labels = c("Loss", "Win", "Line of Best Fit"))+
scale_fill_manual(c("red1", "limegreen")) +
geom_hline(yintercept = 0, col = "red1") +
geom_abline(aes(intercept=0, slope = 1, col = "blue1"))+
ggtitle("FloStrength Spread and Winner Predictions") +
xlab("Flo Point Differential") +
ylab("Point Differential") +
xlim(0, 30) +
ylim(-35, 45)



dfz <- NBAOdds %>%
mutate(AwayFresh = Fresh.x - Fresh.y) %>%
mutate(Away = -1) %>%
mutate(FloProbPlayer = 2* (FloProbPlayer - .5)) %>%
mutate(FloProbTeam = 2* (FloProbTeam - .5)) %>%
mutate(FloProbSched = 2* (FloProbSched - .5)) 



ind <- which(dfz$FloWin != dfz$ATeam)
dfz$Away[ind] <- -1 *dfz$Away[ind]
dfz$AwayFresh[ind] <- -1 *dfz$AwayFresh[ind]
dfz <- dfz %>%
mutate(gr =round(a$coefficients[2] * FloProbTeam+ a$coefficients[3] * FloProbPlayer+  a$coefficients[6]*FloProbSched+ a$coefficients[4] *Away+ a$coefficients[5] * AwayFresh), 3) 
ind <- which(dfz$gr > 0 & dfz$Away == -1)
dfz$FloWin[ind] = dfz$ATeam[ind]
ind <- which(dfz$gr > 0 & dfz$Away == 1)
dfz$FloWin[ind] = dfz$HTeam[ind]
ind <- which(dfz$gr < 0 & dfz$Away == -1)
dfz$FloWin[ind] = dfz$HTeam[ind]
ind <- which(dfz$gr < 0 & dfz$Away == 1)
dfz$FloWin[ind] = dfz$ATeam[ind]
dfz <- dfz %>%
select(gameID, ATeam, HTeam, BookWin, FloWin, gr, BookSpread) %>%
mutate(gr = abs(gr)) %>%
mutate(BookSpread = -1 * BookSpread) %>%
mutate(SpreadProb = NA)
dfhist <- dfy  %>% mutate(diff = PD - gr)
hist(dfhist$diff)
sdx <- sd(dfhist$diff)
meanx <- mean(dfhist$diff)
dfz <- dfz %>% mutate(gr = gr) %>% mutate(WinProb = NA)
dfhist <- dfhist %>% mutate(diff = PD -gr)
ind <- which(dfz$BookWin == dfz$FloWin & dfz$BookSpread > dfz$gr)
dfz$SpreadProb[ind] = pnorm((dfz$BookSpread[ind]-dfz$gr[ind]), mean = 0, sd = sdx)
ind <- which(dfz$BookWin == dfz$FloWin & dfz$BookSpread < dfz$gr)
dfz$SpreadProb[ind] = pnorm((dfz$BookSpread[ind]-dfz$gr[ind]), mean = 0, sd = sdx, lower.tail = FALSE)
ind <- which(dfz$BookWin != dfz$FloWin)
dfz$SpreadProb[ind] = pnorm((-1 *(dfz$BookSpread[ind]+dfz$gr[ind])), mean = 0, sd = sdx, lower.tail = FALSE)
ind <- which(dfz$gr > 0)
dfz$WinProb[ind] = pnorm((dfz$gr[ind]), mean = 0, sd = sdx)

dfhist <- dfhist %>% mutate(SpreadProb = NA) %>% mutate(WinProb = NA)
ind <- which(dfhist$BookWin == dfhist$PredWinner & dfhist$BookSpread > dfhist$gr)
dfhist$SpreadProb[ind] = pnorm((dfhist$BookSpread[ind]-dfhist$gr[ind]), mean = 0, sd = sdx)
ind <- which(dfhist$BookWin == dfhist$PredWinner & dfhist$BookSpread < dfhist$gr)
dfhist$SpreadProb[ind] = pnorm((dfhist$BookSpread[ind]-dfhist$gr[ind]), mean = 0, sd = sdx, lower.tail = FALSE)
ind <- which(dfhist$BookWin != dfhist$PredWinner)
dfhist$SpreadProb[ind] = pnorm((-1 *(dfhist$BookSpread[ind]+dfhist$gr[ind])), mean = 0, sd = sdx, lower.tail = FALSE)

ind <- which(dfhist$gr > 0)
dfhist$WinProb[ind] = pnorm((dfhist$gr[ind]), mean = 0, sd = sdx)
dfhist <- dfhist %>% mutate(WinCorrect = as.numeric(WinCorrect))
mean(dfhist$WinCorrect) 
mean(dfhist$WinProb)
mean(dfhist$SpreadProb)
mean(dfhist$SpreadCorrect)

dfhist <-dfhist %>% mutate(SpreadCorrect = as.character(SpreadCorrect))
ggplot(dfy, aes(x = WinProb, y = gr, col = WinCorrect)) +
geom_point() 



dfhist <- dfy %>% mutate(bpdiff=NA)
ind <- which(dfhist$BookWin == dfhist$PredWinner)
dfhist$bpdiff[ind] =(dfhist$BookSpread[ind]-dfhist$gr[ind])
ind <- which(dfhist$BookWin != dfhist$PredWinner)
dfhist$bpdiff[ind] =-1 * (dfhist$BookSpread[ind]+dfhist$gr[ind])
ggplot(dfhist, aes(x = WinProb, y = -1 *diff, col = WinCorrect)) +
geom_point() 

ateam <- dfy %>% group_by(ATeam) %>%mutate(SpreadCorrect = sum(as.numeric(SpreadCorrect))) %>% mutate(count = 1) %>% mutate(WinCorrect = sum(WinCorrect)) %>% mutate(count = sum(count))%>% slice(1) %>% select("Team" = ATeam, WinCorrect, SpreadCorrect, count)
hteam <- dfy %>% group_by(HTeam) %>%mutate(SpreadCorrect = sum(as.numeric(SpreadCorrect))) %>% mutate(count = 1) %>% mutate(WinCorrect = sum(WinCorrect)) %>% mutate(count = sum(count))%>% slice(1) %>% select("Team" = HTeam, WinCorrect, SpreadCorrect, count)
check <- merge(ateam, hteam, by = "Team")
check <- check %>% mutate(SpreadCorrect = SpreadCorrect.x +SpreadCorrect.y) %>% mutate(Count = count.x + count.y) %>% mutate(WinCorrect = WinCorrect.x+WinCorrect.y) %>% select(Team, WinCorrect, SpreadCorrect, Count) %>% mutate(WinCorrect = WinCorrect / Count)%>% mutate(SpreadCorrect = SpreadCorrect / Count)
checka <- check %>% rename("ATeam" = Team)
checkh <- check %>% rename("HTeam" = Team)

lol <- merge(dfy, checka, by = "ATeam")
lolol <- merge(lol, checkh, by ="HTeam") %>% mutate(SpreadTeam = (SpreadCorrect + SpreadCorrect.y)/2) %>% mutate(WinTeam = (WinCorrect + WinCorrect.y)/2) %>% mutate(NewWinProb = (WinTeam + WinProb)/2)%>% mutate(NewSpreadProb = (SpreadTeam + SpreadProb)/2)

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
ggplot(dfy, aes(x = WinProb, y = gr, col = WinCorrect)) +
    geom_point() 
