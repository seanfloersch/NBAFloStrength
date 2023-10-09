xyz <- dfx %>% 
  mutate(Date = str_extract(gameID, "\\d{1,10}")) %>% 
  mutate(day = NA) %>% 
  mutate(coHome = NA) %>% 
  mutate(coFresh = NA) %>% 
  mutate(coTeam = NA) %>% 
  mutate(coSched = NA) %>% 
  mutate(coPlay = NA)
for (i in c(1:length(unique(xyz$Date)))) {
  n = unique(xyz$Date)[i]
  ind = which(xyz$Date == n)
  xyz$day[ind] = i
}
for (i in c(1:length(unique(xyz$day)))) {
  zyx <- xyz %>%
    filter(day <= i)
  a <- lm(PD~FloProbTeam+FloProbPlayer+HomeWin+FreshWin+FloProbSched, data = zyx)
  ind = which(xyz$day == i)
  xyz$coTeam[ind] = a$coefficients[2]
  xyz$coPlay[ind] = a$coefficients[3]
  xyz$coHome[ind] = a$coefficients[4]
  xyz$coFresh[ind] = a$coefficients[5]
  xyz$coSched[ind] = a$coefficients[6]
  
}
xyz <- xyz %>% group_by(day) %>% slice(1)
plot(xyz$day, xyz$coHome)
plot(xyz$day, xyz$coTeam)
plot(xyz$day, xyz$coPlay)
plot(xyz$day, xyz$coFresh)
plot(xyz$day, xyz$coSched)
