library(e1071)
library(randomForest)
library(gbm)
library(neuralnet)
getNBAmodelResults <- function(NBAModel){
  nbaplot <- NBAModel %>%
    mutate(day = str_remove(gameID, "[A-Z]{1,6}")) %>%
    mutate(daynum = NA) %>% 
    mutate(weeknum = NA)
  nbaplot$BookWin <- ifelse(nbaplot$BookWin == nbaplot$AwayTeam, 1, 0)
  for (i in c(1:(length(unique(nbaplot$day))+1))) {
    i = i
    x <- unique(nbaplot$day)[i]
    ind <- which(nbaplot$day == x)
    nbaplot$daynum[ind] <- length(unique(nbaplot$day)) -i
  }
  
  traindf <- nbaplot %>% 
    filter(daynum < 1) %>%
    select(AWin, TeamDiff, PlayerDiff, SchedDiff, PredDiff, last3Diff,last5Diff,last7Diff,last10Diff, BookWin)
  testdf <- nbaplot %>% 
    filter(daynum == 1)%>%
    select(gameID, AWin, TeamDiff, PlayerDiff, SchedDiff, PredDiff, last3Diff,last5Diff,last7Diff,last10Diff, BookWin, daynum)
  bookpred <- testdf$BookWin  
  actwin <- testdf$AWin
  games <- testdf$gameID
  daynum <- testdf$daynum
  logmod <- glm(AWin~TeamDiff+PlayerDiff+SchedDiff+PredDiff+last3Diff+last5Diff+last7Diff+last10Diff, data = traindf, family = binomial)
  svmmod = svm(AWin~PlayerDiff+TeamDiff+SchedDiff+PredDiff+last3Diff+last5Diff+last7Diff+last10Diff, data = traindf, type = 'C-classification', kernel = 'linear')
  svmpred <- as.vector(predict(svmmod, newdata = testdf, type = "response"))
  rfmod <- randomForest(AWin~TeamDiff+PlayerDiff+SchedDiff+PredDiff+last3Diff+last5Diff+last7Diff+last10Diff, data = traindf)
  nnmod  <- neuralnet(AWin~TeamDiff+PlayerDiff+SchedDiff+PredDiff+last3Diff+last5Diff+last7Diff+last10Diff, data = traindf, hidden=1,act.fct = "logistic",linear.output = FALSE)
  logpred <- predict(logmod, newdata = testdf, type = "response")
  logpred <- ifelse(logpred>.5, 1,0)
  rfpred<-as.vector(predict(rfmod, newdata = testdf))
  rfpred=ifelse(rfpred>0.5,1,0)
  nnpred<-(compute(nnmod,testdf))$net.result
  nnpred <- as.vector(ifelse(nnpred>0.5, 1, 0))
  compdf <- data.frame(games,logpred, svmpred, rfpred, nnpred, bookpred, actwin, daynum) %>% mutate(gbmpred = NA)
  
  for (i in c(2:7)){
    traindf <- nbaplot %>% 
      filter(daynum < i) %>%
      select(AWin, TeamDiff, PlayerDiff, SchedDiff, PredDiff, last3Diff,last5Diff,last7Diff,last10Diff)
    testdf <- nbaplot %>% 
      filter(daynum == i)%>%
      select(gameID, AWin, TeamDiff, PlayerDiff, SchedDiff, PredDiff, last3Diff,last5Diff,last7Diff,last10Diff, BookWin, daynum)
    bookpred <- testdf$BookWin  
    actwin <- testdf$AWin
    games <- testdf$gameID
    daynum <- testdf$daynum
    testdf <- nbaplot %>% 
      filter(daynum == i)%>%
      select(AWin, TeamDiff, PlayerDiff, SchedDiff, PredDiff, last3Diff,last5Diff,last7Diff,last10Diff)
    
    logmod <- glm(AWin~TeamDiff+PlayerDiff+SchedDiff+PredDiff+last3Diff+last5Diff+last7Diff+last10Diff, data = traindf, family = binomial)
    svmmod = svm(AWin~PlayerDiff+TeamDiff+SchedDiff+PredDiff+last3Diff+last5Diff+last7Diff+last10Diff, data = traindf, type = 'C-classification', kernel = 'linear')
    svmpred <- as.vector(predict(svmmod, newdata = testdf, type = "response"))
    rfmod <- randomForest(AWin~TeamDiff+PlayerDiff+SchedDiff+PredDiff+last3Diff+last5Diff+last7Diff+last10Diff, data = traindf)
    nnmod  <- neuralnet(AWin~TeamDiff+PlayerDiff+SchedDiff+PredDiff+last3Diff+last5Diff+last7Diff+last10Diff, data = traindf, hidden=1,act.fct = "logistic",linear.output = FALSE)
    logpred <- predict(logmod, newdata = testdf, type = "response")
    logpred <- ifelse(logpred>.5, 1,0)
    rfpred<-as.vector(predict(rfmod, newdata = testdf))
    rfpred=ifelse(rfpred>0.5,1,0)
    nnpred<-(compute(nnmod,testdf))$net.result
    nnpred <- as.vector(ifelse(nnpred>0.5, 1, 0))
    df <- data.frame(games,logpred, svmpred, rfpred, nnpred, bookpred, actwin,daynum) %>% mutate(gbmpred = NA)
    compdf <- rbind(compdf, df) 
  }
  for (i in c(8:(length(unique(nbaplot$daynum))-1))){
    traindf <- nbaplot %>% 
      filter(daynum < i) %>%
      select(AWin, TeamDiff, PlayerDiff, SchedDiff, PredDiff, last3Diff,last5Diff,last7Diff,last10Diff)
    testdf <- nbaplot %>% 
      filter(daynum == i)%>%
      select(gameID, AWin, TeamDiff, PlayerDiff, SchedDiff, PredDiff, last3Diff,last5Diff,last7Diff,last10Diff, BookWin, daynum)
    bookpred <- testdf$BookWin  
    actwin <- testdf$AWin
    games <- testdf$gameID
    daynum <- testdf$daynum
    testdf <- nbaplot %>% 
      filter(daynum == i)%>%
      select(AWin, TeamDiff, PlayerDiff, SchedDiff, PredDiff, last3Diff,last5Diff,last7Diff,last10Diff)
    
    logmod <- glm(AWin~TeamDiff+PlayerDiff+SchedDiff+PredDiff+last3Diff+last5Diff+last7Diff+last10Diff, data = traindf, family = binomial)
    svmmod = svm(AWin~PlayerDiff+TeamDiff+SchedDiff+PredDiff+last3Diff+last5Diff+last7Diff+last10Diff, data = traindf, type = 'C-classification', kernel = 'linear')
    svmpred <- as.vector(predict(svmmod, newdata = testdf, type = "response"))
    rfmod <- randomForest(AWin~TeamDiff+PlayerDiff+SchedDiff+PredDiff+last3Diff+last5Diff+last7Diff+last10Diff, data = traindf)
    nnmod  <- neuralnet(AWin~TeamDiff+PlayerDiff+SchedDiff+PredDiff+last3Diff+last5Diff+last7Diff+last10Diff, data = traindf, hidden=1,act.fct = "logistic",linear.output = FALSE)
    logpred <- predict(logmod, newdata = testdf, type = "response")
    logpred <- ifelse(logpred>.5, 1,0)
    rfpred<-as.vector(predict(rfmod, newdata = testdf))
    rfpred=ifelse(rfpred>0.5,1,0)
    nnpred<-(compute(nnmod,testdf))$net.result
    nnpred <- as.vector(ifelse(nnpred>0.5, 1, 0))
    gbmmod <- gbm(AWin~TeamDiff+PlayerDiff+SchedDiff+PredDiff+last3Diff+last5Diff+last7Diff+last10Diff, data = traindf,interaction.depth = 1,  distribution = "bernoulli",n.trees = 10000) 
    gbmpred <- predict(gbmmod, n.trees = gbmmod$n.trees, testdf, type = "response")
    gbmpred=ifelse(gbmpred>0.5,1,0)
    df <- data.frame(games,logpred, svmpred, rfpred, nnpred, gbmpred,bookpred, actwin,daynum) 
    compdf <- rbind(compdf, df) 
  }
  compdf <- compdf %>%
    mutate(weeknum = NA) %>%
    mutate(LogCorr = ifelse(logpred == actwin, 1,0))%>%
    mutate(SVMCorr = ifelse(svmpred == actwin, 1,0))%>%
    mutate(RFCorr = ifelse(rfpred == actwin, 1,0))%>%
    mutate(NNCorr = ifelse(nnpred == actwin, 1,0))%>%
    mutate(BookCorr = ifelse(bookpred == actwin, 1,0))%>%
    mutate(GBMCorr = ifelse(gbmpred == actwin, 1,0))
  numweeks <- round(max(compdf$daynum)/7)
  for (i in c(1: numweeks)){
    ind <- which(compdf$daynum < ((i * 7)+1) & compdf$daynum > ((i-1)*7))
    compdf$weeknum[ind] = i
  }
  
  nbaplot1 <- compdf %>%
    na.omit() %>%
    group_by(weeknum) %>%
    mutate(LogAcc = mean(LogCorr)) %>%
    mutate(NNAcc = mean(NNCorr)) %>%
    mutate(SVMAcc = mean(SVMCorr)) %>%
    mutate(RFAcc = mean(RFCorr)) %>%
    mutate(GBMAcc = mean(GBMCorr)) %>%
    mutate(BookAcc = mean(BookCorr)) %>%
    slice(1) %>%
    ungroup() %>%
    select(weeknum, LogAcc,NNAcc,SVMAcc,RFAcc, GBMAcc,BookAcc)
  return(nbaplot1)
}
NBAmodelResults <-getNBAmodelResults(NBAModel)
plotNBAModelResults <- function(NBAmodelResults){
  logdf<- NBAmodelResults %>%
    mutate(type = "log") %>%
    select(type, weeknum, "corr"="LogAcc")
  svmdf<- NBAmodelResults %>%
    mutate(type = "svm") %>%
    select(type, weeknum, "corr"="SVMAcc")
  rfdf<- NBAmodelResults %>%
    mutate(type = "rf") %>%
    select(type, weeknum, "corr"="RFAcc")
  gbmdf<- NBAmodelResults %>%
    mutate(type = "gbm") %>%
    select(type, weeknum, "corr"="GBMAcc")
  nndf<- NBAmodelResults %>%
    mutate(type = "nn") %>%
    select(type, weeknum, "corr"="NNAcc")
  bookdf<- NBAmodelResults %>%
    mutate(type = "book") %>%
    select(type, weeknum, "corr"="BookAcc")
  dfcomplete <- rbind(logdf, svmdf)
  dfcomplete <- rbind(dfcomplete, rfdf)
  dfcomplete <- rbind(dfcomplete, gbmdf)
  dfcomplete <- rbind(dfcomplete, nndf)
  dfcomplete <- rbind(dfcomplete, bookdf)
  daplot <- ggplot(data = dfcomplete, aes(x = as.character(weeknum), y = corr, col = type)) + geom_point() + geom_smooth(method = "loess")
  return(daplot)
}
nbamodplot <- plotNBAModelResults(NBAmodelResults)
nbamodplot
