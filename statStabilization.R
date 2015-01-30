# function statStabilization - calculates the pValue for
# a number of players, using their tried attempts (tA) 
# and tried Mades (tM)
# Resulting pStats are two tailed pValues -log(pVal)
# positive pStats is for players with above average percentage
# negative pStats for players with below average percentage
# Example, if player A has a 3 point percentage above league average
# and the calculated pValues is 0.05 
# (95% probability that the stat is not random)
# -> his pStats value is -log(0.05)=2.996
# For a below average shooter with negative pValue it would be log(0.05)=-2.996
# names are test object names (usually player names)
# paraName is to save the figures with an according name

#install.packages(c("wordcloud","tm"),repos="http://cran.r-project.org")
library(wordcloud)
library(ggplot2)

statStabilization <- function(tA,tM,names,paraName) {
  nbArgs=nargs()
  
  #PARAMETERS
  #confidence Interval we use for statistical outliers
  confInt=0.05
  # use only players with at least minA attempts
  minA=50
  
  #DATA PREPARATION
#   #Getting rid of duplicates (e.g. traded players). Warning! This does not necessarily work
#   if (nbArgs>2){
#     nonDup=!duplicated(names)
#     tA=tA[nonDup]
#     tM=tM[nonDup]
#     names=names[nonDup]
#   }
  
  #place filler in case paraName is not given (figures won't be saved!)
  if (nbArgs<4){
    paraName="VAR"
  }
  
  nbPlayers<-length(tA)
  
  # percentage for each player
  pctM=tM/tA*100
  
  # estimate an average player (note: there are different possibilities)
  # avgMade=sum(tM)/sum(tA)
  avgMade=median(pctM[tA>minA])
  print(sprintf("median percentage (min attempts %d):",minA))
  print(avgMade)
  print("percentage for all attempts:")
  print(sum(tM)/sum(tA)*100)
  
  
  # sort unique Elements of Attempts (speeds up calculations)
  uniA<-unique(tA)
  maxA<-max(uniA)
  #estimate pValues for each possible outcome of each uniA,
  # using a binomial distribution with the given average Probability
  listPVal<- vector("list", maxA+1)
  #upBound and lowBound calculate the upper and lower threshold for the confidence intervall
  upBound <- rep(NA, maxA+1)
  lowBound <- rep(NA, maxA+1)
  for (tmpA in 0:max(uniA)){
    listPVal[[tmpA+1]]<-dbinom(0:tmpA, tmpA, avgMade/100, log = FALSE)
    lowBound[tmpA+1]=(sum(cumsum(listPVal[[tmpA+1]])<confInt/2))/tmpA*100
    upBound[tmpA+1]=(tmpA+1-(sum(cumsum(listPVal[[tmpA+1]])>(1-confInt/2))))/tmpA*100
  }

  
  # for each player estimate pStats 
  # (random chance that an avgMade player would get 
  # same or worse outcome / better outcome)
  # *2 because of two tailed
  pStats=numeric(nbPlayers)
  for (i in 1:nbPlayers){
    if (tA[i]>0){
      if (pctM[i]<=avgMade){
        #same outcome or worse
        tmpSum=min(1,2*sum(listPVal[[tA[i]+1]][1:tM[i]+1]))
        pStats[i]<-log(tmpSum)
      } else {
        #better outcome
        tmpSum=min(1,2*sum(listPVal[[tA[i]+1]][(tM[i]+1):(tA[i]+1)]))
        pStats[i]<--log(tmpSum)
      } 
    } else {
      pStats[i]<-0
    }
  }
  
  print("percentage of outliers:")
  print(sum(tA>minA & exp(-abs(pStats))<confInt)/sum(tA>minA))
  

  #VISUALIZATION: attemptsv VS pValue
  if (nbArgs>3){
    png(file=sprintf('%s_pStats.png',paraName),width=800,height=700)
  }
  if (nbArgs>2){
    loc<- matrix(data=NA,nrow=nbPlayers,ncol=2)
    loc[,1]<-tA
    loc[,2]<-pStats
    mx <- apply(loc,2,max)
    mn <- apply(loc,2,min)
    #print those names for which random occurence has less than confInt probability
    textplot(tA[abs(pStats)>-log(confInt) & tA>minA], pStats[abs(pStats)>-log(confInt) & tA>minA], names[abs(pStats)>-log(confInt) & tA>minA],cex=0.7,xlim=c(mn[1],mx[1]),ylim=c(mn[2],mx[2]),main=sprintf("%s: %.1f%% confidence intervall",paraName,100*(1-confInt)),xlab="Number of attempts",ylab="log(pVal)")
    points(tA, pStats,col="blue", pch = 1, cex = 0.3, lty = "solid", lwd = 2)
  }else{
    plot(tA, pStats,col="blue", pch = 1, cex = 0.3, lty = "solid", lwd = 2)
  }
  lines(c(min(tA),max(tA)),c(-log(confInt),-log(confInt)), lty=3)
  lines(c(min(tA),max(tA)),c(log(confInt),log(confInt)), lty=3)
  lines(c(minA-0.5,minA-0.5),c(min(pStats),max(pStats)), lty=3)

  if (nbArgs>3){
    dev.off()
  }
  

  #VISUALIZATION: attemptsv VS percent made
  if (nbArgs>3){
    png(file=sprintf('%s_percent.png',paraName),width=800,height=700)
  }
  print(pStats[pStats>10])
  print(names[pStats>10])
  loc2<- matrix(data=NA,nrow=sum(tA>minA),ncol=2)
  loc2[,1]<-tA[tA>minA]
  loc2[,2]<-pctM[tA>minA]

#ESTIMATE correlation and a linear fit (to show for example that better players take more shots)
  corrPct=cor(loc2[,1],loc2[,2])
# linear fit could be weighted by attempts
#  res=lm(loc2[,2] ~ loc2[,1],weights=loc2[,1])
  res=lm(loc2[,2] ~ loc2[,1])

  mx <- c(max(tA),max(pctM[tA>0]))
  mn <- c(min(tA),min(pctM[tA>0]))
  if (nbArgs>2){
    textplot(tA[abs(pStats)>-log(confInt) & tA>minA], pctM[abs(pStats)>-log(confInt) & tA>minA], names[abs(pStats)>-log(confInt) & tA>minA],cex=0.7,xlim=c(mn[1],mx[1]),ylim=c(mn[2],mx[2]),main=sprintf("%s: %.1f%% confidence intervall, Corr: %.3f",paraName,100*(1-confInt),corrPct) ,xlab="Number of attempts",ylab="% made")
    points(tA, pctM,col="blue", pch = 1, cex = 0.3, lty = "solid", lwd = 2)
  }else{
    plot(tA, pctM,col="blue", pch = 1, cex = 0.3, lty = "solid", lwd = 2,main=sprintf("%s: %.1f%% confidence intervall, Corr: %.3f",paraName,100*(1-confInt),corrPct) ,xlab="Number of attempts",ylab="% made")
  }
  lines(c(min(tA),max(tA)),c(avgMade,avgMade))
  abline(res, col="red")
  lines(0:maxA,lowBound, lty=3)
  lines(0:maxA,upBound, lty=3)
  lines(c(minA-0.5,minA-0.5),c(min(pctM[tA>0]),max(pctM[tA>0])), lty=3)
  
  if (nbArgs>3){
    dev.off()
  }
  

#VISUALIZATION: real shooting percentage distribution VS estimated binomial distribution
  if (nbArgs>3){
    png(file=sprintf('%s_distribution.png',paraName),width=800,height=700)
  }
  nbSim=100
  tA2=tA[tA>minA]
  tM2=tM[tA>minA]
  pctM2=pctM[tA>minA]
  pctSim<- matrix(data=NA,nrow=nbSim*length(tA2),ncol=1)
  icount=0
  for (i in 1:length(tA2)){
    randTMP=runif(nbSim,0,1)
    for (j in 1:nbSim){
      icount=icount+1
      pctSim[icount]=sum(cumsum(listPVal[[tA2[i]+1]])<randTMP[j])/tA2[i]*100
    }
  }
  
  #HISTOGRAM
  pctMHist <- data.frame(percentMade = pctM2)
  pctSimHist <- data.frame(percentMade = pctSim)
  pctMHist$hist <- sprintf('real %s %% Distribution',paraName)
  pctSimHist$hist <- sprintf('sim %s %% Distribution',paraName)
  distPct <- rbind(pctMHist, pctSimHist)
  print(ggplot(distPct, aes(percentMade, fill = hist)) + geom_density(alpha = 0.2))
  if (nbArgs>3){
    dev.off()
  }


  return(pStats) 
}


