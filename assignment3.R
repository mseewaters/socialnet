setwd("D:/0 Stern MSBA/2.3 Network analytics/propensityMatching")

g <- read.delim("D:/0 Stern MSBA/2.3 Network analytics/post module/socialnet/completeGraph.csv", header=FALSE)
all <- read.csv("D:/0 Stern MSBA/2.3 Network analytics/propensityMatching/allUsers.csv")
wc <- read.csv("D:/0 Stern MSBA/2.3 Network analytics/propensityMatching/worldcup.csv")
selfie <- read.csv("D:/0 Stern MSBA/2.3 Network analytics/propensityMatching/selfie.csv")
love <- read.csv("D:/0 Stern MSBA/2.3 Network analytics/propensityMatching/love.csv")
tbt <- read.csv("D:/0 Stern MSBA/2.3 Network analytics/propensityMatching/tbt.csv")

# Create table of users that follow someone
colnames(g) <- c("from","to")
library(data.table)
t1 <- data.table(g)[ , list(num_follows=length(from)), by = to] 
colnames(t1) <- c("id","num_follows")

# Merge users that follow someone and allUsers
# Creating a table of users with followers and all attributes
t2 <- as.data.frame(merge(t1, all, by = "id"))


create.formatch <- function(h) 
{
  # Calculate mean timestamp for each hashtag
  tm <- median(h$timeStamp)
  
  # Create table of users with last tweet after tm
  t3 <- t2[t2$timeStamp > tm,]
    
  # Create vectors with initial setting as FALSE
  treated = rep(FALSE, times = nrow(t3))
  adopted = rep(FALSE, times = nrow(t3))

  # A user is treated if
  # user tt > tm  (the user has tweeted something after the "treatment" period)
  # friend th < t*  (someone they follow tweeted the hashtag in the treatment period)
  # user th > min friend th  (if the user is an adopter, they tweeted the hashtag for the first time after a friend did)
  #
  # A user is an adopter if they tweeted the hastag at any time
  
  # if a user did not adopted, 
  #   th (time of tweeting the hashtag) is set to a large number
  # if a user has no friends that tweeted in the treatment period, 
  #   min.fth (minimum of firend's tweeting of hashtag) is set 
  #   to the same large number
  # This converts an empty set to something useful for the logical criteria
  
  for (i in 1:nrow(t3))
  {
    
    th <- h[h$id == t3$id[i], "timeStamp"]
    if (length(th) != 0) adopted[i] = TRUE
    if (length(th) == 0) th = 1000
    
    temp <- subset(g, from == t2$id[i])
    colnames(temp) = c('from','id')
    temp3 <- subset(merge(temp,h), timeStamp<tm)
    min.fth <- min(temp3$timeStamp, 1000)
    
    if (th > min.fth) treated[i] = TRUE
  
  }
  
  # Create final table
  t.h <- cbind(treated, t3[,4:7])
  t.h2 <- cbind(treated, t3)
  
  # Run logit to predict probability of being treated based on attributes
  model.h <- glm(treated ~ ., data=t.h, family=binomial(link="logit"))
  pred.h <- round(predict(model.h, type = "response"),5)
  
  return(cbind(adopted, t.h2, pred.h))

}

randomMatch <- function(y,l)
{
  h.random = NULL
  for (i in 1:100)
  {
    idx <- which(y$treated == FALSE)
    match.random <- y[sample(idx,l),]
    h.random[i] <- sum(match.random$adopted)
  }
  return(round(mean(h.random),0))
}


ratioCalcSort <- function(z)
{
  ratio = NULL
  
  #Using sorted lists
  match.treat <- subset(z, treated==TRUE)
  match.treat <- match.treat[order(match.treat$pred.h),]
  match.control <- subset(z, treated==FALSE)
  match.control <- match.control[order(match.control$pred.h),]
  match.control <- match.control[1:nrow(match.treat),]

  ratio[1] <- sum(match.treat$adopted)
  ratio[2] <- sum(match.control$adopted)
  ratio[3] <- randomMatch(z, nrow(match.treat))
  ratio[4] <- round(ratio[1]/ratio[2],2)
  ratio[5] <- round(ratio[1]/ratio[3],2)
  return(ratio)
}

ratioCalcMatch <- function(z)
{
  ratio = NULL
  
  #Using score matching  
  match.treat <- subset(z, treated==TRUE)
  match.treat <- match.treat[order(match.treat$pred.h),]
  match.untreat <- subset(z, treated==FALSE)
  match.control <- match.treat  
  for (i in 1:nrow(match.treat))
  {
    x <- match.treat[i,10]
    ind <- min(which(abs(match.untreat$pred.h - x)==min(abs(match.untreat$pred.h - x))))
    match.control[i,] <- match.untreat[ind,]
  }
  
  ratio[1] <- sum(match.treat$adopted)
  ratio[2] <- sum(match.control$adopted)
  ratio[3] <- randomMatch(z, nrow(match.treat))
  ratio[4] <- round(ratio[1]/ratio[2],2)
  ratio[5] <- round(ratio[1]/ratio[3],2)
  return(ratio)
}



# WorldCup
match.data.wc <- create.formatch(wc)
xtabs(~adopted+treated, data = match.data.wc)
ratios.wc <- ratioCalcSort(match.data.wc)
ratiom.wc <- ratioCalcMatch(match.data.wc)

# Selfie
match.data.selfie <- create.formatch(selfie)
xtabs(~adopted+treated, data = match.data.selfie)
ratios.selfie <- ratioCalcSort(match.data.selfie)
ratiom.selfie <- ratioCalcMatch(match.data.selfie)

# Love
match.data.love <- create.formatch(love)
xtabs(~adopted+treated, data = match.data.love)
ratios.love <- ratioCalcSort(match.data.love)
ratiom.love <- ratioCalcMatch(match.data.love)

# tbt
match.data.tbt <- create.formatch(tbt)
xtabs(~adopted+treated, data = match.data.tbt)
ratios.tbt <- ratioCalcSort(match.data.tbt)
ratiom.tbt <- ratioCalcMatch(match.data.tbt)

# Clean and combine
ratio.s.list <- as.data.frame(rbind(ratios.wc, ratios.selfie, ratios.love, ratios.tbt))
colnames(ratio.s.list) = c("#Adopters:Treated","#Adopters:Control","#Adopters:Random",
                         "Ratio Treated/Control","Ratio Treated/Random")
ratio.s.list$hashtag = c("#worldcup","#selfie","#love","#tbt")
ratio.s.list$treated = c(sum(match.data.wc$treated), sum(match.data.selfie$treated), sum(match.data.love$treated), sum(match.data.tbt$treated))

ratio.m.list <- as.data.frame(rbind(ratiom.wc, ratiom.selfie, ratiom.love, ratiom.tbt))
colnames(ratio.m.list) = c("#Adopters:Treated","#Adopters:Control","#Adopters:Random",
                         "Ratio Treated/Control","Ratio Treated/Random")
ratio.m.list$hashtag = c("#worldcup","#selfie","#love","#tbt")
ratio.m.list$treated = c(sum(match.data.wc$treated), sum(match.data.selfie$treated), sum(match.data.love$treated), sum(match.data.tbt$treated))
