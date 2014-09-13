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
tm <- mean(h$timeStamp)

# For each user with followers
# identify if non-adopters, isolate followees
# Determine if followees tweeted hashtag before t* treated = TRUE
# Otherwise treated = FALSE
# For adopters
# if tweeted "world cup" before t*, treated = FALSE
# otherwise isolate followers and follow logic as above


treated = NULL
adopted = NULL

for (i in 1:nrow(t2))
{
  
  tt <- h[h$id == t2$id[i], "timeStamp"]
  if (length(tt) == 0) 
    {
      adopted[i] = FALSE
      temp <- subset(g, from == t2$id[i])
      colnames(temp) = c('from','id')
      temp3 <- subset(merge(temp,h), timeStamp<tm)
      
      if (nrow(temp3) >= 1) treated[i] = TRUE else
        treated[i] = FALSE
    }
    else if (tt<tm) 
      {
        treated[i] = FALSE 
        adopted[i] = TRUE
      }
  
    else
      {
        adopted[i] = TRUE
        temp <- subset(g, from == t2$id[i])
        colnames(temp) = c('from','id')
        temp3 <- subset(merge(temp,h), timeStamp<tm)
        
        if (nrow(temp3) >= 1) treated[i] = TRUE else
          treated[i] = FALSE
       
      }  
}

# Create final table
t.h <- cbind(treated, t2[,4:7])
t.h2 <- cbind(treated, t2)

# Run logit to predict probability of being treated based on attributes
model.h <- glm(treated ~ ., data=t.h, family=binomial(link="logit"))
pred.h <- round(predict(model.h, type = "response"),5)
tpred.h <- cbind(t.h2,pred.h)

predall.h <- cbind(tpred.h,adopted)
return(predall.h)
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

ratioCalc <- function(z)
{
  ratio = NULL
  match <- matchit(treated ~ pred.h, data = z, method='nearest')
  summary(match)
  match.treat <- match.data(match, group = "treat")
  match.control <- match.data(match, group = "control")
  ratio[1] <- sum(match.treat$adopted)
  ratio[2] <- sum(match.control$adopted)
  ratio[3] <- randomMatch(z, nrow(match.treat))
  ratio[4] <- round(ratio[1]/ratio[2],2)
  ratio[5] <- round(ratio[1]/ratio[3],2)
  return(ratio)
}

library(MatchIt)

# WorldCup
match.data.wc <- create.formatch(wc)
xtabs(~adopted+treated, data = match.data.wc)
ratio.wc <- ratioCalc(match.data.wc)

# Selfie
match.data.selfie <- create.formatch(selfie)
xtabs(~adopted+treated, data = match.data.selfie)
ratio.selfie <- ratioCalc(match.data.selfie)

# Love
match.data.love <- create.formatch(love)
xtabs(~adopted+treated, data = match.data.love)
ratio.love <- ratioCalc(match.data.love)

# tbt
match.data.tbt <- create.formatch(tbt)
xtabs(~adopted+treated, data = match.data.tbt)
ratio.tbt <- ratioCalc(match.data.tbt)

# Clean and combine
ratio.list <- as.data.frame(rbind(ratio.wc, ratio.selfie, ratio.love, ratio.tbt))
colnames(ratio.list) = c("#Adopters:Treated","#Adopters:Control","#Adopters:Random",
                         "Ratio Treated/Control","Ratio Treated/Random")
ratio.list$hashtag = c("#worldcup","#selfie","#love","#tbt")
