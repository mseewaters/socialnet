setwd("D:/0 Stern MSBA/2.3 Network analytics/propensityMatching")

g <- read.delim("D:/0 Stern MSBA/2.3 Network analytics/post module/socialnet/completeGraph.csv", header=FALSE)
all <- read.csv("D:/0 Stern MSBA/2.3 Network analytics/propensityMatching/allUsers.csv")
wc <- read.csv("D:/0 Stern MSBA/2.3 Network analytics/propensityMatching/worldcup.csv")


# Create table of users that follow someone
colnames(g) <- c("from","to")
library(data.table)
t1 <- data.table(g)[ , list(num_follows=length(from)), by = to] 
t1 <- t3[order(to),] 
colnames(t1) <- c("id","num_follows")

# Merge users that follow someone and allUsers
# Creating a table of users with followers and all attributes
t2 <- as.data.frame(merge(t1, all, by = "id"))


# Calculate mean timestamp for each hashtag
tm <- mean(wc$timeStamp)

# For each user with followers, isolate followers
# Determine if followers tweeted hashtag before mean timestamp treated = TRUE
# Otherwise treated = FALSE

treated = NULL
for (i in 1:nrow(t2))
{
  
  temp <- subset(g, from == t2$id[i])
  colnames(temp) = c('from','id')
  temp3 <- subset(merge(temp,wc), timeStamp<tm)
  
  if (nrow(temp3) >= 1) treated[i] = TRUE else
    treated[i] = FALSE
  
}

# Create final table
t.wc <- cbind(treated, t2[,4:7])
t.wc2 <- cbind(treated, t2)

# Run logit to predict probability of being treated based on attributes
model.wc <- glm(treated ~ ., data=t.wc, family=binomial(link="logit"))
pred.wc <- round(predict(model.wc, type = "response"),5)
tpred.wc <- cbind(t.wc2,pred.wc)

temp4 <- merge(tpred.wc, wc, by = 'id') #isolate adopters

adopted = NULL
for (i in 1:nrow(tpred.wc))
{

  temp5 <- subset(temp4, id == tpred.wc$id[i])
  if (nrow(temp5) >= 1) adopted[i] = 1 else
    adopted[i] = 0
}

predall.wc <- cbind(tpred.wc,adopted)

#library(Matching)
#Y <- predall.wc$adopted
#Tr <- predall.wc$treated
#X <- predall.wc$pred.wc
#match <- Match(Y = Y, Tr = Tr, X = X)

library(MatchIt)
match <- matchit(treated ~ pred.wc, data = predall.wc, method='optimal')
summary(match)
match.treat <- match.data(match, group = "treat")
match.control <- match.data(match, group = "control")
m1 <- sum(match.treat$adopted)
m2 <- sum(match.control$adopted)

