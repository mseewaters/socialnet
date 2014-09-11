setwd("D:/0 Stern MSBA/2.3 Network analytics/post module/socialnet")
tweets <- read.csv("D:/0 Stern MSBA/2.3 Network analytics/post module/socialnet/tweets.csv")

inf <- xtabs(retweets ~ screen_name, aggregate(retweets ~ screen_name, tweets, mean))
inf2 <- as.data.frame(round(inf,4))[order(-inf2$Freq),]
head(inf2,5)
