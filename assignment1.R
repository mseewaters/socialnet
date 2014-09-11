setwd("D:/0 Stern MSBA/2.3 Network analytics/data")
library(igraph)

# Load graphSubset.dl
g <- read.graph(file="graphSubset.dl",format="dl",directed = TRUE)
summary(g)

# Calculate alpha centrality and show 20 most central users
ac <- alpha.centrality(g)
ac2 <- cbind(read.table(text=names(ac)),ac)[order(-ac),]
head(ac2,20)


# Load completeGraph.csv and convert to graph object
data <- read.delim("D:/0 Stern MSBA/2.3 Network analytics/data/completeGraph.csv", header=FALSE)
g2 <- graph.data.frame(data, directed = TRUE)
summary(g2)


# Calculate page rank
pr <- page.rank(g2, directed = TRUE, damping = 0.9)
head(pr$vector,20)

# Calculate in degree
deg <- degree(g2, mode='in')
head(deg,20)

#Plot in degree vs pagerank
plot.data <- as.data.frame(cbind(deg, pr$vector))
plot.data$color <- "dark grey"
plot.data$color[4555] <- "red"
plot.data$color[37815] <- "blue"
plot.data$color[39880] <- "green"
plot(plot.data$deg, plot.data$V2, log = 'xy', col=plot.data$color, 
     pch=16, xlab = "in degree", ylab = 'pagerank')
#identify(plot.data$deg, plot.data$V2)  # used to dynamically identify points


# Additional context for graph, specific examples

# From pre-module, number of followers by ID
d <- data
colnames(d) <- c("from","to")
t3 <- data.table(d)[ , list(num_follows=length(from)), by = to] 
t3 <- t3[order(to),] 


row.num <- c("4555","37815","39880")
ex.label <- c("Many typical followers - red", "Few influencial followers - blue", "Many less influential followers - green")

ex = NULL
ex.f = NULL
ex.ff = NULL

ex[1] <- names(deg[4555])
ex[2] <- names(deg[37815])
ex[3] <- names(deg[39880])

follow.ex1 <- subset(data, V2 == ex[1])
ex.f[1] <- nrow(follow.ex1)
names(follow.ex1) <- c("to","example")
temp <- merge (follow.ex1, t3)
ex.ff[1] <- sum(temp$num_follows)

follow.ex2 <- subset(data, V2 == ex[2])
ex.f[2] <- nrow(follow.ex2)
names(follow.ex2) <- c("to","example")
temp2 <- merge (follow.ex2, t3)
ex.ff[2] <- sum(temp2$num_follows)

follow.ex3 <- subset(data, V2 == ex[3])
ex.f[3] <- nrow(follow.ex3)
names(follow.ex3) <- c("to","example")
temp3 <- merge (follow.ex3, t3)
ex.ff[3] <- sum(temp3$num_follows)

final <- cbind(ex.label, row.num, ex, ex.f, ex.ff)
colnames(final) <- c("Example", "Row#","UniqueID", "#Followers","#Followers' Followers")
