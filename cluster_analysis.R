
require(ggplot2)

# example data set to look at what is going on use the 
library(datasets)
head(iris)

set.seed(20)
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
irisCluster

irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$cluster)) + geom_point()
# K-means clustering with 3 clusters of sizes 46, 54, 50


### included for Brady
# importing rugosity data
pcl <- read.csv("laserquest_master_output.CSV", header = TRUE)
colnames(pcl)[3] <- "plotID"

# now we can clean this shit up a bit
pcl$site <- as.factor(gsub("_.*$", "", pcl$plotID))
pcl$plotnumber <- sub(".*_", "", pcl$plotID)

pcl$plot <- paste0(pcl$site, substr(pcl$plotnumber, 0,2))

# pcl means
pcl %>% group_by(plot) %>% summarise_each(funs(mean, sd), -X, -X1, -plotID, -transect.length, -plotnumber, -site) -> pcl.means 

pcl.means$plot <- as.factor(pcl.means$plot) 
pcl.means <- data.frame(pcl.means)

head(pcl.means)

# 
master2 <- pcl.means[,c(1:16)]

# creating site column again
master2$site <- substr(master2$plot, 0, 4)
master2 <- na.omit(master2)
row.names(master2) <- master2$plot

# now doing the cluster analysis w/ the pcl data
set.seed(20)
master.cluster <- kmeans(master2[, 2:16], 3, nstart = 20)

# printing out
master.cluster

# table data
table(master.cluster$cluster, master2$site)

require(fpc)
plotcluster(master2[, 2:16], master.cluster$cluster )

# this doesn't work yet, b/c NA's are created through coercion and a distance or dissimilariy matrix doesn't seem to work yet.
library(cluster)
clusplot(master2[, 2:16], master.cluster$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE, plotchar = TRUE, col.txt= TRUE, col.p = TRUE,
         labels=2, lines=0)


## plotting iris (data frame) in a 2-dimensional plot and partitioning
## into 3 clusters.
data(iris)
iris.x <- iris[, 1:4]
cl3 <- pam(iris.x, 3)$clustering
op <- par(mfrow= c(2,2))
clusplot(iris.x, cl3, color = TRUE)
U <- par("usr")
## zoom in :
rect(0,-1, 2,1, border = "orange", lwd=2)
clusplot(iris.x, cl3, color = TRUE, xlim = c(0,2), ylim = c(-1,1))
box(col="orange",lwd=2); mtext("sub region", font = 4, cex = 2)
##  or zoom out :
clusplot(iris.x, cl3, color = TRUE, xlim = c(-4,4), ylim = c(-4,4))
mtext("`super' region", font = 4, cex = 2)
rect(U[1],U[3], U[2],U[4], lwd=2, lty = 3)

### how many clusters?
wssplot <- function(data, nc=15, seed=1234){
     wss <- (nrow(data)-1)*sum(apply(data,2,var))
     for (i in 2:nc){
          set.seed(seed)
          wss[i] <- sum(kmeans(data, centers=i)$withinss)}
     plot(1:nc, wss, type="b", xlab="Number of Clusters",
          ylab="Within groups sum of squares")}

wssplot(master2[, 2:16], nc=20) 

#make it a factor

master.cluster$cluster <- as.factor(master.cluster$cluster)
master.cluster$centers


x11()
ggplot(master2, aes(x = rugosity_mean, y = fPAR_mean, color = ))

library(cluster) 
d <- dist(master2)
d <-na.omit(d)
clusplot(master2, master.cluster$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)