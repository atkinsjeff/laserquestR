pcl <- read.csv("laserquest_master_pcl.CSV")

df <- data.frame(pcl$site, pcl$plot, pcl$rugosity)

#sorting
df <- df[order(df$pcl.rugosity),]

grsm <- subset(df, df$pcl.site =="GRSM")
plot(grsm$pcl.rugosity)

require(boot)

grsm.mean <- mean(grsm$pcl.rugosity)
grsm.mean
B = 1000
n = nrow(grsm)

boot.samples = matrix(sample(grsm$pcl.rugosity, size = B * n, replace = TRUE), B, n)

boot.statistics = apply(boot.samples, 1, mean)

require(ggplot2)
ggplot(data.frame(meanRc = boot.statistics),aes(x=meanRc)) +
     geom_histogram(binwidth=0.25,aes(y=..density..)) +
     geom_density(color="red")

rugosity.se = sd(boot.statistics)
rugosity.se

me = ceiling(10 * 2 * rugosity.se)/10
round(grsm.mean, 1) + c(-1, 1) * me
## [1] 27.2 31.0

