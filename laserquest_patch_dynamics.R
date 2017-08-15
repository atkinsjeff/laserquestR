# patch dynamics andlandscape metrics in R
# 
require(SDMTools)
require(tidyr)
require(dplyr)

#define a simple binary matrix
tmat = { matrix(c( 0,0,0,1,0,0,1,1,0,1,
                   0,0,1,0,1,0,0,0,0,0,
                   0,1,NA,1,0,1,0,0,0,1,
                   1,0,1,1,1,0,1,0,0,1,
                   0,1,0,1,0,1,0,0,0,1,
                   0,0,1,0,1,0,0,1,1,0,
                   1,0,0,1,0,0,1,0,0,1,
                   0,1,0,0,0,1,0,0,0,1,
                   0,0,1,1,1,0,0,0,0,1,
                   1,1,1,0,0,0,0,0,0,1),nr=10,byrow=TRUE) }

#do the connected component labelling
ccl.mat = ConnCompLabel(tmat)
ccl.mat
image(t(ccl.mat[10:1,]),col=c('grey',rainbow(length(unique(ccl.mat))-1)))
image(t(tmat[10:1,]),col=c('grey',rainbow(length(unique(tmat))-1)))


#calculate the patch statistics
ps.data = PatchStat(ccl.mat)
ps.data

#calculate the class statistics
cl.data = ClassStat(tmat)
cl.data

#identify background data is 0
cl.data = ClassStat(tmat,bkgd=0)
cl.data

DF <- data.frame(a = c(0,1,0), b = c(0,1,0),
                 c = seq(as.Date("2004-01-01"), by = "week", len = 3),
                 stringsAsFactors = TRUE)
mDF <- data.matrix(DF[1:2])
data.matrix(DF)

ccl.df <- ConnCompLabel(mDF)
image(t(ccl.df[3:1,]),col=c('grey',rainbow(length(unique(ccl.df))-1)))

ps.df <- PatchStat(ccl.df)

small.m <- data.frame(m5[c(1,2, 15)])
small.m$vai <- as.integer(small.m$vai)
 
m5.wide <- small.m %>% spread(xbin, vai)
m5.wide <- m5.wide[-1]

m5.wide[m5.wide > 0] <- 1
ccl.df <- data.matrix(m5.wide)
ccl.df <- ConnCompLabel(ccl.df)
x11()
image(t(ccl.df[1:30,]),col=c('grey',rainbow(length(unique(ccl.df))-1)))

ps.df <-PatchStat(ccl.df)
ps.df

cl.df <- ClassStat(ccl.df)
cl.df
# library(dplyr)
# stocks <- data.frame(
#      time = as.Date('2009-01-01') + 0:9,
#      X = rnorm(10, 0, 1),
#      Y = rnorm(10, 0, 2),
#      Z = rnorm(10, 0, 4)
# )
# stocksm <- stocks %>% gather(stock, price, -time)
# stocksm %>% spread(stock, price)
# stocksm %>% spread(time, price)


####   NDCI
x.sd <- ddply(m5, ~xbin, summarise, sd=sd(vai))
z.sd <- ddply(m5, ~zbin, summarise, sd=sd(vai))

NDCI.mean <- (mean(x.sd$sd) - mean(z.sd$sd)) / (mean(x.sd$sd) + mean(z.sd$sd))
NDCI.mean

NDCI.sum <- (sum(x.sd$sd) - sum(z.sd$sd)) / (sum(x.sd$sd) + sum(z.sd$sd))
NDCI.sum

NDCI.sd <- (sd(x.sd$sd) - sd(z.sd$sd)) / (sd(x.sd$sd) + sd(z.sd$sd))
NDCI.sd

####
x.mean <- ddply(m5, ~xbin, summarise, sum=sum(vai))
z.mean <- ddply(m5, ~zbin, summarise, sum=sum(vai))

NDCI.mean <- (mean(x.mean$mean) - mean(z.mean$mean)) / (mean(x.mean$mean) + mean(z.mean$mean))
NDCI.mean

NDCI.sum <- (sum(x.mean$mean) - sum(z.mean$mean)) / (sum(x.mean$mean) + sum(z.mean$mean))
NDCI.sum

NDCI.mean <- (mean(x.mean$mean) - mean(z.mean$mean)) / (mean(x.mean$mean) + mean(z.mean$mean))
NDCI.mean




