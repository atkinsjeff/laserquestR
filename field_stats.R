# looking at broadscale connections
require(ggplot2)
require(lmtest)
master <- read.csv("laserquest_master_output.csv")

master <- master[-307,]

x11()

ggplot(master, aes(x = mean.height, y = rugosity))+
     geom_point()+
     xlab("Mean Canopy Height (m)")+
     ylab("Rugosity")+
     stat_smooth(method = lm, se = FALSE)

x11()
ggplot(master, aes(x = (max.can.ht), y = rugosity))+
     geom_point()+
     xlab("Max Canopy Height (m)")+
     ylab("Rugosity")+
     geom_smooth(method = "nls", formula = y ~ a^x, method.args = list(start = c(a = 1)), se = FALSE)


x11()
ggplot(master, aes(x = mean.max.ht, y = rugosity))+
     geom_point()+
     xlab("Mean Max Canopy Height (m)")+
     ylab("Rugosity")+
     geom_smooth(method = "nls", formula = y ~ a^x, method.args = list(start = c(a = 1)), se = FALSE)

bptest(mean.max.ht ~ rugosity, data = master)


lm.mean.canopy <- lm(rugosity ~ mean.height, data = master)
summary(lm.mean.canopy)

#simulate some data
x <- 1:100
y <- 1 + x^0.15 + rnorm(100, 0, 0.01)
m <- nls(y ~a + b * I(x^z), start = list(a =1, b =1, z =1))
plot( y ~ x)
summary(m)
# some model evaluation, residual sum of square and R²
RSS <- sum(residuals(m)^2)
TSS <- sum((y - mean(y))^2)
R.square <- 1 - (RSS/TSS)


lm.max.ht <- lm(rugosity ~ log(max.can.ht), data = master)

m.max.mean <- nls(rugosity ~ a^mean.max.ht, data = master, start = list(a = 1))
RSS <- sum(residuals(m.max.mean)^2)
TSS <- sum((master$rugosity - mean(master$rugosity))^2)
R.square <- 1 - (RSS/TSS)
R.square

# now we make the residuals by substracting the rugosity values form predicted based on max values
master$max.pred.rugosity <- 1.102^master$max.can.ht
master$rugosity.resid <- master$rugosity - master$max.pred.rugosity

# now let's look at some correlation matrix
nums <- sapply(master, is.numeric)
row.names(master) <- master$plot

m.master <-master[, nums]
require(psych)

require(corrplot)
m.master <- m.master[, c(3:20)]
m <- cor(m.master)
x11()
corrplot(m, method = "circle")

corr.test(m.master)

x11()
ggplot(master, aes(x = mode.2, y = rugosity.resid))+
     geom_point()+
     xlab("Mode 2 (units, no one knows)")+
     ylab("Residuals")+
     geom_smooth(method = lm, se =FALSE)

lm.mode.2 <- lm(rugosity.resid ~ mode.2, data = master)
summary(lm.mode.2)




