#NLS modeling of light relationships using Chris's rectangular hyperbolic function
#

summary(master)

summary(direct)

summary(diffuse)

# Direct Light modelling
# model should be fPAR = a * x / (b + x) ...Michaelis-Menten
y <- direct$fPAR
x <- direct$rugosity


m.direct <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
summary(m.direct)

RSS <- sum(residuals(m.direct)^2)
TSS <- sum((y - mean(y))^2)
R.square <- 1 - (RSS/TSS)
print(R.square)

x11()
plot(x, y, xlab = "Canopy Rugosity (m)", ylab = "fPAR")
a <- coef(m.direct)[1]
b <- coef(m.direct)[2]
curve((a * x) / (b + x), col = "blue", add = TRUE)

x11()
ggplot()+
     geom_point(aes(x = x, y = y), size = 2, shape = 2)+
     theme_classic()+
     xlab("Rugosity (m)")+
     ylab("fPAR")


# fPAR and porosity

x <- direct$porosity
y <- direct$fPAR

x11()
plot(x,y, xlab = "porosity", ylab = "fPAR", main = "fPAR and Porosity--direct light")
abline(lm(y ~ x), col = "blue")


lm.porosity <- lm(y ~ x)
summary(lm.porosity)

#exp
x <- direct$porosity
y <- direct$fPAR

m.2 <- nls(y ~ a * x^b, start = list(a = 1, b =1))
summary(m.2)

RSS <- sum(residuals(m.2)^2)
TSS <- sum((y - mean(y))^2)
R.square <- 1 - (RSS/TSS)
print(R.square)

x11()
plot(x,y, xlab = "porosity", ylab = "fPAR", main = "fPAR and Porosity--direct light")
a <- coef(m.2)[1]
b <- coef(m.2)[2]
curve(a * x^b, col = "blue", add = TRUE)

#model 3
x <- direct$porosity
y <- direct$fPAR

rm(a)
rm(b)
m.3 <- lm(y ~ poly(x, 3))
summary(m.3)

m.poly <- function(x) m.3$coefficient[4]

x11()
plot(x,y, xlab = "Canopy Porosity", ylab = "fPAR")
lines(sort(x), fitted(m.3)[order(x)], col = "blue")

plot(m.3)

#ineraction

Rc <- direct$rugosity
P <- direct$porosity
y <- direct$fPAR

m.one <- lm(y ~ Rc * P)
summary(m.one)

x11()
plot(Rc * P, y)

plot(Rc, y)




###diffuse light
x <- diffuse$porosity
y <- diffuse$fPAR

x11()
plot(x, y, xlab = "porosity", ylab = "fPAR", main = "fPAR and Porosity--diffuse light")

# #clumping index
# x <- direct$
# 
# x11()
# plot(x,y, xlab = "porosity", ylab = "fPAR", main = "fPAR and Porosity--direct light")




####################
# Residuals of the fPAR ~ rugosity model against porosity
y <- residuals(m.direct)
x <- direct$porosity

x11()
plot(x, y, xlab = "Porosity", ylab = "residuals (fPAR ~ Rc model)", main = "Residuals of fPAR ~ Rc against Porosity")


##################
##################
##################
# residuals of the fPAR ~ porosity model against rugosity
# 

y <- residuals(m.3)
x <- direct$rugosity

x11()
plot(x, y, xlab = "Rugosity", ylab = "residuals (fPAR ~ Pc model)", main = "Residuals of fPAR ~ Pc against Rugosity")

#model 3
x <- direct$mean.vai
y <- direct$fPAR

# rm(a)
# rm(b)
m.vai <- lm(y ~ poly(x, 2))
summary(m.vai)
# summary(m.3)

#m.poly <- function(x) m.3$coefficient[4]

x11()
plot(x,y, xlab = "VAI", ylab = "fPAR")
lines(sort(x), fitted(m.vai)[order(x)], col = "blue")


#
#model 3
x <- direct$clumping.index
y <- direct$fPAR

# rm(a)
rm(a)
rm(b)
m.ci <- lm(y ~ poly(x, 5))
#m.ci <- lm(y ~ poly(-x, 2))
m.ci <- nls(y ~ x^a + b, start = list(a = 1, b = 1))
summary(m.ci)
RSS <- sum(residuals(m.ci)^2)
TSS <- sum((y - mean(y))^2)
R.square <- 1 - (RSS/TSS)
print(R.square)
# summary(m.vai)
# summary(m.3)


#m.poly <- function(x) m.3$coefficient[4]

x11(width = 7, height = 6.13)
plot(x, y, xlab = "Clumping Index", ylab = "fPAR")
a <- coef(m.ci)[1]
b <- coef(m.ci)[2]
line(y ~ x^a + b, col = "blue", add = TRUE)


lines(sort(x), fitted(m.ci)[order(x)], col = "blue")

rm(a)
rm(b)
m.3 <- lm(y ~ poly(x, 3))
summary(m.3)

m.poly <- function(x) m.3$coefficient[4]

x11()
plot(x,y, xlab = "Canopy Porosity", ylab = "fPAR")
lines(sort(x), fitted(m.3)[order(x)], col = "blue")


m.direct <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
summary(m.direct)

x <- direct$rugosity
y <- direfct$fpar

m.one <- lm((1/y) ~ x)
summary(m.one)

RSS <- sum(residuals(m.one)^2)
TSS <- sum((y - mean(y))^2)
R.square <- 1 - (RSS/TSS)
print(R.square)

x11()
plot(x, (1/y), xlab = "1 / Rc", ylab = "fPAR")










#############
#############
#############

x <- pcl$rugosity
y <- pcl$mean.vai

x11()
plot(x, y, xlab = "Canopy Rugosity", ylab = "Mean VAI")

require(ggplot2)
x11()
ggplot(pcl, aes(x = exp(rugosity), y = mean.vai, color = site))+
     geom_point()


x <- direct$mean.vai * log(direct$rugosity) 

fit <- lm(fPAR ~ mean.vai * log(rugosity), data = direct)
summary(fit)

fit.vai <- lm(fPAR ~ mean.vai, data = direct)
summary(fit.vai)

resid <- residuals(fit.vai)
x <- log(direct$rugosity)

x11()
plot(direct$rugosity, resid)
# Other useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics

# predicting fit
newdat <- data.frame(x = seq(min(x), max(x), length.out = 261))
newdat$pred <- predict(fit, newdata = newdat)

x11()
plot(direct$mean.vai, direct$fPAR)

summary(fit.x)
lines(sort(x), fitted(fit)[order(x)], col='red', type='b') 
# Stepwise Regression
require(MASS)

step <- stepAIC(fit, direction="both")

step$anova # display results
