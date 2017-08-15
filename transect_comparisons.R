#transect level comparisons

# Rumple
x <- master$rugosity
y <- master$rumple
x11()
plot(x, y, xlab = "Rugosity (m)", ylab = "Rumple", main = "Transect level Rugosity vs. Rumple")

# Rumple vs. fPAR
x <- direct$rumple
y <- direct$fPAR
x11()
plot(x, y, xlab = "Rumple", ylab = "fPAR", main = "Direct Light")

#clumping index
x <- master$rugosity
y <- master$clumping.index
x11()
plot(x, y, xlab = "Rugosity (m)", ylab = "Clumping Index", main = "Transect level Rugosity vs. Clumping Index")

#clumping index vs fPAR
x <- direct$clumping.index
y <- direct$fPAR
x11()
plot(x, y, xlab = "Clumping Index", ylab = "fPAR", main = "Direct Light")

x <- direct$mean.vai
y <- direct$fPAR
x11()
plot(x, y, xlab = "VAI", ylab = "fPAR", main = "Direct Light")
line(lm(y ~ poly(x, 2)))
# need to model the one above

model <- lm(y ~ x)

x <- direct$mean.vai
y <- direct$rumple
x11()
plot(x, y, xlab = "VAI", ylab = "Rumple", main = "Direct Light")




