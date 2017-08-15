# importing the high and low light dfs and using ANCOVA

head(high)
head(low)

high$site <- substr(high$plot,0, 4)
high$site <- as.factor(high$site)

high.vai.aov <- aov(fPAR_mean ~ mean.vai_mean * site, data = high)
model.tables(high.vai.aov)

high.rc.aov <- aov(fPAR_mean ~ rugosity_mean * site, data = high)

high.combo <- aov(fPAR_mean ~ mean.vai_mean * rugosity_mean * site, data = high)

lm.high <- lm(fPAR_mean ~ mean.vai_mean + log(rugosity_mean), data = high )



#### Finding max and mins
df <- read.csv("laserquest_plot_means.csv")
df$site <- substr(df$plot, 0,4)
df$site <- as.factor(df$site)
head(df)

light[light == "UVAA"] <- "UVAX"

library(dplyr)
library(magrittr)
light %>%
     group_by(site) %>%
     summarise(   sd(fPAR))

table(light$plot)

df %>%
     group_by(site) %>%
     summarise( min(rugosity_mean),  max(rugosity_mean))

var(light$fPAR)

str(high)
x <- high$max.ht_mean
y <- high$fPAR_mean
plot(x, y)
summary(lm(y ~ x))
