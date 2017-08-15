# 03_fpar_modelling
require(plyr)
require(dplyr)
require(ggplot2)

# # Importing light data
# light <- read.csv("light_data.CSV", header = TRUE)
# 
# light <- light[, c("plot", "transect", "Average.Above.PAR", "Average.Below.PAR",  "Leaf.Area.Index..LAI.")]
# 
# light <- plyr::rename(light, c( "Average.Above.PAR" = "aPAR", "Average.Below.PAR" = "bPAR",  "Leaf.Area.Index..LAI." = "LAI"))
# 
# # calculate fPAR - the fraction of photosynthetically availble radiation absorbed by the canopy
# light$fPAR <- 1 - (light$bPAR / light$aPAR)
# 
# light$site <- substr(light$transect, 0, 4)
# # light$plot <- substr(light$transect, 0, 6)
# light$site <- as.factor(light$site)
# 
# 
# #If aPAR which is the above canopy par is >1000 then we can call that high light
# 
# light$regime <- ifelse(light$aPAR >= 1000, "high",
#                        "low")
# light$regime <- as.factor(light$regime)
# 
# # we want to keep those LAI and fPAR values that are no accurate now so we can caluclate later, but for now let's go with the good ones
# 
# light <- subset(light, fPAR > 0)
# light
# 
# # Making light means
# light %>% group_by(plot) %>% summarise_each(funs(mean, sd), -plot, -transect, -regime, -site ) -> light.means
# light.means <- data.frame(light.means)
# #
# 
# 
# #### PCL data
# pcl <- read.csv("laserquest_master_pcl.CSV")
# #pcl <-  names(pcl)[!names(pcl) %in% c("plot_1", "notes", "site.1", "plot.1")]
# 
# # # pcl plot means
pcl %>% group_by(plot) %>% summarise_each(funs(mean, sd), -site, -transect, -notes) -> pcl.plot.means
# 
pcl.plot.means$plot <- as.factor(pcl.plot.means$plot)
pcl.plot.means <- data.frame(pcl.plot.means)
# 
# # merge by plot. Should be 92?
 master.plot <- merge(pcl.plot.means, light.means, by = "plot")
# #######################
# # SITE MEANS
# # # pcl plot means
# pcl %>% group_by(site) %>% summarise_each(funs(mean, sd), -plot -site, -transect, -notes, -transect.length) -> pcl.site.means
# pcl.site.means <- data.frame(pcl.site.means)
# 
light %>% group_by(site) %>% summarise_each(funs(max), -plot, -transect, -regime, -site ) -> light.site.means
light.site.means <- data.frame(light.site.means)
# 
# # master.site
# master.site <- merge(pcl.site.means, light.site.means, by = "site")
# 
# # writing data
# write.csv(master.site, "laserquest_master_fpar_site.csv")
# write.csv(master.plot, "laserquest_master_fpar_plot.csv")


# custom plot theme
theme_new <- function(base_size = 12){
     theme_bw(base_size = base_size) %+replace%
          theme(
               #line = element_line(colour="black"),
               #text = element_text(colour="black"),
               #axis.text = element_text(colour="black", size=8),
               #strip.text = element_text(size=12),
               legend.key=element_rect(colour=NA, fill =NA),
               panel.grid = element_blank(),   
               panel.border = element_blank(),
               panel.background = element_rect(fill = "white", colour = "white"), 
               strip.background = element_rect(fill = NA),
               axis.text = element_text( size = 14),
               axis.title  = element_text( size = 16, margin = margin(12, unit = "cm")),
               legend.title=element_blank()
          )
     
}

# custom plot label

vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
lai.label =  expression(paste(LAI~(m^2 ~m^-2)))
log.lai.label = expression(paste(log~( LAI~(m^2 ~m^-2))))

#
x11()
hist(master.plot$aPAR_mean)

master.plot$regime <- ifelse(master.plot$aPAR_mean >= 1000, "high",
                             "low")

master.plot$regime <- as.factor(master.plot$regime)
high <- subset(master.plot, aPAR_mean >= 1000)
low <- subset(master.plot, aPAR_mean < 1000)


### Making functions

##### RUGOSITY

# Direct Light modeling for HIGH light
# model should be fPAR = a * x / (b + x) ...Michaelis-Menten
y <- high$fPAR_mean
x <- high$rugosity_mean

# 1
m.rugosity1 <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
summary(m.rugosity1)


RSS <- sum(residuals(m.rugosity1)^2)
TSS <- sum((y - mean(y))^2)
R.square <- 1 - (RSS/TSS)
print(R.square)
AIC(m.rugosity1)

# plot w/ coefficents
x11()
plot(x, y, xlab = "Canopy Rugosity (m)", ylab = "fPAR")
a <- coef(m.direct)[1]
b <- coef(m.direct)[2]
curve((a * x) / (b + x), col = "blue", add = TRUE)

# 2
m.rugosity2 <- nls(y ~ a + (b * x) / (c + x), start = list(a = 0, b = 0.1, c = 0.1))

summary(m.rugosity2)


RSS <- sum(residuals(m.rugosity2)^2)
TSS <- sum((y - mean(y))^2)
R.square <- 1 - (RSS/TSS)
print(R.square)

# plot w/ coefficents
x11()
plot(x, y, xlab = "Canopy Rugosity (m)", ylab = "fPAR")
a <- coef(m.direct)[1]
b <- coef(m.direct)[2]
curve(a + (b * x) / (c + x), col = "blue", add = TRUE)
### LOW LIGHT
y <- low$fPAR_mean
x <- low$rugosity_mean
m.low <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))

x11()
plot(x, y, xlab = "Canopy Rugosity (m)", ylab = "fPAR")
a <- coef(m.direct)[1]
b <- coef(m.direct)[2]
curve((a * x) / (b + x), col = "blue", add = TRUE)


RSS <- sum(residuals(m.low)^2)
TSS <- sum((y - mean(y))^2)
R.square <- 1 - (RSS/TSS)
print(R.square)

fpar.high <- function(x) {(1.075147 * x) / ( 3.085004+ x)}
fpar.low <- function(x) {(0.89777 * low$rugosity_mean) / ( 1.16947 + low$rugosity_mean)}

x11(width = 5, height = 5)
ggplot(high, aes(x = rugosity_mean, y = fPAR_mean))+
     geom_point(size = 4, shape = 21)+
     theme_new()+
     xlab(r.label)+
     ylab("fPAR")+
     stat_function(fun = fpar.high, col = "blue", size = 1)+
     geom_point(data = low, aes(x = rugosity_mean, y = fPAR_mean), size = 4, color = "black")+
     stat_function(fun = function(x) (0.89777 * low$rugosity_mean) / ( 1.16947 + low$rugosity_mean), col = "red")

#### VAI
y <- high$fPAR_mean
x <- high$mean.vai_mean


m.vai <- lm(y ~ x)
summary(m.vai)


RSS <- sum(residuals(m.direct)^2)
TSS <- sum((y - mean(y))^2)
R.square <- 1 - (RSS/TSS)
print(R.square)

x11()
ggplot(aes(x = x, y = y))+
     geom_point(size = 2, shape = 21)+
     theme_classic()+
     xlab("Rugosity (m)")+
     ylab("fPAR")+
     geom_smooth(method = "lm")


##### MEAN VAI

# Direct Light modeling for HIGH light
# model should be fPAR = a * x / (b + x) ...Michaelis-Menten
y <- high$fPAR_mean
x <- high$mean.vai_mean


m.vai.high <- lm(y ~ x)
summary(m.vai.high)


x11(width = 5, height = 5)
ggplot(high, aes(x = mean.vai_mean, y = fPAR_mean))+
     geom_point(size = 4, shape = 21)+
     theme_new()+
     xlab(vai.label)+
     ylab("fPAR")+
     geom_smooth(method = "lm", col = "red", size = 1, se = FALSE)

x11()
plot(log(high$rugosity_mean), resid(m.vai.high))

summary(lm(resid(m.vai.high) ~ log(high$rugosity_mean)))
# x11()
# ggplot(master.site, aes(x = rugosity_mean, y = fPAR,  color = site))+
#      geom_point(size = 5)+
#      #geom_text()+
#      #scale_color_manual(values=cbPalette)+
#      #scale_color_brewer(palette="Dark2")+
#      geom_errorbarh(aes(xmin =rugosity_mean - rugosity_sd, xmax = rugosity_mean + rugosity_sd)) + 
#      #geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
#      theme_new()+
#      ylab("fPAR ")+
#      xlab("Rugosity (m)")+
#      stat_function(fun = function(x) (0.97624 * x) / (1.74132 + x), colour = "dark grey", size = 1.25)


### RUMPLE
##### RUGOSITY

# Direct Light modeling for HIGH light
# model should be fPAR = a * x / (b + x) ...Michaelis-Menten
y <- low$fPAR_mean
x <- low$rugosity_mean


m.light <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
summary(m.light)


RSS <- sum(residuals(m.light)^2)
TSS <- sum((y - mean(y))^2)
R.square <- 1 - (RSS/TSS)
print(R.square)


fpar.rump <- function(x) {(2.3478 * x) / ( 10.4647 + x)}
fpar.low <- function(x) {(0.89777 * low$rugosity_mean) / ( 1.16947 + low$rugosity_mean)}

x11(width = 5, height = 5)
ggplot(high, aes(x = rugosity_mean, y = fPAR_mean))+
     geom_point(size = 4, shape = 21)+
     theme_new()+
     xlab(r.label)+
     ylab("fPAR")+
     stat_function(fun = fpar.high, col = "blue", size = 1)+
     geom_point(data = low, aes(x = rugosity_mean, y = fPAR_mean), size = 4, color = "black")+
     stat_function(fun = function(x) (0.89777 * low$rugosity_mean) / ( 1.16947 + low$rugosity_mean), col = "red")


m.multi <- lm(fPAR_mean ~ mean.vai_mean * rugosity_mean * rumple_mean, data = master.site)


####
####
m.vai <- lm(rugosity_mean ~ mean.vai_mean, data = high)
x11(width = 5, height = 5)
ggplot(master.plot, aes(x = rugosity_mean, y = fPAR_mean, color = regime ))+
     geom_point(size = 4, shape = 21)+
     theme_new()+
     xlab(r.label)+
     ylab("fPAR")
