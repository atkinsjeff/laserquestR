# Plots for publication

#
require(plyr)
require(ggplot2)
require(dplyr)
require(vegan)
require(ggrepel)
require(magrittr)

#adding in palette
library(RColorBrewer)
darkcols <- brewer.pal(10, "Dark2")

#Data import
plot.fpar <- read.csv("laserquest_master_fpar_plot.csv")

# designate light regime based on aPAR or above canopy PAR
plot.fpar$regime <- ifelse(plot.fpar$aPAR_mean >= 1000, "high",
                             "low")

plot.fpar$regime <- as.factor(plot.fpar$regime)

### Modeling section
#Subset data for modelling
high <- subset(plot.fpar, aPAR_mean >= 1000)
low <- subset(plot.fpar, aPAR_mean < 1000)

#write.csv(high, "laserquest_master_fpar_plot_high.csv")
#RUGOSITY
#HIGH
y <- high$fPAR_mean
x <- high$rugosity_mean


h.rc<- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
summary(h.rc)

RSS <- sum(residuals(h.rc)^2)
TSS <- sum((y - mean(y))^2)
R.square <- 1 - (RSS/TSS)
print(R.square)

fpar.high <- function(x) {(1.07515 * x) / ( 3.085004 + x)}
#LOW
# the higher model doesnt work.
y <- low$fPAR_mean
x <- low$rugosity_mean


#l.rc<- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
summary(l.rc)

RSS <- sum(residuals(l.rc)^2)
TSS <- sum((y - mean(y))^2)
R.square <- 1 - (RSS/TSS)
print(R.square)

l.rc.lm <- lm(y ~ x)
summary(l.rc.lm)

# TOP RUGOSITY

# CANOPY POROSITY

#high
y <- high$fPAR_mean
x <- high$porosity_mean

h.pc.lm <- lm(y ~ x)
summary(h.pc.lm)

#low
y <- low$fPAR_mean
x <- low$porosity_mean

low.pc.lm <- lm(y ~ x)
summary(low.pc.lm)

# VAI

#high
y <- high$fPAR_mean
x <- high$mean.vai_mean

high.vai.lm <- lm(y ~ x)
summary(high.vai.lm)

# RUMPLE
#HIGH
y <- high$fPAR_mean
x <- high$rumple_mean


high.rumple <- nls(y ~ (a * x) / (b + x) + 4, start = list(a = 1, b = 1))
high.rumple2 <- nls(y ~ a + (b * x) / (c + x), start = list(a = 0, b = 0.1, c = 0.1))
high.rumple3 <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 1, b = 1))
summary(high.rumple)
summary(high.rumple2)
summary(high.rumple3)

RSS <- sum(residuals(high.rumple)^2)
TSS <- sum((y - mean(y))^2)
R.square <- 1 - (RSS/TSS)
print(R.square)

fpar.high.rumple <- function(x) {(-2.7819 * x) / (-0.5596 + x) }
fpar.high.rumple2 <- function(x) {2.849 + (-1.660 * x) / ( -0.821 + x)}
fpar.high.rumpl3 <- function(x) { 1 / (1 + exp(-0.7972 * (x - 2.5677)))}
high.rumple.lm <- lm(y ~ x)
summary(high.rumple.lm)

#sky fraction/
y <- high$fPAR_mean
x <- high$sky.fraction_mean

high.sky <- lm(y ~ x)
summary(high.sky)

# Deep gaps
y <- high$fPAR_mean
x <- high$deep.gaps_mean

high.deep <- lm(y ~ x)
summary(high.deep)
# # importing data from LP_80
# #light <- read.csv("LP_80.csv", header = TRUE)
# light <- read.csv("light_data.CSV", header = TRUE)
# 
# 
# 
# 
# #light <- light[, c(3,4,5,6,7)]
# light <- light[, c("Annotation", "Average.Above.PAR", "Average.Below.PAR", "Tau....", "Leaf.Area.Index..LAI."]
# 
# light <- plyr::rename(light, c("Annotation" = "transect", "Average.Above.PAR" = "aPAR", "Average.Below.PAR" = "bPAR", "Tau...." = "tau", "Leaf.Area.Index..LAI." = "LAI"))
# 
# # calculate fPAR - the fraction of photosynthetically availble radiation absorbed by the canopy
# light$fPAR <- 1 - (light$bPAR / light$aPAR)
# 
# #If aPAR which is the above canopy par is >1000 then we can call that high light
# 
# light$regime <- ifelse(light$aPAR >= 1000, "high",
#                        "low")
# light$regime <- as.factor(light$regime)
# 
# # we want to keep those LAI and fPAR values that are no accurate now so we can caluclate later, but for now let's go with the good ones
# 
# light <- subset(light, LAI > 0)
# light <- subset(light, fPAR != Inf)
# light <- subset(light, fPAR > 0)
# light
# 
# #
# 
# light$site <- substr(light$transect, 0, 4)
# light$plot <- substr(light$transect, 0, 6)
# light$site <- as.factor(light$site)
# 
# 
# # importing rugosity data
# pcl <- read.csv("laserquest_master_output_2017_03_22.CSV", header = TRUE)
# colnames(pcl)[3] <- "plotID"
# 
# # now we can clean this shit up a bit
# pcl$site <- as.factor(gsub("_.*$", "", pcl$plotID))
# 
# # 
# pcl.bad.names <- subset(pcl, pcl$site == c("AF", "FERN", "ARNO", "UVAX"))
# pcl.bad.names$plotnumber <- gsub("_([^_]*)$", "", pcl.bad.names$plotID)
# pcl.bad.names$plotnumber <- gsub(".+_", "", pcl.bad.names$plotnumber)
# # # # 
# pcl.good <- subset(pcl, pcl$site != "AF" & pcl$site != "FERN" & pcl$site !=  "ARNO")
# pcl.good$plotnumber <- sub(".*_", "", pcl.good$plotID)
# 
# 
# pcl <- rbind(pcl.good, pcl.bad.names)
# pcl$plot <- paste0(pcl$site, substr(pcl$plotnumber, 0,2))
# 
# #####
# pcl <- subset(pcl, pcl$rugosity < 50)
# 
# # merging
# master<- merge(pcl, light, by = "plot")
# 
# master$site <- substr(master$plot, 0, 4)
# x11()
# plot(master$rugosity, master$LAI)

###########

#new.master <- read.csv("laserquest_plot_means_with_fPAR.CSV")
#custom plot theme for all
theme_new <- function(base_size = 12){
     theme_bw(base_size = base_size) %+replace%
          theme(
               #line = element_line(colour="black"),
               #text = element_text(colour="black"),
               #axis.text = element_text(colour="black", size=8),
               #strip.text = element_text(size=12),
               #legend.key=element_rect(colour=NA, fill =NA),
               panel.grid = element_blank(),   
               # panel.border = element_blank(),
               panel.background = element_rect(fill = "white", colour = "white"), 
               strip.background = element_rect(fill = NA),
               axis.text = element_text( size = 14),
               axis.ticks.x = element_blank(),
               axis.ticks.y = element_blank(),
               axis.title  = element_text( size = 16, margin = margin(12, unit = "cm"))
          )
     
}
# custom plot label

vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
lai.label =  expression(paste(LAI~(m^2 ~m^-2)))
log.lai.label = expression(paste(log~( LAI~(m^2 ~m^-2))))
r.label = expression("R"[C])
fpar.label = expression(paste(italic("f"), "PAR"))



# break up the light regimes
# 
# high <- subset(master, master$regime == "high")
# low <- subset(master, master$regime == "low")

#Imports high and low from fPAR modeling script

# Canopy Rugosity Model under high light
y <- high$fPAR_mean
x <- high$rugosity_mean
rc.model <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = 0.1, b = 1, k = 0.1))
AIC(rc.model)

#coefficients
a <- coef(rc.model)[1] 
b <- coef(rc.model)[2]
k <- coef(rc.model)[3]

rc.fit <- function(x) {a / (1 + b * exp(-k * x)) }

x11(width = 4, height = 4)
rc.plot <- ggplot(plot.fpar, aes(x = rugosity_mean, y = fPAR_mean, fill = regime))+
     geom_point(size = 3, shape = 21)+
     scale_fill_manual(values = c("black", "white"))+
     # geom_point( data = low, aes( x = rugosity_mean, y = fPAR_mean), size = 3, shape = 21)+
     theme_new()+
     ylim(c(0, 1))+
     scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1))+
     xlim(c(0,45))+
     xlab(r.label)+
     ylab(fpar.label)+
     stat_function(fun = rc.fit, col = "blue", size = 1)+
     theme(legend.position="none")

# Canopy Porosity
x11(width = 4, height = 4)
pc.plot <- ggplot(plot.fpar, aes(x = porosity_mean, y = fPAR_mean, fill = regime))+
     geom_point(size = 3, shape = 21)+
     scale_fill_manual(values = c("black", "white"))+
     theme_new()+
     xlim(0.4, 1)+
     ylim(c(0, 1))+
     xlab(expression("P"[C]))+
     ylab(fpar.label)+
     scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1))+
     geom_smooth(data = subset(plot.fpar, plot.fpar$regime == "high"), method = "lm", size = 1, se = FALSE, color = "blue")+
     theme(legend.position="none")

# Clumping Index
y <- high$fPAR_mean
x <- high$clumping.index_mean

ci.model <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 0.1, b = 1))
AIC(ci.model)

#coefficients
a <- coef(ci.model)[1] 
b <- coef(ci.model)[2]

ci.fit <- function(x) {1 / (1 + exp(-a * (x - b))) }
     
x11(width = 4, height = 4)
ci.plot <- ggplot(plot.fpar, aes(x = clumping.index_mean, y = fPAR_mean, fill = regime))+
     geom_point(size = 3, shape = 21)+
     scale_fill_manual(values = c("black", "white"))+
     theme_new()+
     ylim(c(0, 1))+
     xlim(c(0.7, 1))+
     xlab(expression(Omega))+
     ylab(fpar.label)+
     scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1))+
     stat_function(fun = ci.fit, col = "blue", size = 1)+
     theme(legend.position="none")

# Top Rugosity

x11(width = 4, height = 4)
rt.plot <- ggplot(plot.fpar, aes(x = top.rugosity_mean, y = fPAR_mean, fill = regime))+
     geom_point(size = 3, shape = 21)+
     scale_fill_manual(values = c("black", "white"))+
     theme_new()+
     xlim(c(0,15))+
     ylim(c(0, 1))+
     xlab(expression("R"[T]))+
     ylab(fpar.label)+
     scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1))+
     theme(legend.position="none")

# Mean VAI
# sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 0.1, b = 1))

y <- high$fPAR_mean
x <- high$mean.vai_mean

vai.model <-nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 0.1, b = 1))
AIC(vai.model)

#coefficients
a <- coef(vai.model)[1] 
b <- coef(vai.model)[2]

vai.fit <- function(x) {1 / (1 + exp(-a * (x - b)))}

x11(width = 4, height = 4)
vai.plot <- ggplot(plot.fpar, aes(x = mean.vai_mean, y = fPAR_mean, fill = regime))+
     geom_point(size = 3, shape = 21)+
     scale_fill_manual(values = c("black", "white"))+
     theme_new()+
     xlim(c(0,8))+
     ylab(fpar.label)+
     xlab("VAI")+
     scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1))+
     stat_function(fun = vai.fit, col = "blue", size = 1)+
     theme(legend.position="none")

# Rumple

y <- high$fPAR_mean
x <- high$rumple_mean

rump.model <-nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 0.1, b = 1))
AIC(rump.model)

#coefficients
a <- coef(rump.model)[1] 
b <- coef(rump.model)[2]

rump.fit <- function(x) {1 / (1 + exp(-a * (x - b)))}

x11(width = 4, height = 4)
rump.plot <- ggplot(plot.fpar, aes(x = rumple_mean, y = fPAR_mean, fill = regime))+
     geom_point(size = 3, shape = 21)+
     scale_fill_manual(values = c("black", "white"))+
     theme_new()+
     xlim(c(0,15))+
     scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1))+
     xlab("Rumple")+
     ylab(fpar.label)+
     stat_function(fun = rump.fit, col = "blue", size = 1)+
     theme(legend.position="none")

# Deep Gaps (ADJUSTED)

y <- high$fPAR_mean
x <- high$deep.gaps_mean / 40

#dg.model <-nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = -6, b = 0.1, k = -1))
dg.model <- nls(y ~ a + ((b * x) / (c + x)), start = list( a = 0.1, b = 1, c =  1) )
AIC(dg.model)

#coefficients
a <- coef(dg.model)[1] 
b <- coef(dg.model)[2]
k <- coef(dg.model)[3]

#dg.fit <- function(x) {a / (1 + b * exp(-k * x))}
#dg.fit <- function(x) {a + ((b * x) / (c + x))}

dg.fit <- function(x) {0.9379 + ((-1.50454 * x) / (0.66598 + x))}

x11(width = 4, height = 4)
dg.plot <- ggplot(plot.fpar, aes(x = (deep.gaps_mean / 40), y = fPAR_mean, fill = regime))+
     geom_point(size = 3, shape = 21)+
     scale_fill_manual(values = c("black", "white"))+
     theme_new()+
     xlim(c(0,1))+
     ylim(c(0, 1))+
     xlab("Deep Gaps")+
     ylab(fpar.label)+
     scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1))+
     scale_x_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1))+
     stat_function(fun = dg.fit, col = "blue", size = 1)+
     theme(legend.position="none")


# Sky/ Gap Fraction

x11(width = 4, height = 4)
gf.plot <- ggplot(plot.fpar, aes(x = sky.fraction_mean / 100, y = fPAR_mean, fill = regime))+
     geom_point(size = 3, shape = 21)+
     scale_fill_manual(values = c("black", "white"))+
     theme_new()+
     xlim(c(0, 1))+
     ylim(c(0, 1))+
     xlab(expression(Theta))+
     ylab(fpar.label)+
     scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1))+
     scale_x_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1))+
     geom_smooth(data = subset(plot.fpar, plot.fpar$regime == "high"), method = "lm", size = 1, se = FALSE, color = "blue")+
     theme(legend.position="none")

# MOCH

y <- high$fPAR_mean
x <- high$mean.max.ht_mean

moch.model <-nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = 1, b = 10, k = 0.1))
AIC(moch.model)

#coefficients
a <- coef(moch.model)[1] 
b <- coef(moch.model)[2]
k <- coef(moch.model)[3]

moch.fit <- function(x) {a / (1 + b * exp(-k * x))}

x11(width = 4, height = 4)
moch.plot <- ggplot(plot.fpar, aes(x = mean.max.ht_mean , y = fPAR_mean, fill = regime))+
     geom_point(size = 3, shape = 21)+
     scale_fill_manual(name = "Light Regime",
                         breaks = c("high", "low"),
                         labels = c("High", "Low"),values = c("black", "white"))+
     theme_new()+
     xlim(c(0,25))+
     ylim(c(0, 1))+
     xlab("MOCH")+
     ylab("fPAR")+
     scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1))+
     stat_function(fun = moch.fit, col = "blue", size = 1)

# PANEL PLOT PRE EDIT ( but this kills all the model runs now for some reason)

require(cowplot)
p <- plot_grid(rc.plot, rt.plot, moch.plot, rump.plot, vai.plot, pc.plot, gf.plot, ci.plot, dg.plot, labels = "AUTO", align = 'h', ncol = 3)
x11(width = 11, height = 14)
p


# x11(width = 4, height = 4)
# ggplot(high, aes(x = max.can.ht, y = fPAR))+
#      geom_point(size = 3, shape = 21)+
#      theme_new()+
#      xlim(c(0,60))+
#      xlab("Max Canopy Ht (m)")+
#      ylab("fPAR")
# 
# x11(width = 4, height = 4)
# ggplot(low, aes(x = max.can.ht, y = fPAR))+
#      geom_point(size = 3, shape = 21)+
#      theme_new()+
#      xlim(c(0,60))+
#      xlab("Max Canopy Ht (m)")+
#      ylab("fPAR")
#######SITE FACET PLOTS

#modelling fpar at site level
y <- master.site$fPAR_mean
x <- master.site$rugosity_mean
m.power <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
m.power <- nls(y ~ a * (x^b), start = list(a = 1, b = 1))
summary(m.power)
# m.direct <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
# summary(m.direct)

RSS <- sum(residuals(m.power)^2)
TSS <- sum((y - mean(y))^2)
R.square <- 1 - (RSS/TSS)
print(R.square)

x11()
plot(x, y, xlab = "Canopy Rugosity (m)", ylab = "fPAR")
a <- coef(m.direct)[1]
b <- coef(m.direct)[2]
curve((a * x) / (b + x), col = "blue", add = TRUE)

rm(a)
rm(b)

# the money graph
master.plot <- read.csv("laserquest_plot_means_with_fPAR.csv")

master.plot$regime <-  ifelse(master.plot$aPAR_mean >= 1000, "high",
                                            "low")
master.plot$regime <- as.factor(master.plot$regime)

high <- subset(master.plot, regime == 'high')
low <- subset(master.plot, regime == 'low')

# # Rugosity
x11()
ggplot(high, aes(x = rugosity, y = fPAR, color = site))+
     geom_point(size = 3, shape = 21)+
     #scale_color_manual(values=cbPalette)+
     stat_function(fun = function(x) (1.10026 * x) / (3.78358 + x), geom = "line", color = 'black', size = 1)+
     # geom_smooth(aes(rugosity, fPAR), method = "nls", formula = y ~  a * (x^b), start = list(a = 1, b = 1), se = FALSE)+
     geom_smooth(aes(rugosity, fPAR, color = site), method = lm, se = FALSE)+
     theme_new()+
     xlab(r.label)+
     ylab("fPAR")


######## Now w are working with VAI
x11()
m.lm.vai <- lm(fPAR ~ mean.vai, data = high)
summary(m.lm.vai)

# Testing model assumptions
par(mar = c(4,4,2,4), mfrow = c(1,2))
plot(m.lm.vai, which = c(1,2))

## mean VAI
x11()
ggplot(high, aes(x = mean.vai_mean, y = fPAR_mean, color = site))+
     geom_point(size = 3, shape = 21)+
     #scale_color_manual(values=cbPalette)+
     #stat_function(fun = function(x) (1.10026 * x) / (3.78358 + x), geom = "line", color = 'black', size = 1)+
     geom_smooth(method = "lm",  se = TRUE, color = 'black')+
     geom_smooth(aes(mean.vai_mean, fPAR_mean, color = site), method = lm, se = FALSE)+
     theme_new()+
     xlab(vai.label)+
     ylab("fPAR")

     stat_smooth(method = "nls", formula = "y ~ a * (x^b)", start = list(a = 1, b = 1), se = TRUE)
     facet_wrap( ~ site, ncol = 2)+
     theme(strip.background = element_blank(),
           strip.text.y = element_blank())

x11()
ggplot(high, aes(x = (rugosity/ mean.vai), y = fPAR, color = site))+
     geom_point(size = 3, shape = 21)+
     geom_smooth(aes( (rugosity/mean.vai), fPAR, color = site), method = lm, se = FALSE)+
     theme_new()+
     xlab("Rc/VAI")+
     ylab("fPAR")+
     facet_wrap( ~ site, ncol = 2)+
     theme(strip.background = element_blank(),
           strip.text.y = element_blank())

x11()
ggplot(high, aes(x = mode.el, y = fPAR, color = site))+
     geom_point(size = 3, shape = 21)+
     geom_smooth(aes( (rugosity/mean.vai), fPAR, color = site), method = lm, se = FALSE)+
     theme_new()+
     xlab("mode el")+
     ylab("fPAR")+
     facet_wrap( ~ site, ncol = 2)+
     theme(strip.background = element_blank(),
           strip.text.y = element_blank())
####### SITE LEVEL
####### SITE LEVEL
# Light!

require(plyr)
require(ggplot2)
require(dplyr)
require(vegan)
require(magrittr)
require(ggrepel)

#adding in palette
library(RColorBrewer)
darkcols <- brewer.pal(10, "Dark2")

# # importing data from LP_80
# #light <- read.csv("LP_80.csv", header = TRUE)
# light <- read.csv("light_data.CSV", header = TRUE)
# 
# 
# 
# 
# #light <- light[, c(3,4,5,6,7)]
# light <- light[, c("Annotation", "Average.Above.PAR", "Average.Below.PAR", "Tau....", "Leaf.Area.Index..LAI."]
# 
# light <- plyr::rename(light, c("Annotation" = "transect", "Average.Above.PAR" = "aPAR", "Average.Below.PAR" = "bPAR", "Tau...." = "tau", "Leaf.Area.Index..LAI." = "LAI"))
# 
# # calculate fPAR - the fraction of photosynthetically availble radiation absorbed by the canopy
# light$fPAR <- 1 - (light$bPAR / light$aPAR)
# 
# #If aPAR which is the above canopy par is >1000 then we can call that high light
# 
# light$regime <- ifelse(light$aPAR >= 1000, "high",
#                        "low")
# light$regime <- as.factor(light$regime)
# 
# # we want to keep those LAI and fPAR values that are no accurate now so we can caluclate later, but for now let's go with the good ones
# 
# light <- subset(light, LAI > 0)
# light <- subset(light, fPAR != Inf)
# light <- subset(light, fPAR > 0)
# light
# 
# #
# 
# light$site <- substr(light$transect, 0, 4)
# light$plot <- substr(light$transect, 0, 6)
# light$site <- as.factor(light$site)
# 
# 
# # importing rugosity data
# pcl <- read.csv("laserquest_master_output_2017_02_20.CSV", header = TRUE)
# colnames(pcl)[3] <- "plotID"
# 
# # now we can clean this shit up a bit
# pcl$site <- as.factor(gsub("_.*$", "", pcl$plotID))
# 
# 
# pcl.bad.names <- subset(pcl, pcl$site == c("AF", "FERN", "ARNO"))
# pcl.bad.names$plotnumber <- gsub("_([^_]*)$", "", pcl.bad.names$plotID)
# pcl.bad.names$plotnumber <- gsub(".+_", "", pcl.bad.names$plotnumber)
# 
# pcl.good <- subset(pcl, pcl$site != "AF" & pcl$site != "FERN" & pcl$site !=  "ARNO")
# pcl.good$plotnumber <- sub(".*_", "", pcl.good$plotID)
# 
# pcl <- rbind(pcl.good, pcl.bad.names)
# pcl$plot <- paste0(pcl$site, substr(pcl$plotnumber, 0,2))
# 
# #####
# pcl <- subset(pcl, pcl$rugosity < 50)
# 
# # # pcl means
# # pcl %>% group_by(plot) %>% summarise_each(funs(mean, sd), -X, -X1, -plotID, -transect.length, -plotnumber, -site) -> pcl.means 
# # 
# # pcl.means$plot <- as.factor(pcl.means$plot) 
# # pcl.means <- data.frame(pcl.means)
# # 
# # # light stuff
# # 
# # light %>% group_by(plot) %>% summarise_each(funs(mean, sd), -transect, -site) -> light.means 
# # 
# # light.means$plot <- as.factor(light.means$plot) 
# # light.means <- data.frame(light.means)
# 
# # merging
# master<- merge(pcl, light, by = "plot")
# 
# master$site <- substr(master$plot, 0, 4)
# plot(master$rugosity, master$LAI)
# 
# #master <- na.omit(master)
# 
# # custom plot theme
# 
# # custom plot label
# 
# vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
# lai.label =  expression(paste(LAI~(m^2 ~m^-2)))
# log.lai.label = expression(paste(log~( LAI~(m^2 ~m^-2))))
# 
# 
# ### now site level
# 
# # pcl means
# pcl %>% group_by(site) %>% summarise_each(funs(mean, sd), -X, -X1, -plotID, -transect.length, -plotnumber, -plot) -> pcl.site.means 
# 
# pcl.site.means$site <- as.factor(pcl.site.means$site) 
# pcl.site.means <- data.frame(pcl.site.means)
# #write.csv(pcl.site.means, "laserquest_site_means_no_fpar.csv")
# 
# # light stuff
# 
# light %>% group_by(site) %>% summarise_each(funs(mean, sd), -transect, -plot) -> light.site.means 
# 
# light.site.means$site <- as.factor(light.site.means$site) 
# light.site.means <- data.frame(light.site.means)

# merging
#master.site<- merge(pcl.site.means, light.site.means, by = "site")
#write.csv(master.site, "laserquest_site_means.csv")

# SITE MODELLING
#RUGOSITY
#HIGH
y <- master.site$fPAR_mean
x <- master.site$rugosity_mean


m.rc.site<- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
summary(m.rc.site)
AIC(m.rc.site)

fit.rc.site <- function(x) {(1.08604 * x) / (3.43236 + x)}

x11(width = 4, height = 4)
site.rc <- ggplot(master.site, aes(x = rugosity_mean, y = fPAR_mean))+
     geom_point(size = 2)+
     geom_text_repel(data = master.site, aes(rugosity_mean, fPAR_mean, label = site))+
     #scale_color_manual(values=cbPalette)+
     #scale_color_brewer(palette="Dark2")+
     #geom_errorbarh(aes(xmin =rugosity_mean - rugosity_sd, xmax = rugosity_mean + rugosity_sd)) + 
     #geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
     theme_new()+
     xlim(c(0, 45))+
     scale_y_continuous(breaks = seq(0.2, 1, 0.2), limits = c(0.2, 1))+
     xlab(expression("R"[C]))+
     ylab(fpar.label)+
     stat_function(fun = fit.rc.site, col = "blue", size = 1)
# stat_function(fun = function(x) (0.97624 * x) / (1.74132 + x), colour = "dark grey", size = 1.25)

# Top Rugosity
x11(width = 4, height = 4)
site.rt <- ggplot(master.site, aes(x = top.rugosity_mean, y = fPAR_mean))+
     geom_point(size = 2)+
     geom_text_repel(data = master.site, aes(label = site))+
     #scale_color_manual(values=cbPalette)+
     #scale_color_brewer(palette="Dark2")+
     #geom_errorbarh(aes(xmin =rugosity_mean - rugosity_sd, xmax = rugosity_mean + rugosity_sd)) + 
     #geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
     theme_new()+
     xlim(c(0, 15))+
     scale_y_continuous(breaks = seq(0.2, 1, 0.2), limits = c(0.2, 1))+
     xlab(expression("R"[T]))+
     ylab(fpar.label)

# Porosity
summary(lm(master.site$fPAR_mean ~ master.site$rumple_mean))

x11(width = 4, height = 4)
site.pc <- ggplot(master.site, aes(x = porosity_mean, y = fPAR_mean))+
     geom_point(size = 2)+
     geom_text_repel(data = master.site, aes(porosity_mean, fPAR_mean, label = site), force = 10)+
     #scale_color_manual(values=cbPalette)+
     #scale_color_brewer(palette="Dark2")+
     #geom_errorbarh(aes(xmin =rugosity_mean - rugosity_sd, xmax = rugosity_mean + rugosity_sd)) + 
     #geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
     theme_new()+
     xlim(c(0.6, 1))+
     scale_y_continuous(breaks = seq(0.2, 1, 0.2), limits = c(0.2, 1))+
     xlab(expression("P"[C]))+
     ylab(fpar.label)+
     geom_smooth(method = "lm", formula = y ~ x, color = "blue", size = 1, se = FALSE)

# Rumple

y <- master.site$fPAR_mean
x <- master.site$rumple_mean


m.rump.site<- nls(y ~ 1 / (1 + exp(a * (x - b))), start = list(a = 0.1, b = 1))
summary(m.rump.site)
AIC(m.rump.site)

x11(width = 4, height = 4)
site.rump <- ggplot(master.site, aes(x = rumple_mean, y = fPAR_mean))+
     geom_point(size = 2)+
     geom_text_repel(data = master.site, aes(label = site))+
     #scale_color_manual(values=cbPalette)+
     #scale_color_brewer(palette="Dark2")+
     #geom_errorbarh(aes(xmin =rugosity_mean - rugosity_sd, xmax = rugosity_mean + rugosity_sd)) + 
     #geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
     xlim(c(0, 15))+
     scale_y_continuous(breaks = seq(0.2, 1, 0.2), limits = c(0.2, 1))+
     theme_new()+
     xlab("Rumple")+
     ylab("fPAR")+
     geom_smooth(method = "nls",
                 formula = y ~ 1 / (1 + exp(a * (x - b))),
                 method.args = list(start = list(a = 0.1, b = 1)),
                 se = FALSE,
                 color = "blue",
                 size = 1)

# VAI
# sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 0.1, b = 1))

y <- master.site$fPAR_mean
x <- master.site$mean.vai_mean

m.vai.site <-nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 0.1, b = 1))
AIC(m.vai.site)

#coefficients
a <- coef(vai.model)[1] 
b <- coef(vai.model)[2]

vai.fit <- function(x) {1 / (1 + exp(-a * (x - b)))}

x11(width = 4, height = 4)
site.vai <- ggplot(master.site, aes(x = mean.vai_mean, y = fPAR_mean))+
     geom_point(size = 2)+
     geom_text_repel(data = master.site, aes(label = site))+
     #scale_color_manual(values=cbPalette)+
     #scale_color_brewer(palette="Dark2")+
     #geom_errorbarh(aes(xmin =rugosity_mean - rugosity_sd, xmax = rugosity_mean + rugosity_sd)) + 
     #geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
     theme_new()+
     xlim(c(2, 8))+
     scale_y_continuous(breaks = seq(0.2, 1, 0.2), limits = c(0.2, 1))+
     xlab("VAI")+
     ylab(fpar.label)+
     geom_smooth(method = "nls",
                 formula = y ~ (1 / (1 + exp(-a * (x - b)))),
                 method.args = list(start = list(a = 0.1, b = 1)),
                 se = FALSE,
                 color = "blue",
                 size = 1)
#MOCH
# y <- master.site$fPAR_mean
# x <- master.siteh$mean.max.ht_mean
# 
# site.moch.model <-nls(y ~ a + ((b * x) / (c + x)), start = list(a = 16 , b = -15, c = - 1))
# AIC(site.moch.model)

x11(width = 4, height = 4)
site.moch <- ggplot(master.site, aes(x = mean.max.ht_mean , y = fPAR_mean))+
     geom_point(size = 2)+
     geom_text_repel(data = master.site, aes(label = site))+
     theme_new()+
     xlim(c(5,25))+
     ylim(c(0, 1))+
     xlab("MOCH")+
     ylab("fPAR")+
     scale_y_continuous(breaks = seq(0.2, 1, 0.2), limits = c(0.2, 1))
# 
# +
#      geom_smooth(method = "nls",
#                  formula = y ~ a + ((b * x) / (c + x)),
#                  method.args = list(start = list(a = 16, b = -15, c = -1)),
#                  se = FALSE,
#                  color = "blue",
#                  size = 1)
# DEEP GAP ADJ SITE
x11(width = 4, height = 4)

site.dg <- ggplot(master.site, aes(x = (deep.gaps_mean / 40), y = fPAR_mean))+
     geom_point(size = 2)+
     geom_text_repel(data = master.site, aes(label = site))+
     theme_new()+
     xlim(c(0, 0.45))+
     scale_y_continuous(breaks = seq(0.2, 1, 0.2), limits = c(0.2, 1))+
     xlab("Deep Gaps")+
     ylab(fpar.label)+
     geom_smooth(method = "lm", size = 1, color = "blue", se = FALSE)

# Gap Fraction/Sky Fraction
x11(width = 4, height = 4)

site.gf <- ggplot(master.site, aes(x = sky.fraction_mean / 100, y = fPAR_mean))+
     geom_point(size = 2)+
     geom_text_repel(data = master.site, aes(label = site))+
     theme_new()+
     xlim(c(0, 0.6))+
     scale_y_continuous(breaks = seq(0.2, 1, 0.2), limits = c(0.2, 1))+
     scale_x_continuous(breaks = seq(0, 0.6, 0.3), limits = c(0, 0.6))+
     xlab(expression(Theta))+
     ylab(fpar.label)+
     geom_smooth(method = "lm", size = 1, color = "blue", se = FALSE)

# Clumping Index!
# x <- master.site$clumping.index_mean
# y <- master.site$fPAR_mean
# m.site.ci <- nls(y ~ a + (b * log(x)), start = list(a = -0.1, b = 1))
# AIC(m.site.ci)

x11(width = 4, height = 4)
site.ci <- ggplot(master.site, aes(x = clumping.index_mean, y = fPAR_mean))+
     geom_point(size = 2)+
     geom_text_repel(data = master.site, aes(label = site))+
     #scale_color_manual(values=cbPalette)+
     #scale_color_brewer(palette="Dark2")+
     #geom_errorbarh(aes(xmin =rugosity_mean - rugosity_sd, xmax = rugosity_mean + rugosity_sd)) + 
     #geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
     theme_new()+
     xlim(c(0.8, 1))+
     scale_y_continuous(breaks = seq(0.2, 1, 0.2), limits = c(0.2, 1))+
     scale_x_continuous(breaks = seq(0.8, 1, 0.1), limits = c(0.8, 1))+
     xlab(expression(Omega))+
     ylab(fpar.label)

# x11(width = 4, height = 4)
site.vai <- ggplot(master.site, aes(x = mean.vai_mean, y = fPAR_mean))+
     geom_point(size = 2)+
     geom_text_repel(data = master.site, aes(label = site))+
     #scale_color_manual(values=cbPalette)+
     #scale_color_brewer(palette="Dark2")+
     #geom_errorbarh(aes(xmin =rugosity_mean - rugosity_sd, xmax = rugosity_mean + rugosity_sd)) + 
     #geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
     theme_new()+
     xlim(c(2, 8))+
     scale_y_continuous(breaks = seq(0.2, 1, 0.2), limits = c(0.2, 1))+
     xlab(vai.label)+
     ylab(fpar.label)+
     geom_smooth(method = "lm", se = FALSE)


# x11(width = 4, height = 4)
site.rump <- ggplot(master.site, aes(x = rumple_mean, y = fPAR_mean))+
     geom_point(size = 2)+
     geom_text_repel(data = master.site, aes(label = site))+
     #scale_color_manual(values=cbPalette)+
     #scale_color_brewer(palette="Dark2")+
     #geom_errorbarh(aes(xmin =rugosity_mean - rugosity_sd, xmax = rugosity_mean + rugosity_sd)) + 
     #geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
     xlim(c(0, 15))+
     scale_y_continuous(breaks = seq(0.2, 1, 0.2), limits = c(0.2, 1))+
     theme_new()+
     xlab("Rumple")+
     ylab("fPAR")+
     geom_smooth(method = "lm", formula= y ~ log(x), se = FALSE)

require(cowplot)
q <- plot_grid(site.rc, site.rt, site.pc, site.ci, site.vai, site.rump, labels = "AUTO", align = 'h', ncol = 2)
x11(width = 11, height = 14)
q 

geom_point(data = df, aes(x = long, y = lat), color = "black", pch = 21, fill = "yellow", size = 3)+
     geom_text_repel(data = df, aes(label = paste(" ", as.character(site_code), sep = ""), x = long, y = lat), angle = 0, hjust = 0, color = "black")+
     

x11()


#####

site.means <- read.csv("laserquest_site_stats_0616.csv")

x11()
ggplot(site.means)
