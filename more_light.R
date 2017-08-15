# Light!

require(plyr)
require(ggplot2)
require(dplyr)
require(vegan)
require(magrittr)

#adding in palette
library(RColorBrewer)
darkcols <- brewer.pal(10, "Dark2")

# importing data from LP_80
#light <- read.csv("LP_80.csv", header = TRUE)
light <- read.csv("light_data.CSV", header = TRUE)




#light <- light[, c(3,4,5,6,7)]
light <- light[, c("Annotation", "Average.Above.PAR", "Average.Below.PAR", "Tau....", "Leaf.Area.Index..LAI."]

light <- plyr::rename(light, c("Annotation" = "transect", "Average.Above.PAR" = "aPAR", "Average.Below.PAR" = "bPAR", "Tau...." = "tau", "Leaf.Area.Index..LAI." = "LAI"))

# calculate fPAR - the fraction of photosynthetically availble radiation absorbed by the canopy
light$fPAR <- 1 - (light$bPAR / light$aPAR)

#If aPAR which is the above canopy par is >1000 then we can call that high light

light$regime <- ifelse(light$aPAR >= 1000, "high",
                       "low")
light$regime <- as.factor(light$regime)

# we want to keep those LAI and fPAR values that are no accurate now so we can caluclate later, but for now let's go with the good ones

light <- subset(light, LAI > 0)
light <- subset(light, fPAR != Inf)
light <- subset(light, fPAR > 0)
light

#

light$site <- substr(light$transect, 0, 4)
light$plot <- substr(light$transect, 0, 6)
light$site <- as.factor(light$site)


# importing rugosity data
pcl <- read.csv("laserquest_master_output_2017_03_22.CSV", header = TRUE)
colnames(pcl)[3] <- "plotID"

# now we can clean this shit up a bit
pcl$site <- as.factor(gsub("_.*$", "", pcl$plotID))

# 
pcl.bad.names <- subset(pcl, pcl$site == c("AF", "FERN", "ARNO", "UVAX"))
pcl.bad.names$plotnumber <- gsub("_([^_]*)$", "", pcl.bad.names$plotID)
pcl.bad.names$plotnumber <- gsub(".+_", "", pcl.bad.names$plotnumber)
# # # 
pcl.good <- subset(pcl, pcl$site != "AF" & pcl$site != "FERN" & pcl$site !=  "ARNO")
 pcl.good$plotnumber <- sub(".*_", "", pcl.good$plotID)


pcl <- rbind(pcl.good, pcl.bad.names)
pcl$plot <- paste0(pcl$site, substr(pcl$plotnumber, 0,2))

#####
pcl <- subset(pcl, pcl$rugosity < 50)

# # pcl means
pcl %>% group_by(plot) %>% summarise_each(funs(mean, sd), -X, -X1, -plotID, -transect.length, -plotnumber, -site) -> pcl.means

pcl.means$plot <- as.factor(pcl.means$plot)
pcl.means <- data.frame(pcl.means)

# light stuff

light %>% group_by(plot) %>% summarise_each(funs(mean, sd), -transect, -site) -> light.means

light.means$plot <- as.factor(light.means$plot)
light.means <- data.frame(light.means)
# 
# master.plot<- merge(pcl.means, light.means, by = "plot")
# write.csv(master.plot, "laserquest_plot_means_with_fPAR.csv")
# write.csv(pcl.means, "laserquest_plot_means.csv")


plot(master.site$rugosity_mean, master.site$LAI_mean)
# merging
master<- merge(pcl, light, by = "plot")

master$site <- substr(master$plot, 0, 4)
plot(master$rugosity, master$LAI)

#master <- na.omit(master)

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


x11()
ggplot(master, aes(x = LAI, y = mean.vai, color = regime))+
     geom_point(size = 5)+
     # geom_errorbar(aes(ymin =mean.vai_mean - mean.vai_sd, ymax = mean.vai_mean + mean.vai_sd), width=.25) + 
     # geom_errorbarh(aes(xmin = LAI_mean -LAI_sd, xmax = LAI_mean + LAI_sd), width=.25)+
     theme_new()+
     xlab("LAI (Ceptometer)")+
     ylab("VAI (PCL)")

x11()
ggplot(master, aes(y = LAI, x = rugosity, color = regime))+
     geom_point(size = 5)+
     theme_new()+
     ylab("LAI (Ceptometer)")+
     xlab("Rugosity (PCL)")



x11()
ggplot(master, aes(y = fPAR, x = LAI, color = regime))+
     geom_point(size = 5)+
     theme_new()+
     ylab("fPAR (Ceptometer)")+
     xlab("LAI (Ceptometer)")

x11()
ggplot(master, aes(y = fPAR, x = rugosity, color = regime))+
     geom_point(size = 3)+
     theme_new()+
     ylab("fPAR (Ceptometer)")+
     xlab("Rugosity (PCL")



x11()
ggplot(master, aes(x = fPAR, y = mode.el, color = regime))+
     geom_point(size = 5)+
     theme_new()+
     xlab("fPAR (Ceptometer)")+
     ylab("mode.el (PCL")



### now site level

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
# 
# # merging
# master.site<- merge(pcl.site.means, light.site.means, by = "site")
# write.csv(master.site, "laserquest_site_means_with_fPAR.csv")
# write.csv(pcl.site.means, "laserquest_site_means.csv")
# 
# plot(master.site$rugosity_mean, master.site$LAI_mean)

#palette

master.site <- read.csv("laserquest_site_means_with_fPAR.csv")
# The palette with black:
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#666666", "#333333")

# To use for fills, add
scale_fill_manual(values=cbPalette)

vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
lai.label =  expression(paste(LAI~(m^2 ~m^-2)))
log.lai.label = expression(paste(log~( LAI~(m^2 ~m^-2))))
r.label = expression("R"[C])
fpar.label = expression(paste(italic("f"), "PAR"))

x11()
ggplot(master.site, aes(x = rugosity_mean, y = fPAR_mean,  label = site))+
     #geom_point(size = 5)+
     geom_text()+
     #scale_color_manual(values=cbPalette)+
     #scale_color_brewer(palette="Dark2")+
     #geom_errorbarh(aes(xmin =rugosity_mean - rugosity_sd, xmax = rugosity_mean + rugosity_sd)) + 
     #geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
     theme_new()+
     ylab(fpar.label)+
     xlab(r.label)+
     stat_function(fun = function(x) (0.97624 * x) / (1.74132 + x), colour = "blue", size = 1.25)

#modelling fpar at site level
y <- master.plot$fPAR_mean
x <- master.plot$rugosity_mean
m.power <- nls(y ~ a * (x^b), start = list(a = 1, b = 1))
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

x11()
ggplot(master.site, aes(x = mean.vai_mean, y = fPAR_mean, color = site))+
     geom_point(size = 5)+
     scale_color_manual(values=cbPalette)+
     geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+ 
     geom_errorbarh(aes(xmin = mean.vai_mean - mean.vai_sd, xmax = mean.vai_mean + mean.vai_sd))+
     theme_new()+
     ylab(fpar.label)+
     xlab(vai.label)

x11()
ggplot(master.site, aes(x = porosity_mean, y = fPAR_mean,  color = site))+
     geom_point(size = 5)+
     scale_color_manual(values=cbPalette)+     geom_errorbarh(aes(xmin =porosity_mean - porosity_sd, xmax = porosity_mean + porosity_sd)) + 
     geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
     theme_new()+
     ylab("fPAR ")+
     xlab("Canopy Porosity")

x11()
ggplot(master.site, aes(x = clumping.index_mean, y = fPAR_mean,  color = site))+
     geom_point(size = 5)+
     scale_color_manual(values=cbPalette)+     geom_errorbarh(aes(xmin =clumping.index_mean - clumping.index_sd, xmax = clumping.index_mean + clumping.index_sd)) + 
     geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
     theme_new()+
     ylab("fPAR ")+
     xlab("Clumping Index")


lai.fit <- lm(LAI_mean ~ rugosity_mean, data = master.site)

master[!is.finite(master)] <- NA
master <- na.omit(master)
master$site <- as.factor(master$site)
pca.fit <- princomp(master, cor = TRUE)




########
# statistical reconfigurin'
#simulate some data
set.seed(20160227)
x<-seq(0,50,1)
y<-((runif(1,10,20)*x)/(runif(1,0,10)+x))+rnorm(51,0,1)
#for simple models nls find good starting values for the parameters even if it throw a warning
m<-nls(y~a*x/(b+x))
#get some estimation of goodness of fit
cor(y, predict(m))

#plot
plot(x,y)
lines(x,predict(m),lty=2,col="red",lwd=3)


###
###
y <- master$fPAR
x <- master$LAI
plot(log(x),y)
# first we take the log of fPAR
master$log.LAI <- log(master$LAI)

#build or model

lm.fit.log.LAI <- lm(fPAR ~ log.LAI, data = master)
summary(lm.fit.log.LAI)
#graph to show than jenk
x11()
ggplot(master, aes(y = fPAR, x = log.LAI,  color = site))+
     geom_point(size = 5)+
     scale_color_manual(values=cbPalette)+
     theme_new()+
     ylab("fPAR ")+
     xlab(log.lai.label)+
     geom_smooth(method = "lm", se = FALSE, color = "black")

master$predict.fPAR <- (0.2645999 * master$log.LAI) + 0.5057390
###
y <- master$predict.fPAR
x <- master$log.LAI
plot(x, y)


#now we make the residuals
master$resid.fPAR <- master$predict.fPAR - master$fPAR_mean

plot(master$porosity_mean, master$resid.fPAR)
lm.r.fit <- lm(resid.fPAR ~ deep.gaps_mean, data = master)
summary(lm.r.fit)


## making multiple regression
lm.multi <- lm(fPAR_mean ~ log.LAI + porosity_mean + deep.gaps_mean, data = master)
summary(lm.multi)

x11()
ggplot(master, aes(y = resid.fPAR, x = rugosity_mean,  color = site))+
     geom_point(size = 5)+
     scale_color_manual(values=cbPalette)+
     theme_new()+
     ylab("fPAR")+
     xlab("Rugosity (m)")+
     geom_smooth(method = "lm", se = FALSE, color = "black")

# now let's look at some correlation matrix
nums <- sapply(master, is.numeric)
row.names(master) <- master$plot

m.master <-master[, nums]
require(psych)

require(corrplot)
m.master <- m.master[, c(4:18)]
m <- cor(m.master)
x11()
corrplot(m, method = "circle")

corr.test(m.master)



### density curve for comparison of msmts
require(sm)
attach(mtcars)

# create value labels 
cyl.f <- factor(cyl, levels= c(4,6,8),
                labels = c("4 cylinder", "6 cylinder", "8 cylinder")) 

# plot densities 
x11()
sm.density.compare(pcl$rugosity, pcl$site, model = "equal")
title(main="Site distribution of Rugosity")
legend("topright", levels(pcl$site), fill=2+(0:nlevels(pcl$site)))

#fpar light densities
x11()
sm.density.compare(master$rugosity, master$regime, xlab = "Rugosity (PCL)", ylab = "Kernel Density")
title(main="Rugosity by Light Regime")
legend("topright", levels(light$regime), fill=2+(0:nlevels(light$regime)))




# add legend via mouse click
colfill<-c(2:(2+length(levels(cyl.f)))) 
legend(locator(1), levels(cyl.f), fill=colfill
       
       ### ggplot
       pcl.x <- subset(pcl, pcl$site != "AF" )
       #pcl.x <- subset(pcl.x, pcl.x$site != "UVAX")
       x11()
       ggplot(data = pcl.x, aes(x=rugosity))+
            geom_density( aes(fill = site), alpha = 0.4)+
            facet_wrap( ~ site)+
            xlab("Rugosity")+
            ylab("")+
            xlim(0,50)+
            theme(strip.text.x = element_text(size=10),
                  strip.background = element_rect(colour="white", fill="white"))+
            scale_fill_discrete(breaks = c("ARNO", "FERN", "GRSM", "HARV", "MLBS", "OSBS", "SCBI","SERC", "TALL", "TREE", "UNDE"),
                                labels = c("Arnot Forest", "Fernow Exp. Forest", "Great Smokies", "Harvard Forest", "Mountain Lake", "Ordway-Swisher", "SCBI","SERC", "Talladega Natl. Forest", "Treehaven", "UNDERC") )
       
       
       
       
       #simulate some data
       x <- 1:100
       y <- 1 + x^0.15 + rnorm(100, 0, 0.01)
       
       m.fpar <- lm(fPAR ~ log(rugosity), data = master)
       x11()
       plot( master$fPAR ~ log(master$rugosity), xlab = "ln (Rugosity)", ylab = "fPAR")
       abline(lm(fPAR ~ log(rugosity), data = master), col = "blue")
       summary(m.fpar)
       # some model evaluation, residual sum of square and R²
       RSS <- sum(residuals(m)^2)
       TSS <- sum((y - mean(y))^2)
       R.square <- 1 - (RSS/TSS)
       
       #
       Lob.329 <- Loblolly[ Loblolly$Seed == "329", ]
       SSasympOrig( Lob.329$age, 100, -3.2 )  # response only
       Asym <- 100
       lrc <- -3.2
       SSasympOrig( Lob.329$age, Asym, lrc ) # response and gradient
       getInitial(height ~ SSasympOrig(age, Asym, lrc), data = Lob.329)
       ## Initial values are in fact the converged values
       fm1 <- nls(height ~ SSasympOrig( age, Asym, lrc), data = Lob.329)
       summary(fm1)
       
       #
       xy <- master[,c("fPAR", "rugosity")]
       
       #now that we have extracted this set, we sort
       xy <- xy[order(xy$fPAR),]
       xy$fPAR <- xy$fPAR * 100
       
       #deriving model parameters
       NLSstAsymptotic(sortedXyData(expression(rugosity), expression(fPAR), xy))
       
       #setting model parameters
       Asym <- 100
       lrc <- -0.90634
       
       m.xy <- nls(fPAR ~ SSasympOrig(rugosity, Asym, lrc), data = xy)
       summary(m.xy)
   #model evaluation
       y <- xy$fPAR
       RSS <- sum(residuals(m.xy)^2)
       TSS <- sum((y - mean(y))^2)
       R.square <- 1 - (RSS/TSS)
       R.square
       
       
       lm.lai <- lm(fPAR ~ porosity, data = master)
       summary(lm.lai)
       
       x11()
       plot(master$fPAR, master$mean.vai)
       
       
##### Now modelling with consideration for light
       RMSE <- function(m, o){
            sqrt(mean((m - o)^2))
       }
       
       
       
       direct <- subset(master, master$regime == "direct")
       m.fpar <- lm(fPAR ~ log(rugosity), data = direct)
       x11()
       plot(direct$fPAR ~ direct$oneover, xlab = "ln (Rugosity)", ylab = "fPAR", main= "Direct 1/Rc")
       x11()
       plot( log(direct$fPAR) ~ log(direct$rugosity), xlab = "ln (Rugosity)", ylab = "ln(fPAR)", main= "Direct power law")
       
       abline(lm(fPAR ~ log(rugosity), data = direct), col = "blue")
       summary(m.fpar)
       
       #testing model fits
       direct.fitted <- direct[, c("rugosity", "fPAR")]
       direct.fitted$model <- (0.1445 * log(direct.fitted$rugosity)) + 0.50108
       
       direct.rmse <- RMSE(direct.fitted$model, direct.fitted$fPAR)
       
       diffuse <- subset(master, master$regime == "diffuse")
       m.fpar <- lm(fPAR ~ log(rugosity), data = diffuse)
       x11()
       plot(diffuse$fPAR ~ log(diffuse$rugosity), xlab = "ln (Rugosity)", ylab = "fPAR", main= "Diffuse Light")
       abline(lm(fPAR ~ log(rugosity), data = diffuse), col = "blue")
       summary(m.fpar)   
       
       
       
       write.csv(master, "master_light_csc.csv")
       
       
       
       
       
       
       
       
       
       
       
       
       