# Light!

require(plyr)
require(ggplot2)
require(dplyr)
require(vegan)

# importing data from LP_80
#light <- read.csv("LP_80.csv", header = TRUE)
light <- read.csv("light_data.CSV", header = TRUE)

#light <- light[, c(3,4,5,6,7)]
light <- light[, c("Annotation", "Average.Above.PAR", "Average.Below.PAR", "Tau....", "Leaf.Area.Index..LAI."]

light <- plyr::rename(light, c("Annotation" = "transect", "Average.Above.PAR" = "aPAR", "Average.Below.PAR" = "bPAR", "Tau...." = "tau", "Leaf.Area.Index..LAI." = "LAI"))

# calculate fPAR - the fraction of photosynthetically availble radiation absorbed by the canopy
light$fPAR <- 1 - (light$bPAR / light$aPAR)


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
pcl <- read.csv("laserquest_master_output.CSV", header = TRUE)
colnames(pcl)[3] <- "plotID"

# now we can clean this shit up a bit
pcl$site <- as.factor(gsub("_.*$", "", pcl$plotID))


pcl.bad.names <- subset(pcl, pcl$site == c("AF", "FERN", "ARNO"))
pcl.bad.names$plotnumber <- gsub("_([^_]*)$", "", pcl.bad.names$plotID)
pcl.bad.names$plotnumber <- gsub(".+_", "", pcl.bad.names$plotnumber)

pcl.good <- subset(pcl, pcl$site != "AF" & pcl$site != "FERN" & pcl$site !=  "ARNO")
pcl.good$plotnumber <- sub(".*_", "", pcl.good$plotID)

pcl <- rbind(pcl.good, pcl.bad.names)
pcl$plot <- paste0(pcl$site, substr(pcl$plotnumber, 0,2))

# pcl means
pcl %>% group_by(plot) %>% summarise_each(funs(mean, sd), -X, -X1, -plotID, -transect.length, -plotnumber, -site) -> pcl.means 

pcl.means$plot <- as.factor(pcl.means$plot) 
pcl.means <- data.frame(pcl.means)

# light stuff

light %>% group_by(plot) %>% summarise_each(funs(mean, sd), -transect, -site) -> light.means 

light.means$plot <- as.factor(light.means$plot) 
light.means <- data.frame(light.means)

# merging
master<- merge(pcl.means, light.means, by = "plot")

master$site <- substr(master$plot, 0, 4)
plot(master$rugosity_mean, master$LAI_mean)

master <- na.omit(master)

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
ggplot(master, aes(x = LAI_mean, y = mean.vai_mean, color = site))+
     geom_point(size = 5)+
     # geom_errorbar(aes(ymin =mean.vai_mean - mean.vai_sd, ymax = mean.vai_mean + mean.vai_sd), width=.25) + 
     # geom_errorbarh(aes(xmin = LAI_mean -LAI_sd, xmax = LAI_mean + LAI_sd), width=.25)+
     theme_new()+
     xlab("LAI (Ceptometer)")+
     ylab("VAI (PCL)")

x11()
ggplot(master, aes(y = LAI_mean, x = rugosity_mean, color = site))+
     geom_point(size = 5)+
     theme_new()+
     ylab("LAI (Ceptometer)")+
     xlab("Rugosity (PCL)")

x11()
ggplot(master, aes(y = fPAR_mean, x = rugosity_mean, color = site))+
     geom_point(size = 5)+
     geom_errorbarh(aes(xmin =rugosity_mean - rugosity_sd, xmax = rugosity_mean + rugosity_sd), width=.05) + 
     geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd), width=.05)+
     theme_new()+
     ylab("fPAR (Ceptometer)")+
     xlab("Rugosity (PCL)")

x11()
ggplot(master, aes(x = fPAR_mean, y = LAI_mean, color = site))+
     geom_point(size = 5)+
     theme_new()+
     xlab("fPAR (Ceptometer)")+
     ylab("LAI (Ceptometer)")

x11()
ggplot(master, aes(x = fPAR_mean, y = mode.el_mean, color = site))+
     geom_point(size = 5)+
     theme_new()+
     xlab("fPAR (Ceptometer)")+
     ylab("mode.el (PCL")



### now site level

# pcl means
pcl %>% group_by(site) %>% summarise_each(funs(mean, sd), -X, -X1, -plotID, -transect.length, -plotnumber, -plot) -> pcl.site.means 

pcl.site.means$site <- as.factor(pcl.site.means$site) 
pcl.site.means <- data.frame(pcl.site.means)

# light stuff

light %>% group_by(site) %>% summarise_each(funs(mean, sd), -transect, -plot) -> light.site.means 

light.site.means$site <- as.factor(light.site.means$site) 
light.site.means <- data.frame(light.site.means)

# merging
master.site<- merge(pcl.site.means, light.site.means, by = "site")
master.site <-na.omit(master.site)

plot(master.site$rugosity_mean, master.site$LAI_mean)

#palette

# The palette with black:
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#666666")

# To use for fills, add
scale_fill_manual(values=cbPalette)

x11()
ggplot(master.site, aes(x = rugosity_mean, y = fPAR_mean,  color = site))+
     geom_point(size = 5)+
     scale_color_manual(values=cbPalette)+
     #scale_color_brewer(palette="Dark2")+
     geom_errorbarh(aes(xmin =rugosity_mean - rugosity_sd, xmax = rugosity_mean + rugosity_sd)) + 
     geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
     theme_new()+
     ylab("fPAR ")+
     xlab("Rugosity (m)")

x11()
ggplot(master.site, aes(y = LAI_mean, x = rugosity_mean, color = site))+
     geom_point(size = 5)+
     scale_color_manual(values=cbPalette)+
     geom_errorbarh(aes(xmin =rugosity_mean - rugosity_sd, xmax = rugosity_mean + rugosity_sd)) + 
     geom_errorbar(aes(ymin = LAI_mean - LAI_sd, ymax = LAI_mean + LAI_sd))+
     theme_new()+
     ylab(lai.label)+
     xlab("Rugosity (m)")
     geom_smooth(method = "lm", se = FALSE, color = "black")
     
x11()
ggplot(master.site, aes(x = LAI_mean, y = fPAR_mean,  color = site))+
     geom_point(size = 5)+
     scale_color_brewer(palette="Dark2")+
     geom_errorbarh(aes(xmin =LAI_mean - LAI_sd, xmax = LAI_mean + LAI_sd)) + 
     geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
     theme_new()+
     ylab("fPAR ")+
     xlab(lai.label)


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
y <- master$fPAR_mean
x <- master$LAI_mean
plot(log(x),y)
# first we take the log of fPAR
master$log.LAI <- log(master$LAI_mean)

#build or model

lm.fit.log.LAI <- lm(fPAR_mean ~ log.LAI, data = master)
summary(lm.fit.log.LAI)
#graph to show than jenk
x11()
ggplot(master, aes(y = fPAR_mean, x = log.LAI,  color = site))+
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
sm.density.compare(pcl$rugosity, pcl$site)
title(main="Site distribution of Rugosity")
legend("topright", levels(pcl$site), fill=2+(0:nlevels(pcl$site)))


# add legend via mouse click
colfill<-c(2:(2+length(levels(cyl.f)))) 
legend(locator(1), levels(cyl.f), fill=colfill
       
       ### ggplot
pcl.x <- subset(pcl, pcl$site != "AF" )
x11()
ggplot(data = pcl.x, aes(x=rugosity))+
     geom_density( aes(fill = site), alpha = 0.4)+
     facet_wrap( ~ site)+
     xlab("Rugosity")+
     ylab("")+
     theme(strip.text.x = element_text(size=10),
           strip.background = element_rect(colour="white", fill="white"))+
     scale_fill_discrete(breaks = c("ARNO", "FERN", "GRSM", "HARV", "MLBS", "OSBS", "SCBI","SERC", "TALL", "TREE", "UNDE", "UVAX"),
                         labels = c("Arnot Forest", "Fernow Exp. Forest", "Great Smokies", "Harvard Forest", "Mountain Lake", "Ordway-Swisher", "SCBI","SERC", "Talladega Natl. Forest", "Treehaven", "UNDE", "Univ. of Virginia") )




#simulate some data
x <- 1:100
y <- 1 + x^0.15 + rnorm(100, 0, 0.01)

m.fpar <- lm(fPAR_mean ~ log(rugosity_mean), data = master)
plot( master$fPAR_mean ~ log(master$rugosity_mean))
summary(m)
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
xy <- master[,c("fPAR_mean", "rugosity_mean")]

#now that we have extracted this set, we sort
xy <- xy[order(xy$fPAR_mean),]
xy$fPAR_mean <- xy$fPAR_mean * 100

NLSstAsymptotic(sortedXyData(expression(fPAR_mean), expression(rugosity_mean), xy))

m.xy <- nls(fPAR_mean ~ SSasympOrig(rugosity_mean))

print(NLSstAsymptotic(sortedXyData(expression(fPAR_mean),
                                   expression(rugosity_mean),
                                   xy)), digits = 3)














