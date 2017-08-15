# Looking at how site factors influence things

fact <- read.csv("site_factors_data.CSV")

require(ggplot2)
require(plyr)
require(dplyr)
require(vegan)
require(ggrepel)
require(magrittr)


x11()
ggplot(fact, aes(x = richness, y = rugosity_mean))+
     geom_point(size = 2)+
     geom_text_repel(data = fact, aes( richness, rugosity_mean, label = site))+
     #scale_color_manual(values=cbPalette)+
     #scale_color_brewer(palette="Dark2")+
     #geom_errorbarh(aes(xmin =rugosity_mean - rugosity_sd, xmax = rugosity_mean + rugosity_sd)) + 
     #geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
     theme_new()+
     ylim(c(0, 45))+
     ylab(expression("R"[C]))+
     xlab("Species Richness")+
     geom_smooth(method = lm, se = FALSE)
     
richness.model <- lm(fact$rugosity_mean ~ fact$richness, data = fact)
summary(richness.model)


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
     stat_function(fun = fpar.high, col = "blue", size = 1)


###
species <- read.csv("NEON_species_richness_by_plot.CSV")
pcl <- read.csv("laserquest_master_pcl.CSV")
colnames(pcl)[2] <- ("plotID")
colnames(species)[3] <- "richness"
pcl.rich <- merge(species, pcl, by = "plotID")

pcl.rich %>%
     group_by(site, plotID) %>%
     summarize_each(funs(mean)) -> pcl.rich.means
pcl.rich.means <- data.frame(pcl.rich.means)

x11()
ggplot(pcl.rich.means, aes(x = richness, y = rugosity, color = site))+
     geom_point(size = 2)+
     #geom_text_repel(data = fact, aes(richness, rugosity, label = site))+
     #scale_color_manual(values=cbPalette)+
     #scale_color_brewer(palette="Dark2")+
     #geom_errorbarh(aes(xmin =rugosity_mean - rugosity_sd, xmax = rugosity_mean + rugosity_sd)) + 
     #geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
     theme_new()+
     ylim(c(0, 45))+
     ylab(expression("R"[C]))+
     xlab("Species Richness")+
     geom_smooth(method = lm, se = FALSE)

rich.plot.model <- lm(pcl.rich.means$rugosity ~ pcl.rich.means$richness, data = fact)
summary(rich.plot.model)

