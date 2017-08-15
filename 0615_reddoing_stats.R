# Making a site means script because the old one is bust.
# 
require(magrittr)
require(tidyr)
require(plyr)
require(dplyr)
require(ggplot2)

pcl <- read.csv("laserquest_master_pcl.csv")

pcl$gap.fraction <- pcl$sky.fraction / 100
pcl$gap.norm <- pcl$deep.gaps / pcl$transect.length
dim(pcl)

# plot means
pcl %>% 
     group_by(plot, site) %>% 
     summarise_each(funs(mean), -transect, -notes) -> plot.means

plot.means <- as.data.frame(plot.means)

# Site means
plot.means %>% 
     group_by(site) %>% 
     summarise_each(funs(mean, sd, var, min, max), -plot) -> site.means

# Making SE
site.means$rugosity_se <- site.means$rugosity_sd / sqrt(site.means$count)
site.means$mean.vai_se <- site.means$mean.vai_sd / sqrt(site.means$count)
site.means$top.rugosity_se <- site.means$top.rugosity_sd / sqrt(site.means$count)
site.means$porosity_se <- site.means$porosity_sd / sqrt(site.means$count)
site.means$rugosity_se <- site.means$rugosity_sd / sqrt(site.means$count)
site.means$rugosity_se <- site.means$rugosity_sd / sqrt(site.means$count)
site.means$rugosity_se <- site.means$rugosity_sd / sqrt(site.means$count)
site.means$rugosity_se <- site.means$rugosity_sd / sqrt(site.means$count)

site.means <- as.data.frame(site.means)

plot.means %>%
     group_by(site) %>%
     summarise(count = n()) -> site.no.observations

site.no.observations <- as.data.frame(site.no.observations)

site.means <- join(site.no.observations, site.means)

#write.csv(site.means, "laserquest_site_stats_0616.csv")

print(levels(site.means$site))
site.means$site <- ordered(site.means$site, levels = c("OSBS", "TALL", "GRSM", "MLBS", "RICE", "UVAX", "SERC", "SCBI", "FERN", "ARNO", "HARV", "TREE", "UNDE", "UMBS"))

toBeRemoved <- which(site.means$site=="UMBS")
site.means <- site.means[-toBeRemoved,]

plot(site.means$max.can.ht_mean, site.means$mean.max.ht_mean)


ggplot(plot.means, aes(x = site, y = rugosity))+
     geom_boxplot()+
     theme_classic()+
     ylab(r.label)+
     xlab("")+
     coord_flip()

x11( width = 3, height = 6)
ggplot(site.means, aes(x = site, y = rugosity_mean))+
     geom_pointrange( aes(ymin = rugosity_mean - rugosity_se, ymax = rugosity_mean + rugosity_se))+
     theme_classic()+
     ylab(r.label)+
     xlab("")+
     coord_flip()


x11( width = 3, height = 6)
ggplot(site.means, aes(x = site, y = mean.vai_mean))+
     geom_pointrange( aes(ymin = mean.vai_mean - mean.vai_se, ymax = mean.vai_mean +mean.vai_se))+
     theme_classic()+
     ylab(vai.label)+
     xlab("")+
     coord_flip()

x11( width = 3, height = 6)
ggplot(site.means, aes(x = site, y = top.rugosity_mean))+
     geom_pointrange( aes(ymin = top.rugosity_mean - top.rugosity_se, ymax = top.rugosity_mean + 
                               top.rugosity_se))+
     theme_classic()+
     ylab(expression("R"[T]))+
     xlab("")+
     coord_flip()

x11( width = 3, height = 6)
ggplot(site.means, aes(x = site, y = porosity_mean))+
     geom_pointrange( aes(ymin = porosity_mean - porosity_se, ymax = porosity_mean + 
                               porosity_se))+
     theme_classic()+
     ylab(expression("P"[C]))+
     xlab("")+
     coord_flip()



vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
lai.label =  expression(paste(LAI~(m^2 ~m^-2)))
log.lai.label = expression(paste(log~( LAI~(m^2 ~m^-2))))
r.label = expression("R"[C])
fpar.label = expression(paste(italic("f"), "PAR"))
