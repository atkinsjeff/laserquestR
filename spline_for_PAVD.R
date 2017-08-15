require(plyr)
require(dplyr)
require(ggplot2)

head(m5)
stuff

total.vai <- sum(m5$vai)

df.z <- aggregate(vai ~ zbin, data = m5, FUN = sum)

df.z$ratio.vai <- df.z$vai / total.vai

sf <- sum(df.z$ratio.vai > 0)
max(df.z$ratio.vai)
x11()
ggplot(df.z, aes(y = df.z$ratio.vai, x = df.z$zbin))+
     geom_bar(stat = "identity", color = "light grey")+
     geom_smooth(method = "lm", se = FALSE, formula = y ~ splines::ns(x, sf))+
     theme_classic()+
     coord_flip(xlim = NULL, ylim = c(0, max(df.z$ratio.vai) + 0.05), expand = FALSE)+
     ylab("Plant Area Veg. Density (PAVD)")+
     xlab("Height Above Ground (m)")


spl <- smooth.spline(df.z$ratio.vai, df.z$zbin, df = 3, nknots = 1)
x11()
plot(spl)
lines(spl)

d <- density(df.z$vai)
plot(d)


smooth.spline(sum[,1],vai, df = 19)




df.z<-data.frame(zbin= seq(1,30,by=1),
                 ratio.vai=c(0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0.100641,
                             0.289242,
                             0.463483,
                             0.42692,
                             0.263234,
                             0.140628,
                             0.055054,
                             0.114379,
                             0.181285,
                             0.270438,
                             0.298528,
                             0.206125,
                             0.169099,
                             0.113229,
                             0.031012,
                             0.000364,
                             0.00015,
                             0.000029))

s.fun<-as.data.frame(spline(df.z,n=10))

ggplot(df.z, aes(y = df.z$ratio.vai, x = df.z$zbin))+
     geom_bar(stat = "identity", color = "light grey")+
     geom_path(data=s.fun, aes(x,y))+
     theme_classic()+
     coord_flip(xlim = NULL, ylim = c(0, max(df.z$ratio.vai)), expand = FALSE)+
     ylab("Plant Area Veg. Density (PAVD)")+
     xlab("Height Above Ground (m)")



library(zoo)

ggplot(df.z, aes(y = df.z$ratio.vai, x = df.z$zbin))+
     geom_bar(stat = "identity", color = "light grey")+
     # geom_smooth(method = "mean", se = FALSE, n =20)+
     # geom_path(data=s.fun, aes(x,y), color = "green",size = 1.5)+
     geom_path(data=df.z, aes(zbin, ratio.vai), color = "red", size = 1.5)+
     # geom_path(y=rollmean(df.z$ratio.vai, 5, na.pad=TRUE), color = "blue", size = 1.5)+
     theme_classic()+
     coord_flip(xlim = NULL, ylim = c(0, max(df.z$ratio.vai)), expand = FALSE)+
     ylab("Plant Area Veg. Density (PAVD)")+
     xlab("Height Above Ground (m)")






