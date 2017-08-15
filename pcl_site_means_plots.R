# new plotting of data

df <- pcl.site.means

df.rugosity <- df[]

require(gcookbook)
require(ggplot2)
dev.off()
tophit <- tophitters2001[1:25, ] # Take the top 25 from the tophitters data set

ggplot(tophit, aes(x=avg, y=name)) + geom_point()
x11()
ggplot(df, aes(x = rugosity_mean, y = reorder(site, rugosity_mean) ))+
     geom_point()


# scale_fill_discrete(breaks = c("ARNO", "FERN", "GRSM", "HARV", "MLBS", "OSBS", "SCBI","SERC", "TALL", "TREE", "UNDE"),
# labels = c("Arnot Forest", "Fernow Exp. Forest", "Great Smokies", "Harvard Forest", "Mountain Lake", "Ordway-Swisher", "SCBI","SERC", "Talladega Natl. Forest", "Treehaven", "UNDERC") )

rug.label <- expression(paste(bar(R[c])))
por.label <- expression(paste(bar(P[c])))
vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
mean.vai.label =  expression(paste(bar(VAI~(m^2 ~m^-2))))
lai.label =  expression(paste(LAI~(m^2 ~m^-2)))
log.lai.label = expression(paste(log~( LAI~(m^2 ~m^-2))))
c.i.label = expression(paste(bar(Omega)))

x11(width = 5, height = 8)
ggplot(df, aes(x = rugosity_mean, y = reorder(site, rugosity_mean) ))+
     geom_point(size=3) + # Use a larger dot
     theme_bw() +
     theme(panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           panel.grid.major.y = element_line(colour="grey60", linetype="dashed"),
           axis.text = element_text(size = 14))+
     xlab(rug.label)+
     ylab("")

x11(width = 5, height = 8)
ggplot(df, aes(x = porosity_mean, y = reorder(site, porosity_mean) ))+
     geom_point(size=3) + # Use a larger dot
     theme_bw() +
     theme(panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           panel.grid.major.y = element_line(colour="grey60", linetype="dashed"),
           axis.text = element_text(size = 14))+
     xlab(por.label)+
     ylab("")


x11(width = 5, height = 8)
ggplot(df, aes(x = mean.vai_mean, y = reorder(site, mean.vai_mean) ))+
     geom_point(size=3) + # Use a larger dot
     theme_bw() +
     theme(panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           panel.grid.major.y = element_line(colour="grey60", linetype="dashed"),
           axis.text = element_text(size = 14))+
     xlab(mean.vai.label)+
     ylab("")

x11(width = 5, height = 8)
ggplot(df, aes(x = clumping.index_mean, y = reorder(site, clumping.index_mean) ))+
     geom_point(size=3) + # Use a larger dot
     theme_bw() +
     theme(panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           panel.grid.major.y = element_line(colour="grey60", linetype="dashed"),
           axis.text = element_text(size = 14))+
     xlab(c.i.label)+
     ylab("")
