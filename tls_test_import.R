# Source functions
source("functions.R")


data_dir <- "./laserquestR/data/UVA_TLS/"
#filename <- "osbs_28_west.csv"
#filename <- "VDGIF-C5-06072016.csv"
filename <- "center_VAI_TLS.CSV"

m.tls <- read.csv(paste0(data_dir, filename), header = FALSE, col.names = c("xbin", "zbin", "vai"), blank.lines.skip = FALSE)
m.tls$xbin <- m.tls$xbin + 21
x11()
plot(m.tls$vai)

m.tls$og.vai <- m.tls$vai

require(plyr)
require(dplyr)
require(magrittr)



m.tls.vai <- aggregate(og.vai ~ xbin, data = m.tls,  sum)
 sum(m.tls.vai$og.vai) / length(m.tls.vai$og.vai ) 
#  
# sum(m.tls.vai) / length(m.tls.vai)
 

# m.tls$vai <- m.tls$vai * 8
# 
# aggregate(vai ~ zbin, data = m.tls, FUN = sum)
#######new test
#m.tls <- calc_tls_vai(m.tls)
plot(m.tls$vai, m.tls$og.vai)

m.tls <- calc_tls_mean_leaf_ht(m.tls)

x11()
plot(m.tls$vai)
calc_tls_std_bin(m.tls)

uva <- read.csv("c:/github/output/red_pine_plain1_output_hit_matrix.CSV")
uva$vai[uva$vai == 0] <- NA
     
     df[df == 0] <- NA
     
     
img <- readPNG('c:/github/pine.PNG')

g <- rasterGrob(img, interpolate=FALSE) 


vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
x11(width = 8, height = 6)
ggplot(uva, aes(x = xbin, y = zbin))+ 
     coord_fixed(ratio = 1)+
     annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
     # geom_tile(aes(fill = vai))+
     geom_raster(aes(fill = vai,alpha = 0.5))+
     scale_fill_gradient(low="palegreen", high="dark green", na.value = "transparent", 
                         name=vai.label) +
     
     #scale_y_continuous(breaks = seq(0, 20, 5))+
     # scale_x_continuous(minor_breaks = seq(0, 40, 1))+
     theme(axis.line = element_line(colour = "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank())
           # plot.background = element_rect(fill = "transparent", colour = NA))+
     #scale_x_continuous(expand = c(0,0))+
     #scale_y_continuous( expand = c(0,0))+
     #xlim(0,30)+
     #ylim(0,20)+
     xlab("Distance along transect (m)")+
     ylab("Height above ground (m)")+
     ggtitle("UVA A4-01 Center (TLS)")+
     coord_cartesian(xlim = c(0, 30), ylim = c(0, 20))

ggsave("test.png", bg = "transparent")



