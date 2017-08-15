#Rice maps
require(maps)
require(mapdata)
require(maptools)
require(scales)
require(ggplot2)
require(ggmap)
require(ggrepel)
df <- read.csv("site_table_gis.csv")

#df <- subset(df, df$done == "y")
df <- subset(df, df$Affiliation == "NEON" | df$Affiliation ==  "Ameriflux" | df$Affiliation ==  "LTER" | df$Affiliation ==  "USFS" | df$Affiliation ==  "Other" )

# make your coordinates a data frame 
coords <- as.data.frame(cbind(lon=lon,lat=lat))

# make it a spatial object by defining its coordinates in a reference system
coordinates(coords) <- ~lat+lon 

# you also need a reference system, the following should be a fine default
proj4string(coords) <- CRS("+init=epsg:4326")

# then just plot
a <- map(df)
# here `a <-` avoids that you get flooded by the html version of what you plot 
# 
# 

x11()
map('state', region = 'us', fill = FALSE, myborder = 0.01)
points(coords, pch = 16)
text(lat, lon, "Rice Rivers Center", offset = 0, cex = 1.2, adj = c(1,1) )

#
## The palette with grey:
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

# 
# n <- 50
# x <- rnorm(n)*10
# y <- rnorm(n)*10
# plot(x, y, col = "red", pch = 20)
# pointLabel(x, y, as.character(round(x,5)), offset = 0, cex = .7)
# plot(x, y, col = "red", pch = 20)
# pointLabel(x, y, expression(over(alpha, beta[123])), offset = 0, cex = .8)

eastcoast <- subset(states, region %in% c("maine","new hampshire", "massachusetts","vermont", "new york", "connecticut", "rhode island", "pennsylvania", "delaware", "new jersey","maryland", "virginia", "ohio", "north carolina", "south carolina", "georgia", "florida", "alabama", "indiana", "illinois", "wisconsin", "michigan", "tennessee", "kentucky", "west virginia", "missouri", "arkansas", "mississippi", "iowa", "minnesota", "lousiana"))
x11()
usa <- map_data("usa") # we already did this, but we can do it again
ggplot() + geom_polygon(data = eastcoast, aes(x=long, y = lat, group = group), fill = "light grey") + 
     coord_fixed(1.3)+
     scale_fill_manual(values=cbPalette)+
     theme_bw()+
     theme(legend.justification=c(1,0), legend.position=c(1,0))+
     geom_point(data = df, aes(x = long, y = lat,  fill = Affiliation), color = "black", pch = 21, size = 3)+
     geom_text_repel(data = df, aes(label = paste(" ", as.character(site_code), sep = ""), x = long, y = lat), angle = 0, hjust = 0)+
     xlab("Longitude")+
     ylab("Latitude")