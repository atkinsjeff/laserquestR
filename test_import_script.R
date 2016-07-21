# Source functions
source("functions.R")


# Set parameters
data_dir <- "./data/"
filename <- "osbs_28_west.CSV"
DEBUG <- FALSE
write_out <- FALSE

# Looking at test.data from Sweet Briar College
test.data <- read_and_check_pcl(data_dir, filename, DEBUG = TRUE)

test.2 <- read.pcl("./data/osbs_28_west.CSV")
test.2 <- add_sky_hits(test.2)
test.2 <- add_can_hits(test.2)
test.2 <- add_markers(test.2)
head(test.2)
pcl.diagnostic.plot(test.2, "OSBS", 15)
# adding bins
length(test.2[test.2$return_distance < -9999, 2])
length(which(test.2$return_distance < -9999))


which((test.2$return_distance <= -9999))
test.2[test.2$return_distance <= -9999 & !is.na(test.2$return_distance), ] 

test.data.binned <- split_transects_from_pcl(test.2)

head(test.data.binned)
summary(test.data.binned)

m1 <- make_matrix(test.data.binned)

m.sky <- just_the_hits(test.data.binned)

m1$vai <- bin_vai(m1)

calc_rugosity(m1)
### heat map
library(akima)
library(fields)
m.m1 <- interp(m1$xbin, m1$ybin, m1$lidar_hits)

image.plot(m.m1, zlim= c(0, 500), col = rev(tim.colors(64)))

####rry, the output is here:
####
####stdStd     = 0.9514       <- Rugosity
####height2    = 3.9023       <- 
####mode2      = 3.6332       <-
####meanHt     = 3.500        <- mean height of the canopy
####meanStd    = 3.0526       <- 
####meanLAI    = 0.5845       <- mean LAI, but how measured?
####meanTopel  = 7.1905       <- mean top of the canopy, like surface
# stdStd height2 mode2 modeEl meanHeight meanStd meanLAI meanTopel_CPAll
# 
# 
# ans =
#      
#      0.9514    3.9023    3.6332    3.5000    3.0526    0.5845    1.9851    7.1905
# the code prints it to the command line
# i usually then pasted it into an excel sheet
# 

ggplot(m1, aes(x = xbin, y = ybin))+ 
     geom_tile(aes(fill = lidar_hits))+
     scale_fill_gradient(low="light green", high="dark green", 
                         name="LiDAR\nhit density")+
     # scale_y_continuous(breaks = seq(0, 20, 5))+
     # scale_x_continuous(minor_breaks = seq(0, 40, 1))+
     theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank())+
     xlim(0,40)+
     ylim(0,10)+
     xlab("Distance along transect (m)")+
     ylab("Height above ground (m)")



