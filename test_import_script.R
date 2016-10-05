# Source functions
source("functions.R")


# Set parameters
# data_dir <- "./data/"
# #filename <- "osbs_28_west.csv"
# #filename <- "VDGIF-C5-06072016.csv"
# filename <- "GRSM-64-C.CSV"
# # DEBUG <- FALSE
# write_out <- FALSE
# 
# 
# # Looking at test.data from Sweet Briar College
# test.data <- read.pcl(data_dir, filename)
# 
# # test.2 <- read.pcl("./data/GRSM-64-C.CSV")
# test.2 <- code_hits(test.data)
# # test.2 <- add_sky_hits(test.2)
# # test.2 <- add_can_hits(test.2)
# # test.2 <- add_mar
# 
data_dir <- "./data/rice/"
#filename <- "osbs_28_west.csv"
#filename <- "VDGIF-C5-06072016.csv"
filename <- "rice_control_two.CSV"
# DEBUG <- FALSE
write_out <- FALSE

test.data <- read.pcl(data_dir, filename)

test.2 <- code_hits(test.data)
test.2 <- adjust_by_user(df, 1.2)
# test.2 <- add_sky_hits(test.2)
# test.2 <- add_can_hits(test.2)
# test.2 <- add_markers(test.2)
head(test.2)
# head(test.2)

pcl.diagnostic.plot(test.2, "RICE Control 2", -1e+08)
# # # adding bins
# length(test.2[test.2$return_distance < -9999, 2])
# g<- length(which(test.2$return_distance < -9999))
# 
# 
# which((test.2$return_distance <= -9999))
# test.2[test.2$return_distance <= -9999 & !is.na(test.2$return_distance), ]

test.data.binned <- split_transects_from_pcl(test.2, 30, 10)

head(test.data.binned)
summary(test.data.binned)


###########33
###########
###########
m1 <- make_matrix(test.data.binned)


#now VAI
m1 <- calc_vai(m1)


#######new test
m1 <- calc_mean_leaf_ht(m1)
m1 <- calc_std_bin(m1)

calc_rugosity(m1)



# m1$mean.leaf.ht.by.xbin <- m1$x / m1$sum.vai.by.xbin
# 
# df <- m1
# p <- aggregate(vai ~ zbin, data = df, FUN = function(x) sum( (x$vai * x$zbin)/ sum(x$vai))
# stdBin = sum( ((df$vai * (df$zbin - mean.leaf.ht)^2) )/ sum(df$vai))

rugosity <- calc_rugosity(m1)
m2 <- m1

m1.test <- calc_sum_vai(m1)
m1.test$mean.leaf.ht  <- calc_rugosity(m1.test)
df <- m1.test
yup <- sum( (df$vai * df$zbin)/ df$sum.vai.by.zbin) 

max.vai <- max_vai(m1)
m2$vai <- vai_adjust_lai_max(m2)
rugosity2 <- calc_rugosity(m2)


############################################################333
### playing around here


p <- aggregate(vai ~ xbin, data = m1, FUN = sd, na.rm = FALSE)

# new method
z = test.data.binned
z <- subset(z, return_distance >= 0)
summary(z)
m.new <- aggregate(return_distance ~ xbin, data = z, FUN = sd) 

sd(m.new$return_distance, na.rm=TRUE)

m.vai <- bin_vai(m1)

table(m1[c(1,3)])


m1$vai[!is.finite(m1$vai)] <- 0


a[!is.finite(a)] <- 0
calc_rugosity(m1)
sd(m1$vai)
# ### heat map
# library(akima)
# library(fields)
# m.m1 <- interp(m1$xbin, m1$ybin, m1$lidar_hits)
# 
# image.plot(m.m1, zlim= c(0, 500), col = rev(tim.colors(64)))

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




#### this makes a hit grid. keep it.
x11()
ggplot(m1, aes(x = xbin, y = zbin))+ 
     geom_tile(aes(fill = vai))+
     scale_fill_gradient(low="palegreen1", high="dark green", 
                         name="LiDAR\n Method One")+
     #scale_y_continuous(breaks = seq(0, 20, 5))+
     # scale_x_continuous(minor_breaks = seq(0, 40, 1))+
     theme(axis.line = element_line(colour = "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank())+
     xlim(0,40)+
     ylim(0,40)+
     xlab("Distance along transect (m)")+
     ylab("Height above ground (m)")

calc_rugosity_jess(m1)

m1$adj.vai <- vai_adjust_lai_max(m1)

x11()
ggplot(m1, aes(x = xbin, y = zbin))+ 
     geom_tile(aes(fill = adj.vai))+
     scale_fill_gradient(low="palegreen1", high="dark green", 
                         name="LiDAR\n Method Two")+
     #scale_y_continuous(breaks = seq(0, 20, 5))+
     # scale_x_continuous(minor_breaks = seq(0, 40, 1))+
     theme(axis.line = element_line(colour = "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank())+
     xlim(0,40)+
     ylim(0,20)+
     xlab("Distance along transect (m)")+
     ylab("Height above ground (m)")

calc_rugosity(m1)
calc_rugosity_adj(m1)




p <- aggregate(vai ~ xbin, data = m1, FUN = sd)
sd(p$vai)




#####
#####
#####
#####
# looking at VAI stuff
vai.seq <- seq(0.1, 8, by =0.01)

test.vai <- data.frame(vai.seq)
plot(test.vai$vai.seq)
test.vai$vai.seq <- vai.seq * -1


test.vai$vai.seq <- log(1.0 - test.vai$vai.seq*0.9817)/0.5
plot(test.vai$vai.seq)


