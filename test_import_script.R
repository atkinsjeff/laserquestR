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

