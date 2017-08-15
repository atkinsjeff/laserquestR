# Set parameters
# Source functions
source("functions.R")
require(ggplot2)
require(tools)
require(forestr)

data_dir <- "data/simulated_canopies/"
filename <- "test_data_max_35_5.CSV"

f <- paste(data_dir, filename, sep = "")

f <- "data/processed/HARV_37C.CSV"

# this will run an example data set on a transect from Ordway-Swisher Biological Station
process_pcl(f, 1, 5)
# start.time <- Sys.time()
# 
# # #calls the function from functions.R
# process.single.transect(data_dir, filename)
# 
# rice.c1 <- read.pcl(data_dir, filename)
# 
# plot(rice.c1$return_distance, ylim = c(0, 50))
# 
# which(rice.c1$return_distance ==  44)
# 
# require(devtools)
# install_github("atkinsjeff/forestr")
# require(forestr)

# this will run an example data set on a transect from Ordway-Swisher Biological Station
process_pcl(f, 1, 10)


#process.multiple.transects(data_dir)

# df <- read.csv(filename)
# 
# df.vai <- aggregate(vai ~ zbin, data = df, FUN = mean)
# 
# x11()
# plot(df.vai$vai, df.vai$zbin)
# abline()
# 
# x11()
# ggplot(df.vai, aes(y = vai, x = zbin))+
#      geom_line()

#x11()
#plot(example.set$return_distance, ylim = c(-10^8, 40), ylab = ("Return Distance"), xlab = ("Index"))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


