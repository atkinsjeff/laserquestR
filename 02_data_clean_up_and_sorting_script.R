# 02 Data sorting and clean up script.

# This script formats the output data to master file.
####merge files
# output_directory <- "./output/output/"
# 
# 
# multmerge = function(output_directory){
#      filenames=list.files(path=output_directory, full.names=TRUE)
#      datalist = lapply(filenames, function(x){read.csv(file=x,header=TRUE)})
#      Reduce(function(x,y) {merge(x,y)}, datalist)
# }
# 
# laserquest_master_data = multmerge(output_directory)
# 
# file_names <-  #where you have your files
#      
#      your_data_frame <- do.call(rbind,lapply(output_directory,read.csv))
# # 
# library(dplyr)
# library(readr)
# df <- list.files(path = output_directory, full.names = TRUE) %>%
#      lapply(read_csv) %>%
#      bind_rows
# 
# df <- data.frame(df)
# write.csv(df, "laserquest_master_output_2017_03_22.CSV")



## Making plot means
# importing rugosity data
pcl <- read.csv("laserquest_master_pcl.CSV", header = TRUE)
colnames(pcl)[3] <- "plotID"

pcl <- pcl[, -(1:2)]

# adding the site
pcl$site <- as.factor(gsub("_.*$", "", pcl$plotID))

# 
pcl.bad.names <- subset(pcl, site == "AF" | site == "FERN" | site == "ARNO" | site == "UVAX")
pcl.bad.names$plotnumber <- gsub("_([^_]*)$", "", pcl.bad.names$plotID)
pcl.bad.names$plotnumber <- gsub(".+_", "", pcl.bad.names$plotnumber)
# # # 
pcl.good <- subset(pcl, pcl$site != "AF" & pcl$site != "FERN" & pcl$site !=  "ARNO" & pcl$site != "UVAX")
pcl.good$plotnumber <- sub(".*_", "", pcl.good$plotID)


pcl <- rbind(pcl.good, pcl.bad.names)
pcl$plot <- paste0(pcl$site, substr(pcl$plotnumber, 0,2))

write.csv(pcl, "laserquest_master_transect_data_20170322.csv")

#####
pcl <- subset(pcl, pcl$rugosity < 50)

# # pcl means
pcl %>% group_by(plot) %>% summarise_each(funs(mean, sd), -X, -X1, -plotID, -transect.length, -plotnumber, -site) -> pcl.means

pcl.means$plot <- as.factor(pcl.means$plot)
pcl.means <- data.frame(pcl.means)
