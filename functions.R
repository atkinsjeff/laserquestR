

#
#Libraries
library(plyr)
library(dplyr)
library(magrittr)



######################3
# Import PCL data function
read.pcl <- function(f) {
     df <- read.csv(f, header=FALSE, col.names = c("return_distance", "intensity"))
     df$index <- as.numeric(rownames(df))
     df = df[,c(3, 1, 2)]
     df
}

# this function accounts for the NAs that are in return distance which are actually the sky hits (i.e. when the lidar does not record a canopy hit)
add_sky_hits <- function(df) {
     for(i  in 1:nrow(df)){
     if (is.na(df$return_distance[i]) == TRUE) {
          df$sky_hit[i] = TRUE
     }else{
          df$sky_hit[i] = FALSE
     }
     }
     df
}

add_can_hits <- function(df) {
     for(i  in 1:nrow(df)){
          if (is.na(df$return_distance[i]) == TRUE) {
               df$can_hit[i] = FALSE
          }else{
               df$can_hit[i] = TRUE
          }
     }
     df
}

add_markers <- function(df) {
     for (i in 1:nrow(df)){
          if (df$return_distance[i] == -99999999) {
               df$marker[i] = TRUE
          }else{
               df$marker[i] = FALSE
          }
     }
}

# this function mirrors the read.table, read.csv function, but is written for pcl data

pcl.diagnostic.plot <- function(df, site, max.height) {
     plot(df$index,
          df$return_distance,
          title = site,
          ylim = c(0, max.height),
          ylab = "Canopy Height (m)",
          xlab = "Index Value")
     
}


##########################################
##########################################
# Function to read in a pcl text file and
# optionally print out a head and plot
# to check if it's ok.
#
# Expects a character string of a path to
# a data directory, a filename, and
# optionally a boolean DEBUG option
##########################################
##########################################
read_and_check_pcl <- function(data_dir, filename, DEBUG = FALSE) {
     
     # Load data
     pcl.in <- read.pcl(paste0(data_dir, filename))
     pcl.in <- add_sky_hits(pcl.in)
     # Sanity checks
     if (DEBUG) head(pcl.in)
     if (DEBUG) pcl.diagnostic.plot(pcl.in, "SWBR", 25)
     
     pcl.in
}


##########################################
##########################################
# Function to add two additional columns
# to the pcl dataset, one for the segment
# (which should only be from 1-4) and is
# designated by a -99999999 value in the
# return_distance column
# The only required parameter is the data
# frame of pcl data, but this can
# optionally also write out the results
# to csv if a path and name are given
##########################################
##########################################
split_transects_from_pcl <- function(pcl_data, DEBUG = FALSE, write_out = FALSE, data_dir, output_file_name) {
     
     # Initialize count for segments (expecting 4 segments per transect)
     # Some returns before beginning of first segment and some after last
     segment_num <- 0
     
     # Check for how many segment boundaries we have (should be 5)
     #stopifnot(length(pcl_data[pcl_data$return_distance == -99999999, 2]) = 5)
     stopifnot(length(which(pcl_data$return_distance < -9999)) == 5)
     
     
     # Walk through rows and add the segment number in a new column
     for (i in 1:nrow(pcl_data)) {
          pcl_data$seg_num[i] <- segment_num
          
          if(pcl_data$return_distance[i] <= -9999 & !is.na(pcl_data$return_distance[i])){
               segment_num <- segment_num + 1  
          }
     }
     
     # Check to see if it worked
     if (DEBUG) head(pcl_data)
     
     # Initialize empty data frame to store results
     results <- data.frame()
     
     # For each segment there should only be 4 in total -- checked with test
     # but we're flexible here. Uses cut() with labels = FALSE to return
     # a vector of integer categories for each "chunk" within each segment
     # This should go from 1-10 and be spaced evenly in "index" space
     # for (i in 1:(max(pcl_data$seg_num) - 1)) {
      for (i in 1:(max(pcl_data$seg_num))) {
          this_segment <- subset(pcl_data, pcl_data$seg_num == i)
          this_segment$chunk_num <- cut(this_segment$index, 10, labels = FALSE)
          results <- rbind(results, this_segment)
     }
     
     # Make sure we didn't make too many chunks in any segment
     stopifnot(max(results$chunk_num) < 11)
     
     # Code segment to create ybin and xbin
     results$xbin <- ((results$seg_num * 10) - 10)  +results$chunk_num
     results$ybin <- floor(results$return_distance)
     # Check final output
     if (DEBUG) head(results)
     if (DEBUG) tail(results)
     
     # Write out if write parameter is set at top
     if (write_out) write.csv(results, paste0(data_dir, output_file_name, ".with_categories.csv"), row.names = FALSE)
     
     results
}


##########################################
##########################################
# Summarizes (mean and sd) return distance
#and intensity values by segment and chunk
# and returns results sorted by chunk 1-4
##########################################
##########################################
##########################################
##########################################


make_matrix <- function(df) {
     #ultimately this should actually make an empty data frame or something
     #and it should go from x 1:40 and y to whatever so there are empty values in there
     
     m <- aggregate(return_distance ~ xbin + ybin, data = df, FUN = length)
     m <- m[!m$ybin < 0, ]
     n <- aggregate(sky_hit ~ xbin, data = df, FUN = sum)
     k <- aggregate(can_hit ~ xbin, data = df, FUN = sum)
     m <- merge(m, n, by = c("xbin"))
     m <- merge(m, k, by = c("xbin"))
     plyr::rename(m, c("xbin" = "xbin", "ybin" = "ybin", "return_distance" = "lidar_hits", "sky_hit" = "sky_hits", "can_hit" = "can_hits") )

} 
   
make_sky <- function(df) {
     aggregate(sky_hit ~ xbin, data = df, FUN = sum)     
}

make_can <- function(df) {
     aggregate(can_hit ~ xbin, data = df, FUN = sum)     
}

just_the_hits <- function(df) {
     p <- make_sky(df)
     q <- make_can(df)
     merge(p, q)
}

calc_vai <- function(df) {
     vai <- df$lidar_hits/(df$can_hits + df$sky_hits)
}

bin_vai <- function(df) {
     df$vai <- calc_vai(df)
}

calc_rugosity <- function(df) {
     p <- aggregate(vai ~ xbin, data = df, FUN = sd)
     print(p)
     sd(p$vai, na.rm = TRUE)
}
# 
# 
# 
# summarize_categorized_pcl <- function(pcl_data) {
#      
#      # Aggregate a categorized pcl transect and calculate mean, sd, and count for each chunk of each segment
#      aggregated_result <- aggregate(cbind(return_distance, intensity) ~ seg_num + chunk_num, data = pcl_data, FUN = function(x) c(mean = mean(x), sd = sd(x))) 
#      aggregated_result[order(aggregated_result$seg_num), ]
# }
# 
# 
# # Do all the things
# summarize_categorized_pcl(split_transects_from_pcl(read_and_check_pcl(data_dir, filename)))
# 
# 
