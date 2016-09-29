

#
#Libraries
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)



######################3
# Import PCL data function
read.pcl <- function(data_dir, filename) {
     f <- file.path(data_dir, filename)
     df <- read.csv(f, header=FALSE, col.names = c("return_distance", "intensity"))
     df$index <- as.numeric(rownames(df))
     df = df[,c(3, 1, 2)]
     df
}

# this function accounts for the NAs that are in return distance which are actually the sky hits (i.e. when the lidar does not record a canopy hit)
# 
code_hits <- function(df) {
     for(i in 1:nrow(df)){
          if (is.na(df$return_distance[i]) == TRUE) {
               df$sky_hit[i] = TRUE
               df$can_hit[i] = FALSE
               df$marker[i] = FALSE
          }else{
          if (df$return_distance[i] > 0){
               df$sky_hit[i] = FALSE
               df$can_hit[i] = TRUE
               df$marker[i] = FALSE
          }else{
               df$sky_hit[i] = FALSE
               df$can_hit[i] = FALSE
               df$marker[i] = TRUE
               print(df)
          }
     }
     }
     df
}

# add_sky_hits <- function(df) {
#      for(i  in 1:nrow(df)){
#      if (is.na(df$return_distance[i]) == TRUE) {
#           df$sky_hit[i] = TRUE
#      }else{
#           df$sky_hit[i] = FALSE
#      }
#      }
#      df
# }
# 
# add_markers <- function(df) {
#      for(i  in 1:nrow(df)){
#           if (is.na(df$return_distance[i]) == TRUE & df$return_distance[i] < 0) {
#                df$marker[i] = TRUE
#           }else{
#                df$marker[i] = FALSE
#           }
#      }
#      df
# }
# 
# # this function as a logical true or false based on if there is a canopy hit
# add_can_hits <- function(df){
#      for(i  in 1:nrow(df)){
#           if (is.na(df$return_distance[i]) == TRUE)) {
#                df$can_hit[i] = FALSE
#           }else{
#                df$can_hit[i] = TRUE
#           }
#      }
#      df
# }
# 


# this function mirrors the read.table, read.csv function, but is written for pcl data

pcl.diagnostic.plot <- function(df, site, min.height) {
     plot(df$index,
          df$return_distance,
          title = site,
          ylim = c(min.height, 40),
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
    # stopifnot(length(which(pcl_data$return_distance < -9999)) == 4)
     
     
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
     
     # Code segment to create zbin and xbin
     results$xbin <- ((results$seg_num * 10) - 10)  +results$chunk_num
     results$zbin <- floor(results$return_distance)
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
     #and it should go from x 1:40 and z to whatever so there are empty values in there
     z = df
     z <- subset(z, return_distance >= 0)
     zz <- setNames(aggregate(return_distance ~ xbin, data = z, FUN = mean), c("xbin", "mean.ht"))
     zzz <-setNames(aggregate(return_distance ~ xbin, data = z, FUN = sd), c("xbin", "sd.ht"))
     zzzz <- setNames(aggregate(return_distance ~ xbin, data = z, FUN = max), c("xbin", "max.ht"))
     l <- setNames(aggregate(index ~ xbin, data = df, FUN = length), c("xbin", "lidar.pulses"))
     m <- setNames(aggregate(return_distance ~ xbin + zbin, data = df, FUN = length), c("xbin", "zbin","bin.hits")) 
     m <- m[!m$zbin < 0, ]
     n <- setNames(aggregate(sky_hit ~ xbin, data = df, FUN = sum), c("xbin", "sky.hits"))
     k <- setNames(aggregate(can_hit ~ xbin, data = df, FUN = sum), c("xbin", "can.hits"))
     p <- merge(l, m, by = c("xbin"), all = TRUE)
     p <- merge(p, n, by = c("xbin"), all = TRUE)
     p <- merge(p, k, by = c("xbin"), all = TRUE)
     p <- merge(p, zz, by = c("xbin"), all = TRUE)
     p <- merge(p, zzz, by = c("xbin"), all = TRUE)
     p <- merge(p, zzzz, by = c("xbin"), all = TRUE)
     # 
     # plyr::rename(p, c("xbin" = "xbin", "zbin" = "zbin", "index" = "lidar_pulses", "return_distance" = "bin_height_sd","return_distance.y" = "bin_height_mean","return_distance.x" = "lidar_returns" , "sky_hit" = "sky_hits", "can_hit" = "can_hits") )
      
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

# this, if applied to the whole column would be cover fraction
calc_vai <- function(df) {
     # bin_cover_fraction <- df$lidar.pulses/(df$can_hits + df$sky_hits)/df$lidar_returns
     

     # for(i in 1:nrow(df)){
     #  if (is.na(df$vai[i]) == TRUE) {
     #       df$vai[i] = 0
     #  }
     # }
     vai = (df$bin.hits / df$lidar.pulses) 
     vai = vai * 8 #adjust for max lai?
     vai = vai * -1
     vai <- log(1.0 - vai*0.9817)/0.5
     
     #subfunction to create max.vai for each column
     z = df
     max.vai <- aggregate(vai ~ xbin, data = z, FUN = max)
     p <- merge(df, z, by = c("xbin"), all = TRUE)
}

vai_adjust_lai_max <- function(df) {
     vai.adj = df$vai * 8 * df$zbin
}

vai_extinct <- function(df) {
     vai = log(1.0 - df$vai*0.9817) / 0.5
     vai = df$vai * -1
     }




bin_vai <- function(df) {
     df$vai <- calc_vai(df)
}

calc_rugosity <- function(df) {
     df$vai = df$vai * df$zbin
     p <- aggregate(vai ~ xbin, data = df, FUN = sd)
     p$vai[is.na(p$vai)] <- 0
     p$vai[!is.finite(p$vai)] <- 0
     print(p)
     sd(p$vai)
     
}

calc_rugosity_adj <- function(df) {
     p <- aggregate(adj.vai ~ xbin, data = df, FUN = sd)
     p$adj.vai[is.na(p$adj.vai)] <- 0
     p$adj.vai[!is.finite(p$adj.vai)] <- 0
     print(p)
     sd(p$adj.vai)
     
}

calc_rugosity_jess <- function(df) {
     p <- aggregate(max.ht ~ xbin, data = df, FUN = sd)
     p
     
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
