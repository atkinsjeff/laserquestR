

######################3
# Import PCL data function
read.pcl <- function(f) {
     df <- read.csv(f, header=FALSE, col.names = c("return_distance", "intensity"))
     df$index <- as.numeric(rownames(df))
     df = df[,c(3, 1, 2)]
     df
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
     stopifnot(length(pcl_data[pcl_data$return_distance == -99999999, 2]) == 5)
     
     # Walk through rows and add the segment number in a new column
     for (i in 1:nrow(pcl_data)) {
          pcl_data$seg_num[i] <- segment_num
          if (pcl_data$return_distance[i] == -99999999) {
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
     for (i in 1:(max(pcl_data$seg_num) - 1)) {
          this_segment <- subset(pcl_data, pcl_data$seg_num == i)
          this_segment$chunk_num <- cut(this_segment$index, 10, labels = FALSE)
          results <- rbind(results, this_segment)
     }
     
     # Make sure we didn't make too many chunks in any segment
     stopifnot(max(results$chunk_num) < 11)
     
     # Code segment to create ybin and xbin
     results$xbin <- results$chunk_num * results$seg_num
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
summarize_categorized_pcl <- function(pcl_data) {
     
     # Aggregate a categorized pcl transect and calculate mean, sd, and count for each chunk of each segment
     aggregated_result <- aggregate(cbind(return_distance, intensity) ~ seg_num + chunk_num, data = pcl_data,
                                    FUN = function(x) c(mean = mean(x), sd = sd(x))) 
     aggregated_result[order(aggregated_result$seg_num), ]
}

# Do all the things
summarize_categorized_pcl(split_transects_from_pcl(read_and_check_pcl(data_dir, filename)))


