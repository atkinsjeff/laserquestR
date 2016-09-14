# Sandbox


vai_x = 0.9

vai_x = vai_x * -1
vai_x <- log(1.0 - vai_x*0.9817)/0.5
vai_x




# Source functions
source("functions.R")

# Set parameters
data_dir <- "./data/SWBR/"
filename <- "SWBR_plot_four_run_one.CSV"
DEBUG <- FALSE
write_out <- FALSE


zz <- setNames(aggregate(return_distance ~ xbin, data = z, FUN = mean), c("mean.ht")
               


require(graphics); require(grDevices)
x  <- as.matrix(mtcars)
rc <- rainbow(nrow(x), start = 0, end = .3)
cc <- rainbow(ncol(x), start = 0, end = .3)
hv <- heatmap(x, col = cm.colors(256), scale = "column",
              RowSideColors = rc, ColSideColors = cc, margins = c(5,10),
              xlab = "specification variables", ylab =  "Car Models",
              main = "heatmap(<Mtcars data>, ..., scale = \"column\")")
utils::str(hv) # the two re-ordering index vectors



## "no nothing"
heatmap(x, Rowv = NA, Colv = NA, scale = "column",
        main = "heatmap(*, NA, NA) ~= image(t(x))")

m1 <- data.matrix(m1)

heatmap(m1, Rowv = NA, Colv = NA, scale = "column",
        main = "heatmap(*, NA, NA) ~=image(t(x))" )





1:10 %in% c(1,3,5,9)



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


##########################################
##########################################

                         
hist(test.data$return_distance, xlim = c(0,25))


#maybe we can do the binning by x,y

test.data$ybin <- floor(test.data$return_distance)

count(test.data$ybin == 11)




# Need to likely make a functoin that goes through data and finds any value that is less than a value,
# replaces it with some other value, then labels it as a marker?
#filter.pcl <- 

marker = -99999999

library(plyr)
#df <- daply(test.data, .(marker), function(x)return(x))
df <- test.data

marker.locations <- df[which(df$return_distance == marker), ]

(marker.locations)



#count between markers, count in index space, you have 1-10 in index space. 

# splitting?
# I am trying to get the xbin here. This is difficult

df.out <- split(test.data, f = marker.locations$index)

str(df.out[[2]])

c1 <- cut(df.out[[1]]$index, breaks = seq(1, 10, by = 1))

