# Sandbox

# Source functions
source("functions.R")

# Set parameters
data_dir <- "./data/SWBR/"
filename <- "SWBR_plot_four_run_one.CSV"
DEBUG <- FALSE
write_out <- FALSE

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


### now trying to cycle through the data to make section ids that will then be used to calculate x bins
for( i in 1:dim(df[1]))    {
     section_id = 0
     df$section_id = section_id
     if (df$index == marker) {
          section_id = section_id + 1}

}

#count between markers, count in index space, you have 1-10 in index space. 

# splitting?
# I am trying to get the xbin here. This is difficult

df.out <- split(test.data, f = marker.locations$index)

str(df.out[[2]])

c1 <- cut(df.out[[1]]$index, breaks = seq(1, 10, by = 1))

##################
## generate data for clinical trial example
clinical.trial <-
     data.frame(patient = 1:100,              
                age = rnorm(100, mean = 60, sd = 8),
                year.enroll = sample(paste("19", 85:99, sep = ""),
                                     100, replace = TRUE))
summary(clinical.trial)
# patient            age         year.enroll
# Min.   :  1.00   Min.   :41.18   1991   :12  
# 1st Qu.: 25.75   1st Qu.:52.99   1988   :11  
# Median : 50.50   Median :60.08   1985   : 9  
# Mean   : 50.50   Mean   :59.67   1993   : 7  
# 3rd Qu.: 75.25   3rd Qu.:65.67   1995   : 7  
# Max.   :100.00   Max.   :76.40   1997   : 7  
# (Other):47 

c1 <- cut(clinical.trial$age, breaks = 4)
table(c1)

age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
     
     labs <- c(paste(seq(lower, upper - by, by = by),
                     seq(lower + by - 1, upper - 1, by = by),
                     sep = sep),
               paste(upper, above.char, sep = ""))
     
     cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
         right = FALSE, labels = labs)
}

table(age.cat(clinical.trial$age, upper = 70))
