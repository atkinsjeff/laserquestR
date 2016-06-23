# Source functions
source("functions.R")

# Set parameters
data_dir <- "./data/SWBR/"
filename <- "SWBR_plot_four_run_one.CSV"
DEBUG <- FALSE
write_out <- FALSE

# Looking at test.data from Sweet Briar College
test.data <- read_and_check_pcl(data_dir, filename, DEBUG = FALSE)

# adding bins

test.data.binned <- split_transects_from_pcl(test.data)

head(test.data.binned)

