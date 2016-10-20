###functions.R for LaserquestR project focused on calculating CSC metrics for LiDAR data


#Libraries
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)



######################3
# Import PCL data function
read.pcl <- function(data_dir, filename) {
     f <- file.path(data_dir, filename)
     df <- read.csv(f, header=FALSE, col.names = c("return_distance", "intensity"), blank.lines.skip = FALSE)
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
               
          }
     }
     }
     return(df)
}

adjust_by_user <- function(df, user.ht) {
     df$return_distance <- df$return_distance + user.ht
     df
}
# this function mirrors the read.table, read.csv function, but is written for pcl data

pcl.diagnostic.plot <- function(df, filename) {
     plot(df$index,
          df$return_distance,
          main = filename,
          
          ylab = "Canopy Height (m)",
          xlab = "")
}

get.transect.length <- function (df) {
     
     transect.length <- (length(which((df$return_distance <= -9999))) - 1) * 10
     
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
     if (DEBUG) pcl.diagnostic.plot(pcl.in, "", 25)
     
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
split_transects_from_pcl <- function(pcl_data, transect.length, marker.distance, DEBUG = FALSE, write_out = FALSE, data_dir, output_file_name) {
     
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
          if (segment_num == ((transect.length/marker.distance) + 1)) {
               break
          }
     }
     
     # Check to see if it worked
     if (DEBUG) head(pcl_data)
     
     # Initialize empty data frame to store results
     results <- data.frame()
     
     # For each segment there should only be 4 in total -- checked with test
     # but we're flexible here. Uses cut() with labels = FALSE to return
     
     #### this needs to be adjusted to account for smaller transects
     # a vector of integer categories for each "chunk" within each segment
     # This should go from 1-10 and be spaced evenly in "index" space
for (i in 1:(max(pcl_data$seg_num) - 1)) {
      for (i in 1:(max(pcl_data$seg_num))) {
          this_segment <- subset(pcl_data, pcl_data$seg_num == i)
          this_segment$chunk_num <- cut(this_segment$index, 10, labels = FALSE)
          results <- rbind(results, this_segment)
     }
     
     # Make sure we didn't make too many chunks in any segment
     stopifnot(max(results$chunk_num) < 11)
     stopifnot(max(results$seg_num) < ((transect.length/marker.distance) + 1))
}
     # Code segment to create zbin and xbin
     results$xbin <- ((results$seg_num * 10) - 10)  + results$chunk_num
     results$zbin <- ceiling(results$return_distance)
     results$zbin[results$sky_hit == "TRUE"] <- 0
     # Check final output
     if (DEBUG) head(results)
     if (DEBUG) tail(results)
     
     # Write out if write parameter is set at top
     if (write_out) write.csv(results, paste0(data_dir, output_file_name, ".with_categories.csv"), row.names = FALSE)
     
     results <- distinct(results, index, .keep_all = TRUE)
     results
}


#####Canopy metrics before matrix creations

csc_metrics <- function(df, filename) {
     z <- df
     z <- subset(z, return_distance >= 0)
     
     
     mean.return.ht = mean(z$return_distance, na.rm = TRUE)
     message("Mean Return Height (m) -- meanHeight in old code")
     print(mean.return.ht)
     
     sd.ht = sd(z$return_distance, na.rm = TRUE)
     message("Standard Deviation of Canopy Height returns-- meanStd in old code")
     print(sd.ht)
     
     sky.fraction = (1 - (length(which(df$can_hit == TRUE)) / length(df$return_distance))) * 100
     message("Sky Fraction (%)")
     print(sky.fraction)
     
     cover.fraction = 100 - sky.fraction
     message("Cover Fraction (%)")
     print(cover.fraction)
     
     max.ht = max(df$return_distance, na.rm = TRUE)
     message("Max Measured Canopy Height (%)")
     print(max.ht)
     
     csc.variable.list <- list(plot = filename,
                               mean.return.ht = mean.return.ht,
                               sd.return.ht = sd.ht,
                               sky.fraction = sky.fraction,
                               cover.fraction = cover.fraction,
                               max.ht = max.ht)
     csc.variable.list <- data.frame(csc.variable.list)
     return(csc.variable.list)
}

##########################################
##########################################
# This section creates the matrix that is
# required to claculate what we need
##########################################
##########################################
##########################################
##########################################


make_matrix_part_one <- function(df) {
     #ultimately this should actually make an empty data frame or something
     #and it should go from x 1:40 and z to whatever so there are empty values in there
     z = df
     z <- subset(z, return_distance >= 0)
     # zz <- setNames(aggregate(return_distance ~ xbin, data = z, FUN = mean), c("xbin", "mean.ht"))
     # zzz <-setNames(aggregate(return_distance ~ xbin, data = z, FUN = sd), c("xbin", "sd.ht"))
     # zzzz <- setNames(aggregate(return_distance ~ xbin, data = z, FUN = max), c("xbin", "max.ht"))
     l <- setNames(aggregate(index ~ xbin, data = df, FUN = length), c("xbin", "lidar.pulses"))
     m <- setNames(aggregate(return_distance ~ xbin + zbin, data = df, FUN = length), c("xbin", "zbin","bin.hits"))
     m <- m[!m$zbin < 0, ]
     n <- setNames(aggregate(sky_hit ~ xbin, data = df, FUN = sum), c("xbin", "sky.hits"))
     k <- setNames(aggregate(can_hit ~ xbin, data = df, FUN = sum), c("xbin", "can.hits"))
     
     p <- Reduce(function(x, y) merge(x,y, all = TRUE), list(m, l, n, k))
     # p <- merge(l, m, by = c("xbin"), all = TRUE)
     # p <- merge(p, n, by = c("xbin"), all = TRUE)
     # p <- merge(p, k, by = c("xbin"), all = TRUE)
     # p <- merge(p, zz, by = c("xbin"), all = TRUE)
     # p <- merge(p, zzz, by = c("xbin"), all = TRUE)
     # p <- merge(p, zzzz, by = c("xbin"), all = TRUE)
     replace(p, is.na(p), 0)#This will correct for any gaps w/out msmts as all NAs will be 0
 
} 


make_matrix_part_two <- function(df) {
     #ultimately this should actually make an empty data frame or something
     p <- df
     
     df2 <- expand.grid(xbin = c(1:max((p$xbin))),
                       zbin = c(0:max((p$zbin))))
     
     #
     q <- merge(p, data.frame(table(df2[1:2])), all.y=TRUE)
     #now to add empty rows as NA
     #q <- merge(p, data.frame(table(p[1:2]))[-c(3:9)],all.y=TRUE)
     replace(q, is.na(q), 0)#This will correct for any gaps w/out mesmts as all NAs will be 0

} 

# this command combines the previous functions
make_matrix <- function(df) {
     df <- make_matrix_part_one(df)
     df <- make_matrix_part_two(df)
     df$xbin <- as.integer(as.character(df$xbin))
     df$zbin <- as.integer(as.character(df$zbin))
     return(df)
}


#####this series of functions creates VAI
calc_vai <- function(df) {
     
     df$cover.fraction <- (df$bin.hits / df$lidar.pulses) 
      #adjust for max lai?
     # cover.one <- which(df$vai != 0)
     
     df$vai <- (log(1.0 - df$cover.fraction*0.9817)  * -1) /0.5
     df[is.na(df)] <- 0
     return(df)
 
}

#############################
#############################
#############################
## Need to make a summary matrix now based on each column

make_summary_matrix <- function(df, m) {
     # df$xbin <- as.factor(df$xbin)
     df <- subset(df, return_distance > 0)
     
     a <- setNames(aggregate(return_distance ~ xbin, data = df, FUN = mean, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "mean.ht"))
     
     
     b <- setNames(aggregate(return_distance ~ xbin, data = df, FUN = sd, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "sd.ht"))
     
     c <- setNames(aggregate(return_distance ~ xbin, data = df, FUN = max, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "max.ht"))
     
     d <- setNames(aggregate(vai ~ xbin, data = m, FUN = max, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "max.vai"))
     e <- setNames(aggregate(vai ~ xbin, data = m, FUN = sum, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "sum.vai"))
     f <- setNames(aggregate(vai ~ xbin, data = m, FUN = sd, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "sd.vai"))
     
     # this is height at which max vai occurs
     g <- m$zbin[match(d$max.vai, m$vai)]  
     g <- data.frame(g)
     colnames(g) <- c("max.vai.z")
     
     #mean column leaf height that is the "heightBin" from Matlab code
     m$vai.z <- m$vai * m$zbin
     h <- setNames(aggregate(vai.z ~ xbin, data = m, FUN = sum, na.rm = FALSE,  na.action = 'na.pass'), c("xbin", "vai.z.sum"))
           
  
     # this section joins all these guys together
     p <- join_all(list(a, b, c, d, e, f, h), by = "xbin", type = "full")
     p <- cbind(p, g)

    
     
     #need to fix the missing first here
     p$mean.ht[is.na(p$mean.ht)] <- 0
     p$sd.ht[is.na(p$sd.ht)] <- 0
     p$max.ht[is.na(p$max.ht)] <- 0
      
     p$height.bin <- p$vai.z.sum / p$sum.vai
     
     return(p)
}


##########################
##########################
# BEGIN COMPLEXITY METRIC CALCS
# 

# RUGOSITY
calc_rugosity <- function(df, m, filename) {
     
     a <- subset(df, max.vai.z > 0)
     
     mean.height = mean(df$mean.ht)
     message("MeanHeight")
     print(mean.height)
     
     transect.length = max(df$xbin)
     message("Transect Length (m)")
     print(transect.length)
     
     mode.el = mean(df$max.vai.z)
     message("Mean Height of Maximum Return Density -- modeEl")
     print(mode.el)
     
     height.2 <- sd(df$mean.ht)
     message("Standard Deviation of mean height for each xbin - height2")
     print(height.2)
     
     max.el = max(df$max.vai.z)
     message("Maximum VAI for entire transect -- max el!")
     print(max.el)
     
     b <- subset(df, max.vai > 0)
     b$max.vai.sq <- b$max.vai^2
     mode.2 <- mean(b$max.vai.sq)
     message("Mean height of squared max VAI whatever the hell that is -- or mode2")
     print(mode.2)
     
     max.can.ht = max(df$max.ht)
     message("Max canopy height (m)")
     print(max.can.ht)
     
    
     
     mean.max.ht = mean(df$max.ht)
     message("Mean Max canopy height (m) -- meanTopel w/ deep gaps removed")
     print(mean.max.ht)
     
     
     mean.vai = mean(df$sum.vai)
     message("Mean VAI")
     print(mean.vai)
     
     message("Maximum VAI")
     max.vai = max(df$sum.vai)
     print(max.vai)
     
     e <- subset(df, max.ht == 0)
     deep.gaps <- nrow(e)
     message("Deep Gaps")
     print(deep.gaps)
     
     porosity = deep.gaps/transect.length
     message("Bin porosity")
     print(porosity)
     
     #being rugosity intermediates
      
     #first we adjust the vai at each x,z by the z height of the bin
     combo.meal <- merge(df, m, by = "xbin")
     
     combo.meal$std.bin.num <- combo.meal$vai * ((combo.meal$zbin  - combo.meal$height.bin)^2)
     j <- aggregate(std.bin.num ~ xbin, data = combo.meal, FUN = sum, na.rm = FALSE, na.action = 'na.pass')
     j[is.na(j)] <- 0
     
     super.size <- merge(df, j, by = "xbin")
     super.size$std.bin <- super.size$std.bin.num / super.size$sum.vai
     

     super.size$std.std.pre <- (super.size$std.bin * super.size$std.bin) / transect.length
     super.size$std.std.pre[is.na(super.size$std.std.pre)] <- 0
     std.std <- sum(super.size$std.std.pre)
     # super.size$std.std.pre[is.infinite(super.size$std.std.pre)] <- 0

     super.size$mean.std.pre <- (super.size$std.bin / transect.length)
     super.size$mean.std.pre[is.na(super.size$mean.std.pre)] <- 0
     
     # super.size$mean.std.pre[is.infinite(super.size$mean.std.pre)] <- 0
     mean.std = sum(super.size$mean.std.pre)

     message("Square of leaf height variance (stdStd from old script)")
     print(std.std)
     
 
     message("Mean Standard deviation of leaf heights -- meanStd")
     print(mean.std)
    
     rugosity = (std.std - mean.std * mean.std)^0.5
     message("Canopy Rugosity")
     print(rugosity)
     
     # #uses temp. data frame with deep gaps removed
     
     jess.rugosity = sd(df$max.ht)
     
     
     message("Surface Rugosity--TopRugosity")
     print(jess.rugosity)
     
     #Rumple is the ratio of top rugoisty to ground area.
     rumple = jess.rugosity / transect.length
     message("Rumple--surface rugosity/ground area")
     print(rumple)
     
     variable.list <- list(plot = filename,
                           mean.height = mean.height,
                           transect.length = transect.length, 
                           mode.el = mode.el, 
                           height.2 = height.2, 
                           max.el = max.el,
                           mode.2 = mode.2,
                           max.can.ht = max.can.ht,
                           mean.max.ht = mean.max.ht,
                           mean.vai = mean.vai,
                           max.vai = max.vai,
                           deep.gaps = deep.gaps,
                           porosity = porosity,
                           std.std = std.std,
                           mean.std = mean.std,
                           rugosity = rugosity,
                           top.rugosity = jess.rugosity,
                           rumple = rumple)
    
     
     #now to write to csv
     variable.list <- data.frame(variable.list)
     return(variable.list)
     
}

write.pcl.to.csv <- function(variable.list, filename) {
     
     filename2 <- paste(filename, ".csv", sep="")
     write.csv(variable.list, file = filename2)
}

     
     
     

 


