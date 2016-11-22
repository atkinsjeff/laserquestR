# Set parameters
# Source functions
source("functions.R")


data_dir <- "./data/"

start.time <- Sys.time()

file.names <- dir(data_dir, pattern =".CSV")
length(file.names)
for(i in 1:length(file.names)){
    filename <- file.names[i]
     
#      <- read.table(file.names[i],header=TRUE, sep=";", stringsAsFactors=FALSE)
#      out.file <- rbind(out.file, file)
# }
# write.table(out.file, file = "cand_Brazil.txt",sep=";", 
#             row.names = FALSE, qmethod = "double",fileEncoding="windows-1252")



#filename <- "HARV_47C.csv"
filename <- "OSBS_41W.csv"
#filename <- "AF_G8a_0729016.CSV"
#filename <- "rice_treatment_two.CSV"

write_out <- FALSE


test.data <- read.pcl(data_dir, filename)
transect.length <- get.transect.length(test.data)
test.2 <- code_hits(test.data)


(length(which((test.data$return_distance <= -9999))) - 1)

test.2 <- adjust_by_user(test.2, 1.05)

# need to code in diagnostic plot better
pcl.diagnostic.plot(test.2, filename)

test.data.binned <- split_transects_from_pcl(test.2, transect.length, 10)

csc_metrics(test.data.binned, filename)

m1 <- make_matrix(test.data.binned)

m2 <- normalize_pcl_one(m1)
m3 <- normalize_pcl_two(m2)
m4 <- normalize_pcl_three(m3)

m5 <- calc_vai(m4)


summary.matrix <- make_summary_matrix(test.data.binned, m5)

stuff <- calc_rugosity(summary.matrix, m5, filename)

outputname = substr(filename,1,nchar(filename)-4)
outputname <- paste(outputname, "output", sep = "_")

dir.create("output", showWarnings = FALSE)
output_directory <- "./output/"
write.pcl.to.csv(stuff, outputname)




}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# 


####merge files
multmerge = function(output_directory){
     filenames=list.files(path=output_directory, full.names=TRUE)
     datalist = lapply(filenames, function(x){read.csv(file=x,header=TRUE)})
     Reduce(function(x,y) {merge(x,y)}, datalist)
}

laserquest_master_data = multmerge(output_directory)

file_names <-  #where you have your files

your_data_frame <- do.call(rbind,lapply(output_directory,read.csv))

library(dplyr)
library(readr)
df <- list.files(path = output_directory, full.names = TRUE) %>% 
     lapply(read_csv) %>% 
     bind_rows 

df <- data.frame(df)
write.csv(df, "laserquest_master_output.CSV")
# dee <- setNames(aggregate(dee ~ xbin, data = m4, FUN = sum), c("xbin", "sum.dee"))
# 
# 
# m2 <- m1[with(m1, order(xbin, zbin)), ]
# 
# # for loop for this jenk
# sat.count <- 0
# m2$sat.count <- 0
# for (i in 1:nrow(m2)) {
#      x.counter = 1
#      
#      for(j in 2:nrow(m2)){
#          if(m2$xbin[j] == x.counter ){
#      
#           m2$sat.count[j] = m2$sat.count[j-1] + m2$bin.hits[j]
#           
#      }else {
#           x.counter = x.counter + 1 
#           next
#      }
#           next
#      }
# }
# 
# m2$phi <- (m2$can.hits)
# 
# m2$sat.pct <- m2$sat.count / m2$can.hits
# 
# k <- setNames(aggregate(can.hits ~ xbin, data = df, FUN = max), c("xbin", "can.hits"))
# 
# m3 <- m2
# 
# m3$can.hits <- k$can.hits[match(m3$xbin, k$xbin)]
# m3 <- merge(m2, k, by ="xbin", all = FALSE)
# 
# m1 <- calc_vai(m1)
summary.matrix <- make_summary_matrix(test.data.binned, m5)
# #####################
# df <- m1
# df$cvr <- (df$bin.hits / df$lidar.returns) 
# df[is.na(df)] <- 0
# #adjust for max lai?
# cvr1 <- which(df$cvr < 0.999999)
# 
# df$cvr[cvr1]
# df$vai <- df$cvr * 8
# 
# df$vai <- (log(1.0 - df$cvr[cvr1]*0.9817)  * -1) /0.5
# 
# 
# m5 <- merge(m1, summary.matrix, by = "xbin")
# 
# m5$el <- (m5$vai/ m5$sum.vai ) * 100
# m5$std.bin.num <- m5$el * ((m5$zbin  - m5$height.bin)^2)
# print(j)

stuff <- calc_rugosity(summary.matrix, m5, filename)

write.pcl.to.csv(stuff, filename)
}
#write.csv(m1, "dgif-t5.csv")
#### this makes a hit grid. keep it.
#### 
#### 
vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
x11(width = 8, height = 6)
ggplot(m5, aes(x = xbin, y = zbin))+ 
     geom_tile(aes(fill = vai))+
     scale_fill_gradient(low="white", high="dark green", 
                         limits=c(0,8.5),
                         name=vai.label)+
     #scale_y_continuous(breaks = seq(0, 20, 5))+
     # scale_x_continuous(minor_breaks = seq(0, 40, 1))+
     theme(axis.line = element_line(colour = "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           axis.text.x= element_text(size = 14),
           axis.text.y = element_text(size = 14),
           axis.title.x = element_text(size = 20),
           axis.title.y = element_text(size = 20))+
     xlim(0,transect.length)+
     ylim(0,40)+
     xlab("Distance along transect (m)")+
     ylab("Height above ground (m)")+
     ggtitle(filename)+
     theme(plot.title = element_text(lineheight=.8, face="bold"))



