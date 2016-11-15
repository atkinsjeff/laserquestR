start.time <- Sys.time()
# Source functions
source("functions.R")

# Set parameters
#data_dir <- "./rice_control_one"
data_dir <- "./data/"
# <- "osbs_28_west.csv"
#filename <- "VDGIF-T5-06072016.csv"
filename <- "AF_G8_0729016.CSV"
#filename <- "rice_control_one.CSV"

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
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# 
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

write.csv(m1, "dgif-t5.csv")
#### this makes a hit grid. keep it.
#### 
#### 
vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
x11(width = 8, height = 6)
ggplot(m5, aes(x = xbin, y = zbin))+ 
     geom_tile(aes(fill = vai))+
     scale_fill_gradient(low="white", high="dark green", 
                         name=vai.label)+
     #scale_y_continuous(breaks = seq(0, 20, 5))+
     # scale_x_continuous(minor_breaks = seq(0, 40, 1))+
     theme(axis.line = element_line(colour = "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank())+
     xlim(0,transect.length)+
     ylim(0,35)+
     xlab("Distance along transect (m)")+
     ylab("Height above ground (m)")+
     ggtitle(filename)+
     theme(plot.title = element_text(lineheight=.8, face="bold"))



