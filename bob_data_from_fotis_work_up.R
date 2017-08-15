require(tidyr)
require(magrittr)
require(plyr)
library(dplyr)

source("bob_functions.R")

data_dir <-  "./bob"
# filename <- "RICE-C1-C.CSV"

file.names <- dir(data_dir, pattern =".csv")
length(file.names)

for(i in 1:length(file.names)){
     filename <- file.names[i]
     write_out <- FALSE
 
     
# read.csv("./bob/TSCA1.csv", header = FALSE)
#Import the file
bob <- read.bob(data_dir, filename)

# Make the zbin from the file source
a <- tibble::rownames_to_column(bob, "zbin")
a$zbin <- rev(a$zbin)

# Data tidying to get xbin
b <- gather(a, "xbin", "vai", 2:51)

b$xbin <- substring(b$xbin, 2)
b$xbin <- as.integer(b$xbin)
b$zbin <- as.integer(b$zbin)

c <- make_bob_summary(b)



rump.rump <- calc_rumple(c)


gap.gap <- calc_bob_gap_fraction(b)


variable.list <- calc_bob_rugosity(b, c, filename)


output.variables <- combine_variables(variable.list, rump.rump, gap.gap)

outputname = substr(filename,1,nchar(filename)-4)
outputname <- paste(outputname, "output", sep = "_")
dir.create("bob_output", showWarnings = FALSE)
output_directory <- "./bob_output/"
write.pcl.to.csv(output.variables, filename, output_directory)



#get filename first
plot.filename <- file_path_sans_ext(filename)

plot.file.path <- file.path(paste(output_directory, plot.filename, ".png", sep = ""))

vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
#x11(width = 8, height = 6)
hit.grid <- ggplot(b, aes(x = xbin, y = zbin))+ 
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
     ylim(0,41)+
     xlab("Distance along transect (m)")+
     ylab("Height above ground (m)")+
     ggtitle(filename)+
     theme(plot.title = element_text(lineheight=.8, face="bold"))

ggsave(plot.file.path, hit.grid)
}




library(dplyr)
library(readr)
df <- list.files(path = output_directory, full.names = TRUE) %>%
     lapply(read_csv) %>%
     bind_rows

df <- data.frame(df)
df$ID <- substr(df$plot, 1, nchar(df$plot) - 4)

df <- df[, c(21, 2:20)]
write.csv(df, "bob_data_merged_table.CSV")
# 
# bob <- add_rownames(bob, "zbin")
# bob$zbin <- as.factor(bob$zbin)
# 
# df <- gather(bob, "xbin", "hits", 2:51)
# 
# df$xbin <- substring(df$xbin, 2)
# 
# df$xbin <- as.integer(df$xbin)
# df$zbin <- as.integer(df$zbin)
# 
# df <- data.frame(df)
# 
# df %>%
#      expand(zbin, xbin) %>%
#      left_join(df) -> df
# df <- data.frame(df)
# 
# x.sum <- aggregate(hits ~ xbin, data = df, FUN = sum)
# 
# names(x.sum)[2] <- "total"
# 
# df <- join_all(list(df, x.sum), by = "xbin")
# 
# df$density <- df$hits / df$total
# 
# # empirical VAI model based on input
# df$vai <- (log(1.0 - (df$density)*0.97401)  * -1) /0.5
# 
# aggregate(vai ~ xbin, data = df, FUN = sum)
# 
# 
# 
# 
# head(df)
# 
# df2 <- df[,c(2,1,5)]
# 
# df2 <- df2[with(df2, order(xbin, zbin)),]
# 
# bob.vai <- calc_tls_vai(df2)
# bob.vai <- calc_tls_mean_leaf_ht(bob.vai)
# calc_tls_std_bin(bob.vai)
# 
# 
# 
# vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
# x11(width = 8, height = 6)
# ggplot(bob.vai, aes(x = xbin, y = zbin))+ 
#      geom_tile(aes(fill = vai))+
#      scale_fill_gradient(low="white", high="dark green", 
#                          name=vai.label)+
#      #scale_y_continuous(breaks = seq(0, 20, 5))+
#      # scale_x_continuous(minor_breaks = seq(0, 40, 1))+
#      theme(axis.line = element_line(colour = "black"),
#            panel.grid.major = element_blank(),
#            panel.grid.minor = element_blank(),
#            panel.background = element_blank())+
#      xlim(0,40)+
#      ylim(0,30)+
#      xlab("Distance along transect (m)")+
#      ylab("Height above ground (m)")+
#      ggtitle("")+
#      theme(plot.title = element_text(lineheight=.8, face="bold"))
# 
# 
