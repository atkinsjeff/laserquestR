

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

