# Sandbox
source(functions.R)

f <- ("data/test_data.csv")



test.data <- read.pcl(f)
head(test.data)

pcl.diagnostic.plot(test.data, "SWBR", 25)



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