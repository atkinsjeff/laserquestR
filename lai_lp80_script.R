#Author: Jeff Atkins
#Date: 11/30/2016 
#Purpose: Script to analyze par and CSC metrics from Laserquest_2016

require(magrittr)
require(plyr)

setwd("C:/github/laserquestR")

lai.master <- read.csv("LP_80.csv")

lai <-  lai.master[,c(3,4,5,7)]


rename(d, c("beta"="two", "gamma"="three"))
#>   alpha two three
#> 1     1   4     7
#> 2     2   5     8
#> 3     3   6     9