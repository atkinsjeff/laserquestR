require(tidyr)
require(dplyr)
require(magrittr)

xbin <- c(1,1,1,1,1,2,2,3,3,4,4,4)
zbin <- c(1,4,5,6,7,1,4,2,8,1,2,5)
value <-c(9,9,9,9,9,9,9,9,9,9,9,9)


df <- data.frame(xbin, zbin, value)
df2 <- data.frame(xbin = c(1:max((df$xbin))),
                  zbin = c(1:max((df$zbin))))
df3 <- expand.grid(xbin = c(1:max((df$xbin))),
                   zbin = c(1:max((df$zbin))))


x <- seq(0, 10, length.out = 100)
y <- seq(-1, 1, length.out = 20)
d1 <- expand.grid(x = x, y = y)

#
merge(df, data.frame(table(df2[1:2])),all.y=TRUE)

df2$xbin <- c(1:max((x)))
mat <- matrix(0, max(z), max(x))

data.frame(x,z, value)

mat[cbind(df$z, df$x)] <- df$value

mat2 <- data.frame(mat)
mat2$zbin <- seq.int(nrow(mat2))
gather(mat2, key = xbin)


     mat %>% 
     gather(type, num_widgets) %>% ## gather the "num_widgets" columns
     mutate(type = sub("_num_widgets", "", type)) ## remove the suffix
####
####
a <- data_frame(my_type_1_num_widgets = c(1, 2, 3), my_type_2_num_widgets = c(4, 5, 6))
a %>% 
     gather(type, num_widgets) %>% ## gather the "num_widgets" columns
     mutate(type = sub("_num_widgets", "", type)) ## remove the suffix

mutate(iris, sepal = Sepal.Length + Sepal.Width)


master <- data.frame(A = rep(1:2, each = 12),
                     B = rep(1:3, each = 4),
                     C = rep(1:4, times = 6))


library(iris)
iris %>%
     group_by(Species) %>%
     summarise(avg = mean(Sepal.Width)) %>%
     arrange(avg)
library(dplyr)

master %>%
     left_join(., mydf) %>%
     mutate(D = ifelse(D %in% NA, 0, D))

A <- c(1,1,1,1,1,1,2,2,2)
B <- c(1,1,1,2,2,6,1,1,6)
C <- c(1,2,3,1,4,1,2,4,2)
D <- c(200,50,15,150,50,300,40,90,80)

df <- data.frame(cbind(A,B,C,D))

#merge(df, data.frame(table(df[1:3]))[-4],all.y=TRUE)  #the original that works
max.height = 32
transect.length = 40
m.one <- matrix(nrow = max.height, ncol = transect.length)
merge(df, data.frame(table(df[1:3]))[-4],all.y=TRUE)

table(30:23)
table(df[1:3])


l <- function(x) x + 1
m <- function() {
     l <- function(x) x * 3
     l(10)
}
m()



