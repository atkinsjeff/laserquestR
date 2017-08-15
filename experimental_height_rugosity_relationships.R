plot(site.means$mean.height_mean, site.means$mean.max.ht_mean)

lm.height <- lm(site.means$mean.height_mean ~ site.means$mean.max.ht_mean)
summary(lm.height)

plot(resid(lm.height), site.means$rugosity_mean)

lm.resid <- lm(resid(lm.height) ~ site.means$rugosity_mean)
require(ggplot2)

y <- site.means$mean.max.ht_mean - site.means$mean.height_mean
x <- site.means$rugosity_mean
z <- site.means$site

lm.moch.mean <- lm(y ~ x)
summary(lm.moch.mean)
df1 <- data.frame(x, y, z)

ggplot(df1, aes( x = x, y = y, color = z))+
     geom_point(size = 4)+
     xlab("rugosity")+
     ylab("MOCH - Mean Leaf Height")+
     theme_classic()+
     geom_smooth(method = "lm", se = FALSE, size = 5)

light.means %>%
     group_by(site)