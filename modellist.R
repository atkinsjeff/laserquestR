
# Required packages
require(broom)
require(ggplot2)
require(nls2)

####### Data import section for plot level analysis
#Data import
# plot.fpar <- read.csv("laserquest_master_fpar_plot.csv")
# 
# # designate light regime based on aPAR or above canopy PAR
# plot.fpar$regime <- ifelse(plot.fpar$aPAR_mean >= 1000, "high",
#                            "low")
# 
# plot.fpar$regime <- as.factor(plot.fpar$regime)
# 
# ### Modeling section
# #Subset data for modelling
high <- subset(master.plot, aPAR_mean >= 1000)
low <- subset(plot.fpar, aPAR_mean < 1000)
# 
# # Define x and y for script

high$gap.norm <- high$deep.gaps_mean / high$transect.length_mean
x <- high$gap.norm
y <- high$fPAR_mean



# ###### Data import section for site level analysis
# x <- master.site$deep.gaps_mean
# y <- master.site$fPAR_mean
# 
# #w/o OSBS
# x <- subset(master.site$mean.max.ht_mean, master.site$site != "OSBS")
# y <- subset(master.site$fPAR_mean, master.site$site != "OSBS")
# 
plot(x, y)

x.label <- "Rc"
y.label <- "fPAR"

# 
df <- data.frame(x, y)
# 
# #################
#model list
lm(log(y) ~ log(x))

#### Michaelis Menten equation ### generating some data
# building a Self Start object, finding the optimal values
print(getInitial(y ~ SSmicmen(x, max(y), 1), data = df), digits = 3)

linear.model        <- lm(y ~ x)
rec.hyper           <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
rec.hyper2          <- nls(y ~ a + ((b * x) / (c + x)), start = list(a = 0.1, b = 1, c = 1))
sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = -1, b = 1))
log.growth          <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = -1, b = -2, k = 0.5) )
logarithmic         <- nls(y ~ a + (b * log(x)), start = list(a = -0.1, b = 1))
power               <- nls(y ~ a * (x^b), start = list(a = 1, b = 1))
log                 <- lm(y ~ log(x))

# 

ggplot(df, aes(x = x, y = y))+
     geom_point()+
     geom_smooth(method = "nls", 
                 formula = y ~ (a * x) / (b + x),
                 method.args = list(start = list(a = 1 , b = 1)),
                 se = FALSE,
                 color = "blue")+
     geom_smooth(method = "nls",
                 formula = y ~ (1 / (1 + exp(-a * (x - b)))),
                 method.args = list(start = list(a = -0.011 , b = 1)),
                 se = FALSE,
                 color = "dark green")+
     geom_smooth(method = "nls",
                 formula = y ~ a + (b * x) / (c + x),
                 method.args = list(start = list(a = 1, b = -0.1, c = -1)),
                 se = FALSE,
                 color = "purple")+
     geom_smooth(method = "lm", se = FALSE, color = "red")+
     geom_smooth(method = "nls",
                 formula = y ~ (a * (x^b)),
                 method.args = list(start = list(a = 1, b = 1)),
                 se = FALSE,
                 color = "yellow")+
     theme_classic()+
     xlab(x.label)+
     ylab(y.label)

     geom_line(data = df, aes(x = x, y = predict(linear.model), color = "LinearModel"), size = 1.25)+
     geom_line(aes(x = x, y = predict(rec.hyper), color = "RecHyper"), size = 1.25)+
     geom_line(aes(x = x, y = predict(rec.hyper2), color = "RecHyper2"), size = 1.25)+
     geom_line(aes(x = x, y = predict(sigmoidal.curve), color = "Sigmoid"), size = 1.25)+
     geom_line(aes(x = x, y = predict(log.growth), color = "LogGrowth"), size = 1.25)+
     geom_line(aes(x = x, y = predict(logarithmic), color = "Logarithmic"), size = 1.25)+
     scale_color_manual(name = "Line Color",
                        values = c(LinearModel = "red", RecHyper = "blue", RecHyper2 = "purple", Sigmoid = "dark green",
                                   LogGrowth = "green", Logarithmic = "orange"))+
     theme_classic()+
     xlab(x.label)+
     ylab(y.label)
# 
# 



## Collates model list
model.list <- list(linear.model, rec.hyper, rec.hyper2, sigmoidal.curve,  log.growth, logarithmic, power, log)

#names the model items
names(model.list) <- c("linear.model","rectangular hyperbola", "rectangular hyperbola 2", "sigmoidal curve",  "log growth", "logarithmic", "power", "log")

# reorganizes
model.output$model.name <- model.names

#### AIC and summary stats
#AIC
lapply(model.list, AIC)
lapply(model.list, tidy)
#create model output date frame first

# List of R-squared values. Unadjusted.
for (name in names(model.list)) {

     #model Name
     model.output[i,1] = name     

     #R squared
     RSS <- sum(residuals(model.list[[name]])^2)
     TSS <- sum((y - mean(y))^2)
     r2 = 1 - (RSS/TSS)
     print(name)
     print(r2)
}

# 


