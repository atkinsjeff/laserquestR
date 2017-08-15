

ls.data<-read.csv(file.path("/Users/bhardima/Dropbox/1 Manuscripts/In Progress/Rugosity vs Landsat (Hardiman)/data/kyla_outputs/BH_landsat_products_ndvi_evi_tc_20160920.csv"))

ndvi.k1.mean   <-aggregate(ls.data$NDVI[ls.data$kernel==1],     list(ls.data$plotID[ls.data$kernel==1], ls.data$kernel[ls.data$kernel==1]),mean) #Only includes kernel==1
evi.k1.mean    <-aggregate(ls.data$EVI[ls.data$kernel==1],      list(ls.data$plotID[ls.data$kernel==1], ls.data$kernel[ls.data$kernel==1]),mean)
tc.k1.br.mean  <-aggregate(ls.data$TC_bright[ls.data$kernel==1],list(ls.data$plotID[ls.data$kernel==1], ls.data$kernel[ls.data$kernel==1]),mean)
tc.k1.gr.mean  <-aggregate(ls.data$TC_green[ls.data$kernel==1], list(ls.data$plotID[ls.data$kernel==1], ls.data$kernel[ls.data$kernel==1]),mean)
tc.k1.wt.mean  <-aggregate(ls.data$TC_wet[ls.data$kernel==1],   list(ls.data$plotID[ls.data$kernel==1], ls.data$kernel[ls.data$kernel==1]),mean)

ls.data.temp<-ls.data[ls.data$kernel!=5,-8] #define new onject that excludes all kernel==5 for next step

ndvi.k3.mean   <-aggregate(ls.data.temp$NDVI,list(ls.data.temp$plotID),mean)
evi.k3.mean    <-aggregate(ls.data.temp$EVI,list(ls.data.temp$plotID),mean)
tc.k3.br.mean  <-aggregate(ls.data.temp$TC_bright,list(ls.data.temp$plotID),mean)
tc.k3.gr.mean  <-aggregate(ls.data.temp$TC_green,list(ls.data.temp$plotID),mean)
tc.k3.wt.mean  <-aggregate(ls.data.temp$TC_wet,list(ls.data.temp$plotID),mean)

#revert back to main data
ndvi.k5.mean   <-aggregate(ls.data$NDVI,list(ls.data$plotID),mean) #Includes all kernels (1,3,5)
evi.k5.mean    <-aggregate(ls.data$EVI,list(ls.data$plotID),mean)
tc.k5.br.mean  <-aggregate(ls.data$TC_bright,list(ls.data$plotID),mean)
tc.k5.gr.mean  <-aggregate(ls.data$TC_green,list(ls.data$plotID),mean)
tc.k5.wt.mean  <-aggregate(ls.data$TC_wet,list(ls.data$plotID),mean)

all.temp<-data.frame(seq(1,nrow(ndvi.k5.mean),1))
all.temp$plot<-ndvi.k1.mean$Group.1
all.temp$ndvi.k1.mean<-ndvi.k1.mean$x
all.temp$ndvi.k3.mean<-ndvi.k3.mean$x
all.temp$ndvi.k5.mean<-ndvi.k5.mean$x
all.temp$evi.k1.mean<-evi.k1.mean$x
all.temp$evi.k3.mean<-evi.k3.mean$x
all.temp$evi.k5.mean<-evi.k5.mean$x
all.temp$tc.k1.br.mean<-tc.k1.br.mean$x
all.temp$tc.k3.br.mean<-tc.k3.br.mean$x
all.temp$tc.k5.br.mean<-tc.k5.br.mean$x
all.temp$tc.k1.gr.mean<-tc.k1.gr.mean$x
all.temp$tc.k3.gr.mean<-tc.k3.gr.mean$x
all.temp$tc.k5.gr.mean<-tc.k5.gr.mean$x
all.temp$tc.k1.wt.mean<-tc.k1.wt.mean$x
all.temp$tc.k3.wt.mean<-tc.k3.wt.mean$x
all.temp$tc.k5.wt.mean<-tc.k5.wt.mean$x
ls.all<-all.temp[,2:17]

plot(ls.all$ndvi.k1.mean,ls.all$ndvi.k5.mean)
colnames(ls.all)[1]<-"Site_Plot"

##Read in PCL data

# pcl.data<-read.csv("/Users/bhardima/Dropbox/1 Manuscripts/In Progress/Rugosity vs Landsat (Hardiman)/data/2016_laserquest_data/20161126_laserquest_master_output_CLEANED.csv")
setwd("C:/github/laserquestR")
pcl.data <- read.csv (file="data/20161126_laserquest_master_output_CLEANED.csv", header = TRUE)

pcl.data.mean<-aggregate(pcl.data[,5:20],list(pcl.data$Site,pcl.data$Plot),mean)
colnames(pcl.data.mean)[1]<-"Site"
colnames(pcl.data.mean)[2]<-"Plot"
pcl.data.mean$Site_Plot<-paste(pcl.data.mean$Site,"_0",pcl.data.mean$Plot,sep="")

# pcl.data.mean.short<-cbind(pcl.data.mean$Site_Plot,pcl.data.mean[,5:19])


head(pcl.data.mean)

pcl.data.mean$Site_Plot < -as.factor(pcl.data.mean$Site_Plot)

ls.pcl<-merge(ls.all, pcl.data.mean, by="Site_Plot",all.y=T,sort=T) #merge PCL and Landsat data
dim(ls.pcl)
ls.pcl<-ls.pcl[1:dim(ls.pcl)[1]-1,]

dim(ls.pcl)

ls.pcl$site.col<-NA
ls.pcl$site.col[ls.pcl$Site=="UNDE"]<-"dark green"
ls.pcl$site.col[ls.pcl$Site=="TREE"]<-"light green"
ls.pcl$site.col[ls.pcl$Site=="MLBS"]<-"dark blue"
ls.pcl$site.col[ls.pcl$Site=="SCBI"]<-"light blue"
ls.pcl$site.col[ls.pcl$Site=="TALL"]<-"dark red"
ls.pcl$site.col[ls.pcl$Site=="OSBS"]<-"pink"

########################################################################################################
##
##   NDVI
##
########################################################################################################

sites<-unique(ls.pcl$Site)
out.path<-"/Users/bhardima/Dropbox/1 Manuscripts/In Progress/Rugosity vs Landsat (Hardiman)/data/Figures"

pdf(file.path(out.path,"NDVI.pdf"),width=7,height=4)
par(mfrow=c(1,3))
for(i in 20:34){
  for(j in 2:4){
    plot(ls.pcl[,i],ls.pcl[,j],xlab=names(ls.pcl[i]),ylab=names(ls.pcl[j]),pch="")
    par(new=T)
    for(s in 1:6){
      col=ls.pcl$site.col[ls.pcl$Site==sites[s]]
      text(ls.pcl[ls.pcl$Site==sites[s],i],ls.pcl[ls.pcl$Site==sites[s],j],labels=ls.pcl$Site[ls.pcl$Site==sites[s]], col=col)
      fit<-lm(ls.pcl[ls.pcl$Site==sites[s],j]~ls.pcl[ls.pcl$Site==sites[s],i])
      if(!is.na(fit$coefficients[2])){
        X <- ls.pcl[ls.pcl$Site==sites[s],i]
        Y <- predict(fit, newdata=data.frame(ls.pcl[ls.pcl$Site==sites[s],i]))
        if(summary(fit)$adj.r.squared>0.25){
          width<-3;type<-1
        }else{
          width<-1;type<-3
        }
        lines(x=X, y=Y,col=col,lwd=width,lty=type)
      }
    print(c(s,i,j))}#for s (sites)
  }#for i (pcl vars)
} #for j (landsat vars)
dev.off()


########################################################################################################
##
##   EVI
##
########################################################################################################

pdf(file.path(out.path,"EVI.pdf"),width=7,height=4)
par(mfrow=c(1,3))
for(i in 20:34){
  for(j in 5:7){
    plot(ls.pcl[,i],ls.pcl[,j],xlab=names(ls.pcl[i]),ylab=names(ls.pcl[j]),pch="")
    par(new=T)
    for(s in 1:6){
      col=ls.pcl$site.col[ls.pcl$Site==sites[s]]
      text(ls.pcl[ls.pcl$Site==sites[s],i],ls.pcl[ls.pcl$Site==sites[s],j],labels=ls.pcl$Site[ls.pcl$Site==sites[s]], col=col)
      fit<-lm(ls.pcl[ls.pcl$Site==sites[s],j]~ls.pcl[ls.pcl$Site==sites[s],i])
      if(!is.na(fit$coefficients[2])){
        X <- ls.pcl[ls.pcl$Site==sites[s],i]
        Y <- predict(fit, newdata=data.frame(ls.pcl[ls.pcl$Site==sites[s],i]))
        if(summary(fit)$adj.r.squared>0.25){
          width<-3;type<-1
        }else{
          width<-1;type<-3
        }
        lines(x=X, y=Y,col=col,lwd=width,lty=type)
      }
      print(c(s,i,j))}#for s (sites)
  }#for i (pcl vars)
} #for j (landsat vars)
dev.off()

########################################################################################################
##
##   TC_Brightness
##
########################################################################################################
pdf(file.path(out.path,"TC_Brightness.pdf"),width=7,height=4)
par(mfrow=c(1,3))
for(i in 20:34){
  for(j in 8:10){
    plot(ls.pcl[,i],ls.pcl[,j],xlab=names(ls.pcl[i]),ylab=names(ls.pcl[j]),pch="")
    par(new=T)
    for(s in 1:6){
      col=ls.pcl$site.col[ls.pcl$Site==sites[s]]
      text(ls.pcl[ls.pcl$Site==sites[s],i],ls.pcl[ls.pcl$Site==sites[s],j],labels=ls.pcl$Site[ls.pcl$Site==sites[s]], col=col)
      fit<-lm(ls.pcl[ls.pcl$Site==sites[s],j]~ls.pcl[ls.pcl$Site==sites[s],i])
      if(!is.na(fit$coefficients[2])){
        X <- ls.pcl[ls.pcl$Site==sites[s],i]
        Y <- predict(fit, newdata=data.frame(ls.pcl[ls.pcl$Site==sites[s],i]))
        if(summary(fit)$adj.r.squared>0.25){
          width<-3;type<-1
        }else{
          width<-1;type<-3
        }
        lines(x=X, y=Y,col=col,lwd=width,lty=type)
      }
      print(c(s,i,j))}#for s (sites)
  }#for i (pcl vars)
} #for j (landsat vars)
dev.off()

########################################################################################################
##
##   TC_Greenness
##
########################################################################################################
pdf(file.path(out.path,"TC_Greenness.pdf"),width=7,height=4)
par(mfrow=c(1,3))
for(i in 20:34){
  for(j in 11:13){
    plot(ls.pcl[,i],ls.pcl[,j],xlab=names(ls.pcl[i]),ylab=names(ls.pcl[j]),pch="")
    par(new=T)
    for(s in 1:6){
      col=ls.pcl$site.col[ls.pcl$Site==sites[s]]
      text(ls.pcl[ls.pcl$Site==sites[s],i],ls.pcl[ls.pcl$Site==sites[s],j],labels=ls.pcl$Site[ls.pcl$Site==sites[s]], col=col)
      fit<-lm(ls.pcl[ls.pcl$Site==sites[s],j]~ls.pcl[ls.pcl$Site==sites[s],i])
      if(!is.na(fit$coefficients[2])){
        X <- ls.pcl[ls.pcl$Site==sites[s],i]
        Y <- predict(fit, newdata=data.frame(ls.pcl[ls.pcl$Site==sites[s],i]))
        if(summary(fit)$adj.r.squared>0.25){
          width<-3;type<-1
        }else{
          width<-1;type<-3
        }
        lines(x=X, y=Y,col=col,lwd=width,lty=type)
      }
      print(c(s,i,j))}#for s (sites)
  }#for i (pcl vars)
} #for j (landsat vars)
dev.off()

########################################################################################################
##
##   TC_Wetness
##
########################################################################################################
pdf(file.path(out.path,"TC_Wetness.pdf"),width=7,height=4)
par(mfrow=c(1,3))
for(i in 20:34){
  for(j in 14:16){
    plot(ls.pcl[,i],ls.pcl[,j],xlab=names(ls.pcl[i]),ylab=names(ls.pcl[j]),pch="")
    par(new=T)
    for(s in 1:6){
      col=ls.pcl$site.col[ls.pcl$Site==sites[s]]
      text(ls.pcl[ls.pcl$Site==sites[s],i],ls.pcl[ls.pcl$Site==sites[s],j],labels=ls.pcl$Site[ls.pcl$Site==sites[s]], col=col)
      fit<-lm(ls.pcl[ls.pcl$Site==sites[s],j]~ls.pcl[ls.pcl$Site==sites[s],i])
      if(!is.na(fit$coefficients[2])){
        X <- ls.pcl[ls.pcl$Site==sites[s],i]
        Y <- predict(fit, newdata=data.frame(ls.pcl[ls.pcl$Site==sites[s],i]))
        if(summary(fit)$adj.r.squared>0.25){
          width<-3;type<-1
        }else{
          width<-1;type<-3
        }
        lines(x=X, y=Y,col=col,lwd=width,lty=type)
      }
      print(c(s,i,j))}#for s (sites)
  }#for i (pcl vars)
} #for j (landsat vars)
dev.off()
