
     
read.bob <- function(data_dir, filename) {
          f <- file.path(data_dir, filename)
          bob <- read.csv(f, header=FALSE)
          return(bob)
     }
    
make_bob_summary<- function(m) {
          
          
          
          # maximum value of VAI in the column
          d <- setNames(aggregate(vai ~ xbin, data = m, FUN = max, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "max.vai"))
          
          # sum of VAI in column
          e <- setNames(aggregate(vai ~ xbin, data = m, FUN = sum, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "sum.vai"))
          
          # standard deviation of VAI for column
          f <- setNames(aggregate(vai ~ xbin, data = m, FUN = sd, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "sd.vai"))
          
          # this is height at which max vai occurs
          g <- m$zbin[match(d$max.vai, m$vai)]  
          g <- data.frame(g)
          colnames(g) <- c("max.vai.z")
          #print(g)
          
          #mean column leaf height that is the "heightBin" from Matlab code
          # first we make el
          #m$el <- (m$vai / m$sum.vai) * 100
          m$vai.z <- m$vai * (m$zbin +0.5)
          h <- setNames(aggregate(vai.z ~ xbin, data = m, FUN = sum, na.rm = FALSE,  na.action = 'na.pass'), c("xbin", "vai.z.sum"))
          
          
          # this section joins all these guys together
          p <- join_all(list( d, e, f, h), by = "xbin", type = "full")
          p <- p[with(p, order(xbin)), ]
          p <- cbind(p, g)
          
          j <- m[m$vai != 0,]
          
          m %>% group_by(xbin) %>% filter(vai != 0) %>% summarize(max.ht=max(zbin)) -> k
          k <-data.frame(k)
          
          p$height.bin <- p$vai.z.sum / p$sum.vai
          p[is.na(p)] <- 0
          # p$std.bin.num <- p$vai * ((p$zbin - p$height.bin)^2)
          # 
          # j <- aggregate(std.bin.num ~ xbin, data = p, FUN = sum, na.rm = FALSE, na.action = 'na.pass')
          # j[is.na(j)] <- 0
          # print(j)
          # 
          # p <- merge(p, j, by = "xbin")
          # p$std.bin <- p$std.bin.num / p$sum.vai 
          # first we sort
          p <- merge(p, k, by = c("xbin"), keep.all = TRUE)
          return(p)
          
     }
     # j <- b[b$vai != 0,]
     # k <- t(sapply(split(j, j$xbin), function(a) a[NROW(a),1:2]))
     # k <- data.frame(k)
     # k <- plyr::rename(k, c("zbin" = "max.ht", "xbin" = "xbin"))
     # b %>% group_by(xbin) %>% filter(vai != 0) %>% summarize(max.ht=max(zbin)) -> k

     
     
     
calc_rumple <- function(df){
          df$rump.diff <- 0
          
          for (i in 2:nrow(df)) {
               
               df$rump.diff[i] <- abs(ceiling(df$max.ht[i - 1]) - ceiling(df$max.ht[i])) 
               
          }
          #print(df$rump.diff)
          rumple = (sum(df$rump.diff) + max(df$xbin)) / max(df$xbin)
          message("Rumple")
          print(rumple)
          return(rumple)
     }
     
 

     
     calc_bob_gap_fraction <- function(m){
          transect.length <- max(m$xbin)
          
          for(i in 1:nrow(m)){
               if (m$vai[i] == 0) {
                    m$gap[i] = 1
               }else{
                    m$gap[i] = 0
               }
          }
          # the thinking here is that you can average across the z plane to get the gap fraction, or the portion of sky/canopy unobstructed by canopy. so if four pixels out of five were empty, that would be 0.8 gap fraction  or the mean(c(1,1,1,1,0))--think of the 1 as gap = true
          #print(m$gap)
          gap.list <- setNames(aggregate(gap ~ zbin, data = m, FUN = mean, na.rm = FALSE, na.action = 'na.pass'), c("zbin", "gap.fraction"))
          #print(gap.list)
          
          mean.gap.fraction = mean(gap.list$gap.fraction)
          message("Mean Gap Fraction ---as error check should be same as porosity")
          print(mean.gap.fraction)
          
          message("now we replace the 0's with 1's so when we take the ln they = 0")
          gap.list[gap.list == 0] <- 1
          
          #print(gap.list)
          gap.list$ln.gap.fraction <- log(gap.list$gap.fraction)
          
          #print(gap.list)
          clump <- log(mean.gap.fraction) / mean(gap.list$ln.gap.fraction)
          message("Clumping Index")
          print(clump)
          return(clump)
          
     }
     
  
     
     
     # RUGOSITY
     calc_bob_rugosity <- function(b, c, filename) {
          #df = the summary matrix
          #m = vai matrix
          
          
          
          transect.length = max(c$xbin)
          message("Transect Length (m)")
          print(transect.length)
          
          mean.height = mean(c$height.bin)
          message("MeanHeight - plot mean of column mean return height")
          print(mean.height)
          
          height.2 <- sd(c$height.bin)
          message("Standard Deviation of mean height for each xbin - height2")
          print(height.2)
          
          
          mode.el = mean(c$max.vai.z)
          message("Mean Height of Maximum Return Density -- modeEl")
          print(mode.el)
          
          
          c$max.vai.sq <- c$max.vai.z^2
          mode.2 <- mean(c$max.vai.sq)
          mode.2 = (mode.2 - (mode.el * mode.el))^0.5
          message("Mean height of squared max VAI whatever the hell that is -- or mode2")
          print(mode.2)
          
          
          
          max.el = max(c$max.vai.z)
          message("Maximum VAI for entire transect -- max el!")
          print(max.el)
          
          
          
          max.can.ht = max(c$max.ht)
          message("Max canopy height (m)")
          print(max.can.ht)
          
          mean.max.ht = mean(c$max.ht)
          message("Mean Max canopy height (m) -- meanTopel w/ deep gaps removed")
          print(mean.max.ht)
          
          mean.vai = mean(c$sum.vai)
          message("Mean VAI")
          print(mean.vai)
          
          message("Maximum VAI")
          max.vai = max(c$sum.vai)
          print(max.vai)
          
          e <- subset(c, max.ht == 0)
          deep.gaps <- nrow(e)
          message("Deep Gaps")
          print(deep.gaps)
          
          porosity = sum(b$vai == 0) / length(b$vai)
          message("Canopy porosity")
          print(porosity)
          
          #being rugosity intermediates
          
          #first we adjust the vai at each x,z by the z height of the bin
          combo.meal <- merge(b, c, by = "xbin")
          
          combo.meal$std.bin.num <- combo.meal$vai * (((combo.meal$zbin + 0.5)  - combo.meal$height.bin)^2)
          
          j <- aggregate(std.bin.num ~ xbin, data = combo.meal, FUN = sum, na.rm = FALSE, na.action = 'na.pass')
          j[is.na(j)] <- 0
          
          
          super.size <- merge(c, j, by = "xbin")
          
          super.size$std.bin <- super.size$std.bin.num / super.size$sum.vai
          
          super.size$std.bin.squared <- (super.size$std.bin^2)
          
          super.size[is.na(super.size)] <- 0
          #print(super.size)
          std.std = mean(super.size$std.bin.squared)
          #std.std = std.std/transect.length
          
          mean.std = mean(super.size$std.bin)
          
          
          message("Square of leaf height variance (stdStd from old script)")
          print(std.std)
          
          
          message("Mean Standard deviation of leaf heights -- meanStd")
          print(mean.std)
          
          rugosity = (std.std - mean.std * mean.std)^0.5
          message("Canopy Rugosity")
          print(rugosity)
          
          # #uses temp. data frame with deep gaps removed
          
          jess.rugosity = sd(c$max.ht)
          
          # sum(el_CP(CP(p)+k-1,:).*((z_CP(CP(p)+k-1,:)-heightBin).^2))/sum(el_CP(CP(p)+k-1,:))
          
          
          
          message("Surface Rugosity--TopRugosity")
          print(jess.rugosity)
          
          variable.list <- list(plot = filename,
                                mean.height = mean.height,
                                transect.length = transect.length, 
                                mode.el = mode.el, 
                                height.2 = height.2, 
                                max.el = max.el,
                                mode.2 = mode.2,
                                max.can.ht = max.can.ht,
                                mean.max.ht = mean.max.ht,
                                mean.vai = mean.vai,
                                max.vai = max.vai,
                                deep.gaps = deep.gaps,
                                porosity = porosity,
                                std.std = std.std,
                                mean.std = mean.std,
                                rugosity = rugosity,
                                top.rugosity = jess.rugosity)
          
          
          #now to write to csv
          variable.list <- data.frame(variable.list)
          return(variable.list)
          
     }
     

     
     
     
     combine_variables <- function(variable.list,  rumple, clumping.index){
          
          output.variables <- cbind(variable.list,  rumple, clumping.index)
          return(output.variables)
          
     }
     

     
     write.pcl.to.csv <- function(output.variables, filename, output_directory) {
          
          filename2 <- paste(filename, ".csv", sep="")
          write.csv(output.variables, file.path(output_directory, filename2))
     }
     
