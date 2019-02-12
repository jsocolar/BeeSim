library(fields)
library(spam)
library(viridis)
setwd("/Users/JacobSocolar/Dropbox/Work/Beesim_fullrun/")

load("bee_output.Rdata")
col <- colorRampPalette(c("navy", "coral"))(120)

attach(bee_output$inputs)

# matrix of panels for split-screen with a 3x3 panel matrix to plot each combo of bee preference sds, and extra panels
# of x and y axis labels
splits <- rbind(c(0,1,0,.1), # x axis label
                c(0,.1,0,1), # y axis label
                
                c(.1,.4,.1,.4), # bottom left
                c(.1,.4,.4,.7), # middle left
                c(.1,.4,.7,1), # top left

                c(.4,.7,.1,.4), # bottom center
                c(.4,.7,.4,.7), # middle center
                c(.4,.7,.7,1), # top center
                
                c(.7,1,.1,.4), # bottom right
                c(.7,1,.4,.7), # middle right
                c(.7,1,.7,1)) # top right

coexistence_values <- array(dim=lapply(bee_output$inputs, FUN=length))
cvs <- rep(NA,4)
for(i1 in 1:length(bee_output$inputs[[1]])){
  for(i2 in 1:length(bee_output$inputs[[2]])){
    for(i3 in 1:length(bee_output$inputs[[3]])){
      for(i4 in 1:length(bee_output$inputs[[4]])){
        for(i5 in 1:length(bee_output$inputs[[5]])){
          for(i6 in 1:length(bee_output$inputs[[6]])){
            for(i7 in 1:length(bee_output$inputs[[7]])){
              for(i8 in 1:length(bee_output$inputs[[8]])){
                for(i9 in 1:length(bee_output$inputs[[9]])){
                  for(i10 in 1:length(bee_output$inputs[[10]])){
                    for(j in 1:4){
                      cvs[j] <- (bee_output$output[[j]][[i1]][[i2]][[i3]][[i4]][[i5]][[i6]][[i7]][[i8]][[i9]][[i10]]$plant_timeseries[101,2] *
                        bee_output$output[[j]][[i1]][[i2]][[i3]][[i4]][[i5]][[i6]][[i7]][[i8]][[i9]][[i10]]$plant_timeseries[101,3]) > 0
                    }
                    coexistence_values[i1,i2,i3,i4,i5,i6,i7,i8,i9,i10] <- mean(cvs)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

# Make sure we aren't dealing with the vestiges of a previous plotting object or screen splitting
dev.off()

for(i5 in 1:length(bee_output$inputs$rare_advantage)){
  for(i6 in 1:length(bee_output$inputs$EP)){
    for(i7 in 1:length(bee_output$inputs$e21)){
      for(i8 in 1:length(bee_output$inputs$e12)){
        for(i9 in 1:length(bee_output$inputs$beefluct)){
          for(i10 in 1:length(bee_output$inputs$bee_plant_ratio)){
            close.screen(all.screens=T)
            pdf(file=paste("bee_output_plotting",i5,i6,i7,i8,i9,i10, ".pdf", sep="_"), width=13, height=15)
            
            split.screen(splits)
            
            screen(1)
            text("mean preference; bee sp. 1",x=.5,y=.7)
            lines(x=c(0,1),y=c(.5,.5))
            text("1", x=.5,y=.35,cex=1.3)
            lines(x=c(.5,.5), y=c(.45,.5))
            text("0", x=.14,y=.35,cex=1.3)
            lines(x=c(.14,.14), y=c(.45,.5))
            text("3", x=.86,y=.35,cex=1.3)
            lines(x=c(.86,.86), y=c(.45,.5))
            text("sd preference; bee sp. 1", x=.5, y=.1, cex=1.2)
            
            screen(2)
            text("mean preference; bee sp. 2",x=.7,y=.5, srt=90)
            lines(x=c(.5,.5),y=c(0,1))
            text("1", x=.35,y=.5,cex=1.3, srt=90)
            lines(x=c(.45,.5), y=c(.5,.5))
            text("0", x=.35,y=.13,cex=1.3, srt=90)
            lines(x=c(.45,.5), y=c(.14,.14))
            text("3", x=.35,y=.86,cex=1.3, srt=90)
            lines(x=c(.45,.5), y=c(.86,.86))
            text("sd preference; bee sp. 2", x=.1, y=.5, cex=1.2, srt=90)
            
            
            scrnum <- 2
            for(sd1 in 1:3){
              for(sd2 in 1:3){
                scrnum <- scrnum+1
                screen(scrnum)
                par(mar=rep(1.5, 4))
                mymatrix <- t(coexistence_values[,,sd1,sd2,i5,i6,i7,i8,i9,i10])
                mymatrix <- mymatrix[nrow(mymatrix):1, ]
                xvals <- c(rep(bee_output$inputs$bee_logmean1[1], length(bee_output$inputs$bee_logmean2)),
                           rep(bee_output$inputs$bee_logmean1[2], length(bee_output$inputs$bee_logmean2)),
                           rep(bee_output$inputs$bee_logmean1[3], length(bee_output$inputs$bee_logmean2)))
                yvals <- rep(rev(bee_output$inputs$bee_logmean2), length(bee_output$inputs$bee_logmean1))
                mv <- as.vector(mymatrix)
                for(k in 1:length(mv)){
                  mv[k] <- plasma(100)[min(which(c(1:100) >= (25 + 50*as.numeric(mv[k]))))]
                }
                plot(xvals, yvals, pch=19, col=mv, cex=3, axes = F, xlab='', ylab='', xlim=c(min(bee_output$inputs$bee_logmean1) - 1,max(bee_output$inputs$bee_logmean1) + 2),
                     ylim=c(min(bee_output$inputs$bee_logmean2) - 1,max(bee_output$inputs$bee_logmean2) + 2))
                axis(side=1, at=bee_output$inputs$bee_logmean1)
                axis(side=2, at=bee_output$inputs$bee_logmean2)
              }
            }
            dev.off()
          }
        }
      }
    }
  }
}
