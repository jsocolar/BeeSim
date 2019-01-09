library(fields)
library(spam)

library(viridis)
col <- colorRampPalette(c("navy", "coral"))(120)


load("C:/Users/betha/Dropbox/Work/Research/Konzmann & Lunau follow up/L&A 1970 follow up - R figures/code/Beesim_fullrun/OCarray7.Rdata")
bee_logsd1 <- c(0,1,3)
bee_logsd2 <- c(0,1,3)

rare_advantage <- seq(.1,1.9,.2)
rare_advantage <- round(10*rare_advantage)/10
X <- c(0,2,4,6,10)
pos_x <- c(1:5)
Y <- c(-10,-6,-4,-2,0,2,4,6,10)
pos_y <- c(1:9)

rA <- c(.1,.5,.9,1.5)


pdf(file=paste('C:/Users/betha/Dropbox/Work/Research/Konzmann & Lunau follow up/L&A 1970 follow up - R figures/code/Beesim_fullrun/OCarray7_test_Oct30.Rdata',i1,'.pdf',sep=""), width=50, height=50)
par(mfrow=c(3,3))
dev.off()
split.screen(splits)

ps1 <- 8
scrnum <- ps1

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
text("mean preference; bee sp. 1",x=.5,y=.7)
lines(x=c(0,1),y=c(.5,.5))
text("1", x=.5,y=.35,cex=1.3)
lines(x=c(.5,.5), y=c(.45,.5))
text("0", x=.14,y=.35,cex=1.3)
lines(x=c(.14,.14), y=c(.45,.5))
text("3", x=.86,y=.35,cex=1.3)
lines(x=c(.86,.86), y=c(.45,.5))
text("sd preference; bee sp. 1", x=.5, y=.1, cex=1.2)

screen(3)
text("mean preference; bee sp. 1",x=.5,y=.7)
lines(x=c(0,1),y=c(.5,.5))
text("1", x=.5,y=.35,cex=1.3)
lines(x=c(.5,.5), y=c(.45,.5))
text("0", x=.14,y=.35,cex=1.3)
lines(x=c(.14,.14), y=c(.45,.5))
text("3", x=.86,y=.35,cex=1.3)
lines(x=c(.86,.86), y=c(.45,.5))
text("sd preference; bee sp. 1", x=.5, y=.1, cex=1.2)

screen(4)
text("mean preference; bee sp. 1",x=.5,y=.7)
lines(x=c(0,1),y=c(.5,.5))
text("1", x=.5,y=.35,cex=1.3)
lines(x=c(.5,.5), y=c(.45,.5))
text("0", x=.14,y=.35,cex=1.3)
lines(x=c(.14,.14), y=c(.45,.5))
text("3", x=.86,y=.35,cex=1.3)
lines(x=c(.86,.86), y=c(.45,.5))
text("sd preference; bee sp. 1", x=.5, y=.1, cex=1.2)

screen(5)
text("mean preference; bee sp. 2",x=.7,y=.5, srt=90)
lines(x=c(.5,.5),y=c(0,1))
text("1", x=.35,y=.5,cex=1.3, srt=90)
lines(x=c(.45,.5), y=c(.5,.5))
text("0", x=.35,y=.13,cex=1.3, srt=90)
lines(x=c(.45,.5), y=c(.14,.14))
text("3", x=.35,y=.86,cex=1.3, srt=90)
lines(x=c(.45,.5), y=c(.86,.86))
text("sd preference; bee sp. 1", x=.1, y=.5, cex=1.2, srt=90)




screen(6)
text("mean preference; bee sp. 2",x=.7,y=.5, srt=90)
lines(x=c(.5,.5),y=c(0,1))
text("1", x=.35,y=.5,cex=1.3, srt=90)
lines(x=c(.45,.5), y=c(.5,.5))
text("0", x=.35,y=.13,cex=1.3, srt=90)
lines(x=c(.45,.5), y=c(.14,.14))
text("3", x=.35,y=.86,cex=1.3, srt=90)
lines(x=c(.45,.5), y=c(.86,.86))
text("sd preference; bee sp. 1", x=.1, y=.5, cex=1.2, srt=90)

screen(7)
text("mean preference; bee sp. 2",x=.7,y=.5, srt=90)
lines(x=c(.5,.5),y=c(0,1))
text("1", x=.35,y=.5,cex=1.3, srt=90)
lines(x=c(.45,.5), y=c(.5,.5))
text("0", x=.35,y=.13,cex=1.3, srt=90)
lines(x=c(.45,.5), y=c(.14,.14))
text("3", x=.35,y=.86,cex=1.3, srt=90)
lines(x=c(.45,.5), y=c(.86,.86))
text("sd preference; bee sp. 1", x=.1, y=.5, cex=1.2, srt=90)

screen(8)
text("mean preference; bee sp. 2",x=.7,y=.5, srt=90)
lines(x=c(.5,.5),y=c(0,1))
text("1", x=.35,y=.5,cex=1.3, srt=90)
lines(x=c(.45,.5), y=c(.5,.5))
text("0", x=.35,y=.13,cex=1.3, srt=90)
lines(x=c(.45,.5), y=c(.14,.14))
text("3", x=.35,y=.86,cex=1.3, srt=90)
lines(x=c(.45,.5), y=c(.86,.86))
text("sd preference; bee sp. 1", x=.1, y=.5, cex=1.2, srt=90)


for(i1 in 1:length(rA)){
  for(i3 in 1:length(bee_logsd2)){
    for(i2 in 1:length(bee_logsd1)){
      scrnum <- scrnum+1
      screen(scrnum)
      mymatrix <- OCarray7[i2,,,i3,which(rare_advantage==rA[i1])]
      par(mar=c(1,1,1,1))
      image(x=Y, y=X, z=t(mymatrix[pos_x,pos_y]), zlim = c(0,10), col = col, xaxt='n', yaxt='n')
      if(scrnum %in% (c(1,2,3,10,11,12,19,20,21,28,29,30)+ps1)){axis(side=1,at=c(-10,0,10))}
      if(scrnum %in% (c(1,4,7,10,13,16,19,22,25,28,31,34)+ps1)){axis(side=2, at=c(0,6,12))}
    }
  }
}


dev.off()

#LRBT
splits <- rbind(c(.1,.4,.6,.7),
                c(.6,.9,.6,.7),
                c(.1,.4,.1,.2),
                c(.6,.9,.1,.2),
                
                c(0,.1,.7,1),
                c(0,.1,.2,.5),
                c(.5,.6,.7,1),
                c(.5,.6,.2,.5),
                
                
                
                c(.1,.2,.7,.8),
                c(.2,.3,.7,.8),
                c(.3,.4,.7,.8),
                c(.1,.2,.8,.9),
                c(.2,.3,.8,.9),
                c(.3,.4,.8,.9),
                c(.1,.2,.9,1),
                c(.2,.3,.9,1),
                c(.3,.4,.9,1),
                
                c(.6,.7,.7,.8),
                c(.7,.8,.7,.8),
                c(.8,.9,.7,.8),
                c(.6,.7,.8,.9),
                c(.7,.8,.8,.9),
                c(.8,.9,.8,.9),
                c(.6,.7,.9,1),
                c(.7,.8,.9,1),
                c(.8,.9,.9,1),
                                
                c(.1,.2,.2,.3),
                c(.2,.3,.2,.3),
                c(.3,.4,.2,.3),
                c(.1,.2,.3,.4),
                c(.2,.3,.3,.4),
                c(.3,.4,.3,.4),
                c(.1,.2,.4,.5),
                c(.2,.3,.4,.5),
                c(.3,.4,.4,.5),
                
                c(.6,.7,.2,.3),
                c(.7,.8,.2,.3),
                c(.8,.9,.2,.3),
                c(.6,.7,.3,.4),
                c(.7,.8,.3,.4),
                c(.8,.9,.3,.4),
                c(.6,.7,.4,.5),
                c(.7,.8,.4,.5),
                c(.8,.9,.4,.5)
                

              
                )











