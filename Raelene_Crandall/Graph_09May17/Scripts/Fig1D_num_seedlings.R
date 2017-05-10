
# ===========================================
# some part of the original code from Rae
# ===========================================
require(sciplot)
lineplot.CI(SDLChange$Trt_Burn, SDLChange$Change,cex=1.5, type='p',
            ylim=c(-2,8), xlab='', ylab='', axes=FALSE, xaxt="n", font.lab=2, col=c(rep("black",3), rep("gray60", 3)))
title(ylab = 'Number of seedlings (2013-2012)', font.lab = 2, line = 2.5)
axis(2, at=c(-2, 0, 2, 4, 6, 8), las=1, cex.axis=0.7, tck=-0.03)
mtext(c("Control","Competitor", "Herbivore", "Control","Competitor", "Herbivore"),
      side=1,line=0.1,at=c(1,2,3,4,5,6), cex=0.7, font=1)
mtext(c("Removal","Removal", "Removal"),
      side=1,line=0.7,at=c(2,3,5,6), cex=0.7, font=1)
mtext(c("Unburned","Burned"),
      side=1,line=1.6,at=c(2,5), cex=0.8, font=2)
box(lwd=1)
text(x=0.92, y=7.95, "D",font=2, cex=1.1, las=1)
