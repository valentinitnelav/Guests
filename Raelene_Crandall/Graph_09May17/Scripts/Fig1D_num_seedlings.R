# install.packages("data.table")
library(data.table)

myDT <- fread("Data/Ailanthus year 2 post-fire.csv")
str(myDT)
myDT.noNA <- myDT[!is.na(Survival2)]

myDT.gr <- myDT.noNA[,
                     .(alive = sum(Survival2, na.rm=TRUE),
                       total = .N
                     ),
                     by = .(Trt, Burn)]
myDT.all <- myDT[,.(all=.N),by = .(Trt, Burn)]
myDT.gr.all <- cbind(myDT.gr, myDT.all[,.(all)])
myDT.gr.all <- myDT.gr.all[Trt != "All"]


bootstrap.CI.percentile <- function(x, N = 1000, CI = 0.95){
    stopifnot(is.vector(x), N > 0, is.numeric(CI), CI > 0, CI < 1)
    rep_avg <- replicate(N, mean(sample(x, size = length(x), replace = TRUE), na.rm = TRUE))
    rep_avg <- sort(rep_avg, na.last = NA)
    low.prc <- (1 - CI)/2
    up.prc  <- 1 - (1 - CI)/2
    low.CI  <- rep_avg[round(low.prc * N)]
    names(low.CI) <- paste0("low.CI:", low.prc * 100, "%")
    up.CI   <- rep_avg[round(up.prc * N)]
    names(up.CI) <- paste0("up.CI:", up.prc * 100, "%")
    return(c(low.CI, up.CI))
}
# Example
set.seed(2017)
bootstrap.CI.percentile(x = myDT.noNA[Trt %like% "Herb|All" & Burn %like% "Y|All", Survival2])

# For testing resons: the above gives same results as in boot::boot.ci function
library(boot)
x <- myDT.noNA[Trt %like% "Herb|All" & Burn %like% "N|All", Survival2]
set.seed(2017)
b <- boot(x, function(u,i) mean(u[i]), R = 999)
boot.ci(b, type = "perc")
rm(b,x)


set.seed(2017)
CIs <- lapply(unique(myDT.gr.all[,Trt]), function(trt){
    trt.Y <- bootstrap.CI.percentile(x=myDT.noNA[Trt %like% paste0(trt,"|All") & Burn %like% "Y|All", Survival2])
    trt.N <- bootstrap.CI.percentile(x=myDT.noNA[Trt %like% paste0(trt,"|All") & Burn %like% "N|All", Survival2])
    res   <- data.table(rbind(trt.Y, trt.N), 
                        Trt  = trt, 
                        Burn = c("Y","N"))
    return(res)
})
CIs <- rbindlist(CIs)

myDT.gr.all <- merge(myDT.gr.all, CIs, by = c("Trt", "Burn"))
myDT.gr.all



# ===========================================
# some part of the original code from Rae
# ===========================================
# require(sciplot)
# lineplot.CI(SDLChange$Trt_Burn, SDLChange$Change,cex=1.5, type='p',
#             ylim=c(-2,8), xlab='', ylab='', axes=FALSE, xaxt="n", font.lab=2, col=c(rep("black",3), rep("gray60", 3)))
# title(ylab = 'Number of seedlings (2013-2012)', font.lab = 2, line = 2.5)
# axis(2, at=c(-2, 0, 2, 4, 6, 8), las=1, cex.axis=0.7, tck=-0.03)
# mtext(c("Control","Competitor", "Herbivore", "Control","Competitor", "Herbivore"),
#       side=1,line=0.1,at=c(1,2,3,4,5,6), cex=0.7, font=1)
# mtext(c("Removal","Removal", "Removal"),
#       side=1,line=0.7,at=c(2,3,5,6), cex=0.7, font=1)
# mtext(c("Unburned","Burned"),
#       side=1,line=1.6,at=c(2,5), cex=0.8, font=2)
# box(lwd=1)
# text(x=0.92, y=7.95, "D",font=2, cex=1.1, las=1)
