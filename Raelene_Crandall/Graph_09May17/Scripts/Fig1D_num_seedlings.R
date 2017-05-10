# install.packages("data.table")
library(data.table)
library(ggplot2)

# ==================================
# Read & prepare data
# ==================================
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

myDT.gr.all[, prob.surv := alive/total]

# ==================================
# Bootstrap CIs
# ==================================
bootstrap.CI.percentile <- function(x, N = 1000, CI = 0.95){
    stopifnot(is.vector(x), N > 0, is.numeric(CI), CI > 0, CI < 1)
    rep_avg <- replicate(N, mean(sample(x, size = length(x), replace = TRUE), na.rm = TRUE))
    rep_avg <- sort(rep_avg, na.last = NA)
    low.prc <- (1 - CI)/2
    up.prc  <- 1 - (1 - CI)/2
    low.CI  <- rep_avg[round(low.prc * N)]
    names(low.CI) <- "low.CI"
    up.CI   <- rep_avg[round(up.prc * N)]
    names(up.CI) <- "up.CI"
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

# ==================================
# Plot barplot with ggplot
# ==================================
ggplot(data = myDT.gr.all, 
       aes(x    = Trt, 
           y    = prob.surv, 
           fill = Burn)) +
    
    # add the bars (means)
    geom_bar(stat="identity", position=position_dodge()) +
    # fill the bars manually
    scale_fill_manual(name   = "Burned",
                      breaks = c("N", "Y"),
                      values = c("N" = "gray70", 
                                 "Y" = "gray40"),
                      labels = c("Unburned", "Burned")) + 
    
    # plot CIs (add a point shape as well)
    geom_errorbar(aes(ymax=up.CI, ymin=low.CI), size=.4, width=.15, linetype="solid", position=position_dodge(.9)) +
    geom_point(position=position_dodge(width=.9), shape="-", show.legend = FALSE) +
    
    # set order of discrete values on OX axes & adjust the distance (gap) from OY axes
    scale_x_discrete(limits = c("Control", "Comp", "Herb"),
                     labels = c("Control", "Competitor \n Removal", "Herbivore \n Removal"),
                     expand = c(0, .5)) +
    # set range on OY axes and adjust the distance (gap) from OX axes
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    
    # Final adjustments:
    # set axis labels
    labs(x = "", 
         y = "Probability of Survival") +
    theme_bw() + # eliminate default background 
    theme(panel.grid.major = element_blank(), # eliminate major grids
          panel.grid.minor = element_blank(), # eliminate minor grids
          # set font family for all text within the plot ("serif" should work as "Times New Roman")
          # note that this can be overridden with other adjustment functions below
          text = element_text(family="serif"),
          # adjust X-axis title
          axis.title.x = element_text(size = 10, face = "bold"),
          # adjust X-axis labels
          axis.text.x = element_text(size = 10, face = "bold", color="black"),
          # adjust Y-axis title
          axis.title.y = element_text(size = 10, face = "bold"),
          # adjust legend title appearance
          legend.title = element_text(size = 8, face = "bold"),
          # adjust legend label appearance
          legend.text = element_text(size = 8, face = "plain"),
          # change spacing between legend items
          legend.key.height = unit(4, "mm"),
          # don't draw legend box (check element_rect() for borders and backgrounds)
          legend.background = element_blank(),
          # Put upper-left corner of legend box in upper-left corner of graph
          # Note that the numeric position in legend.position below is relative to the entire area, 
          # including titles and labels, not just the plotting area
          legend.justification = c(0,1),
          legend.position = c(0,1))

# save as pdf
ggsave("barplot with CI bars - ggplot.pdf", width=12, height=8, units="cm")

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
