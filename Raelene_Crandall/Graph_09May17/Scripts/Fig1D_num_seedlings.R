# install.packages("data.table")
library(data.table)
library(ggplot2)

# ==================================
# Read & prepare data
# ==================================
# read data & make data.table object
myDT <- fread("Data/Ailanthus year 2 post-fire.csv")
str(myDT) # check structure

# remove all rows with NA-s in Survival2 column
myDT.noNA <- myDT[!is.na(Survival2)]
# aggregate by Trt & Burn columns + compute alive & total columns
myDT.gr <- myDT.noNA[,
                     .(alive = sum(Survival2, na.rm=TRUE),
                       total = .N # .N is a key word in data.table syntax, 
                       # it counts number of rows for each group combination defined in "by":  by = .(Trt, Burn)
                     ),
                     by = .(Trt, Burn)]

# remove rows with "All" from aggregation table
myDT.gr <- myDT.gr[Trt != "All"]
# compute Probability of Survival column
myDT.gr[, prob.surv := alive/total]

# ==================================
# Bootstrap CIs
# ==================================
# Below is a function that can compute a bootstrapping percentile CI-s for a given sample (vector) x.
# Number of iterations (N) and CI% can be also customized.
# It samples N times with replacements, each times computes an average of the new sample;
# Sorts the N averages and takes the positions corresponding to given CI%
bootstrap.CI.percentile <- function(x, N = 1000, CI = 0.95, seed = 2017){
    stopifnot(is.vector(x), N > 0, is.numeric(CI), CI > 0, CI < 1)
    set.seed(seed)
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
y <- myDT.noNA[Trt %like% "Comp|All" & Burn %like% "N|All", Survival2]
y <- myDT.noNA[Trt %like% "Comp|All" & Burn %like% "Y|All", Survival2]
y <- myDT.noNA[Trt %like% "Control|All" & Burn %like% "N|All", Survival2]
y <- myDT.noNA[Trt %like% "Control|All" & Burn %like% "Y|All", Survival2]
y <- myDT.noNA[Trt %like% "Herb|All" & Burn %like% "N|All", Survival2]
y <- myDT.noNA[Trt %like% "Herb|All" & Burn %like% "Y|All", Survival2]
bootstrap.CI.percentile(x = y, seed = 2017)
bootstrap.CI.empirical(x = y, seed = 2017)

set.seed(2017)
sort(replicate(1000, mean(sample(y, length(y), replace = TRUE))))[25]
set.seed(2017)
sort(replicate(1000, mean(sample(y, length(y), replace = TRUE))))[975]

# For testing resons: the above gives same results as in boot::boot.ci function
# library(boot)

set.seed(2017)
b <- boot(y, function(u,i) mean(u[i]), R = 999)
set.seed(2017)
boot.ci(b, type = "all")
rm(b,y)
identical(y,x)
# Compute percentile bootstrap CI 
set.seed(2017)
CIs <- lapply(unique(myDT.gr[,Trt]), function(my.trt){
    trt.Y <- bootstrap.CI.percentile(x=myDT.noNA[Trt %like% paste0(my.trt,"|All") & Burn %like% "Y|All", Survival2])
    trt.N <- bootstrap.CI.percentile(x=myDT.noNA[Trt %like% paste0(my.trt,"|All") & Burn %like% "N|All", Survival2])
    res   <- data.table(rbind(trt.Y, trt.N), 
                        Trt  = my.trt, 
                        Burn = c("Y","N"))
    return(res)
})
# Rbind the list of data tables from above in one big data table
CIs <- rbindlist(CIs)

# Merge the CI results to the main aggregation table
myDT.gr.CI <- merge(myDT.gr, CIs, by = c("Trt", "Burn"))

# Create a column to dictate order of bar plotting in ggplot
myDT.gr.CI[, TrtBurn := paste(Trt,Burn,sep="_")]

# ==================================
# Plot barplot with ggplot
# ==================================
ggplot(data = myDT.gr.CI, 
       aes(x    = TrtBurn, 
           y    = prob.surv,
           fill = Burn)) +
    
    # add the bars (means)
    geom_col(position = position_dodge(.9), 
             width    = .7) +
    # fill the bars manually
    scale_fill_manual(name   = "",
                      breaks = c("Y", "N"),
                      values = c("Y" = "gray70", 
                                 "N" = "gray40"),
                      labels = c("Burned", "Unburned")) + 
    # set order of discrete values on OX axes & adjust the distance (gap) from OY axes
    scale_x_discrete(limits = c("Control_Y", "Comp_Y", "Herb_Y", "Control_N", "Comp_N", "Herb_N"),
                     labels = rep(c("Control", "Competitor\nRemoval", "Herbivore\nRemoval"), times=2),
                     expand = c(0, .5)) +
    # set range on OY axes and adjust the distance (gap) from OX axes
    scale_y_continuous(limits = c(0, 1), 
                       expand = c(0, 0)) +
    geom_text(aes(label = total, 
                  y     = prob.surv + 0.05),
              # position  = position_dodge(.9),
              vjust     = "right",
              hjust     = "right",
              nudge_x   = -.05,
              size      = 2,
              show.legend = FALSE) +
    # plot CI bars
    geom_errorbar(aes(ymax = up.CI, 
                      ymin = low.CI), 
                  size     = .4, 
                  width    = .15, 
                  linetype = "solid", 
                  position = position_dodge(.9),
                  show.legend = FALSE) +
    # add a small indicator shape at the middle of CI bars
    geom_point(position = position_dodge(.9), 
               shape    = "-", 
               show.legend = FALSE) +
    # Add annotation - letter D
    annotate(geom  = "text", 
             x     = 0.7, 
             y     = 0.97, 
             label = "D",
             size  = 3,
             fontface = 2) +
    
    # Final adjustments:
    # set axis labels
    labs(x = "", 
         y = "Probability of Survival") +
    theme_bw() + # eliminate default background 
    theme(panel.grid.major = element_blank(), # eliminate major grids
          panel.grid.minor = element_blank(), # eliminate minor grids
          # set font family for all text within the plot ("serif" should work as "Times New Roman")
          # note that this can be overridden with other adjustment functions below
          # text = element_text(family="serif"),
          # adjust X-axis title
          # axis.title.x = element_text(size = 10, face = "bold"),
          # adjust X-axis labels
          axis.text.x  = element_text(size = 8, face = "plain", color="black"),
          # adjust Y-axis title
          axis.title.y = element_text(size = 10, face = "bold"),
          # adjust Y-axis labels
          axis.text.y  = element_text(size = 8, face = "plain", color="black"),
          # adjust legend title appearance
          # legend.title = element_text(size = 10, face = "bold"),
          # adjust legend label appearance
          legend.text  = element_text(size = 8, face = "plain"),
          # change spacing between legend items
          legend.key.height = unit(5, "mm"),
          # don't draw legend box (check element_rect() for borders and backgrounds)
          legend.background = element_blank(),

          legend.justification = c(1, 1),
          legend.position = c(1, 1.05))

# save as pdf
ggsave("Output/Fig1D - barplot with CI bars - ggplot.pdf", width=12, height=10, units="cm")

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
