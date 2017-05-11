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

# Create a column to dictate order of bar plotting in ggplot
myDT.gr[, TrtBurn := paste(Trt,Burn,sep="_")]
myDT.gr

# ==================================
# Bootstrap CIs
# ==================================
# Below is a function that can compute a bootstrapping percentile CI-s (95%) for a given sample (vector) x.
# It samples 1000 times with replacements, each times computes an average of the new sample;
# Sorts the 1000 averages and takes the 25th and 975th positions
boot.CI.percentile <- function(x, seed = 2017){
    set.seed(seed)
    rep_avg <- replicate(1000, mean(sample(x, size = length(x), replace = TRUE), na.rm = TRUE))
    rep_avg <- sort(rep_avg, na.last = NA)
    low.CI  <- rep_avg[25]  
    up.CI   <- rep_avg[975] 
    CIs <- list(low.CI, up.CI)
    names(CIs) <- c("low.CI", "up.CI")
    return(CIs)
}

# Compute percentile bootstrap CI
CIs <- list(
    myDT.noNA[Trt %like% "Comp|All"    & Burn %like% "N|All", boot.CI.percentile(Survival2)][, TrtBurn := "Comp_N"][],
    myDT.noNA[Trt %like% "Comp|All"    & Burn %like% "Y|All", boot.CI.percentile(Survival2)][, TrtBurn := "Comp_Y"][],
    myDT.noNA[Trt %like% "Control|All" & Burn %like% "N|All", boot.CI.percentile(Survival2)][, TrtBurn := "Control_N"][],
    myDT.noNA[Trt %like% "Control|All" & Burn %like% "Y|All", boot.CI.percentile(Survival2)][, TrtBurn := "Control_Y"][],
    myDT.noNA[Trt %like% "Herb|All"    & Burn %like% "N|All", boot.CI.percentile(Survival2)][, TrtBurn := "Herb_N"][],
    myDT.noNA[Trt %like% "Herb|All"    & Burn %like% "Y|All", boot.CI.percentile(Survival2)][, TrtBurn := "Herb_Y"][]
)
# Rbind the list of data tables from above in one big data table
CIs <- rbindlist(CIs)

# Merge the CI results to the main aggregation table
myDT.gr.CI <- merge(myDT.gr, CIs, by = "TrtBurn")

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
                      breaks = c("N", "Y"),
                      values = c("N" = "gray70", 
                                 "Y" = "gray40"),
                      labels = c("Unburned", "Burned")) + 
    # set order of discrete values on OX axes & adjust the distance (gap) from OY axes
    scale_x_discrete(limits = c("Control_N", "Comp_N", "Herb_N", "Control_Y", "Comp_Y", "Herb_Y"),
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
         y = "Probability of survival") +
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
write.csv(myDT.gr.CI, "Output/summary.csv", row.names = FALSE)
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
