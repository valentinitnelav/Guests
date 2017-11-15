###############################################################################
# Script to make barplot with counts or percent
###############################################################################

# install.packages("ggplot2")
library(ggplot2)
# install.packages("data.table")
library(data.table)
# install.packages("scales")
library(scales)

# =============================================================================
# Read & prepare data
# =============================================================================
mytbl <- fread("data/barplot_chisq/forvalentine.csv")

# =============================================================================
# barplot of counts
# =============================================================================
plot_counts <- 
  ggplot(data = mytbl) +
  geom_bar(aes(x = Brush,
               y = Total,
               fill = Deer),
           stat = "identity") + # fill with color using Deer variable
  # set range on OY axes and adjust the distance (gap) from OX axes
  scale_y_continuous(limits = c(0, 120), # means from 0 up to 120 on OY axis
                     # or instead of 120 above use something more general like:
                     # round(max(colSums(table(mytbl_exploded[,.(Deer,Brush)])))/10)*10
                     expand = c(0, 0)) + # adjust the distance (gap) from OX axes
  # set axis labels
  labs(x = "", 
       y = "counts") +
  theme_bw() + # eliminate default background 
  theme(panel.grid = element_blank())

# save as pdf
ggsave(plot_counts, file = "output/plot_counts.pdf", width=10, height=8, units="cm")
# save as png
ggsave(plot_counts, file = "output/plot_counts.png", width=10, height=8, units="cm", dpi=300)

# =============================================================================
# barplot of percent as chunk of all data
# =============================================================================
plot_percent_all <- 
  ggplot(data = mytbl) +
  geom_bar(aes(x = Brush,
               y = Total / sum(Total), # computes percent
               fill = Deer),
           stat = "identity") +
  # set range on OY axes and adjust the distance (gap) from OX axes
  scale_y_continuous(labels = percent_format(),
                     limits = c(0, 1),
                     expand = c(0, 0)) + # adjust the distance (gap) from OX axes
  # set axis labels
  labs(x = "", 
       y = "Percent of total") +
  theme_bw() + # eliminate default background 
  theme(panel.grid = element_blank())

# save as pdf
ggsave(plot_percent_all, file = "output/plot_percent_of_all_data.pdf", width=10, height=8, units="cm")
# save as png
ggsave(plot_percent_all, file = "output/plot_percent_of_all_data.png", width=10, height=8, units="cm", dpi=300)


# =============================================================================
# barplot of percent as chunk of each Brush category
# =============================================================================

# -----------------------------------------------------------------------------
# Using ggplot2
# -----------------------------------------------------------------------------
# ref: https://stackoverflow.com/questions/9563368/create-stacked-barplot-where-each-stack-is-scaled-to-sum-to-100

plot_percent_brush <-
  ggplot(data = mytbl) + 
  geom_bar(aes(x = Brush, 
               y = Total,
               fill = Deer),
           position = position_fill(), # or position = "fill"
           stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  # set axis labels
  labs(x = "", 
       y = "Percent of each category") +
  theme_bw() + # eliminate default background 
  theme(panel.grid = element_blank())

# save as pdf
ggsave(plot_percent_brush, file = "output/plot_percent_brush.pdf", width=10, height=8, units="cm")
# save as png
ggsave(plot_percent_brush, file = "output/plot_percent_brush.png", width=10, height=8, units="cm", dpi=300)


# -----------------------------------------------------------------------------
# Using basic plot functions
# -----------------------------------------------------------------------------

# prepare data in needed format
mytbl_m <- matrix(mytbl$Total, nrow = 2, byrow = TRUE)
rownames(mytbl_m) <- c("absent", "present")
colnames(mytbl_m) <- c("low", "high")
mytbl_m
mytbl_m_prop <- prop.table(mytbl_m, 2)

par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)

barplot(mytbl_m_prop, col = c("#F8766D", "#00BFC4"))
legend("topright", 
       fill = c("#F8766D", "#00BFC4"),
       legend = c("absent", "present"),
       inset = c(-0.15,0))

# reset par to the default values
dev.off() 

# check otehr examples at
# http://www.thecoatlessprofessor.com/programming/creating-stacked-barplot-and-grouped-barplot-in-r-using-base-graphics-no-ggplot2/
