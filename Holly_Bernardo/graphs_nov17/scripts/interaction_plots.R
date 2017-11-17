###############################################################################
# Script to make Interaction plots
###############################################################################

# install.packages("data.table")
library(data.table)
# install.packages("ggplot2")
library(ggplot2)

# =============================================================================
# Read & prepare data
# =============================================================================

all_data <- fread("data/interaction_plots/ALL_ThreatData.csv")

# Check values
sort(unique(all_data$deer_cate)) # there are empty cells
# Remove the extra row with many empty cells
all_data <- all_data[species != ""] 
sort(unique(all_data$deer_cate))

# Keep only columns (variables) of interest
all_data <- all_data[, .(species, 
                         r.t., 
                         maxbrush_cate, 
                         deer_cate, 
                         maxtrails_cate, 
                         burnyear_cate)]

# Transform variables to ordered factors. The order trickles down to the plot legends.
# Therefore, adjust below in the 'level' argument the desired order.
all_data[, ':=' ( maxbrush_cate  = factor(maxbrush_cate, 
                                          levels  = c("low","high"), 
                                          ordered = TRUE),
                  deer_cate      = factor(deer_cate,
                                          levels  = c("absent","present"), 
                                          ordered = TRUE),
                  maxtrails_cate = factor(maxtrails_cate,
                                          levels  = c("absent","present"), 
                                          ordered = TRUE),
                  burnyear_cate  = factor(burnyear_cate,
                                          levels  = c("burn", "good", "bad"), 
                                          ordered = TRUE) )]

# =============================================================================
# Interaction plot: brush * deer
# =============================================================================

# -----------------------------------------------------------------------------
# Compute averages, SD, No# obs & SE
# -----------------------------------------------------------------------------
# Use data.table syntax for aggregation
avg_brush_deer <- all_data[, .(rate_avg = mean(r.t., na.rm = TRUE), # get means
                               rate_sd  = sd(r.t., na.rm = TRUE),   # get SD-s
                               N_obs    = .N),                      # get number of observations
                           by = .(maxbrush_cate, deer_cate)] # while grouping by maxbrush_cate & deer_cate

# # If you need grouping  for a certain species, then add the line species == "my_sp_name" like below
# # This is valid for all plot cases that fallow below
# avg_brush_deer <- all_data[species == "Asclepias exaltata", # select certain species
#                            .(rate_avg = mean(r.t., na.rm = TRUE), # get means
#                              rate_sd  = sd(r.t., na.rm = TRUE),   # get SD-s
#                              N_obs    = .N),                      # get number of observations
#                            by = .(maxbrush_cate, deer_cate)]

avg_brush_deer[, rate_se := rate_sd/sqrt(N_obs)] # compute SE-s as SD/sqrt(N)
avg_brush_deer # check results
# remove rows with NA-s in grouping variables
avg_brush_deer <- avg_brush_deer[complete.cases(avg_brush_deer[,.(maxbrush_cate, deer_cate)])]
avg_brush_deer # check results

# -----------------------------------------------------------------------------
# Plot
# -----------------------------------------------------------------------------
pd <- position_dodge(width = 0.05) # this trickles down to all next plots

interaction_plot_brush_deer <- 
  ggplot(data = avg_brush_deer,
         aes(x = maxbrush_cate,
             y = rate_avg,
             group = deer_cate,
             linetype = deer_cate)) +
  # Add averages as points
  geom_point(position = pd,
             size = 1.5) +
  # Connect the average points with lines
  geom_line(position = pd, 
            lwd = 0.5) +
  # set type of line, see more types at : http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
  scale_linetype_manual(name = 'Deer', # this sets also the legend title
                        values = c("absent" = "dashed", 
                                   "present" = "solid")) +
  # Plot CIs
  geom_errorbar(aes(ymax = rate_avg + rate_se, 
                    ymin = rate_avg - rate_se), 
                size  = 0.2, 
                width = 0.15, 
                linetype = "solid",
                position = pd) +
  # Set axis labels
  labs(x = "Brush", 
       y = "Growth rate") +
  # Adjust the distance (gap) from OY axes
  scale_x_discrete(expand = c(0, .15)) +
  # Set the final aspect
  # Check more examples here: http://ggplot2.tidyverse.org/reference/ggtheme.html
  theme_classic()

# save plot
ggsave(plot = interaction_plot_brush_deer, 
       file = 'output/interaction_plots/interaction_plot_brush_deer.pdf', 
       height = 7, 
       width  = 9, 
       units  = "cm")

# =============================================================================
# Interaction plot: brush * burn
# =============================================================================

# -----------------------------------------------------------------------------
# Compute averages, SD, No# obs & SE
# -----------------------------------------------------------------------------
# Use data.table syntax for aggregation
avg_brush_burn <- all_data[, .(rate_avg = mean(r.t., na.rm = TRUE), # get means
                               rate_sd  = sd(r.t., na.rm = TRUE),   # get SD-s
                               N_obs    = .N),                      # get number of observations
                           by = .(maxbrush_cate, burnyear_cate)] # while grouping by maxbrush_cate & burnyear_cate
avg_brush_burn[, rate_se := rate_sd/sqrt(N_obs)] # compute SE-s as SD/sqrt(N)
avg_brush_burn # check results
# remove rows with NA-s in grouping variables
avg_brush_burn <- avg_brush_burn[complete.cases(avg_brush_burn[,.(maxbrush_cate, burnyear_cate)])]
avg_brush_burn # check results

# -----------------------------------------------------------------------------
# Plot
# -----------------------------------------------------------------------------
interaction_plot_brush_burn <- 
  ggplot(data = avg_brush_burn,
         aes(x = maxbrush_cate,
             y = rate_avg,
             group = burnyear_cate,
             linetype = burnyear_cate)) +
  # Add averages as points
  geom_point(position = pd,
             size = 1.5) +
  # Connect the average points with lines
  geom_line(position = pd, 
            lwd = 0.5) +
  # set type of line, see more types at : http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
  scale_linetype_manual(name = 'Burn year', # this sets also the legend title
                        values = c("burn" = "dashed", 
                                   "good" = "dotted",
                                   "bad"  = "solid")) +
  # plot CIs
  geom_errorbar(aes(ymax = rate_avg + rate_se, 
                    ymin = rate_avg - rate_se), 
                size  = 0.2, 
                width = 0.15, 
                linetype = "solid",
                position = pd) +
  # set axis labels
  labs(x = "Brush", 
       y = "Growth rate") +
  # Adjust the distance (gap) from OY axes
  scale_x_discrete(expand = c(0, .15)) +
  # Set the final aspect
  # Check more examples here: http://ggplot2.tidyverse.org/reference/ggtheme.html
  theme_classic()

# save plot
ggsave(plot = interaction_plot_brush_burn, 
       file = 'output/interaction_plots/interaction_plot_brush_burn.pdf', 
       height = 7, 
       width  = 9, 
       units  = "cm")

# =============================================================================
# Interaction plot: brush * burn * deer
# =============================================================================

# -----------------------------------------------------------------------------
# Compute averages, SD, No# obs & SE
# -----------------------------------------------------------------------------
# Use data.table syntax for aggregation
avg_brush_burn_deer <- all_data[, .(rate_avg = mean(r.t., na.rm = TRUE), # get means
                                    rate_sd  = sd(r.t., na.rm = TRUE),   # get SD-s
                                    N_obs    = .N),                      # get number of observations
                                by = .(burnyear_cate, maxbrush_cate, deer_cate)] # while grouping
avg_brush_burn_deer[, rate_se := rate_sd/sqrt(N_obs)] # compute SE-s as SD/sqrt(N)
avg_brush_burn_deer # check results
# remove rows with NA-s in grouping variables
avg_brush_burn_deer <- avg_brush_burn_deer[complete.cases(avg_brush_burn_deer[,.(burnyear_cate,
                                                                                 maxbrush_cate,
                                                                                 deer_cate)])]
avg_brush_burn_deer # check results

# -----------------------------------------------------------------------------
# Plot
# -----------------------------------------------------------------------------

interaction_plot_brush_burn_deer <- 
  ggplot(data = avg_brush_burn_deer,
         aes(x = maxbrush_cate,
             y = rate_avg,
             group = deer_cate,
             linetype = deer_cate)) +
  # Add averages as points
  geom_point(position = pd,
             size = 1.5) +
  # Connect the average points with lines
  geom_line(position = pd, 
            lwd = 0.5) +
  # set type of line, see more types at : http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
  scale_linetype_manual(name = 'Deer', # this sets also the legend title
                        values = c("absent" = "dashed", 
                                   "present" = "solid")) +
  # plot CIs
  geom_errorbar(aes(ymax = rate_avg + rate_se, 
                    ymin = rate_avg - rate_se), 
                size  = 0.2, 
                width = 0.15, 
                linetype = "solid",
                position = pd) +
  # set axis labels
  labs(x = "Brush", 
       y = "Growth rate") +
  # Adjust the distance (gap) from OY axes
  scale_x_discrete(expand = c(0, .15)) +
  # Lay out panels in a grid; check more details at http://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(. ~ burnyear_cate) +
  # facet_grid(burnyear_cate ~ .) +
  # Set the final aspect
  # Check more examples here: http://ggplot2.tidyverse.org/reference/ggtheme.html
  theme_classic() +
  # edit strip text for each panel
  theme(strip.text = element_text(size = 8, face = "bold"),
        strip.background = element_blank()) # remove boxes around the strips


# save plot
ggsave(plot = interaction_plot_brush_burn_deer, 
       file = 'output/interaction_plots/interaction_plot_brush_burn_deer.pdf', 
       height = 7, 
       width  = 12, 
       units  = "cm")

# =============================================================================
# Interaction plot: brush * burn * trails
# =============================================================================

# -----------------------------------------------------------------------------
# Compute averages, SD, No# obs & SE
# -----------------------------------------------------------------------------
# Use data.table syntax for aggregation
avg_brush_burn_trail <- all_data[, .(rate_avg = mean(r.t., na.rm = TRUE), # get means
                                     rate_sd  = sd(r.t., na.rm = TRUE),   # get SD-s
                                     N_obs    = .N),                      # get number of observations
                                 by = .(burnyear_cate, maxbrush_cate, maxtrails_cate)] # while grouping
avg_brush_burn_trail[, rate_se := rate_sd/sqrt(N_obs)] # compute SE-s as SD/sqrt(N)
avg_brush_burn_trail # check results
# remove rows with NA-s in grouping variables
avg_brush_burn_trail <- avg_brush_burn_trail[complete.cases(avg_brush_burn_trail[,.(burnyear_cate,
                                                                                    maxbrush_cate,
                                                                                    maxtrails_cate)])]
avg_brush_burn_trail # check results

# -----------------------------------------------------------------------------
# Plot
# -----------------------------------------------------------------------------
interaction_plot_brush_burn_trail <- 
  ggplot(data = avg_brush_burn_trail,
         aes(x = maxbrush_cate,
             y = rate_avg,
             group = maxtrails_cate,
             linetype = maxtrails_cate)) +
  # Add averages as points
  geom_point(position = pd,
             size = 1.5) +
  # Connect the average points with lines
  geom_line(position = pd, 
            lwd = 0.5) +
  # set type of line, see more types at : http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
  scale_linetype_manual(name = 'Trail', # this sets also the legend title
                        values = c("absent" = "dashed", 
                                   "present" = "solid")) +
  # plot CIs
  geom_errorbar(aes(ymax = rate_avg + rate_se, 
                    ymin = rate_avg - rate_se), 
                size  = 0.2, 
                width = 0.15, 
                linetype = "solid",
                position = pd) +
  # set axis labels
  labs(x = "Brush", 
       y = "Growth rate") +
  # Adjust the distance (gap) from OY axes
  scale_x_discrete(expand = c(0, .15)) +
  # Lay out panels in a grid; check more details at http://ggplot2.tidyverse.org/reference/facet_grid.html
  facet_grid(. ~ burnyear_cate) +
  # facet_grid(burnyear_cate ~ .) +
  # Set the final aspect
  # Check more examples here: http://ggplot2.tidyverse.org/reference/ggtheme.html
  theme_classic() +
  # edit strip text for each panel
  theme(strip.text = element_text(size = 8, face = "bold"),
        strip.background = element_blank()) # remove boxes around the strips

# save plot
ggsave(plot = interaction_plot_brush_burn_trail, 
       file = 'output/interaction_plots/interaction_plot_brush_burn_trail.pdf', 
       height = 7, 
       width  = 12, 
       units  = "cm")