# This code graphs love plots of different crime effect influences per land use stratum
# This code uses the dataset from matches without business data.

library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)

# Load the data
all_tests = read.csv("../neat_data/analysis_results/all_pairs_tests_new.csv")
segment_tests = read.csv("../neat_data/analysis_results/land_use_strata_tests_new.csv")

segment_tests$is_signif = segment_tests$p <= 0.05

# Determine if the data is significant
types = c("total", "violent", "nonviolent")

for (crime_type in types) {
  for (crime_radius in c(100, 200, 500)) {
    # Get the relevant scores for the crimes
    sub_tests = subset(segment_tests, type == crime_type & radius == crime_radius)
    all_relevant_scores = subset(all_tests, type == crime_type & radius == crime_radius)
    
    # Plot the stratum scores
    p <- ggplot(sub_tests, aes(est, stratum))
    p <- p + geom_point(aes(colour = factor(percentile), shape = factor(is_signif), size=5)) + guides(size=FALSE)+
      scale_color_manual(values=c('#aaaaaa','#111111'))+
      scale_shape_manual(values=c(1, 16))
    
    # Mark the line for the basic score
    p <- p + geom_vline(xintercept = all_relevant_scores$est)
    p <- p + ggtitle(paste("Land use effects for", crime_type, "crimes with radius", crime_radius))
    p
    
    filename <- paste("../neat_img/land_use_stratum_", crime_type, "_", crime_radius, ".png", sep="")
    
    ggsave(filename, width = 6, height = 4)
  }
}



