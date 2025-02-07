# Libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(stats)

# Data Loading and Preprocessing
data_path <- " est16us.csv" 
raw_data <- read.csv(data_path)

# Clean and prepare data focusing on poverty percentages
cleaned_data <- raw_data %>%
filter(!is.na(Geographic.Information),
Geographic.Information != "State FIPS Code",
Geographic.Information != "United States") %>%
select(
State = Geographic.Information,
Name = Unnamed..2,
Poverty_Percent = Unnamed..12,
Percent_CI_Lower = Unnamed..13,
Percent_CI_Upper = Unnamed..14
) %>%
mutate(
Poverty_Percent = as.numeric(gsub("[^0-9.]", "", Poverty_Percent)),
Percent_CI_Lower = as.numeric(gsub("[^0-9.]", "", Percent_CI_Lower)),
Percent_CI_Upper = as.numeric(gsub("[^0-9.]", "", Percent_CI_Upper))
)

# Create histogram with normal curve overlay
histogram_plot <- ggplot(cleaned_data, aes(x = Poverty_Percent)) +
geom_histogram(aes(y = ..count..),
bins = 15,
fill = "skyblue",
color = "black",
alpha = 0.7) +
stat_function(fun = dnorm,
args = list(mean = mean(cleaned_data$Poverty_Percent, na.rm = TRUE),
sd = sd(cleaned_data$Poverty_Percent, na.rm = TRUE)),
color = "red",
size = 1) +
labs(title = "Distribution of Child Poverty Rates Across States",
x = "Poverty Rate (%)",
y = "Frequency",
subtitle = "With Normal Distribution Curve Overlay") +
theme_minimal() +
theme(
plot.title = element_text(size = 14, face = "bold"),
plot.subtitle = element_text(size = 12),
axis.title = element_text(size = 12)
)

# Filter for CA and TX comparison
comparison_data <- cleaned_data %>%
filter(Name %in% c("California", "Texas"))

# Create comparison bar plot
comparison_plot <- ggplot(comparison_data,
aes(x = Name, y = Poverty_Percent, fill = Name)) +
geom_bar(stat = "identity", width = 0.7) +
geom_errorbar(
aes(ymin = Percent_CI_Lower, ymax = Percent_CI_Upper),
width = 0.2,
size = 1
) +
scale_fill_manual(values = c("California" = "#FF6B6B", "Texas" = "#4ECDC4")) +
labs(
title = "Child Poverty Rates Comparison",
subtitle = "California vs Texas (with 90% Confidence Intervals)",
x = "State",
y = "Poverty Rate (%)"
) +
theme_minimal() +
theme(
plot.title = element_text(size = 14, face = "bold"),
plot.subtitle = element_text(size = 12),
axis.title = element_text(size = 12),
legend.position = "none"
)

# Statistical Analysis for comparison
# Calculate Z-score for difference between proportions
calculate_z_score <- function(p1, p2, se1, se2) {
pooled_se <- sqrt(se1^2 + se2^2)
z_score <- (p1 - p2) / pooled_se
p_value <- 2 * (1 - pnorm(abs(z_score)))
return(list(z_score = z_score, p_value = p_value))
}

# Extract data for comparison
ca_data <- comparison_data %>% filter(Name == "California")
tx_data <- comparison_data %>% filter(Name == "Texas")

# Calculate standard errors
ca_se <- (ca_data$Percent_CI_Upper - ca_data$Percent_CI_Lower) / (2 * 1.645)
tx_se <- (tx_data$Percent_CI_Upper - tx_data$Percent_CI_Lower) / (2 * 1.645)

# Perform statistical test
test_results <- calculate_z_score(
ca_data$Poverty_Percent,
tx_data$Poverty_Percent,
ca_se,
tx_se
)

# Print all results
print("\nShapiro-Wilk Normality Test Results:")
print(shapiro.test(cleaned_data$Poverty_Percent))

print("\nSummary Statistics of Poverty Rates:")
print(summary(cleaned_data$Poverty_Percent))

print("\nComparison Analysis Results:")
cat("California Poverty Rate:", ca_data$Poverty_Percent, "% (",
ca_data$Percent_CI_Lower, "% - ", ca_data$Percent_CI_Upper, "%)\n")
cat("Texas Poverty Rate:", tx_data$Poverty_Percent, "% (",
tx_data$Percent_CI_Lower, "% - ", tx_data$Percent_CI_Upper, "%)\n")
cat("Z-score:", test_results$z_score, "\n")
cat("P-value:", test_results$p_value, "\n")

# Save all plots
ggsave("poverty_histogram.png", plot = histogram_plot, width = 10, height = 6, dpi = 300)
ggsave("poverty_comparison.png", plot = comparison_plot, width = 10, height = 6, dpi = 300)

# Create combined plot
combined_plot <- grid.arrange(histogram_plot, comparison_plot,
ncol = 2,
top = "Child Poverty Analysis")
ggsave("combined_plots.png", plot = combined_plot, width = 15, height = 6, dpi = 300)
