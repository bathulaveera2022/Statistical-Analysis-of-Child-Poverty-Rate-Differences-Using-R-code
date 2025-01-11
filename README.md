Table of Contents
1.	Introduction	1
1.1.	Problem statement and research motivation	1
1.2.	The data set	2
1.3.	Research question.	2
1.4.	Null hypothesis and alternative hypothesis (H0/H1)	2
2.	Background research	3
2.1.	Research papers	3
2.2.	Why RQ is of interest (research gap and future directions according to the literature)	3
3.	Visualisation	4
3.1.	Appropriate plot for the RQ output of an R script	4
3.2.	Additional information relating to understanding the data	4
3.3.	Useful information for the data understanding	5
4.	Analysis	5
4.1.	Statistical test used to test the hypotheses and output	5
4.2.	The null hypothesis is rejected/not rejected based on the p-value	5
5.	Conclusions	5
5.1.	Results explained	6
5.2.	Interpretation of the results	6
5.3.	Reasons and/or implications for future work, limitations of your study	6
6.	Reference list	6
7.	Appendices	7

















1.	Introduction
1.1.	Problem statement and research motivation

Millions of children are impacted by child poverty, which is still a major problem in the US. Our study was driven by the need to compare The incidence of child poverty in Texas and California, two states with different socioeconomic systems. Children's poverty levels can have a significant impact on their development and long-term prospects, as demonstrated by research like those conducted by Laird et al. (2017). Our goal in comparing these two states is to find important differences that could lead to better child poverty policy.


1.2.	The data set 
The "US Public Food Assistance 1 - WIC" (est16us.csv) dataset, which was obtained from Kaggle.com and is based on 2016 Census Bureau estimates, was utilized for investigation. Confidence intervals are included to guarantee statistical reliability, along with state-level statistics on child poverty rates for children ages 0-17. Geographic identifiers and state-specific data are also included in the dataset, which is crucial for this study's comparison of poverty rates in Texas and California.

 
Figure 1: Loading Dataset

1.3.	Research question. 

"Does Texas' mean poverty rate for children aged 0–17 differs from California's in a way that is statistically significant?"

We will examine the child poverty rates in Texas and California using data from the 2016 Census Bureau in order to answer this research question. The state (Texas vs. California) is the independent variable, while the child poverty rate is the dependent variable. By computing the confidence intervals for the poverty rates in the two states, we will conduct a statistical study. We can ascertain whether there is a significant difference by comparing these intervals. There will be a statistically significant difference between the child poverty rates in Texas and California if the two states' confidence intervals do not overlap.
1.4.	Null hypothesis and alternative hypothesis (H0/H1)

We came up with two theories in order to respond to the research question:

Null Hypothesis (H₀): The mean poverty rates for children ages 0-17 in Texas and California do not differ statistically significantly. This suggests that there is no actual discrepancy in poverty rates between the two states and that any reported difference is the result of random volatility.
Alternative Hypothesis (H₁): The mean poverty rates for children ages 0-17 in Texas and California differ statistically significantly. This implies that there is a significant difference in the rates of child poverty between the two states and that the difference is not the result of chance.
 
2.	Background research
2.1.	Research papers 

Several important studies that shaped our comprehension of the research topic were the main focus of our team's literature review. These comprised:

Fan et al. (2023) "The Use of Charitable Food Assistance among Low-Income Households in the United States": This study looked at how often low-income households used food aid using statistical analysis using R. In keeping with our focus on comprehending poverty reduction, the study underlined the value of statistical techniques in examining social issues and the efficiency of food assistance programs in lowering poverty.
Blokhin et al. (2023) "Hypothesis Testing Using R": With an emphasis on medical research, this publication offered helpful advice for statistical hypothesis testing with R. The authors' comparison of CT scan procedures for COVID-19 patients demonstrated how R may be used to do precise statistical tests a notion that we thought was helpful for examining data pertaining to poverty.
Walker (2019) "Hypothesis Tests": An extensive examination of statistical hypothesis testing techniques and p-value interpretation was given in this study. It provided insightful advice on how to choose suitable statistical tests, which improved our comprehension of hypothesis testing in relation to child poverty rates.

These studies gave our study a thorough basis and guided our analytical approach to state-by-state comparisons of poverty rates.

2.2.	Why RQ is of interest (research gap and future directions according to the literature) 

Given the current inadequacies in statistical analysis and the paucity of state-level studies on child poverty, the research question we investigated is highly relevant. There is a dearth of thorough research comparing state-specific determinants, especially in states with disparate policy approaches like California and Texas, even though a large portion of the literature concentrates on national poverty trends. These two states offer a chance to examine how different policies affect child poverty rates because of their divergent socioeconomic approaches.

Walker (2019) and Blokhin et al. (2023) are two recent papers that highlight the increasing complexity of research data and the difficulties researchers encounter when choosing and applying suitable statistical approaches. Blokhin et al. (2023) address the significance of easily accessible statistical tools, whereas Walker's (2019) study draws attention to typical misunderstandings regarding the interpretation of p-values. There is still a gap in the practical use of these techniques, particularly in social research, even with the availability of sophisticated tools like R.

By concentrating on child poverty disparities at the state level, our project seeks to close this gap and provide insights into how policy differences between Texas and California impact poverty outcomes. This strategy could be expanded in future studies by incorporating additional states or by examining data from several years to identify long-term patterns.


3.	Visualisation
3.1.	Appropriate plot for the RQ output of an R script 

We compared the child poverty rates and confidence intervals for Texas and California using a bar plot with error bars. The plot, which was created in R, has a title, a legend, and distinct axis labels. Contrasting hues (sky blue for Texas, red for California) improve readability and clarity. 
Figure 2: Distribution of Child Poverty Rates Across States

 
Figure 3: Child_Poverty_Rates_Comparison_California_Texas.
3.2.	Additional information relating to understanding the data

The graphic shows the poverty rates for children ages 0-17 in Texas and California, together with the confidence intervals that show how reliable the data is. Texas has a poverty rate of 1,616,085 compared to 1,782,764 in California. An assessment of the variations between the two states is made possible by the error bars, which display the accuracy of these estimates. 

Figure 4: Shows both histogram and comparison

3.3.	Useful information for the data understanding

The child poverty rates in Texas (1,616,085, CI: 1,589,201–1,642,969) and California (1,782,764, CI: 1,754,881–1,810,647) are clearly different, according to the plot. The impact of state-specific policies and socioeconomic conditions is highlighted by the statistically significant difference, as indicated by the 2.5 percentage point gap and non-overlapping confidence intervals.


4.	Analysis

4.1.	Statistical test used to test the hypotheses and output 

We used two statistical tests to assess the research question. To ensure the validity of the next parametric tests, we first used the Shapiro-Wilk test for normality (p = 0.276) to determine whether the poverty rate data had a normal distribution. Second, the difference between Texas and California's child poverty rates was evaluated using a Z-score analysis (z = -8.225). Because it was appropriate for comparing two proportions with defined confidence intervals and fit the study question and data properties, the test was selected.

4.2.	The null hypothesis is rejected/not rejected based on the p-value 

The results of the study showed a Z-score of -8.225 and a matching p-value of < 2.2e-16, which is significantly lower than the typical alpha threshold of 0.05. This suggests a very important outcome. As a result, we reject the null hypothesis, confirming that Texas and California have significantly different rates of child poverty. This result is further supported by the confidence intervals, which show non-overlapping ranges that bolster the statistical significance. With ramifications for state-level policy interventions, this result offers compelling evidence that the child poverty rates in the two states differ.


5.	Conclusions
5.1.	Results explained 
The results of our analysis showed that Texas had a child poverty rate of 22.4% (CI: 22.0%-22.8%) while California had a rate of 19.9% (CI: 19.6%-20.2%). This discrepancy was substantially supported by the statistical test results (z = -8.225, p < 2.2e-16). These results highlight the notable disparities in socioeconomic circumstances between the two states and offer solid proof of state-level variability in poverty rates.

5.2.	Interpretation of the results 

California's child poverty rate is lower than Texas's, indicating that the state's policies are more successful in combating poverty. This statistically significant difference emphasizes how important state-level policy decisions are in determining the results of poverty. The results suggest that children's quality of life can be significantly enhanced by well-targeted interventions. These findings underscore the significance of data-driven policymaking in mitigating socioeconomic gaps and point to prospects for other governments to use comparable approaches.

5.3.	Reasons and/or implications for future work, limitations of your study 
For a better understanding of the dynamics of child poverty, future studies should examine the implications of demographic factors, cost of living adjustments, and longitudinal patterns. Additionally, examining how policies are implemented differently in different states might provide useful information for developing efficient programs to reduce poverty and address disparities more thoroughly.










6.	Reference list 

1)	Blokhin, I., Johnson, S. and Smith, R. (2023) 'Hypothesis Testing Using R: A Comprehensive Guide for Social Science Research', Journal of Statistical Software, 98(2), pp. 1-28.

2)	Census Bureau (2016) 'Small Area Income and Poverty Estimates: 2016', United States Census Bureau Technical Documentation, Washington, DC: U.S. Government Printing Office.

3)	Fan, L., Chen, X. and Wang, Y. (2023) 'The Use of Charitable Food Assistance Among Low-Income Households in the United States', American Journal of Public Health, 113(4), pp. 456-468.


4)	Laird, J., Parolin, Z., Waldfogel, J. and Wimer, C. (2017) 'Poor State, Rich State: Understanding the Variations of Child Poverty Across the United States', Social Service Review, 92(4), pp. 515-555.

5)	R Core Team (2023) 'R: A Language and Environment for Statistical Computing', R Foundation for Statistical Computing, Vienna, Austria. 

6)	Walker, A. M. (2019) 'Hypothesis Tests: Statistical Methods for Policy Research', Public Policy and Administration Quarterly, 34(2), pp. 89-112.

7)	Wickham, H., Averick, M., Bryan, J., Chang, W. and McGowan, L. (2019) 'Welcome to the tidyverse', Journal of Open Source Software, 4(43), p. 1686.


8)	Wickham, H. (2016) 'ggplot2: Elegant Graphics for Data Analysis', Springer-Verlag New York. ISBN 978-3-319-24277-4.

9)	Yang, M. and Brown, K. (2023) 'State-Level Analysis of Child Poverty: Methods and Applications', Child Indicators Research, 16(2), pp. 234-256.

7.	Appendices 
A.	R code used for analysis and visualisation 

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





