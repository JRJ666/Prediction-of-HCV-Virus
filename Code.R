library(tidyverse)
library(ggplot2)
library(dplyr)
library(corrplot)
library(psych)
library(GGally)
library(PerformanceAnalytics)
hcv = read.csv("D:/STAT/Final_Project/Stat_final/hcvdat.csv", header = TRUE)

#Making the performance analytics plot by subtracting category datatypes:
hcv_plot=hcv[,-c(1,2,4)]
chart.Correlation(hcv_plot,histogram=TRUE,pch=19)

pairs.panels(hcv)

#Distribution of Disease Stages based on Category.
hcv %>%
  ggplot(aes(x = Category)) +
  geom_bar(fill = "steelblue", color = "black", alpha = 0.8) +
  xlab("Disease Stage") +
  ylab("Total Count") +
  ggtitle("Distribution of Disease Stages") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))

#Distribution of Age of individuals in the dataset.
hcv %>%
  ggplot(aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black", alpha = 0.8) +
  xlab("Age (years)") +
  ylab("Total Patient Count") +
  ggtitle("Distribution of Age") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

#Gender distribution based on males/females.

hcv %>%
  ggplot(aes(x = Sex)) +
  geom_bar(fill = "red", color = "black", alpha = 0.8) +
  xlab("Genders") +
  ylab("Total Patient Count") +
  ggtitle("Gender Distribution in the dataset.") +
  theme_classic() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))


#Boxplot for ALP levels
hcv %>%
  ggplot() +
  geom_boxplot(aes(x = "", y = ALP, fill = "ALP")) +
  guides(fill = guide_legend(title = "Liver Enzyme Levels")) +
  xlab("") +
  ylab("Enzyme Level") +
  ggtitle("Distribution of Liver Enzyme Levels")

#Boxplot for AST levels
hcv %>%
  ggplot() +
  geom_boxplot(aes(x = "", y = AST, fill = "AST")) +
  guides(fill = guide_legend(title = "Liver Enzyme Levels")) +
  xlab("") +
  ylab("Enzyme Level") +
  ggtitle("Distribution of Liver Enzyme Levels")

#Boxplot for ALT levels
hcv %>%
  ggplot() +
  geom_boxplot(aes(x = "", y = ALT, fill = "ALT")) +
  guides(fill = guide_legend(title = "Liver Enzyme Levels")) +
  xlab("") +
  ylab("Enzyme Level") +
  ggtitle("Distribution of Liver Enzyme Levels")

#Boxplot for GGT
hcv %>%
  ggplot() +
  geom_boxplot(aes(x = "", y = GGT, fill = "GGT")) +
  guides(fill = guide_legend(title = "Liver Enzyme Levels")) +
  xlab("") +
  ylab("Enzyme Level") +
  ggtitle("Distribution of Liver Enzyme Levels")

#Scatterplot for the most prominent relation/trend we found was AST vs GGT levels . 
hcv %>%
  ggplot(aes(x = AST, y = GGT)) +
  geom_point() +
  xlab("Aspartate aminotransferase Level") +
  ylab("Cholesterol Level") +
  ggtitle("AST Levels vs. Cholesterol")



