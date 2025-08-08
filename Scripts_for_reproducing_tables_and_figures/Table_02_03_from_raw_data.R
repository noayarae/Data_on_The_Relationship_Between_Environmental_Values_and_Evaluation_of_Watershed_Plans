# ----------------------------------------------------------------------------#
# Analysis: ANOVA Results
# Project: Relationship Between Environmental Values and the Evaluation of Watershed Conservation Plans that Benefit the Community Versus the Individual
# 
# Author: Efrain Noa Yarasca PhD.

# Input:  'Rating_of_participants_to_the_suggested_plans.csv'
# Output: 'Table 2' y 'Table 3'
# ----------------------------------------------------------------------------#


# # install required libraries
# install.packages(c("car", "carData", "ggplot2"))
# install.packages("MASS")
# install.packages("mvoutlier")
# install.packages("mvnormtest")
# install.packages("pastecs")
# install.packages("reshape")
# install.packages(c("akima","robustbase"))
# install.packages("WRS")
# install.packages("WRS",repos="http://R-Forge.R-project.org",type="source")
# 
# library(car)
# library(carData)
# library(ggplot2)
# library(MASS)
# library(mvoutlier)
# library(mvnormtest)
# library(reshape)
# library(reshape)
# library(akima)
# library(robustbase)
# library(WRS)


### ----------------------  ANOVA  (Objective 2) ----------------------
# Set working directory
#setwd("E:/OSU_PC_D_drive/research/thesis_papers/Paper_02_DM_Human_values/analysis/002_obj_2a_2b_influence_of_benefits")
setwd("D:/work/research_o/paper_02/replica")

# Load data
dt <- read.csv('Rating_of_participants_to_the_suggested_plans.csv') # exp2_Stak_NoStak_data.csv


#----------------------------------------------------------------------
# Function to perform ANOVA and print results
#----------------------------------------------------------------------
run_anova <- function(data_list, group_labels, title) {
  # Combine the data and group labels into a single dataframe
  combined_data <- data.frame(
    Group = factor(group_labels),
    Response = unlist(data_list)
  )
  
  anova_result <- aov(Response ~ Group, data = combined_data)
  anova_summary <- summary(anova_result)
  
  f_value <- anova_summary[[1]]$`F value`[1]
  p_value <- anova_summary[[1]]$`Pr(>F)`[1]
  
  cat("\n--------------------------------------------------------------")
  cat("\n", title)
  cat("\n--------------------------------------------------------------")
  cat("\nF value:", f_value, "   Pr(>F):", p_value, "\n")
}




# =====================================================================
### ANOVA for Objective 2a (Table 2 - paper)
### Sub-basin benefits = same & whole-watershed benefits = different
#----------------------------------------------------------------------

# Define the groups of column numbers to be analyzed
obj2a_groups <- list(
  c(1, 5, 9, 13, 17),
  c(2, 6, 10, 14, 18),
  c(3, 7, 11, 15, 19),
  c(4, 8, 12, 16, 20)
)

print("Table 2")
# Loop through each group and run the ANOVA
for (i in 1:length(obj2a_groups)) {
  group_cols <- obj2a_groups[[i]]
  group_names <- paste0('SP', sprintf('%02d', group_cols))
  
  # Create a list of the data columns to be combined
  data_for_anova <- lapply(group_cols, function(col_num) dt[[paste0('SP', col_num)]])
  
  run_anova(data_for_anova, rep(group_names, each = 77), paste("ANOVA for Objective 2a, Group", i))
}
#----------------------------------------------------------------------




# =====================================================================
### ANOVA for Objective 2b (Table 3 - paper).
### Sub-basin benefits = Different & whole-watershed benefits = same
#----------------------------------------------------------------------

# Define the groups of column numbers to be analyzed
obj2b_groups <- list(
  c(1, 2, 3, 4),
  c(5, 6, 7, 8),
  c(9, 10, 11, 12),
  c(13, 14, 15, 16),
  c(17, 18, 19, 20)
)

print("Table 3")
# Loop through each group and run the ANOVA
for (i in 1:length(obj2b_groups)) {
  group_cols <- obj2b_groups[[i]]
  group_names <- paste0('SP', sprintf('%02d', group_cols))
  
  # Create a list of the data columns to be combined
  data_for_anova <- lapply(group_cols, function(col_num) dt[[paste0('SP', col_num)]])
  
  run_anova(data_for_anova, rep(group_names, each = 77), paste("ANOVA for Objective 2b, Group", i))
}
#----------------------------------------------------------------------


























