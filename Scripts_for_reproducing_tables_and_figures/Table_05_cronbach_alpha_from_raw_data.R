# ----------------------------------------------------------------------------#
# Analysis: Cronbach Alpha Values for Environmental Values
# Project: Relationship Between Environmental Values and the Evaluation of Watershed Conservation Plans that Benefit the Community Versus the Individual
# 
# Author: Efrain Noa Yarasca PhD.

# Input:  'Rating_of_participants_to_the_suggested_plans.csv'
# Output: 'Table 5'
# ----------------------------------------------------------------------------#


# # install required libraries
# install.packages("psych")
# library(psych)

# -----------------------------------------------------------------------------
setwd("D:/work/research_o/paper_02/replica")
input_file = 'EPVQ_score_data.csv'

# Define the column names for each Human-value
bio_cols <- c("e.pvq1", "e.pvq2", "e.pvq3", "e.pvq4")
alt_cols <- c("e.pvq5", "e.pvq6", "e.pvq7", "e.pvq8", "e.pvq9")
hed_cols <- c("e.pvq10", "e.pvq11", "e.pvq12")
ego_cols <- c("e.pvq13", "e.pvq14", "e.pvq15", "e.pvq16", "e.pvq17")

# Create a function to perform the analysis
calculate_and_print_alpha <- function(filename, group_label) {
  
  # Load the data from the specified file
  df_all <- read.csv(filename)
  df <- subset(df_all, Particip == group_label)[ , !(names(df_all) %in% c("Particip", "pid"))]
  
  # Perform Cronbach's Alpha analysis for each variable
  alpha_results <- list(
    Bio = alpha(df[, bio_cols]),
    Alt = alpha(df[, alt_cols]),
    Hed = alpha(df[, hed_cols]),
    Ego = alpha(df[, ego_cols])
  )
  
  # Create a data frame to store the results
  alpha_table <- data.frame(
    Variable = character(),
    Cronbach_Alpha = numeric(),
    stringsAsFactors = FALSE
  )
  for (variable in names(alpha_results)) {
    alpha_value <- alpha_results[[variable]]$total$raw_alpha
    alpha_table <- rbind(alpha_table, data.frame(Variable = variable, Cronbach_Alpha = alpha_value))
  }
  
  # Print the results 
  cat("\n--------------------------------------------------------------")
  cat("\nCronbach's Alpha Results for:", group_label, "\n")
  cat("-------------------------------------------------------------- \n")
  
  print(alpha_table, row.names = FALSE)
}

# Call the function for 'Test-users' and 'End-users'
calculate_and_print_alpha(input_file, 'Test-user')
calculate_and_print_alpha(input_file, 'End-user')





















