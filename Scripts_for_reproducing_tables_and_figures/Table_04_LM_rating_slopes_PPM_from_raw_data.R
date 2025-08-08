# ----------------------------------------------------------------------------#
# Analysis: Linear regression of participants' ratings by groups (sets)
# Project: Relationship Between Environmental Values and the Evaluation of Watershed Conservation Plans that Benefit the Community Versus the Individual
# 
# Author: Efrain Noa Yarasca PhD.

# Input:  'Rating_of_participants_to_the_suggested_plans.csv'
# Output: 'Table 4'
# ----------------------------------------------------------------------------#


# Load the necessary library for data manipulation.
library(dplyr)


# -------------  Objective 2c paper 2 ------------- #
### Set the working director
setwd("E:/OSU_PC_D_drive/research/thesis_papers/Paper_02_DM_Human_values/analysis/002_obj2c_gradient_of_ratings_PPM")


# 1. Load the raw dataset (CSV format)
df <- read.csv("Rating_of_participants_to_the_suggested_plans.csv")
head(df); names(df)

# 2. Reshaping the DB in four groups
plan_groups <- list(
  c("SP1", "SP5", "SP9", "SP13", "SP17"),
  c("SP2", "SP6", "SP10", "SP14", "SP18"),
  c("SP3", "SP7", "SP11", "SP15", "SP19"),
  c("SP4", "SP8", "SP12", "SP16", "SP20")
)

reshaped_dfs_list <- lapply(plan_groups, function(sp_cols) {
  df %>%
    # Select the participant info and the specific group's columns
    select(Particip, pid, all_of(sp_cols)) %>%
    # Rename the columns to generic group names for consistent stacking
    rename(
      group_1 = all_of(sp_cols[1]),
      group_2 = all_of(sp_cols[2]),
      group_3 = all_of(sp_cols[3]),
      group_4 = all_of(sp_cols[4]),
      group_5 = all_of(sp_cols[5])
    ) %>%
    # Create the `SPs_G` columns based on the original column names
    mutate(
      SPs_G1 = sp_cols[1],
      SPs_G2 = sp_cols[2],
      SPs_G3 = sp_cols[3],
      SPs_G4 = sp_cols[4],
      SPs_G5 = sp_cols[5]
    ) %>%
    # Reorder the columns to the final desired format
    select(
      Particip, pid,
      SPs_G1, group_1,
      SPs_G2, group_2,
      SPs_G3, group_3,
      SPs_G4, group_4,
      SPs_G5, group_5
    )
})
# Combining all four data frames from the list into a single one.
df_by_groups <- bind_rows(reshaped_dfs_list) %>%
  mutate(order = rep(c(4, 3, 2, 1), each = 77)) # Add the 'order' of SPs
#print(tail(df_by_groups))


# 3. Create a function to perform the linear regression
analyze_group <- function(group_col) {
  formula_str <- paste(group_col, "~ order")
  lm_model <- lm(as.formula(formula_str), data = df_by_groups)
  lm_summary <- summary(lm_model)
  
  # Extract all the specific values to report
  intercept_est <- lm_summary$coefficients[1, "Estimate"]
  order_est <- lm_summary$coefficients[2, "Estimate"]
  std_error <- lm_summary$coefficients[2, "Std. Error"]
  r_squared <- lm_summary$r.squared
  f_statistic <- lm_summary$fstatistic[1]
  
  # Calculate the p-value for the F-statistic
  p_value <- pf(lm_summary$fstatistic[1], lm_summary$fstatistic[2], lm_summary$fstatistic[3], lower.tail = FALSE)
  
  # Create a data frame (a single row) with the results and return it
  results <- data.frame(
    Group = group_col,
    Intercept = intercept_est,
    Slope = order_est,
    p_value = p_value,
    R_squared = r_squared,
    Std_Error = std_error,
    F_stat = f_statistic,
    stringsAsFactors = FALSE
  )
  return(results)
}

# Loop to iterate over the group columns and call the function
group_columns <- c("group_1", "group_2", "group_3", "group_4", "group_5")

results_list <- list()
for (i in 1:length(group_columns)) {
  col <- group_columns[i]
  results_list[[i]] <- analyze_group(col)
}

# Combine all the individual data into a single table
final_results_table <- do.call(rbind, results_list)


# 4. Table 4. Linear Model Parameters of Participants’ Preferences for Plans
print("Table 4")
print(final_results_table, row.names = FALSE)   # Print the table




















