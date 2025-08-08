# ----------------------------------------------------------------------------#
# Analysis: 
#          - Correlation between environmental values and participants’ preferences for conservation plans
#          - Relationship of environmental values and participants’ preferences for conservation plans

# Project: Relationship Between Environmental Values and the Evaluation of Watershed Conservation Plans that Benefit the Community Versus the Individual
# 
# Author: Efrain Noa Yarasca PhD.

# Input:  'Rating_of_participants_to_the_suggested_plans.csv'
#         'EPVQ_score_data.csv'

# Output: 'Table 7: Correlation Coefficients and p-Values Between the Environmental Values and the Participants Preferences Metric (PPM) for Plans',
#         'Table 8: Variance Inflation Factors (VIF) for Environmental Human Values in Test- and End-Users ' y 
#         'Table 9: Regression Weights, t-Values, and p-Values from the Multi-Linear Regression '
# ----------------------------------------------------------------------------#


# # Load the necessary libraries for data manipulation.
# # install.packages("dplyr")
# # install.packages("tibble")
# # install.packages("car", repos = "http://cran.rstudio.com/")
# library(dplyr)
# library(tibble)
# library(car)

# -----------------------------------------------------------------------------
# Set working directory
setwd("D:/work/research_o/paper_02/replica")


# ===================== Part 1: Get PPM values  ====================== #
# 1.1. Read the initial dataset.
SP_df <- read.csv('Rating_of_participants_to_the_suggested_plans.csv')


# 1.2. Reshaping the SP_df before calculations
SP_df_transp <- SP_df %>%
  select(-Particip) %>%
  column_to_rownames("pid") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("pid") %>%
  mutate(ID = row_number(), .before = 1)
# head(SP_df_transp)



# 1.3. Settings and selection of data 
start_col_num <- 3  # Column 'p1' is at index 3
end_col_num <- 79   # Column 'p24' is at index 26
column_indices <- start_col_num:end_col_num

average_PPMs_vector <- c() # Set an empty vector to store average PPM values (slopes)
chunk_size <- 4

# 1.4. Compute PPM. Loop through each End-User (from p1 to p24)
for (col_index in column_indices) {
  slopes <- c()
  # Loop through the data in chunks of 4 rows for the current test-user
  for (i in 1:(nrow(SP_df_transp)/chunk_size)) {
    start_row <- (i - 1) * chunk_size + 1
    end_row <- i * chunk_size
    chunk_data <- SP_df_transp[start_row:end_row, ]
    
    # Compute the PPM value using linear regression approach
    model <- lm(chunk_data[[col_index]] ~ ID, data = chunk_data)
    slopes <- c(slopes, -coef(model)[2])
  }
  average_PPM_for_col <- mean(slopes)
  average_PPMs_vector <- c(average_PPMs_vector, average_PPM_for_col)
}

# 1.5. Print the final vector of average PPM values
# cat("Average PPMs for columns p1 to p24:\n")
# cat(paste(round(average_PPMs_vector, 4), collapse = ", "))

# Create a data frame from the average_PPMs_vector
ppm_df <- as.data.frame(average_PPMs_vector)
colnames(ppm_df) <- "PPM"  # Rename the column to 'PPM'
user_column <- c(rep("end_user", 24), rep("test_user", 53))  # Create the 'user' column
pid_column <- paste0("p", seq_len(length(user_column)))

# Create a data frame 
final_df <- data.frame(
  pid = pid_column,
  user = user_column,
  PPM = average_PPMs_vector
)
final_df$PPM <- round(final_df$PPM, 3)  # Round the 'PPM' 

# Print the PMs values
#print(final_df)
print(head(final_df))
# ==============================================================================






# ==============================================================================
# =============== Part 2: Get EPVQ averages by each Human-value ================

# 2.1. Read the initial dataset.
epvq_df <- read.csv('EPVQ_score_data.csv')


# 2.2. Reshaping the epvq_df before calculations
epvq_df_transp <- epvq_df %>%
  select(-Particip) %>%
  column_to_rownames("pid") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("pid") %>%
  mutate(ID = row_number(), .before = 1)
# head(epvq_df_transp)


# 2.3. Settings and selection of data
row_chunks <- list(
  1:4,    # Biospheric
  5:9,    # Altruistic
  10:12,  # Hedonic
  13:17   # Egoistic
)

# Define the range of columns to process (End-users: p1-p24; Test-users: p25-p77)
start_col_name <- "p1" # "p25" # 
end_col_name <- "p77" # "p77" # 
column_indices <- which(names(epvq_df_transp) == start_col_name):which(names(epvq_df_transp) == end_col_name)
column_names <- names(epvq_df_transp)[column_indices]

# Initialize a data frame to store the average values
average_values_matrix <- matrix(NA, nrow = length(row_chunks), ncol = length(column_names))
average_values_df <- as.data.frame(average_values_matrix)
names(average_values_df) <- column_names

# 2.4. Compute PPM. Loop through each column 
for (col_index in 1:length(column_names)) {
  current_col_name <- column_names[col_index]
  
  # Loop through each Human-value 
  for (chunk_index in 1:length(row_chunks)) {
    chunk_rows <- row_chunks[[chunk_index]]
    chunk_data <- epvq_df_transp[chunk_rows, current_col_name]
    average_value <- mean(chunk_data)
    average_values_df[chunk_index, current_col_name] <- average_value
  }
}
# Print average E-PVQ values
print(round(average_values_df, 2))
# ==============================================================================




# ================= Part 3: Combine EPVQ and PPM into one df ===================
# Transpose, convert to data frame, and assign column names
avg_df_t <- as.data.frame(t(average_values_df))
colnames(avg_df_t) <- c("Bio", "Alt", "Hed", "Ego")
avg_df_t$pid <- rownames(avg_df_t) # Add 'pid' from row names
avg_df_t$user <- c(rep("end_user", 24), rep("test_user", 53)) # Add 'user' column
avg_df_t$PPM <- average_PPMs_vector # Add the PPM column
avg_df_t$Hed <- round(avg_df_t$Hed, 2) # Round 'Hed' values
avg_df_t <- avg_df_t[, c("pid", "user", "Bio", "Alt", "Hed", "Ego", "PPM")] # Reorder columns

# Print result
#print(avg_df_t)
print(head(avg_df_t))
# ==============================================================================





# =========================== Part 4. Get Table 7 ==============================
# Table 7: Correlation Coef and p-Values Between Environmental Values and Participants Preferences Metric (PPM) 

# ------------------ For Test-users --------------------
test_user_data <- avg_df_t %>%
  filter(user == "test_user") %>%
  mutate(across(c(PPM, Bio, Alt, Hed, Ego), as.numeric))

human_vals <- c("Bio", "Alt", "Hed", "Ego")
# Initialize list to store results
results_list <- lapply(human_vals, function(feature) {
  pearson <- cor.test(test_user_data$PPM, test_user_data[[feature]], method = "pearson")
  spearman <- cor.test(test_user_data$PPM, test_user_data[[feature]], method = "spearman")
  
  data.frame(
    Human_value = feature,
    `p-value` = pearson$p.value,
    pearson = pearson$estimate,
    Spearman = spearman$estimate,
    Confid_interval = paste0("(", round(pearson$conf.int[1], 3), ", ", round(pearson$conf.int[2], 3), ")"),
    stringsAsFactors = FALSE
  )
})

# Combine all into a single df and print results
results_df <- do.call(rbind, results_list)
print(results_df, row.names = FALSE)  # Print Table 7 for Test-users


# ---------------------- For End-users ----------------------
end_user_data <- avg_df_t %>%
  filter(user == "end_user") %>%
  mutate(across(c(PPM, Bio, Alt, Hed, Ego), as.numeric))

human_vals <- c("Bio", "Alt", "Hed", "Ego")
# Initialize list to store results
results_list <- lapply(human_vals, function(feature) {
  pearson <- cor.test(end_user_data$PPM, end_user_data[[feature]], method = "pearson")
  spearman <- cor.test(end_user_data$PPM, end_user_data[[feature]], method = "spearman")
  
  data.frame(
    Human_value = feature,
    `p-value` = pearson$p.value,
    pearson = pearson$estimate,
    Spearman = spearman$estimate,
    Confid_interval = paste0("(", round(pearson$conf.int[1], 3), ", ", round(pearson$conf.int[2], 3), ")"),
    stringsAsFactors = FALSE
  )
})

# Combine all into a single df and print results
results_df <- do.call(rbind, results_list)
print(results_df, row.names = FALSE) # Print Table 7 for End-users
# ==============================================================================





# =========================== Part 5: Get Table 8 =============================
# Table 8: Variance Inflation Factors (VIF) for Env. Human Values in Test- and End-Users 

# -------------------- For Test-user
LR_test_user <- lm(PPM ~ Bio + Alt + Hed + Ego, data=test_user_data)
vif_test_user <- vif(LR_test_user)   # Compute VIF
print(vif_test_user)    # Print VIF values

# -------------------- For End-user
LR_end_user <- lm(PPM ~ Bio + Alt + Hed + Ego, data=end_user_data)
vif_end_user <- vif(LR_end_user)   # Compute VIF
print(vif_end_user)    # Print VIF values
# ==============================================================================






# ==============================================================================
# ======================= Part 6: Get Table 9 ============================
# Table 9: Regression Weights, t-Values, and p-Values from the Regression Between 
#          the Environmental Values and the Participant Preference Metric (PPM)

# -------------------- For Test-user
LR_test_user <- lm(PPM ~ Bio + Alt + Hed + Ego, data=test_user_data)
# summary(LR_test_user)
print(summary(LR_test_user)$coefficients)


# -------------------- For End-user
LR_end_user <- lm(PPM ~ Bio + Alt + Hed + Ego, data=end_user_data)
# summary(LR_test_user)
print(summary(LR_end_user)$coefficients)
# ==============================================================================


















