# ----------------------------------------------------------------------------#
# Analysis: Relationship of environmental values and participants’ ratings for conservation plans
# Project: Relationship Between Environmental Values and the Evaluation of Watershed Conservation Plans that Benefit the Community Versus the Individual
# 
# Author: Efrain Noa Yarasca PhD.
# ----------------------------------------------------------------------------#

# # Install and load necessary libraries
# library(dplyr)
# library(tibble)


# Set the working directory
setwd("D:/work/research_o/paper_02/replica")


# ================= Part 1: Get PPM values  ====================== #
# ------------ 1.1  Read and transpose the data set
df_t <- read.csv("Rating_of_participants_to_the_suggested_plans.csv") %>%
  select(-Particip) %>%
  tibble::column_to_rownames("pid") %>%
  t() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("pid")

colnames(df_t) <- c("pid", paste0("p", seq_len(ncol(df_t) - 1))) # Rename columns: "pid", "p1", "p2", ...
df_t <- df_t %>% mutate(ID = row_number(), .before = 1) # Add sequential ID column
print(head(df_t)) # Preview result


# ------------ 1.2 Compute PPM values
rating_data <- df_t 
print(rating_data)

# Define the range of columns to process 
start_col_num <- 3  # Column 'p1' is at index 3
end_col_num <- 79   # Column 'p24' is at index 26
column_indices <- start_col_num:end_col_num

# Set an empty vector to store average PPM values (slopes)
average_PPMs_vector <- c()
chunk_size <- 4

# Loop through each End-User (from p1 to p24)
for (col_index in column_indices) {
  slopes <- c()
  # Loop through the data in chunks of 4 rows for the current test-user
  for (i in 1:(nrow(rating_data)/chunk_size)) {
    start_row <- (i - 1) * chunk_size + 1
    end_row <- i * chunk_size
    chunk_data <- rating_data[start_row:end_row, ]
    
    # Compute the PPM value using linear regression approach
    model <- lm(chunk_data[[col_index]] ~ ID, data = chunk_data)
    slopes <- c(slopes, -coef(model)[2])
  }
  average_PPM_for_col <- mean(slopes)
  average_PPMs_vector <- c(average_PPMs_vector, average_PPM_for_col)
}

# -------------- 1.3 Print the final vector of average PPM values
cat("Average PPMs for columns p1 to p24:\n")
cat(paste(round(average_PPMs_vector, 4), collapse = ", "))
# ==============================================================================






# ============== Part 2: Get average E-PVQ values ============== #
# Load the data from the CSV file
#EPVQ_dat <- read.csv("D:/work/research_o/paper_02/reproducible/for_fig_04.csv")



#EPVQ_data <- read.csv("EPVQ_score_data.csv")

# ------------ 2.1  Read and transpose the data set
EPVQ_data <- read.csv("EPVQ_score_data.csv") %>%
  select(-Particip) %>%
  tibble::column_to_rownames("pid") %>%
  t() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("pid")

colnames(EPVQ_data) <- c("pid", paste0("p", seq_len(ncol(EPVQ_data) - 1))) # Rename columns: "pid", "p1", "p2", ...
EPVQ_data <- EPVQ_data %>% mutate(ID = row_number(), .before = 1) # Add sequential ID column
print(head(EPVQ_data)) # Preview result



# ------------ 2.2 Compute E-PVQ average values
row_chunks <- list(
  1:4,    # Biospheric
  5:9,    # Altruistic
  10:12,  # Hedonic
  13:17   # Egoistic
)

# Define the range of columns to process (End-users: p1-p24; Test-users: p25-p77)
start_col_name <- "p1" # "p25" # 
end_col_name <- "p77" # "p77" # 
column_indices <- which(names(EPVQ_data) == start_col_name):which(names(EPVQ_data) == end_col_name)
column_names <- names(EPVQ_data)[column_indices]

# Initialize a data frame to store the average values
average_values_matrix <- matrix(NA, nrow = length(row_chunks), ncol = length(column_names))
average_values_df <- as.data.frame(average_values_matrix)
names(average_values_df) <- column_names

# Loop through each column from p1 to p24
for (col_index in 1:length(column_names)) {
  current_col_name <- column_names[col_index]
  
  # Loop through each Human-value 
  for (chunk_index in 1:length(row_chunks)) {
    chunk_rows <- row_chunks[[chunk_index]]
    chunk_data <- EPVQ_data[chunk_rows, current_col_name]
    average_value <- mean(chunk_data)
    average_values_df[chunk_index, current_col_name] <- average_value
  }
}

# -------------- 2.3 Print average E-PVQ values
print(average_values_df)
# ==============================================================================





# ============ Part 3: Combine and format the final dataframe ============ #

# Transpose the average_values_df and set the row names
combined_df <- t(average_values_df)
colnames(combined_df) <- c("Bio", "Alt", "Hed", "Ego")
combined_df <- as.data.frame(combined_df)

# Add the PPM values as a new column
combined_df$PPM <- average_PPMs_vector
print(combined_df)

# Save the final dataframe to a CSV file
#write.csv(combined_df, "combined_results.csv", row.names = TRUE)
# ==============================================================================





# ================ Part 4: Plot E-PVQ Scores vs. PPM (Fig 6)================= #
# Load necessary libraries
library(ggplot2)
library(tidyr) # For data transformation (pivot_longer)

df <- combined_df
df$Group <- c(rep("Test-users", 24), rep("End-users", 53))

# 1. Transform the data to a long format
# We'll gather the 'Bio', 'Alt', 'Hed', 'Ego' columns into two new columns:
# 'Variable' (which holds the original column name) and 'Value' (which holds the data)
df_long <- df %>%
  pivot_longer(
    cols = c(Bio, Alt, Hed, Ego), # Columns to pivot
    names_to = "Variable",       # New column for original column names
    values_to = "Value"          # New column for the values
  )

# Optional: Reorder the facets if you want a specific order (e.g., Bio, Alt, Hed, Ego)
df_long$Variable <- factor(df_long$Variable, levels = c("Bio", "Alt", "Hed", "Ego"))

# Define custom titles for each facet (optional, but makes plots clearer)
facet_titles <- c(
  Bio = "Biospheric Human Value",
  Alt = "Altruistic Human Value",
  Hed = "Hedonistic Human Value",
  Ego = "Egoistic Human Value"
)

# 2. Create the faceted plot using ggplot2
ggplot(df_long, aes(x = Value, y = PPM, color = Group, shape = Group)) +
  # Add scatter plot points for both groups
  geom_point(aes(fill = Group), size = 2, stroke = 1) + # Increased marker size to 4
  # Set the specific colors and shapes for each group
  scale_color_manual(values = c("Test-users" = "blue", "End-users" = "red")) +
  scale_fill_manual(values = c("Test-users" = "lightblue", "End-users" = "yellow")) +
  scale_shape_manual(values = c("Test-users" = 21, "End-users" = 22)) +
  # Add linear regression trend lines for each group within each facet
  geom_smooth(method = "lm", se = FALSE, aes(group = Group, color = Group, linetype = Group)) +
  scale_linetype_manual(values = c("Test-users" = "solid", "End-users" = "dashed")) +
  # Set plot titles and labels
  labs(title = "Fig 6. E-PVQ Scores vs. PPM for each Environmental-Value and each User Group ", # General title for the entire plot
       x = "E-PVQ Score", # Generic x-axis label, specific labels will come from facets
       y = "PPM",
       color = "Group",
       shape = "Group",
       fill = "Group") +
  # Set x-axis limits and breaks for all facets
  scale_x_continuous(limits = c(1, 7), breaks = seq(1, 7, by = 1.0)) +
  # Set y-axis limits and breaks for all facets
  scale_y_continuous(limits = c(0, 1.8), breaks = seq(0, 1.8, by = 0.2)) +
  theme_minimal() +
  # Position the legend
  theme(legend.position = c(0.03, 0.03),
        legend.justification = c("left", "bottom"),
        legend.background = element_rect(fill = "white", color = "black", size = 0.5, linetype = "solid"),
        legend.box.background = element_rect(fill = "white", color = "black", size = 0.5, linetype = "solid"),
        strip.text = element_text(face = "bold"), # Make facet titles bold
        panel.border = element_rect(colour = "black", fill = NA, size = 1) # Add a frame border to each subplot
  ) +
  # Create subplots based on the 'Variable' column, arranged in 2 columns
  facet_wrap(~ Variable, ncol = 2, labeller = as_labeller(facet_titles))
# ==============================================================================




















