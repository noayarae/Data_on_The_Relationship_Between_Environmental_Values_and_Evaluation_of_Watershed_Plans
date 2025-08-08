# ----------------------------------------------------------------------------#
# Analysis: Multidimensional Scaling
# Project: Relationship Between Environmental Values and the Evaluation of Watershed Conservation Plans that Benefit the Community Versus the Individual
# 
# Author: Efrain Noa Yarasca PhD.
# ----------------------------------------------------------------------------#


# # install required libraries
# install.packages('ca')
# install.packages('psych')
# library(ca)
# library(psych)
# install.packages('smacof')
# library(smacof)
# library(dplyr)


# Set working directory 
#setwd("D:/research/thesis_papers/Paper_02_DM_Human_values/analysis/003_obj3b_Multid_Analysis")
#setwd("E:/OSU_PC_D_drive/research/thesis_papers/Paper_02_DM_Human_values/analysis/003_obj3b_Multid_Analysis")
setwd("D:/work/research_o/paper_02/replica")


# ======================= MDS Test-Users ======================= #
# Read data & setting up 
df_all_users <- read.csv('EPVQ_score_data.csv')
df_test_user <- df_all_users %>%
  filter(Particip == 'Test-user') %>%
  select(-Particip, -pid)

# Get matrix of distances & Compute MDS
dis_test_user <- dist (t(df_test_user))
res2d_test_user <- smacofSym(dis_test_user)   
summary(res2d_test_user)


# ------------ MDS Plot (Test-Users) --------------
# Create base plot
plot(res2d_test_user, main = "Multidimensional Scaling 2D (Test-users)",
     type = "n",  # Do not plot anything yet
     pch = 21, bg = "lightblue", col = "darkblue", cex = 1.5,
     ylim = c(-0.0004, 0.00025), label.conf = list(label = FALSE)
     )
# Customizing the MDS points
x_coords <- res2d_test_user$conf[,1]
y_coords <- res2d_test_user$conf[,2]
points(x_coords[1:4], y_coords[1:4], pch = 17, col = "red", bg = "red", cex = 1.5)             # Biospheric
points(x_coords[5:9], y_coords[5:9], pch = 15, col = "blue", bg = "gray", cex = 1.5)           # Altruistic
points(x_coords[10:12], y_coords[10:12], pch = 18, col = "darkgreen", bg = "green", cex = 1.5) # Hedonic
points(x_coords[13:17], y_coords[13:17], pch = 21, col = "black", bg = "gray", cex = 1.5)      # Egoistic

# Add labels to all points
text(x_coords, y_coords, labels = rownames(res2d_test_user$conf), cex = 0.7, pos = 4, col = "black")

# Add customized grid
grid(col = "gray80", lty = 2, lwd = 0.8)

# Add custom rays
segments(0, 0, -0.18, 0.7, col = "red", lwd = 1)                # Line 1
segments(0, 0, 1, -0.6, col = "red", lwd = 1)                   # Line 2
segments(0, 0, -0.8, -0.5, col = "red", lwd = 1.0)             # Line 3
lines(c(0, -0.2, -0.8), c(0, 0.05, 0.5), col = "red", lwd = 1)  # Line 4

# Add lines at x=0 and y=0
abline(h = 0, col = "gray40", lwd = 1.5)  # Horizontal line (y = 0)
abline(v = 0, col = "gray40", lwd = 1.5)  # Vertical line (x = 0)

# Add custom text labels
text(x = c(0.025, -0.80, -0.6, 0.35), y = c(-0.48,  0.18, 0.45, 0.20),
     labels = c("Biospheric", "Altruistic", "Hedonic", "Egoistic"),
     col = c("red", "blue", "darkgreen", "black"),
     cex = 1.2, pos = 4
     )
# ==============================================================================






# ======================= MDS End-Users ======================= #
# Read data & setting up 
df_all_users <- read.csv('EPVQ_score_data.csv')

df_end_user <- df_all_users %>%
  filter(Particip == 'End-user') %>%
  select(-Particip, -pid)

# Get matrix of distances & Compute MDS
dis_end_user <- dist (t(df_end_user))
res2d_end_user <- smacofSym(dis_end_user)   
summary(res2d_end_user)


# ------------ MDS Plot --------------
# Create base plot
plot(res2d_end_user, main = "Multidimensional Scaling 2D (End-users)",
     type = "n",  # Do not plot anything yet
     pch = 21, bg = "lightblue", col = "darkblue", cex = 1.5,
     ylim = c(-0.0004, 0.00025), label.conf = list(label = FALSE)
)
# Customizing the MDS points
x_coords <- res2d_end_user$conf[,1]
y_coords <- res2d_end_user$conf[,2]
points(x_coords[1:4], y_coords[1:4], pch = 17, col = "red", bg = "red", cex = 1.5)             # Biospheric
points(x_coords[5:9], y_coords[5:9], pch = 15, col = "blue", bg = "gray", cex = 1.5)           # Altruistic
points(x_coords[10:12], y_coords[10:12], pch = 18, col = "darkgreen", bg = "green", cex = 1.5) # Hedonic
points(x_coords[13:17], y_coords[13:17], pch = 21, col = "black", bg = "gray", cex = 1.5)      # Egoistic

# Add labels to all points
text(x_coords, y_coords, labels = rownames(res2d_end_user$conf), cex = 0.7, pos = 4, col = "black")

# Add customized grid
grid(col = "gray80", lty = 2, lwd = 0.8)

# Add custom rays
segments(0, 0, -0.18, 0.7, col = "red", lwd = 1)                # Line 1
segments(0, 0, 1, -0.7, col = "red", lwd = 1)                   # Line 2
segments(0, 0, -0.8, -0.44, col = "red", lwd = 1.0)             # Line 3
lines(c(0, -0.2, -0.8), c(0, 0.09, 0.5), col = "red", lwd = 1)  # Line 4

# Add lines at x=0 and y=0
abline(h = 0, col = "gray40", lwd = 1.5)  # Horizontal line (y = 0)
abline(v = 0, col = "gray40", lwd = 1.5)  # Vertical line (x = 0)

# Add custom text labels
text(x = c(-0.10, -0.70, -0.6, 0.25), y = c(-0.40,  0.05, 0.45, 0.35),
     labels = c("Biospheric", "Altruistic", "Hedonic", "Egoistic"),
     col = c("red", "blue", "darkgreen", "black"),
     cex = 1.2, pos = 4
)
# ==============================================================================











