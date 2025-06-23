
# Linear regression of participants' ratings by groups (sets)

# -------------  Objective 2c paper 2 ------------- #

### Read directory
setwd("F:/Research/WRESTORE_experiment_Fall2020/P01b_HumanBasicValues/003_obj3_slope_test")
setwd("F:/Research/WRESTORE_experiment_Fall2020/P01b_HumanBasicValues/002_obj2_gradient_of_ratings")

df <- read.csv('ratings_by_groups.csv')
head(df); names(df)

mg1 <- lm(df$group_1 ~ df$order, data=df) # For group-1
summary(mg1)

mg2 <- lm(df$group_2 ~ df$order, data=df) # For group-2
summary(mg2)

mg3 <- lm(df$group_3 ~ df$order, data=df) # For group-3
summary(mg3)

mg4 <- lm(df$group_4 ~ df$order, data=df) # For group-4
summary(mg4)

mg5 <- lm(df$group_5 ~ df$order, data=df) # For group-5
summary(mg5)

