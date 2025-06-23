### MANOVA analisis for Objective 1 - paper 2

install.packages(c("car", "carData", "ggplot2"))
install.packages("MASS")
install.packages("mvoutlier")
install.packages("mvnormtest")
install.packages("pastecs")
install.packages("reshape")
install.packages(c("akima","robustbase"))
install.packages("WRS")
install.packages("WRS",repos="http://R-Forge.R-project.org",type="source")

library(car)
library(carData)
library(ggplot2)
library(MASS)
library(mvoutlier)
library(mvnormtest)
library(reshape)
library(reshape)
library(akima)
library(robustbase)
library(WRS)


### ------------------------  MANOVA  (Objective 1 - paper 2) ----------------------

### MANOVA to compare between End-users & Test-users
setwd("F:/Research/WRESTORE_experiment_Fall2020/P01b_HumanBasicValues/001_ANOVA_MANOVA_exp2_ratings")
setwd("F:/Research/WRESTORE_experiment_Fall2020/P01b_HumanBasicValues/001_obj_1_diff_between_test_end_users")
data_m <- read.csv('exp2_Stak_NoStak_data.csv')

head(data_m); names(data_m)
dd <- data_m  

out_d = cbind(dd$SP1,dd$SP2, dd$SP3, dd$SP4, dd$SP5, dd$SP6, dd$SP7,
              dd$SP8, dd$SP9, dd$SP10, dd$SP11, dd$SP12, dd$SP13, dd$SP14,
              dd$SP15, dd$SP16, dd$SP17, dd$SP18, dd$SP19, dd$SP20)

model_e2 = manova(out_d ~ dd$Particip,data=dd) # manova(X ~ Y, data = data_s).  X: indep.vars, Y: depend.vars
model_e2
summary(model_e2, intercept = TRUE)

#p<0.05 Resultado significativo. Si hay diferencia entre los grupos
summary(model_e2, intercept = TRUE, test = "Wilks")
summary(model_e2, intercept = TRUE, test = "Hotelling")
summary(model_e2, intercept = TRUE, test = "Roy")

# ANOVA individual
summary.aov(model_e2)

