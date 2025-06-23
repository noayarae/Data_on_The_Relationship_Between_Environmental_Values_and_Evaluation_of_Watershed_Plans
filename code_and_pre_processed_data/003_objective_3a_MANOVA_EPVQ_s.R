
##########  MANOVA for objective 3a - paper 2 ##########

setwd("F:/Research/WRESTORE_experiment_Fall2020/P02_HumanBasicValues/003b_test_EPVQ")
dd <- read.csv('exp2_Stak_NoStak_PVQdata.csv')
head(dd); names(dd)

out_d = cbind(dd$pvq1,dd$pvq2, dd$pvq3, dd$pvq4, dd$pvq5, dd$pvq6, dd$pvq7,
              dd$pvq8, dd$pvq9, dd$pvq10, dd$pvq11, dd$pvq12, dd$pvq13, dd$pvq14,
              dd$pvq15, dd$pvq16, dd$pvq17, dd$pvq18, dd$pvq19, dd$pvq20)

model_e2 = manova(out_d ~ dd$Particip,data=dd) # manova(X ~ Y, data = data_s).  X: indep.vars, Y: depend.vars
model_e2
summary(model_e2, intercept = TRUE)

#p<0.05 Resultado significativo. Si hay diferencia entre los grupos
summary(model_e2, intercept = TRUE, test = "Wilks")
summary(model_e2, intercept = TRUE, test = "Hotelling")
summary(model_e2, intercept = TRUE, test = "Roy")

# ANOVA individual
#summary.aov(model_e2)
