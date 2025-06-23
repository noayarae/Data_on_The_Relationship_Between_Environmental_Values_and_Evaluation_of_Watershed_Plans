### Correlation and Linear Model between the 4 high order Human Basic Values and PPM

setwd("F:/Research/WRESTORE_experiment_Fall2020/P02_HumanBasicValues/007_Linear_regress")
dreg <- read.csv('data_linear_reg.csv')

df <- dreg
head(df); names(df)

no_st <- df[df$pid=="no_stak",] # for test-users

## --------- Objective 4 - part 1 (Test-users) ---------
# For Test-users (no-stak). 
### Evaluate correlations between PPM and H Basic-Values

res_b <- cor.test(no_st$slope3, no_st$Biospheric, method = "pearson")
res_b
res2_b <-cor.test(no_st$slope3, no_st$Biospheric,  method = "spearman")
res2_b

res_a <- cor.test(no_st$slope3, no_st$Altruistic, method = "pearson")
res_a
res2_a <-cor.test(no_st$slope3, no_st$Altruistic,  method = "spearman")
res2_a

res_h <- cor.test(no_st$slope3, no_st$Hedonic, method = "pearson")
res_h
res2_h <-cor.test(no_st$slope3, no_st$Hedonic,  method = "spearman")
res2_h

res_e <- cor.test(no_st$slope3, no_st$Egoistic, method = "pearson")
res_e
res2_e <-cor.test(no_st$slope3, no_st$Egoistic,  method = "spearman")
res2_e

## --------- Objective 4 - part 1 (End-users) ---------
# For End-users (stak). 
### Evaluate correlations between PPM and H Basic-Values
st <- df[df$pid =="stak",]  # for end-users

ress_b <- cor.test(st$slope3, st$Biospheric, method = "pearson")
ress_b
ress2_b <-cor.test(st$slope3, st$Biospheric,  method = "spearman")
ress2_b

ress_a <- cor.test(st$slope3, st$Altruistic, method = "pearson")
ress_a
ress2_a <-cor.test(st$slope3, st$Altruistic,  method = "spearman")
ress2_a

ress_h <- cor.test(st$slope3, st$Hedonic, method = "pearson")
ress_h
ress2_h <-cor.test(st$slope3, st$Hedonic,  method = "spearman")
ress2_h

ress_e <- cor.test(st$slope3, st$Egoistic, method = "pearson")
ress_e
ress2_e <-cor.test(st$slope3, st$Egoistic,  method = "spearman")
ress2_e

## --------------- End Obj 4 - part 1 -------------



### ---------------------------
## This part (MLR) was re-done in Obj 4 part 2 

# For Test-users (no-stak).  
# Y(slope3)= a + Biospheric + Altruistic + Hedonic + Egoistic
m2 <- lm(slope3 ~ Biospheric+Altruistic+Hedonic+Egoistic, data=no_st)
summary(m2)


# For End-users 
m1 <- lm(slope3 ~ Biospheric+Altruistic+Hedonic+Egoistic, data=st)
summary(m1)

# For All
m3 <- lm(slope3 ~ Biospheric+Altruistic+Hedonic+Egoistic, data=df)
summary(m3)




