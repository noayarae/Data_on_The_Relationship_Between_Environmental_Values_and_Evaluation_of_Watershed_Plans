### ANOVA Analysis for objective 2a & 2b for paper 2

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

### ----------------  ANOVA  (Objective 2 - PAPER-02) ----------------------
# Ref: https://www.youtube.com/watch?v=qrP7evoNCy4

setwd("F:/Research/WRESTORE_experiment_Fall2020/P01b_HumanBasicValues/001_obj_1a_1b_1c_influence_of_benefits")
dt <- read.csv('exp2_Stak_NoStak_data.csv') # exp2_Stak_NoStak_data.csv




### ANOVA for Objective 2a paper 2
### Sub-basin benefits = same & whole-watershed benefits = different

Gsb_1 <- c(rep('SP01',77),rep('SP05',77),rep('SP09',77),rep('SP13',77),rep('SP17',77))
rgsb_1 <- c(dt$SP1, dt$SP5, dt$SP9, dt$SP13, dt$SP17)
df_sb_1 <- data.frame(Gsb_1, rgsb_1)
summary(aov(rgsb_1 ~ Gsb_1, data = df_sb_1))

Gsb_2 <- c(rep('SP02',77),rep('SP06',77),rep('SP10',77),rep('SP14',77),rep('SP18',77))
rgsb_2 <- c(dt$SP2, dt$SP6, dt$SP10, dt$SP14, dt$SP18)
df_sb_2 <- data.frame(Gsb_2, rgsb_2)
summary(aov(rgsb_2 ~ Gsb_2, data = df_sb_2))

Gsb_3 <- c(rep('SP03',77),rep('SP07',77),rep('SP11',77),rep('SP15',77),rep('SP19',77))
rgsb_3 <- c(dt$SP3, dt$SP7, dt$SP11, dt$SP15, dt$SP19)
df_sb_3 <- data.frame(Gsb_3, rgsb_3)
summary(aov(rgsb_3 ~ Gsb_3, data = df_sb_3))

Gsb_4 <- c(rep('SP04',77),rep('SP08',77),rep('SP12',77),rep('SP16',77),rep('SP20',77))
rgsb_4 <- c(dt$SP4, dt$SP8, dt$SP12, dt$SP16, dt$SP20)
df_sb_4 <- data.frame(Gsb_4, rgsb_4)
summary(aov(rgsb_4 ~ Gsb_4, data = df_sb_4))


### ANOVA for Objective 2b paper 2. 
### Sub-basin benefits = Different & whole-watershed benefits = same

# ANOVA for group 1
G1 <- c(rep('SP01',77),rep('SP02',77),rep('SP03',77),rep('SP04',77))
rg1 <- c(dt$SP1, dt$SP2, dt$SP3, dt$SP4)
df_g1 <- data.frame(G1, rg1)
#SP.anova <- aov(ratings ~ SP, data = df)
summary(aov(rg1 ~ G1, data = df_g1))

G2 <- c(rep('SP05',77),rep('SP06',77),rep('SP07',77),rep('SP08',77))
rg2 <- c(dt$SP5, dt$SP6, dt$SP7, dt$SP8)
df_g2 <- data.frame(G2, rg2)
summary(aov(rg2 ~ G2, data = df_g2))

G3 <- c(rep('SP09',77),rep('SP10',77),rep('SP11',77),rep('SP12',77))
rg3 <- c(dt$SP9, dt$SP10, dt$SP11, dt$SP12)
df_g3 <- data.frame(G3, rg3)
summary(aov(rg3 ~ G3, data = df_g3))

G4 <- c(rep('SP13',77),rep('SP14',77),rep('SP15',77),rep('SP16',77))
rg4 <- c(dt$SP13, dt$SP14, dt$SP15, dt$SP16)
df_g4 <- data.frame(G4, rg4)
summary(aov(rg4 ~ G4, data = df_g4))

G5 <- c(rep('SP17',77),rep('SP18',77),rep('SP19',77),rep('SP20',77))
rg5 <- c(dt$SP17, dt$SP18, dt$SP19, dt$SP20)
df_g5 <- data.frame(G5, rg5)
summary(aov(rg5 ~ G5, data = df_g5))
### ------ End objective 2a













