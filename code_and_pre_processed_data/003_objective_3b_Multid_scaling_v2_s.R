# ST 558: Multivariate Analytics
# Module 8 R - Assignment 5
# By:  Efrain Noa Yarasca

# Multidimensional Scaling

install.packages('ca')
install.packages('psych')
library(ca)
library(psych)
install.packages('smacof')
library(smacof)

# Set working directory 
setwd("D:/research/thesis_papers/Paper_02_DM_Human_values/analysis/003_obj3b_Multid_Analysis")

df <- read.csv('epvq_77p2.csv') # File for MDS for all ratings (end and test users)
df <- read.csv('epvq_53p.csv') # File for final Experiment (53p)
df <- read.csv('epvq_24p.csv') # File for final Experiment (24p)

# Drop no-needed column. Comment this for 53p and 24p
epvq <- within(df, rm('Participant')) # Drop for MDS for all users

# Get matrix of distances
dis <- dist (t(epvq))

res2d <- smacofSym(dis)   ## For 2D
summary(res2d)


