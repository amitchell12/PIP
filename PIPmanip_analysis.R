##### ANALYSIS FOR PIPTOT MANIPULATION CHECK #####
## CREATED BY A.G.MITCHELL - 30.08.21
## ADAPTED FROM RTanalysis.R originally created by H Stocks & R D McIntosh

##### SET UP #####
#loading packages
library(psych)
library(tidyverse)
library(ggplot2)
library(pwr)
library(reshape2)
library(ggpubr)
library(Rmisc)

# getting files
DPath <- '/Users/alex/OneDrive - University of Edinburgh/Experiments/PIPTOT/Data'
setwd(DPath) #Data path

EXP <- read.csv('AGM_pilot.csv') #reading in experimental data
#DEMO <- read.csv() #reading in demographic data

##### DATA WRANGLE #####
## WILL NEED EDITING WHEN IT COMES TO REAL DATA
EXP <- EXP[, c(7,13:14,19,22:23,35,95,112:113,118,123,134,137,150,164,223,225)]

# defining trials
EXP$trial_type <- factor(EXP$trial_type)
levels(EXP$trial_type) <- c('catch','no','go')

# grouping
CATCH <- EXP[EXP$trial_type == 'catch' ,]
GO <- EXP[EXP$trial_type == 'go' ,]
NOGO <- EXP[EXP$trial_type == 'no' ,]

# plotting
GO$sound <- factor(GO$sound)
ggplot(GO) +
  geom_density(aes(response_time, colour = sound))

# summary
RT_GO <- summarySEwithin(data = GO, measurevar = 'response_time', withinvars = 'sound')
