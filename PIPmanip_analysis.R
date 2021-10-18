##### DATA QUALITY CHECK FOR PIPTOT MANIPULATION CHECK #####
## CREATED BY A.G.MITCHELL - 16.09.21
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
library(lme4)
library(effsize)


# getting files
#DPath <- 'C:/Users/amitch17/OneDrive - University of Edinburgh/Experiments/PIPTOT/Data'
DPath <- '/Users/alex/OneDrive - University of Edinburgh/Experiments/PIPTOT/Data'
APath <- '/Users/alex/OneDrive - University of Edinburgh/Experiments/PIPTOT/Analysis'
setwd(DPath) #Data path

EXP <- read.csv('PIPmanip_JATOS.csv') #reading in experimental data
DEMO <- read.csv('PIPmanip_QUAL.csv') #reading in demographic data
# change name of responseID & qualtrics_id to ID
colnames(DEMO)[which(names(DEMO) == "ResponseId")] <- "ID"
colnames(EXP)[which(names(EXP) == "qualtrics_id")] <- "ID"

# count number of participants in EXP (with full data-sets)
N <- count(EXP, 'ID')
# only 23 but all have full number of trials- fab

# check all prolific IDs match
DEMO$IDCHECK <- ifelse(as.character(DEMO$X0.1)==
                         as.character(DEMO$PROLIFIC_PID),"Yes","No")
# remove any that = no
DEMO <- DEMO[DEMO$IDCHECK == 'Yes' ,]

##### DATA WRANGLE #####
## WILL NEED EDITING WHEN IT COMES TO REAL DATA

# merge
DVDAT <- merge(EXP, DEMO, by = 'ID')
# count again
Nnew <- count(DVDAT, 'ID')

###### CHECKING CATCH #####
# isolating catch
# defining trials
DVDAT$trial_type <- factor(DVDAT$trial_type)
levels(DVDAT$trial_type) <- c('catch','no','go')

# grouping
CATCH <- DVDAT[DVDAT$trial_type == 'catch' ,]
CATCH_CK <- aggregate(correct_keyboard_response~ID*sound, sum, data = CATCH)
CATCH_CK <- dcast(ID~sound, value.var = "correct_keyboard_response", data=CATCH_CK)

CATCH_CK$GO_FILT <- CATCH_CK$GO < 4
CATCH_CK$STOP_FILT <- CATCH_CK$STOP < 2
CATCH_CK$FILTER <- CATCH_CK$GO_FILT|CATCH_CK$STOP_FILT

# make data-frame of participants who failed quality check
QC_FAIL1 <- CATCH_CK[CATCH_CK$GO_FILT == 'TRUE' | CATCH_CK$STOP_FILT == 'TRUE' ,]
ID_X <- as.character(CATCH_CK[CATCH_CK$FILT == TRUE, "ID"])

####### CHECKING CALIB #######
CAL <- DVDAT[, c(1,13:48)]
# calibration values for each participant
CAL$ID <- factor(CAL$ID)
CALDAT <- aggregate(.~ID, median, data = CAL, na.action = na.pass)

# melt data for plotting
CALDAT <- reshape2::melt(CALDAT, value.name = 'LOC')
CALDAT$COND <- factor(substr(CALDAT$variable, 7, 9))
CALDAT$AXIS <- factor(substr(CALDAT$variable, 14, 14))
CALDAT$DOT <- factor(substr(CALDAT$variable, 11, 12))

# reshape again - condition
XDAT <- CALDAT[CALDAT$AXIS == 'x' ,]
YDAT <- CALDAT[CALDAT$AXIS == 'y' ,]
names(XDAT)[3] <- 'LOC_X'
names(YDAT)[3] <- 'LOC_Y'

CALPLOT <- merge(XDAT, YDAT, by = c('ID','DOT','COND'))
CALPLOT <- CALPLOT[, c(1:3,5,8)]

# plot
ggplot(CALPLOT, aes(LOC_X, LOC_Y, colour = DOT, shape = COND)) +
  geom_point() +
  scale_shape_manual(values = c(1, 3)) +
  facet_wrap(~ID)

# get data for linear regression
# group by COND
CLK <- CALDAT[CALDAT$COND == 'clk' ,]
DOT <- CALDAT[CALDAT$COND == 'dot' ,]
names(CLK)[3] <- 'LOC_CLK'
names(DOT)[3] <- 'LOC_DOT'

CALFIT <- merge(CLK, DOT, by = c('ID','DOT','AXIS'))
CALFIT <- CALFIT[, c(1:3,5,8)]

# find and remove all participants with a missing value
NONA <- aggregate(LOC_CLK~ID, mean, data = CALFIT, na.action = na.omit)
NONA <- NONA[, 1]
# filtering participants with missing data
CALNA <- CALFIT[!CALFIT$ID %in% NONA ,]
CALNA <- dcast(ID~AXIS, value.var = 'LOC_DOT', data = CALNA)
ID_X <- c(ID_X, as.character(CALNA$ID))

CALFIT <- CALFIT[CALFIT$ID %in% NONA ,]
CALFIT$ID <- factor(CALFIT$ID) #resetting levels


# THEN fit with linear regression below 
# identify Rsq for each participant and see if fit >.9

CAL_CK <- read.csv(text = c('ID,NUMTRIALS,rsq'))

for(ID in levels(CALFIT$ID)){
    tmp <- CALFIT[CALFIT$ID == ID, c("LOC_CLK","LOC_DOT")]
    model <- lm(LOC_CLK~LOC_DOT, data=tmp)
    NUMTRIALS <- nrow(tmp)
    rsq <- summary(model)$r.squared
    #add to dataframe
    CAL_CK <- rbind(CAL_CK, cbind.data.frame(ID,NUMTRIALS,rsq))
}

## participants whose cal_fit < .9
CAL_CK$FILTER <- CAL_CK$rsq < .90
# adding to ID_X data-frame for later removal
ID_X <- c(ID_X, as.character(CAL_CK[CAL_CK$FILT == TRUE, "ID"]))

###### PP INFO #####
# create data-frame of failed participants first
FDAT <- DVDAT[DVDAT$ID %in% ID_X ,]
# remove all failed participants from DVDAT
DVDAT <- DVDAT[!(DVDAT$ID %in% ID_X), ]
Nfilt <- count(DVDAT, 'ID')

# identify PIDs of participants who did & didnot not pass QC
PASS <- aggregate(correct~ID*PROLIFIC_PID*X1.1, mean, data = DVDAT)
FAIL <- aggregate(correct~ID*PROLIFIC_PID*X1.1, mean, data = FDAT)
FAIL <- merge(FAIL, CATCH_CK, by = 'ID') #need to state how many they got incorrect

### not needed but might want to remember how to do this
# one participant with missing data - y2 (middle), find
#MISS <- CAL[(is.na(CAL$calib_clk_y2_y)) ,]
#MISS <- MISS[1,1]
# add this participant to data-frame of missing IDs
#ID_X <- c(ID_X, MISS)

###### ANALYSIS ######
###### adapt this bit of code for main analysis script when have all the data
## keep only relevant data in df
DVDAT <- DVDAT[, c(1,8,13:48,51,61,62,74,75,151,168,169,171,174,179,191,201,207,
                   210,211,221,229,278,296,298,320,321,323)]

# making data-frame nicer
colnames(DVDAT)[which(names(DVDAT) == "response_time_keyboard_response")] <- "RT"
colnames(DVDAT)[which(names(DVDAT) == "correct_keyboard_response")] <- "CORR"
colnames(DVDAT)[which(names(DVDAT) == "X1.1")] <- "GEN"
colnames(DVDAT)[which(names(DVDAT) == "X1.2")] <- "AGE"
DVDAT$ID <- factor(DVDAT$ID)
DVDAT$ID <- factor(as.numeric(DVDAT$ID))

# remove catch
DVDAT <- DVDAT[!DVDAT$trial_type == 'catch' ,]
# transforming pip3 & pip4 into 1 condition 
DVDAT$sound <- factor(DVDAT$sound)
DVDAT$COND <- factor(substr(DVDAT$sound, 1, 1))
levels(DVDAT$COND) <- c('blank','pip')

# reorganise - important vars upfront
DVDAT <- DVDAT[, c(1,60,61,58,63,48,52,51,40,41,45,2:39,42:44,46,47,49,50,53:57,59,62)]
write.csv(DVDAT, 'PIPmanip_allDat.csv', row.names = FALSE)  

# analysing 'go' trials
GO <- DVDAT[DVDAT$trial_type == 'go' ,]
GO <- GO[GO$CORR == 1 ,]

# plotting
ggplot(GO) +
  geom_density(aes(RT, colour = COND), size = 1) +
  labs(title = 'All trials')

# get stats - CORR
ACC <- aggregate(CORR ~ COND*ID*trial_type, mean, data = DVDAT)
ggplot(ACC, aes(trial_type, CORR, colour = COND)) + 
  geom_violin() +
  labs(title = 'All trials')

## SUM STATS
# response time
RT_GO1 <- aggregate(RT ~ COND*ID, median, data = GO)
RT_GO <- dcast(ID~COND, value.var = 'RT', data = RT_GO1)
RTstats <- summarySEwithin(RT_GO1, measurevar = 'RT', withinvars = 'COND')

RT_GO$DIFF <- RT_GO$pip - RT_GO$blank
write.csv(RT_GO, 'PIPmanip_GOcorrect_RT.csv', row.names = FALSE)

res <- wilcox.test(RT_GO$blank, RT_GO$pip, paired = TRUE, alternative = "two.sided")
res #non-sig
# effsize
ES_RT <- mean(RT_GO$DIFF)/sd(RT_GO$DIFF)
ES_RT

# accuracy


###### PLOTTING ######
library(raincloudplots)
library(cowplot)
library(dplyr)
library(readr)

gitPath <- '/Users/alex/Documents/GitHub/'
setwd(gitPath)
source("RainCloudPlots/tutorial_R/R_rainclouds.R")

# response time - option 1
RT_SUM <- summarySEwithin(data = RT_GO1, measurevar = 'RT', withinvars = 'COND')

ggplot(RT_GO1, aes(COND, RT, fill = COND, colour = COND)) +
  geom_flat_violin(aes(fill = COND), position = position_nudge(x = 0.1, y = 0),
                   adjust = 2, alpha = .5) +
  geom_point(aes(x = as.numeric(COND)-.15, y = RT, colour = COND),
             position = position_jitter(width = .1, height = 0), size = .75) +
  stat_summary(aes(y = RT, group = 1), fun = mean, colour = "black", 
               position = position_nudge(x = 0.1, y = 0),
              geom = 'point', shape = 1, size = 4, group = 1) +
  geom_errorbar(data = RT_SUM, 
                aes(x = as.numeric(COND)+.1, y = RT, 
                    ymin = RT-ci, ymax = RT+ci), width = .05, colour = 'black') +
  theme_classic() + theme(legend.position = 'none') +
  scale_colour_manual(values = c('grey60', 'grey60')) + 
  scale_fill_manual(values = c('grey60', 'grey60')) +
  ylab('Response Time (ms)') + xlab('Condition') + ylim(300,800) -> RTplot1

ggsave('RT_FIG2.png', plot = last_plot(), device = NULL, path = APath, dpi = 300,
       width = 4, height = 4)


# option 2
ggplot(RT_GO1, aes(COND, RT, fill = COND, colour = COND, group = ID)) +
  geom_point(aes(x = COND, y = RT, colour = COND),
             position = position_dodge(.2), size = 1.5) +
  geom_line(aes(group = ID), alpha = .5, size = 0.7, position = position_dodge(.2)) +
  stat_summary(aes(y = RT, group = 1), fun = mean, colour = "black", 
               geom = 'point', shape = 3, stroke = 1, size = 4, group = 1) +
  theme_classic() + ylim(300,800) +
  scale_colour_manual(values = c('grey65', 'grey65')) +
  theme(legend.position = 'none') -> RTplot2

ggsave('RT_FIG2.png', plot = last_plot(), device = NULL, path = APath, dpi = 300,
       width = 4, height = 4)

# option 3
ggplot(RT_GO1, aes(COND, RT, fill = COND, colour = COND)) +
  geom_boxplot(aes(x = as.factor(COND), y = RT), outlier.shape = 1, 
               alpha = 0.3, width = .1, colour = "BLACK") +
  theme_classic() + theme(legend.position = 'none') +
  scale_colour_manual(values = c('grey60', 'grey60')) + 
  scale_fill_manual(values = c('grey60', 'grey60')) +
  ylab('Response Time (ms)') + xlab('Condition')

ggsave('RT_FIG.png', plot = last_plot(), device = NULL, path = APath, dpi = 300,
       width = 4, height = 4)

### FIRST 40 TRIALS FOR EACH COND ###
# to match LBT
colnames(DVDAT)[which(names(DVDAT) == "count_block_sequence")] <- "COUNT"
# first extract data-frame for each condition
BLNK <- DVDAT[DVDAT$COND == 'blank' ,]
PIP <- DVDAT[DVDAT$COND == 'pip' ,]
# make sure in right order
BLNK <- BLNK[with(BLNK, order(ID, COUNT)), ]
PIP <- PIP[with(PIP, order(ID, COUNT)), ]

# then extract first 40 trials from each condition - use dlpyr
BLNK40 <-
  BLNK %>% 
  group_by(ID) %>% 
  filter(row_number()<41)
PIP40 <-
  PIP %>% 
  group_by(ID) %>% 
  filter(row_number()<41)

# check
count(BLNK40, 'ID')
count(PIP40, 'ID')
# combine
FIRST40 <- rbind(BLNK40, PIP40)

# get stats - RT
# analysing 'go' trials
GO40 <- FIRST40[FIRST40$trial_type == 'go' ,]
GO40 <- GO40[GO40$CORR == 1 ,]

# summary stats - medians
RT_GO1 <- aggregate(RT ~ COND*ID, median, data = GO40)
RT_GO40 <- dcast(ID~COND, value.var = 'RT', data = RT_GO1)
RT40stats <- summarySEwithin(RT_GO1, measurevar = 'RT', withinvars = 'COND')
   
## effect size
RT_GO40$DIFF <- RT_GO40$pip - RT_GO40$blank
D40 <- mean(RT_GO40$DIFF)/sd(RT_GO40$DIFF)

write.csv(RT_GO40, 'PIPmanip_GOcorrect_RTfirst40.csv', row.names = FALSE)

res <- wilcox.test(RT_GO40$blank, RT_GO40$pip, paired = TRUE, alternative = "two.sided")
res #non-sig

ggplot(GO40) +
  geom_density(aes(RT, colour = COND), size = 1) +
  labs(title = 'First 40 trials')

ggplot(GO40, aes(COND, RT)) + 
  geom_violin()

# get stats - CORR
ACC40 <- aggregate(CORR ~ COND*ID*trial_type, mean, data = FIRST40)
ggplot(ACC40, aes(trial_type, CORR, colour = COND)) + 
  geom_violin() +
  labs(title = 'First 40')



### MID 40 TRIALS FOR EACH COND ###
BLNK40 <-
  BLNK %>% 
  group_by(ID) %>% 
  filter(row_number()>41)
PIP40 <-
  PIP %>% 
  group_by(ID) %>% 
  filter(row_number()>41)
# then reducing to onnly 40
BLNK40 <-
  BLNK40 %>% 
  group_by(ID) %>% 
  filter(row_number()<41)
PIP40 <-
  PIP40 %>% 
  group_by(ID) %>% 
  filter(row_number()<41)

# check
count(BLNK40, 'ID')
count(PIP40, 'ID')
# combine
MID40 <- rbind(BLNK40, PIP40)

# get stats - RT
# analysing 'go' trials
GO240 <- MID40[MID40$trial_type == 'go' ,]
GO240 <- GO240[GO240$CORR == 1 ,]

# summary stats - medians
RT_GO1 <- aggregate(RT ~ COND*ID, median, data = GO240)
RT_GO240 <- dcast(ID~COND, value.var = 'RT', data = RT_GO1)
RT240stats <- summarySEwithin(RT_GO1, measurevar = 'RT', withinvars = 'COND')

res <- wilcox.test(RT_GO240$blank, RT_GO240$pip, paired = TRUE, alternative = "two.sided")
res #non-sig

ggplot(GO240) +
  geom_density(aes(RT, colour = COND), size = 1) +
  labs(title = 'Mid 40 trials')

ggplot(GO240, aes(COND, RT)) + 
  geom_violin()

# get stats - CORR
ACC240 <- aggregate(CORR ~ COND*ID*trial_type, mean, data = MID40)
ggplot(ACC240, aes(trial_type, CORR, colour = COND)) + 
  geom_violin() +
  labs(title = 'First 40')

