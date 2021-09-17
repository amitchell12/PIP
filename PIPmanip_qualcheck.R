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

# getting files
#DPath <- 'C:/Users/amitch17/OneDrive - University of Edinburgh/Experiments/PIPTOT/Data'
DPath <- '/Users/alex/OneDrive - University of Edinburgh/Experiments/PIPTOT/Data'
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
DEMO$IDCHECK <- ifelse(DEMO$X0.1==DEMO$PROLIFIC_PID,"Yes","No")
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
CATCH_CK$STOP_FILT <- CATCH_CK$STOP < 3
CATCH_CK$FILTER <- CATCH_CK$GO_FILT|CATCH_CK$STOP_FILT

# make data-frame of participants who failed quality check
QC_FAIL1 <- CATCH_CK[CATCH_CK$GO_FILT == 'TRUE' | CATCH_CK$STOP_FILT == 'TRUE' ,]
ID_X <- as.character(CATCH_CK[CATCH_CK$FILT == TRUE, "ID"])

# remove from DVDAT
DVDAT <- DVDAT[!(DVDAT$ID %in% ID_X), ]

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


###### REACHED HERE IN CODE
## checking fit - HOW DO I DO THIS?
# melt data for linear regression - use above code but for cal-dot responses instead
# THEN fit with linear regression below 
# identify Rsq for each participant and see if fit >.9

for(ID in levels(DVDAT$ID)){
  model <- lm( ~L+R, data=tmp)
}

# plot
ggplot(CALPLOT, aes(LOC_X, LOC_Y, colour = DOT, shape = COND)) +
  geom_point() +
  scale_shape_manual(values = c(1, 3)) +
  facet_wrap(~ID)


### not needed but might want to remember how to do this
# one participant with missing data - y2 (middle), find
MISS <- CAL[(is.na(CAL$calib_clk_y2_y)) ,]
MISS <- MISS[1,1]
# add this participant to data-frame of missing IDs
ID_X <- c(ID_X, MISS)


## keep only relevant data in df

GO <- DVDAT[DVDAT$trial_type == 'go' ,]
GO <- GO[GO$correct_keyboard_response == 1 ,]
# plotting
GO$sound <- factor(GO$sound)
ggplot(GO) +
  geom_density(aes(response_time_keyboard_response, colour = sound), size = 1) +
  facet_wrap(~ID)

# summary
RT_GO <- summarySEwithin(data = GO, measurevar = 'response_time_keyboard_response', 
                         withinvars = 'sound')
RT_med <- aggregate(response_time_keyboard_response ~ sound, median, data = GO)

