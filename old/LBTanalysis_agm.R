library(lme4)
library(reshape2)
library(ggplot2)
library(Rmisc)
library(psych)
library(tidyverse)
library(data.table)
library(maditr)

# getting data frames - mac
dataPath <- "/Users/alex/OneDrive - University of Edinburgh/Experiments/CuedEWS/Data"
# windows
#dataPath <- 'C:/Users/amitch17/OneDrive - University of Edinburgh/Experiments/CuedEWS/Data'
setwd(dataPath)

bisectData <- read.csv("LBT_jatos.csv")
demoData <- read.csv("LBT_qualtrics.csv") 

##### DATA ORGANISATION & FILTERING #####
#check unique paritcipant IDs
nlevels(factor(demoData$ResponseId))
nlevels(factor(bisectData$qualtrics_id))

#how many trials per participant
ntrials <- aggregate(acc~qualtrics_id, length, data=bisectData)

#make a vector of participants with too many or too few trials (>175)
ID_X <- as.character(ntrials[ntrials$acc>175 | ntrials$acc < 160, "qualtrics_id"])

#exclude participants
demoData <- demoData[!(demoData$ResponseId %in% ID_X), ]
bisectData<- bisectData[!(bisectData$qualtrics_id %in% ID_X), ]

# merge demographics and bisection data
names(demoData)[9] <- 'qualtrics_id'
megaDat <- merge(bisectData, demoData, by = 'qualtrics_id')
# limit to required columns
megaDat <- megaDat[, c(1,317,318,10:30,32:34,38,50,52,54,89,118,119,126:143,159:162,
                       165,166,169,171,173:175,179,204,210,211,214,215,221,222,225,294,
                       296:298,319:328)]

#how many catch_trials
ncatch <- aggregate(Q4~qualtrics_id, length, 
                    data=megaDat[megaDat$trial_type=="catch", ])

# plotting response time to line
ggplot(megaDat, aes(x=response_time_line_mouse_response, fill=trial_type)) + 
  geom_histogram(position = "identity", alpha=.5) +
  xlim(c(750,2100))

# identify trials where response was > the time out
megaDat$timeout <- as.numeric(megaDat$response_time_line_mouse_response >= 2000)

timeouts <- aggregate(timeout~trial_type*qualtrics_id, mean, data=megaDat)
timeouts <- dcast(qualtrics_id~trial_type, value.var = "timeout", data=timeouts)

# justification for these filters?
timeouts$CATCH_filter <- timeouts$catch < .5
timeouts$Go_filter <- timeouts$go > .2
timeouts$FILTER <- timeouts$CATCH_filter|timeouts$Go_filter
sum(timeouts$FILTER)

ID_X <- c(ID_X, as.character(timeouts[timeouts$FILTER == TRUE, "qualtrics_id"]))

#exclude participants whole-study-wise
megaDat <- megaDat[!(megaDat$qualtrics_id %in% ID_X), ]

#recode ID to make more sense :)
megaDat$qualtrics_id <- factor(megaDat$qualtrics_id)
megaDat$ID <- factor(as.numeric(megaDat$qualtrics_id))
megaDat <- megaDat[order(megaDat$ID), ]
rownames(megaDat) <- NULL

##### CHECK CALIBRATION #####
cal <- megaDat[, c(7:24,35:52,88)]
# calibration values for each participant
calibDat <- aggregate(.~ID, median, data = cal)
# renaming so convention works for data wrangling & plotting
setnames(calibDat, 
         old = c('calib_loc_left192x','calib_loc_left320x', 'calib_loc_left480x',
                 'calib_loc_lowery', 'calib_loc_midy', 'calib_loc_right192x',
                 'calib_loc_right320x', 'calib_loc_right480x', 'dot_loc_left192x',
                 'dot_loc_left320x', 'dot_loc_left480x', 'dot_loc_lowery',
                 'dot_loc_midy', 'dot_loc_right192x', 'dot_loc_right320x', 
                 'dot_loc_right480x'), 
         new = c('calib_loc_left192x_x','calib_loc_left320x_x', 'calib_loc_left480x_x',
                 'calib_loc_lowery_y', 'calib_loc_midy_y', 'calib_loc_right192x_x',
                 'calib_loc_right320x_x', 'calib_loc_right480x_x', 'dot_loc_left192x_x',
                 'dot_loc_left320x_x', 'dot_loc_left480x_x', 'dot_loc_lowery_y',
                 'dot_loc_midy_y', 'dot_loc_right192x_x', 'dot_loc_right320x_x', 
                 'dot_loc_right480x_x'))

# melt data for plotting
calibDat <- reshape2::melt(calibDat, value.name = 'RESP')
calibDat$COND <- factor(substr(calibDat$variable, 1, 3))
# dot axes - x and y
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
calibDat$AXIS <- substrRight(as.character(calibDat$variable), 1)
# dot locations
for (var in 1:length(calibDat$variable)){
  if (isTRUE(calibDat$COND[var] == 'cal')){
    calibDat$LOC[var] <- substr(calibDat$variable[var], 11, 
                                nchar(as.character(calibDat$variable[var]))-2)
  }
  else {
    calibDat$LOC[var] <- substr(calibDat$variable[var], 9, 
                                nchar(as.character(calibDat$variable[var]))-2)
  }
}
calibDat <- calibDat[, c(1,3:6)]

# reshape for plotting
Xdat <- calibDat[calibDat$AXIS == 'x' ,]
Ydat <- calibDat[calibDat$AXIS == 'y' ,]
names(Xdat)[2] <- 'RESP_X'
names(Ydat)[2] <- 'RESP_Y'

calPlot <- merge(Xdat, Ydat, by = c('ID','LOC','COND'))

# plot
ggplot(calPlot, aes(RESP_X, RESP_Y, colour = LOC, shape = COND)) +
  geom_point() +
  scale_shape_manual(values = c(3, 1)) +
  facet_wrap(~ID)

CAL <- calPlot[calPlot$COND == 'cal' ,]
DOT <- calPlot[calPlot$COND == 'dot' ,]
names(CAL)[4] <- 'RESP_X_CAL'
names(CAL)[6] <- 'RESP_Y_CAL'
names(DOT)[4] <- 'RESP_X_DOT'
names(DOT)[6] <- 'RESP_Y_DOT'

CAL <- merge(CAL, DOT, by = c('ID','LOC'))
CAL$XERR <- CAL$RESP_X_CAL - CAL$RESP_X_DOT
CAL$YERR <- CAL$RESP_Y_DOT - CAL$RESP_Y_CAL
CAL <- CAL[CAL$YERR < 300 ,]

# summary data
SUM_X <- summarySE(data = CAL, measurevar = 'RESP_X_CAL', withinvars = c('LOC','RESP_X_DOT'))
SUM_Y <- summarySE(data = CAL, measurevar = 'RESP_Y_CAL', withinvars = c('LOC','RESP_Y_DOT'))
SUM_ERRX <- summarySE(data = CAL, measurevar = 'XERR', groupvars = 'LOC')
SUM_ERRY <- summarySE(data = CAL, measurevar = 'YERR', groupvars = 'LOC')

## find out wtf is happening with this discrepancy
HEIGHT <- aggregate(screen_availableHeight ~ ID, median, data = megaDat)
WIDTH <- aggregate(screen_availableWidth ~ ID, median, data = megaDat)
SCREEN <- merge(HEIGHT, WIDTH, by = 'ID')
CAL <- merge(CAL, SCREEN, by = 'ID')
# cleaning
CAL <- CAL[, c(1:4,6,9,11,13:16)]

SCRY_SUM <- summarySE(data = CAL, measurevar = 'YERR', 
                      groupvars = c('LOC', 'screen_availableHeight'))
SCRX_SUM <- summarySE(data = CAL, measurevar = 'XERR', 
                      groupvars = c('LOC', 'screen_availableWidth'))

ggplot(CAL) +
  geom_point(aes(XERR, YERR, colour = LOC))

SCRX_SUM$screen_availableWidth <- factor(SCRX_SUM$screen_availableWidth)
SCRY_SUM$screen_availableWidth <- factor(SCRY_SUM$screen_availableHeight)

ggplot(SCRX_SUM, aes(screen_availableWidth, XERR, colour = LOC)) +
  geom_point(size = 2) +
  theme_bw()

ggplot(SCRY_SUM, aes(screen_availableHeight, YERR, colour = LOC)) +
  geom_point(position = position_dodge(.2))

##### DATA ANALYSIS ####
#recode P
megaDat$P <- megaDat$cursor_x/megaDat$pix_permm
megaDat$L <- megaDat$left_mm
megaDat$R <- megaDat$right_mm

megaDat$P2 <- megaDat$cursor_x
megaDat$L2 <- megaDat$calib_loc_midy_x + megaDat$left_pix
megaDat$R2 <- megaDat$calib_loc_midy_x + megaDat$right_pix

#IDENTIFY EXPERIMENT
megaDat$XPT <- "TOT"
megaDat[is.na(megaDat$count_block_2_sequence), "count_block_2_sequence"] <- -999
megaDat[megaDat$count_block_2_sequence >= 0 & is.na(megaDat$count_block_3_sequence), "XPT"] <- "PIP"

#check XPT id worked
#tst <- bisectData[bisectData$ID==1, c("count_block_2_sequence", "XPT")]

TOT_dat <- megaDat[megaDat$XPT=="TOT", ]
TOT_dat$BLOCK <- "LATE" 
TOT_dat[is.na(TOT_dat$count_block_3_sequence), "BLOCK"] <- "EARLY" 
TOT_dat$BLOCK <- factor(TOT_dat$BLOCK)

#SUMMARISE AND THEN REMOVE CATCH TRIALS
TOT_dat <- TOT_dat[TOT_dat$trial_type=="go", ]
TOT_dat <- TOT_dat[TOT_dat$timeout!=TRUE, ]

#PIP
PIP_dat <- megaDat[megaDat$XPT=="PIP",]

#diagnostic
#table(PIP_dat$trial_type, PIP_dat$sound)

PIP_dat <- PIP_dat[PIP_dat$trial_type=="go", ]
PIP_dat <- PIP_dat[PIP_dat$timeout!=TRUE, ]
PIP_dat$TONE <- PIP_dat$sound != "blank"
PIP_dat$TONE <- factor(PIP_dat$TONE, labels = c("BLANK", "TONE"))

#diagnostic
#table(PIP_dat$trial_type, PIP_dat$sound, PIP_dat$TONE)

## FIRST ANALYSIS - USING MM
#create empty dataframe for DVs
TOT_DV1 <- read.csv(text="ID,BLOCK,NUMTRIALS,rsq,k,dPL,dPR")

for(ID in levels(TOT_dat$ID)){
  for(BLOCK in levels(TOT_dat$BLOCK)){
    tmp <- TOT_dat[TOT_dat$ID == ID & TOT_dat$BLOCK==BLOCK, c("cursor_x","pix_permm","P","L","R")]
    model <- lm(P~L+R, data=tmp)
    NUMTRIALS <- nrow(tmp)
    rsq <- summary(model)$r.squared
    k <- as.numeric(coefficients(model)[1])
    dPL <- as.numeric(coefficients(model)[2])
    dPR <- as.numeric(coefficients(model)[3])
    #add to dataframe
    TOT_DV1 <- rbind(TOT_DV, cbind.data.frame(ID,BLOCK,NUMTRIALS,rsq,k,dPL,dPR))
  }
}

## SECOND ANALYSIS - USING PIX
TOT_DV2 <- read.csv(text="ID,BLOCK,NUMTRIALS,rsq,k,dPL,dPR")

for(ID in levels(TOT_dat$ID)){
  for(BLOCK in levels(TOT_dat$BLOCK)){
    tmp <- TOT_dat[TOT_dat$ID == ID & TOT_dat$BLOCK==BLOCK, c("cursor_x","pix_permm","P2","L2","R2")]
    model <- lm(P2~L2+R2, data=tmp)
    NUMTRIALS <- nrow(tmp)
    rsq <- summary(model)$r.squared
    k <- as.numeric(coefficients(model)[1])
    dPL <- as.numeric(coefficients(model)[2])
    dPR <- as.numeric(coefficients(model)[3])
    #add to dataframe
    TOT_DV2 <- rbind(TOT_DV, cbind.data.frame(ID,BLOCK,NUMTRIALS,rsq,k,dPL,dPR))
  }
}

#calculate composites
TOT_DV2$EWB <- TOT_DV2$dPR-TOT_DV2$dPL
TOT_DV2$EWS <- TOT_DV2$dPR+TOT_DV2$dPL
TOT_DV2$FILTER <- TOT_DV2$rsq < .7

TOT_DV2 <- TOT_DV2[TOT_DV2$FILTER != TRUE, ]

TOT_DV_w <- data.table::dcast(ID~BLOCK, value.var = "EWS", data=TOT_DV2)

TOT_DV_w$D <- TOT_DV_w$EARLY-TOT_DV_w$LATE

TOT_DV_w <- TOT_DV_w[complete.cases(TOT_DV_w),]

describe(TOT_DV_w$D)

t.test(TOT_DV_w$D, mu=0)
mean(TOT_DV_w$D)/sd(TOT_DV_w$D)



#create empty dataframe for DVs
PIP_DV <- read.csv(text="ID,TONE,NUMTRIALS,rsq,k,dPL,dPR")

for(ID in levels(PIP_dat$ID)){
  for(TONE in levels(PIP_dat$TONE)){
    tmp <- PIP_dat[PIP_dat$ID == ID & PIP_dat$TONE==TONE, c("P","L","R")]
    model <- lm(P~L+R, data=tmp)
    NUMTRIALS <- nrow(tmp)
    rsq <- summary(model)$r.squared
    k <- as.numeric(coefficients(model)[1])
    dPL <- as.numeric(coefficients(model)[2])
    dPR <- as.numeric(coefficients(model)[3])
    #add to dataframe
    PIP_DV <- rbind(PIP_DV, cbind.data.frame(ID,TONE,NUMTRIALS,rsq,k,dPL,dPR))
  }
}

#calculate composites
PIP_DV$EWB <- PIP_DV$dPR-PIP_DV$dPL
PIP_DV$EWS <- PIP_DV$dPR+PIP_DV$dPL
PIP_DV$FILTER <- (PIP_DV$rsq < .7 | PIP_DV$NUMTRIALS < 20)

PIP_DV <- PIP_DV[PIP_DV$FILTER != TRUE, ]

PIP_DV_w <- dcast(ID~TONE, value.var = "EWS", data=PIP_DV)

PIP_DV_w$D <- PIP_DV_w$TONE-PIP_DV_w$BLANK

PIP_DV_w <- PIP_DV_w[complete.cases(PIP_DV_w),]

describe(PIP_DV_w$D)

t.test(PIP_DV_w$D, mu=0)
mean(PIP_DV_w$D)/sd(PIP_DV_w$D)