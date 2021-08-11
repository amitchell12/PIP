library(lme4)
library(reshape2)
library(ggplot2)
library(Rmisc)
library(psych)
library(tidyverse)

# getting data frames - mac
#bisectData <- read.csv("/Users/alex/OneDrive - University of Edinburgh/Experiments/CuedEWS/Code/LBT_jatos.csv")
#demoData <- read.csv("/Users/alex/OneDrive - University of Edinburgh/Experiments/CuedEWS/Code/LBT_qualtrics.csv") 
# getting data frames - windows
dataPath <- 'C:/Users/amitch17/OneDrive - University of Edinburgh/Experiments/CuedEWS/Data'
setwd(dataPath)

bisectData <- read.csv("LBT_jatos.csv")
demoData <- read.csv("LBT_qualtrics.csv") 

#check unique paritcipant IDs
nlevels(demoData$ResponseId)
nlevels(bisectData$qualtrics_id)

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
megaDat <- megaDat[, c(1,318,319,10:30,32:34,38,50,52,54,89,118,119,126:143,159:162,
                       165,166,169,171,173:175,179,204,210,211,214,215,221,222,294,
                       296:298,320:329)]

#how many catch_trials
ncatch <- aggregate(Q4~qualtrics_id, length, 
                    data=megaDat[megaDat$trial_type=="catch", ])

# plotting response time to line
ggplot(megaDat, aes(x=response_time_line_mouse_response, fill=trial_type)) + 
  geom_histogram(position = "identity", alpha=.5) +
  xlim(c(750,2100))

# identify trials where response was > the time out
megaDat$timeout <- as.numeric(megaDat$response_time_line_mouse_response >= 2000)

timeouts <- aggregate(timeout~trial_type*qualtrics_id, mean, data=bisectData)
timeouts <- dcast(qualtrics_id~trial_type, value.var = "timeout", data=timeouts)

timeouts$CATCH_filter <- timeouts$catch < .5
timeouts$GoH_filter <- timeouts$go > .2
timeouts$FILTER <- timeouts$CATCH_filter|timeouts$GoH_filter
sum(timeouts$FILTER)

ID_X <- c(ID_X, as.character(timeouts[timeouts$FILTER == TRUE, "qualtrics_id"]))

#exclude participants whole-study-wise
demoData <- demoData[!(demoData$ResponseId %in% ID_X), ]
bisectData<- bisectData[!(bisectData$qualtrics_id %in% ID_X), ]

#recode ID
bisectData$ID <- factor(as.numeric(bisectData$qualtrics_id))
bisectData <- bisectData[order(bisectData$ID), ]
rownames(bisectData) <- NULL

#recode P
#bisectData$P <- (bisectData$cursor_x - bisectData$calib_loc_midy_x)/bisectData$pix_permm
bisectData$P <- bisectData$cursor_x/bisectData$pix_permm
bisectData$L <- bisectData$left_mm
bisectData$R <- bisectData$right_mm

#IDENTIFY EXPERIMENT
bisectData$XPT <- "TOT"
bisectData[is.na(bisectData$count_block_2_sequence), "count_block_2_sequence"] <- -999
bisectData[bisectData$count_block_2_sequence >= 0 & is.na(bisectData$count_block_3_sequence), "XPT"] <- "PIP"

#check XPT id worked
#tst <- bisectData[bisectData$ID==1, c("count_block_2_sequence", "XPT")]

TOT_dat <- bisectData[bisectData$XPT=="TOT", ]
TOT_dat$BLOCK <- "LATE" 
TOT_dat[is.na(TOT_dat$count_block_3_sequence), "BLOCK"] <- "EARLY" 
TOT_dat$BLOCK <- factor(TOT_dat$BLOCK)

#SUMMARISE AND THEN REMOVE CATCH TRIALS
TOT_dat <- TOT_dat[TOT_dat$trial_type=="go", ]
TOT_dat <- TOT_dat[TOT_dat$timeout!=TRUE, ]


#PIP
PIP_dat <- bisectData[bisectData$XPT=="PIP",]

#diagnostic
#table(PIP_dat$trial_type, PIP_dat$sound)

PIP_dat <- PIP_dat[PIP_dat$trial_type=="go", ]
PIP_dat <- PIP_dat[PIP_dat$timeout!=TRUE, ]
PIP_dat$TONE <- PIP_dat$sound != "blank"
PIP_dat$TONE <- factor(PIP_dat$TONE, labels = c("BLANK", "TONE"))

#diagnostic
#table(PIP_dat$trial_type, PIP_dat$sound, PIP_dat$TONE)

#create empty dataframe for DVs
TOT_DV <- read.csv(text="ID,BLOCK,NUMTRIALS,rsq,k,dPL,dPR")

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
    TOT_DV <- rbind(TOT_DV, cbind.data.frame(ID,BLOCK,NUMTRIALS,rsq,k,dPL,dPR))
  }
}

#calculate composites
TOT_DV$EWB <- TOT_DV$dPR-TOT_DV$dPL
TOT_DV$EWS <- TOT_DV$dPR+TOT_DV$dPL
TOT_DV$FILTER <- TOT_DV$rsq < .7

TOT_DV <- TOT_DV[TOT_DV$FILTER != TRUE, ]

TOT_DV_w <- dcast(ID~BLOCK, value.var = "EWS", data=TOT_DV)

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