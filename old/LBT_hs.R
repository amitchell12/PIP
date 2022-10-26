library(lme4)
library(reshape2)
library(ggplot2)
library(Rmisc)
library(psych)
library(tidyverse)

# getting data frames - mac
bisectData <- read.csv("/Users/alex/OneDrive - University of Edinburgh/Experiments/CuedEWS/Code/LBT_jatos.csv")
demoData <- read.csv("/Users/alex/OneDrive - University of Edinburgh/Experiments/CuedEWS/Code/LBT_qualtrics.csv") 
# getting data frames - windows

# get demographic data for each ppt and merge with bisection data
demoData$qualtrics_id <- demoData$ResponseId
bisectData <- merge(bisectData, demoData, by = 'qualtrics_id')

#demographics reporting
mean(demoData$Q5) #age = 32.83
sd(demoData$Q5) #11.93

sum(demoData$Q4 == "Female") #68
sum(demoData$Q4 == "Male") #41
sum(demoData$Q4 == "Non-binary / third gender") #1

### REMOVE CATCH/VOID TRIALS ###

#catch
bisectData$mouse_response_time <- bisectData$response_time_line_mouse_response
catch_df <- filter(bisectData, bisectData$sound == "STOP")
testTime <- aggregate(mouse_response_time ~ qualtrics_id, max, data = catch_df)
#refresh rate fine as all > 2000
catch_df$catch <- ifelse(catch_df$mouse_response_time <2000,
                         "incorrect", "correct")

catch_df_wide <- dcast(catch_df, qualtrics_id ~ catch, 
                       value.var="catch", fun.aggregate=length)

bisectData1 <- bisectData[-c(333:836, 1169:1134, 1667:1998, 
                              2498:2726, 3391:4410, 4743:4908,
                              5075:5406, 5573: 5904, 6071:6236,
                              6569:6993, 7160:7327, 7494:7659,
                              8491:8656, 8823:8988, 9155:9546,
                              10045:10456, 11211:11376, 11543:12040,
                             12207:12539, 12872:13203, 13370:14199,
                             14366:14351, 14698:15196, 16119:16645,
                             16978:17143, 17476:17641, 17974:18172,
                             18339:18671, 18838:19003),]
##54 pps removed:
#10 < 17/28 correct
#2 had too few trials
#the rest had >28 catch trials

#void
bisectData1 <- filter(bisectData, bisectData$void_trial == 0)


#splitting cleaned data by block
block1 <- subset(bisectData1, count_block_1_3_sequence >= 0 & is.na(count_block_2_sequence))

# checking it works by counting total trials in data-frame
b1check <- count(block1, qualtrics_id)

block2 <- subset(bisectData1, count_block_2_sequence >= 0 & is.na(count_block_3_sequence))
# checking it works by counting total trials in data-frame
b2check <- count(block2, qualtrics_id)

block3 <- subset(bisectData, count_block_3_sequence >= 0)
# checking it works by counting total trials in data-frame
b3check <- count(block3, qualtrics_id)

##HYPOTHESIS 1: PIP ##

##### EWB CALCULATE #####
# calculate P
block2$P <- (block2$cursor_x - block2$calib_loc_midy_x)/block2$pix_permm

# calculate and extract end-point weightings for each participant using linear regression
# use large trial by trial data frame to do this
EWmodel <- lmList(P ~ left_mm + right_mm | qualtrics_id, data = block2)
dPL <- coefficients(EWmodel)[2]
dPR <- coefficients(EWmodel)[3]
k <- coefficients(EWmodel)[1]
rsq <- summary(EWmodel)$r.squared

# converting to data-frame
EPW = data.frame(ID = row.names(dPL), dPL, dPR, k, rsq)
# rename columns
names(EPW)[2] <- 'dPL'
names(EPW)[3] <- 'dPR'
names(EPW)[4] <- 'k'
names(EPW)[5] <- 'rsq'

## summary stats
# summary P for each participant
EWdata <- aggregate(P ~ qualtrics_id, mean, data = block2)

# add dPL and dPR
EWdata <- merge(EWdata, EPW, by = 'ID')
# caculating EWB and EWS
allData$EWS <- allData$dPR + allData$dPL
allData$EWB <- allData$dPR - allData$dPL

## EWS AND EWB FILTERS TO BE APPLIED HERE
EWdata$LB_FILTER <- (EWdata$rsq >= .7 & EWdata$EWS > .5 &
                       abs(EWdata$EWB) < .5)
# getting means
EWB <- summarySE(data = allData, measurevar = 'EWB')
EWS <- summarySE(data = allData, measurevar = 'EWS')

bisectData$L <- bisectData$left_mm
bisectData$R <- bisectData$right_mm
#t-test here

## HYPOTHESIS 2: TIME-ON-TASK ##

##### EWB CALCULATE #####
# calculate P
block2$P <- (block2$cursor_x - block2$calib_loc_midy_x)/block2$pix_permm

# calculate and extract end-point weightings for each participant using linear regression
# use large trial by trial data frame to do this
EWmodel <- lmList(P ~ left_mm + right_mm | qualtrics_id, data = block2)
dPL <- coefficients(EWmodel)[2]
dPR <- coefficients(EWmodel)[3]
k <- coefficients(EWmodel)[1]
rsq <- summary(EWmodel)$r.squared

# converting to data-frame
EPW = data.frame(ID = row.names(dPL), dPL, dPR, k, rsq)
# rename columns
names(EPW)[2] <- 'dPL'
names(EPW)[3] <- 'dPR'
names(EPW)[4] <- 'k'
names(EPW)[5] <- 'rsq'

## summary stats
# summary P for each participant
EWdata <- aggregate(P ~ qualtrics_id, mean, data = block2)

# add dPL and dPR
EWdata <- merge(EWdata, EPW, by = 'ID')
# caculating EWB and EWS
allData$EWS <- allData$dPR + allData$dPL
allData$EWB <- allData$dPR - allData$dPL

## EWS AND EWB FILTERS TO BE APPLIED HERE
EWdata$LB_FILTER <- (EWdata$rsq >= .7 & EWdata$EWS > .5 &
                       abs(EWdata$EWB) < .5)
# getting means
EWB <- summarySE(data = allData, measurevar = 'EWB')
EWS <- summarySE(data = allData, measurevar = 'EWS')

bisectData$L <- bisectData$left_mm
bisectData$R <- bisectData$right_mm

#t-test here