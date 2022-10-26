#reading data
jatos <- read.csv('JATOS_RT.csv')
qual <- read.csv("qualtrics_RT.csv")

#loading packages
library(psych)
library(tidyverse)
library(ggplot2)
library(pwr)
library(reshape2)
library(ggpubr)

#combining data frames
rename(qual, qualtrics_ID = ResponseId)
qual$qualtrics_id <- qual$ResponseId

df <- merge(qual, jatos, by = "qualtrics_id", sort = T)
df <- df[-c(721:960, 3361:3600, 4561:4800, 5041:5071),] #removing incomplete observations

EHI <- c(100,
         66,
         100,
         70,
         60,
         64,
         80,
         -23,
         82,
         80,
         100,
         100,
         82,
         100,
         66,
         100,
         82,
         82,
         100,
         100,
         66,
         100,
         -82,
         100,
         -64,
         23,
         40,
         66,
         100,
         82
)

df$EHI <- EHI
df$EHI <- factor(df$EHI)

#getting averages
df_rt <- aggregate(df$response_time_keyboard_response, by = list(df$qualtrics_id, df$stim_shape, df$sound, df$trial_type, df$Q4, df$Q5), FUN = mean)

df_rt <- rename(df_rt, subject = Group.1, stim_shape = Group.2, sound = Group.3, trial_type = Group.4, sex = Group.5, age = Group.6, rt = x)

df_acc <- aggregate(df$accuracy, by = list(df$qualtrics_id, df$stim_shape, df$sound, df$trial_type, df$Q4, df$Q5), FUN = mean)

df_acc <- rename(df_acc, subject = Group.1, stim_shape = Group.2, sound = Group.3, trial_type = Group.4, sex = Group.5, age = Group.6, acc = x)

#long to wide format
df_rt_wide <- dcast(df_rt, subject + sex + age ~ trial_type + sound, value.var="rt")
df_rt_wide$sex <- factor(df_rt_wide$sex)
df_rt_wide$subject <- factor(df_rt_wide$subject)
df_rt_wide$age <- as.numeric(df_rt_wide$age)

df_acc_wide <- dcast(df_acc, subject + sex + age ~ trial_type + sound, value.var="acc")
df_acc_wide$sex <- factor(df_acc_wide$sex)
df_acc_wide$subject <- factor(df_acc_wide$subject)
df_acc_wide$age <- as.numeric(df_acc_wide$age)

#descriptive statistics
describe(df_acc_wide)
describe(df_rt_wide)

#visualising here

#t-test RT tone vs no tone

shapiro.test(df_rt_wide$go_blank) #non-normal
shapiro.test(df_rt_wide$go_pip1) #non-normal
shapiro.test(df_rt_wide$go_pip2) #non-normal

res <- wilcox.test(df_rt_wide$go_blank, df_rt_wide$go_pip1, paired = TRUE, alternative = "two.sided")
res #non-sig

res1 <- wilcox.test(df_rt_wide$go_blank, df_rt_wide$go_pip2, paired = TRUE, alternative = "two.sided")
res1 #non-sig

#t-test acc tone vs no tone (DOESNT RUN YET)

shapiro.test(df_acc_wide$go_blank) #non-normal
shapiro.test(df_acc_wide$go_pip1) #non-normal
shapiro.test(df_acc_wide$go_pip2) #non-normal

res <- wilcox.test(df_acc_wide$go_blank, df_acc_wide$go_pip1, paired = TRUE, alternative = "two.sided")
res #non-sig

res1 <- wilcox.test(df_acc_wide$go_blank, df_acc_wide$go_pip2, paired = TRUE, alternative = "two.sided")
res1 #non-sig
