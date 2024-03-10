#########################
##   P9185 Project1    ##
##   Yanran Li         ##
#########################

library(tidyverse)
library(rstudioapi)
library(dplyr)
library(tidyr)
library(table1)
library(flextable)
library(reshape2)
library(ggplot2)
library(lme4)


## set working directory to wherever this file is located ##
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))


## read in the data with out rowname#
baseline <- read_csv("baseline.csv")
baseline <- baseline[,-1]
endpoints <- read_csv("endpoints.csv")

View(baseline)
View(endpoints)

## 1. Organize data
## Outcome: Y_ijk, i=subject, j=period, k=week
## Covariates:ijk treatment,period,week,sequence,viralload,age,gender,....

# extract the last character of column 2,3,4 and generate a new column "trt_order" for baseline data
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
trt_df <- baseline %>% select(c(2:4))

trt_df <- paste0(substrRight(trt_df$period1,1), substrRight(trt_df$period2,1), substrRight(trt_df$period3,1))

baseline$trt_order <- trt_df



# generate Table 1 for baseline data
names(baseline)
baseline$gender <- as.factor(baseline$gender)
table1<- table1(~ gender + age + race|trt_order, data=baseline)
table1_viral<- table1(~ bviral0  + bviral1+bviral2+bviral3+bviral4+bviral5+bviral6+ sviral0++ sviral1+ sviral2+ sviral3+ sviral4+ sviral5+ sviral6|trt_order, data=baseline)
xtable(as.data.frame(table1))
xtable(as.data.frame(table1_viral))

# melt the data
b_melt <- melt(baseline, id.vars = c("ptid", "trt_order"), 
                      measure.vars = c("bviral0", "bviral1", "bviral2", "bviral3", 
                                       "bviral4", "bviral5", "bviral6"), 
                      variable.name = "week", value.name = "bviral")
b_melt$week <- as.numeric(substr(b_melt$week, 7, 7))
s_melt <- melt(baseline, id.vars = c("ptid", "trt_order"), 
               measure.vars = c("sviral0", "sviral1", "sviral2", "sviral3", "sviral4", "sviral5", "sviral6"), 
               variable.name = "week", value.name = "svital") 
s_melt$week <- as.numeric(substr(s_melt$week, 7, 7))
#order by pid and week
baseline_melt <- merge(b_melt, s_melt, by = c("ptid", "trt_order", "week")) %>% arrange(ptid, week)


baseline_melt_ave <- baseline_melt %>% group_by(trt_order, week) %>% 
  summarise(bviral = mean(bviral), svital = mean(svital))

# plot the viral load

ggplot(baseline_melt_ave, aes(x = week, y = bviral, group = trt_order, color = trt_order)) +
  geom_line() +
  geom_point() +
  labs(title = "Baseline Viral Load", x = "Week", y = "Viral Load") +
  theme_minimal()

ggplot(baseline_melt_ave, aes(x = week, y = svital, group = trt_order, color = trt_order)) +
  geom_line() +
  geom_point() +
  labs(title = "Baseline Viral Load", x = "Week", y = "Viral Load") +
  theme_minimal()



