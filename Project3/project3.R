library(survival)
library(ggsurvfit)
library(rstudioapi)
library(table1)
library(xtable)


## set working directory to wherever this file is located ##
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))


dat <- read.table("Menopause.dat")
colnames(dat) <- c("id", "intake_age", "menopause_age", "menopause", "race", "education")
dat$menopause_time <- dat$menopause_age-dat$intake_age
dat$menopause <- as.factor(dat$menopause)
dat$race <- as.factor(dat$race)
dat$education <- as.factor(dat$education)


#### Table 1 ####

names(dat)
#dat$gender <- as.factor(dat$gender)
table1<- table1(~ menopause_age + intake_age + menopause_time + race +education| menopause, data=dat)


xtable(as.data.frame(table1))


#### Kaplan-Meier ####



fit <- survfit(Surv(menopause_time, menopause)~1, data = dat, conf.type = "log-log")
fit_summary <- summary(fit)
quantile(fit, probs = c(0.5), conf.int = TRUE)
#plot(fit, conf.int = T, mark = "+", xlab = "Time", ylab = "Survival probability")

survfit2(Surv(menopause_time, menopause)~1, data = dat, conf.type = "log-log") %>%
  ggsurvfit() +
  add_risktable() +
  add_confidence_interval() +
  add_quantile() +
  add_censor_mark()
