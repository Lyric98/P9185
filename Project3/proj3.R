library(survival)
library(ggsurvfit)
library(survminer)
library(flexsurv)
library(survMisc)

dat <- read.table("Menopause.dat")
colnames(dat) <- c("id", "intake_age", "menopause_age", "menopause", "race", "education")
dat$menopause_time <- dat$menopause_age-dat$intake_age



### 1a
fit_exp <- survreg(Surv(menopause_time, menopause)~1, data = dat, dist = "exponential")
summary(fit_exp)


### 1b
fit_KM <- survfit(Surv(menopause_time, menopause)~1, data = dat, conf.type = "log-log")
fit_summary <- summary(fit_KM)
quantile(fit_KM, probs = c(0.5), conf.int = TRUE)
#plot(fit, conf.int = T, mark = "+", xlab = "Time", ylab = "Survival probability")

survfit2(Surv(menopause_time, menopause)~1, data = dat, conf.type = "log-log") %>%
  ggsurvfit() +
  add_risktable() +
  add_confidence_interval() +
  add_quantile() +
  add_censor_mark()


### 2

fit_ph <- coxph(Surv(menopause_time, menopause)~as.factor(race)+as.factor(education)+intake_age, data = dat)
summary(fit_ph)

test_ph <- cox.zph(fit_ph)
ggcoxzph(test_ph)

### 3
#fit_exp_2 <- survreg(Surv(intake_age, menopause_age, menopause)~1, data = dat, dist = "exponential")
fit_exp_2 <- flexsurvreg(Surv(intake_age, menopause_age, menopause)~1, data = dat, dist = "exponential")
fit_exp_2$coefficients

fit_KM_2 <- survfit(Surv(intake_age,menopause_age, menopause)~1, data = dat, conf.type = "log-log")
fit_summary <- summary(fit_KM_2)
quantile(fit_KM_2, probs = c(0.5), conf.int = TRUE)
#plot(fit, conf.int = T, mark = "+", xlab = "Time", ylab = "Survival probability")

survfit2(Surv(intake_age, menopause_age, menopause)~1, data = dat, conf.type = "log-log") %>%
  ggsurvfit() +
  add_risktable() +
  add_confidence_interval() +
  add_quantile() +
  add_censor_mark()

### 4

# print(survdiff(Surv(intake_age, menopause_age, menopause)~race, data=dat), digits=5)
# 
# survfit2(Surv(intake_age, menopause_age, menopause)~race, data=dat) %>%
#   ggsurvfit() +
#   add_censor_mark() +
#   add_pvalue(location="annotation",
#              caption="Log-rank {p.value}")
# 
# ten(survfit(Surv(intake_age, menopause_age, menopause)~race,data=dat))

fit_race <- coxph(Surv(intake_age, menopause_age, menopause)~as.factor(race), data = dat)
summary(fit_race)

### 5

fit_5 <- coxph(Surv(intake_age, menopause_age, menopause)~as.factor(race)+as.factor(education), data = dat)
summary(fit_5)

### 5c
S0 <- survfit(fit_5, newdata = data.frame(race=0, education =0))
plot(S0$time, S0$surv)

### 5d
test_ph <- cox.zph(fit_5)
ggcoxzph(test_ph)
