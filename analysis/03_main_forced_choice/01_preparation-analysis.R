library(lme4)
library(Hmisc)

dat <- data.frame(read.delim("data/03_main_forced_choice/data_raw.txt"))
dat <- subset(dat, answer!="NULL")
dat <- subset(dat, otherLang=="no")

# check sanity-check items
subset(dat, itemName=="correct1")
# suspicious answers from output_ids:  #16 and #18 and #30 and #74
subset(dat, itemName=="correct2")
# also suspicious:  #39 and #43 and #81
subset(dat, itemName=="correct3")
# also suspicious:  #30 and #43 and #51 and #74 and #92
subset(dat, itemName=="correct4")
# also suspicious:  #92 and #105

# remove people who didn't get all catch trials right
dat <- subset(dat, output_id!=16 & output_id!=18 & output_id!=30 & output_id!=39 & output_id!=43 & output_id!=51 & output_id!=74 & output_id!=81 & output_id!=92 & output_id!=105)
dat$output_id <- dat$output_id[,drop=T]

# remove people who didn't do all items
tapply(dat$output_id, list(dat$output_id), length)
dat <- subset(dat, output_id!=1 & output_id!=2 & output_id!=14 &
			output_id!=22 & output_id!=23 & output_id!=58)
dat$output_id <- dat$output_id[,drop=T]
tapply(dat$output_id, list(dat$output_id, dat$otherLang), length)

# how many participants
dat$output_id <- factor(dat$output_id)
length(levels(dat$output_id))

# what do their answers look like
tapply(dat$answer, list(dat$answer), length)
dat$answer <- dat$answer[,drop=T]
dat$isHigh <- ifelse(dat$answer=="high", 1, 0)

# look at target answers
dat <- subset(dat, expt=="inf")
dat$cond <- dat$cond[,drop=T]
dat$itemName <- dat$itemName[,drop=T]


# means overall and by-item
tapply(dat$isHigh, list(dat$cond), mean)
tapply(dat$isHigh, list(dat$itemName, dat$cond), mean)
length(levels(factor(dat$output_id)))

# LMER
m_saturated <- glmer(isHigh  ~ cond + (1 + cond | output_id) + (1+cond| itemName), 
                     family="binomial", 
                     data=dat, 
                     control = glmerControl(optimizer="bobyqa", 
                                            optCtrl = list(maxfun = 1e6)))
# m1 drops by-subject slope
m1 <- glmer(isHigh  ~ cond + (1 | output_id) + (1 + cond| itemName), 
           family="binomial", 
           data=dat, 
           control = glmerControl(optimizer="bobyqa", 
                                  optCtrl = list(maxfun = 1e6)))
# m2 drops by-item slope
m2 <- glmer(isHigh  ~ cond + (1 + cond | output_id) + (1 | itemName), 
           family="binomial", 
           data=dat, 
           control = glmerControl(optimizer="bobyqa", 
                                  optCtrl = list(maxfun = 1e6)))
# m3 drops random slopes altogether (intercept-only model)
m3 <- glmer(isHigh  ~ cond + (1 | output_id) + (1 | itemName), 
            family="binomial", 
            data=dat, 
            control = glmerControl(optimizer="bobyqa", 
                                   optCtrl = list(maxfun = 1e6)))
# m4 drops by-item intercept
m4 <- glmer(isHigh  ~ cond + (1 | output_id) , 
            family="binomial", 
            data=dat, 
            control = glmerControl(optimizer="bobyqa", 
                                   optCtrl = list(maxfun = 1e6)))
# m5 drops by-subject intercept
m5 <- glmer(isHigh  ~ cond + (1 | itemName) , 
            family="binomial", 
            data=dat, 
            control = glmerControl(optimizer="bobyqa", 
                                   optCtrl = list(maxfun = 1e6)))

# comparing nested models that remove random slopes
anova(m_saturated, m1, m3)
anova(m_saturated, m2, m3)
# so far, the best model is the intercept-only model

# comparing nested models that remove random slopes
anova(m3, m4)
anova(m3, m5)
# it seems the random intercepts are both necessary
# ==> 'best' model seems to be m3

summary(m3)

tapply(dat$isHigh, list(dat$cond), mean)


