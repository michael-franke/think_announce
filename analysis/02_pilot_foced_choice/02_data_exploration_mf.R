library(tidyverse)
library(brms)
library(HDInterval)

##########################
## helper function(s)
##########################

hdi_vec = function(alpha, beta) {
  hdi(qbeta, shape1 = alpha, shape2 = beta)
}

##########################
## read data
##########################

dat <- read_delim("data/02_pilot_forced_choice/data_raw.txt", delim = "\t")

##########################
## clean data
##########################

# drop by language
dat <- subset(dat, otherLang=="no")

# check sanity-check items
subset(dat, itemName=="correct1")
# suspicious ids:  #16 and #18 and #30
subset(dat, itemName=="correct2")
# #39
subset(dat, itemName=="correct3")
# #30
subset(dat, itemName=="correct4")
# remove people who didn't get all catch trials right
dat <- subset(dat, output_id!=16 & output_id!=18 & output_id!=30 & output_id!=39)
dat$output_id <- dat$output_id[,drop=T]

# remove people who didn't do all items
tapply(dat$output_id, list(dat$output_id), length)
dat <- subset(dat, output_id!=1 & output_id!=2 & output_id!=3 & output_id!=14 &
			output_id!=22 & output_id!=23)
dat$output_id <- dat$output_id[,drop=T]
tapply(dat$output_id, list(dat$output_id), length)

#############################
## prepare data for analysis
## + desc. summary statistics
#############################

# how many participants
dat$output_id <- factor(dat$output_id)
length(levels(dat$output_id))

# add 0/1 column for response variable
dat = dat %>% filter(expt == "inf") %>% 
  mutate(isHigh = ifelse(answer == "high", 1, 0))

# mean judgements by condition (1 = choose higher number)
dat_summary = dat %>% group_by(cond) %>% 
  summarise(mean_rating = mean(isHigh),
            lower = hdi_vec(sum(isHigh)+1, n()-sum(isHigh)+1)[1],
            upper = hdi_vec(sum(isHigh)+1, n()-sum(isHigh)+1)[2])

dat_summary %>% ggplot(aes(x = cond, y = mean_rating, fill = cond)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper), size =0.5, width = 0.2) +
  xlab("condition") + ylab("proportion of choice of higher number") + 
  scale_fill_brewer(palette="Accent")
  
######### by participant 

# means by participant
means_by_participant = dat %>% group_by(cond, output_id) %>% summarise(mean_rating = mean(isHigh)) %>% 
  spread(key = cond, value = mean_rating) %>% 
  mutate(higher_in_announce = announce > think)
means_by_participant

# proportion of participants who selected higher value more often in "announce"
means_by_participant %>% group_by(higher_in_announce) %>% 
  summarise(count = n())
mean(means_by_participant$higher_in_announce)

# by participant plot
gather(means_by_participant, condition, proportion, announce, think) %>% 
  ggplot(aes(x = output_id, y = proportion, fill = condition)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() +
  scale_fill_brewer(palette="Accent") +
  xlab("participant") + ylab("proportion of choice of higher number")

######### by item

# means by item
means_by_item = dat %>% group_by(cond, itemName) %>% 
  summarise(mean_rating = mean(isHigh)) %>% 
  spread(key = cond, value = mean_rating) %>% 
  mutate(higher_in_announce = announce > think)
means_by_item

# proportion of items with higher rate of "high" choice in announce 
means_by_item %>% group_by(higher_in_announce) %>% 
  summarise(count = n())
mean(means_by_item$higher_in_announce)

# by item plot
gather(means_by_item, condition, proportion, announce, think) %>% 
  ggplot(aes(x = itemName, y = proportion, fill = condition)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() +
  scale_fill_brewer(palette="Accent") +
  xlab("item") + ylab("proportion of choice of higher number")

#############################
## logistic mixed effects mod
#############################

m = brm(isHigh ~ cond + (1 + cond | output_id) + (1+cond| itemName), family = "bernoulli", data = dat)
summary(m)
paste0("Our posterior belief, given data and model, that the proportion of higher-number choices is higher in the ANNOUNCE condition is: ",
       mean(posterior_samples(m)[["b_condthink"]] < 0))


