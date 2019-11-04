rm(list = ls())

getwd()

library("tidyverse")

tidyverse_update()

library("DescTools")

#### Problem 9 ####

feathers<- read_csv("datasets/exams/feathers.csv")

feathers <- mutate(feathers, diff = typical - odd)

ggplot(feathers) +
  geom_histogram(aes(diff), binwidth = 0.02)

ggplot(feathers) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(feathers)+
  geom_qq(aes(sample = diff))

# one-sided, paired t-test 

t.test(feathers$typical, feathers$odd, 
       alternative = "greater", paired =  TRUE, conf.level = 0.95)

# non-parametric sign test (for one-sided, paired t-test) just to see the value of p

SignTest(feathers$diff, alternative = "greater", mu = 0, conf.level = 0.95)

#### Problem 10 ####

streptococcus<- read_csv("datasets/exams/baker.csv")

streptococcus<- mutate(streptococcus, diff = After - Before)

ggplot(streptococcus) +
  geom_histogram(aes(diff), binwidth = 0.5)

ggplot(streptococcus) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(streptococcus)+
  geom_qq(aes(sample = diff))

# not normally distributed, cannot be transformed

# non-parametric sign test 

SignTest(streptococcus$diff, alternative = "greater", mu = 0, conf.level = 0.95)

#### Problem 11 ####

algae<- read_csv("datasets/demos/alga_growth.csv")

summ_growthrate <- algae %>%
  group_by(treatment) %>% 
  summarise(mean_growthrate = mean(growthrate),
            sd_growthrate = sd(growthrate),
            n_growthrate = n())

ratio <-(max(summ_growthrate$sd_growthrate))/(min(summ_growthrate$sd_growthrate))
# ratio < 3

ggplot(algae) +
  geom_histogram(aes(growthrate), binwidth = 0.25)+
  facet_wrap(~treatment)

ggplot(algae) +
  geom_boxplot(aes(x = treatment, y = growthrate))

ggplot(algae)+
  geom_qq(aes(sample = growthrate, color = treatment))

# normal distribution
#two-sample, two-sided t-test

t.test(growthrate ~ treatment, data = algae, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

#### CODE RAN WITHOUT BREAKING, 6/6 PTS, GOOD JOB ####