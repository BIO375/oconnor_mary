### Exam 3 practice problem 
rm(list = ls())
library(ggfortify)
library(tidyverse)

peake <- read_csv("datasets/quinn/chpt5/peake.csv")

ggplot(data = peake)+
  geom_point(aes(x = AREA, y = SPECIES))
model01 <-lm(SPECIES ~ AREA, data = peake)
autoplot(model01)
ggplot(data = peake)+
  geom_point(aes(x = AREA, y = SPECIES))
peake <- peake %>%
  mutate(log10area = log10(AREA))
ggplot(data = peake)+
  geom_point(aes(x = log10area, y = SPECIES))

model02<-lm(SPECIES ~ log10area, data = peake)
autoplot(model02)
ggplot(data = peake)+
  geom_point(aes(x = log10area, y = resid(model02)))

summary(model02)

ggplot(data = peake, aes(x = log10area, y = SPECIES)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.95) +
  theme_bw()+
  labs( x = "log10(Clump Area)", y = "Number of Species")
