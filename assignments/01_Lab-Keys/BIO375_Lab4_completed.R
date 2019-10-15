### "Lab" 4: t-test scenarios
# Students submit code in T-test_2019-09-26.R

# Clean up the working environment
rm(list = ls())

# Load tidyverse
library("tidyverse")

### Scenario 1 ##################
# To make sure everyone is working with the same original file, I am going to 
# create a datafile longhand
births <-read_csv("county, birth_1982, birth_2000
                  afgh, 48, 44
                  alge, 44, 37
                  arge, 24, 20
                  aust, 12, 12
                  bang, 49, 42
                  barb, 17, 18
                  belg, 12, 12
                  boli, 42, 35
                  braz, 31, 26
                  cana, 15, 14
                  chil, 24, 21
                  colo, 28, 27
                  cost, 31, 28
                  cuba, 16, 18
                  czec, 15, 14")

births <- births %>%
  mutate(diff = birth_2000 - birth_1982)

ggplot(births) +
  geom_histogram(aes(diff), binwidth = 1)
ggplot(births) +
  geom_boxplot(aes(x = "", y = diff))
ggplot(births)+
  geom_qq(aes(sample = diff))



### Scenario 2 ##################
data01 <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")
data01 <- data01 %>%
  slice(-105)

summ_lizard <- data01 %>%
  group_by(Survival) %>%
  summarise(mean_y = mean(squamosalHornLength),
            sd_y = sd(squamosalHornLength),
            se_y = sd_y/sqrt(n()),
            n_y = n())

ggplot(data01) +
  geom_histogram(aes(squamosalHornLength), binwidth = 1)+
  facet_wrap(~Survival)
ggplot(data01) +
  geom_boxplot(aes(x = Survival, y = squamosalHornLength))
ggplot(data01)+
  geom_qq(aes(sample = squamosalHornLength, color = Survival)) 
ratio <-(max(summ_lizard$sd_y))/(min(summ_lizard$sd_y))
