### Exam 1, Fall 2019

# Clean up the working environment
rm(list = ls())

library(tidyverse)

# Read in polyploid Artemesia file
polyploid <- read_csv("datasets/demos/polyploid.csv", 
                      col_types = cols(X3 = col_skip(), X4 = col_skip(), 
                                       X5 = col_skip(), X6 = col_skip(), 
                                       X7 = col_skip()))

summ_polyploid <- polyploid %>%
  group_by(ploidy) %>% 
  summarise(n = n(),
            mean = mean(length),
            median = median(length),
            IQR = IQR(length),
            var = var(length),
            sd = sd(length),
            se = sd(length)/sqrt(length(length)))

# Plot histograms
ggplot(polyploid) +
  geom_histogram(aes(length), binwidth = 1)+
  facet_wrap(~ploidy)

# Plot box plots
ggplot(polyploid)+
  geom_boxplot(aes(x = ploidy, y = length), notch = FALSE, varwidth = TRUE)
# Plot diploid separately
diploid_only <- polyploid %>%
  filter(ploidy == "2N")
ggplot(diploid_only)+
  geom_boxplot(aes(x = "", y = length), notch = FALSE, varwidth = TRUE)

# Read in Horned Lizard file and get rid of NA on line 105
data01 <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")
data01 <- data01 %>%
  slice(-105)


# Summary statistics
summ_young <- data01 %>%
  group_by(Survival) %>% 
  summarise(n = n(),
            mean = mean(squamosalHornLength),
            median = median(squamosalHornLength),
            IQR = IQR(squamosalHornLength),
            var = var(squamosalHornLength),
            sd = sd(squamosalHornLength),
            se = sd(squamosalHornLength)/sqrt(length(squamosalHornLength)))

# Bar plot with SE 
ggplot(summ_young, aes (x = Survival, y = mean)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 0.2)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_grey() +
  labs( x = "Survival", y = "Horn Length (mm)") +
  theme_classic()

# Bar plot with SD
ggplot(summ_young, aes (x = Survival, y = mean)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd, width = 0.2)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_grey() +
  labs( x = "Survival", y = "Horn Length (mm)") +
  theme_classic()

# Sage cricket extra credit problem
data02 <- read_csv("datasets/abd/chapter13/chap13e5SagebrushCrickets.csv")
ggplot(data02) +
  geom_histogram(aes(timeToMating), binwidth = 10)+
  facet_wrap(~feedingStatus)

data02<- mutate(data02, log_time = log(timeToMating))
ggplot(data02) +
  geom_histogram(aes(log_time), binwidth = .5)+
  facet_wrap(~feedingStatus)
