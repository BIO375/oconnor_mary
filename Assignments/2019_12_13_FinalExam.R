#### Final Exam Code 12-13-19 ####

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

install.packages("DescTools")
library("DescTools")
install.packages("ggfortify")
library("ggfortify")
install.packages("ggmosaic")
library("ggmosaic")
install.packages("epitools")
library("epitools")
install.packages("tibble")
library("tibble")
install.packages("multcomp")
library("multcomp")
install.packages("nlme")
library("nlme")
install.packages("broom")
library("broom")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

#### Question 1 ####
insulation<- read_csv("datasets/final/insulation.csv")

# The assumptions for a linear regression are difficult to test directly,
# so we mostly diagnose departures using residuals plots.  

#In order to plot residuals, we first need to fit a model. y~x

model01 <- lm(heat_loss ~ leanness, data = insulation)

# Use autoplot to give a residual by predicted plot in the upper left panel

autoplot(model01, smooth.colour = NA)

# Create a residual by x plot

# I am using the augment function

insulin_plus <- augment(model01)
ggplot(data = insulin_plus)+
  geom_point(aes(x = leanness, y= .resid))

# The normal Q-Q of the residuals looks like the residuals are normal (not much deviation), 
# which indicates that the data meets the assumptions of y being normally distributed for 
# each value of x.

# The residuals vs. fitted plot (also known as the residuals vs. predicted) indicates no fan 
# shape (actually there is no shape at all). This suggests that the data meets the 2nd 
# assumption of homogeneity of variance.

# Proceed with linear regression

# statistical results

summary(model01)
# p-value of interest is found in the row "leanness", the intercept
# and slope are found under the column header "Estimate".

# A higher amount of leaness yields a larger amount of heat loss.
#(Linear regression: heat_loss = -0.010017 + 0.018970(leanness);
# df = 1, 10, F=68.73, P<0.0001)
#leanness explained more than 87% of the variability in heat_loss (R2 = 0.873).

# Adding a regression line to our plot to give an idea about how confident we
# are in our estimate of that regression line
# Generate confidence bands
# The narrower the band, the more confident we are in our estimate of the line.

# Create confidence bands by adding in a layer : geom_smooth(method = "lm", level=0.95)

ggplot(data = insulation, aes(x = leanness, y = heat_loss)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.99) +
  theme_bw()+
  labs( x = "leanness", y = "heat_yield")


# All but 2 data points (and they are relatively close) fall within or nearly withing the added 
#layer, therefore we are confident in the estimate of our line (~99%). 
#This confirms that the data meets the fourth assumption:
#there is a linear relationship between the predictor (x) and response (y) variables.

#### Question 2 ####

caffeine<- read_csv("datasets/final/caffeine.csv",col_types = cols(
  group = col_factor() ))

# The research question here is two-fold: Q1: mean caffiene metab diff between men and women
#without elevated prog? Q2: mean caffeine metab diff between women w/o elev prog and w/ elev prog

# Look at the data
head(caffeine)
summary(caffeine)

# graphs for assumptions
ggplot(caffeine, aes(x = group, y = half_life))+
  geom_boxplot() +
  theme_bw() 
ggplot(caffeine) +
  geom_histogram(aes(half_life), binwidth = 1)+
  facet_wrap(~group)
ggplot(caffeine)+
  geom_qq(aes(sample = half_life, color = group))

summ_half_life <- caffeine %>%
  group_by(group) %>% 
  summarise(mean_half_life = mean(half_life),
            sd_half_life = sd(half_life),
            n_half_life = n())
ratio <-(max(summ_half_life$sd_half_life))/(min(summ_half_life$sd_half_life))

# constructing anova

model02 <- lm(half_life~group, data = caffeine)

autoplot(model02)

anova(model02)

summary(model02)

planned <- glht(model02, linfct = 
                  mcp(group = c("male - norm_prog = 0",
                                   "norm_prog - high_prog = 0")))
confint(planned)
summary(planned)

#### Question 3 ####

davis<- read_csv("datasets/final/davis.csv",
                 col_types = cols(race_ethn = col_factor()))

davis_summ <- davis %>%
  group_by(race_ethn)%>%
  summarise(race_ethn_n = n())

#Expected values
#aa
1096*0.030
#aian
231*0.018
#api
9967*0.156
#hl
6740*0.319
#w
7415*0.423
#tmu
457*0.054


model03 <-chisq.test(x = davis$observed, p = davis$expected_p)
model03

# assumptions 
# all of the cells have at least one observed 
# the only expected value that is less than 5 is for Am. Indian/ Alaska Native,
#since this is less than 20% of the cells we can procede with X2 goodness of fit.

# X2 = 
# The observed frequencies deviate significantly from the expected frequencies.
# (X2 goodness of fit: X2 = 10,999, df = 5, p = 2.2e-16)


