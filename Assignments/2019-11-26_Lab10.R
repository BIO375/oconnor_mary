#### Lab 10 ####

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

install.packages("ggmosaic")
library("ggmosaic")

install.packages("epitools")
library("epitools")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

#### Binomial Test ####
#Test whether females are overrepresented in the Phratora laticollis 2 population in Bergen op Zoom

#Ha: The proportion of females represented in the population is greater than 0.5.
#Ho: The proportion of females represented in the population is less than/equal to 0.5.

49+41 # n = total number of trials
model01 <- binom.test(x= 41, n=90, p=0.5, alternative = "greater", conf.level = 0.95 )
model01

# expected frequencies of male and female under the null:45

# probablity of null being true: 0.4555556

# We conclude that females are not overrepresented in the Phratora laticollis 2 population in Bergen
# op Zoom (Binomial Test: p = 0.8286, n = 90).

#### Chi-square goodness of fit ####

# Ha: Progeny growth resulted in a 3:1 ratio of yellow-flowered to green-flowered phenotypes.
# Ho: Progeny growth did not result in a 3: 1 ratio of yellow-flowered to green-flowered phenotypes.

#Expected (yellow flowered) = 75
100*0.75
#Expected (green flowered) = 25
100*0.25

# Degrees of Freedom : number of categories - 1 = 1
2-1

flower <- read_csv("datasets/demos/flower.csv",
                   col_types = cols(color = col_factor()))

flower_summ <- flower %>%
  group_by(color)%>%
  summarise(color_n = n())

flower_summ <- add_column(flower_summ, expected= c(75,25)) %>%
  mutate(expected_p = expected/100)

# All the expected values are greater than 5 so we meet the assumptions of the 
#chi-sq goodness of fit test

model02 <-chisq.test(x = flower_summ$color_n, p = flower_summ$expected_p)
model02


# X2 = 4.32
# The observed frequencies deviate significantly from the expected frequencies.
# (X2 goodness of fit: X2 = 4.32, df = 1, p = 0.03767)

#### Contingency Table Analysis ####

# Creating a table of counts

# Ho: Sex ratio of Phratora lacticollis and locality are independent of each other.

tab01 <- matrix(c(17, 30, 49, 41), 2, 2, byrow=TRUE)
dimnames(tab01) <- list("Outcome" = c("Belgium", "Holland"),
                        "Treatment" = c("Male", "Female"))
as.matrix(tab01)
model03 <- chisq.test(tab01, correct = FALSE)
model03

# X2= 4.1299
# df = 1

# We can conclude that beetle sex ratio is dependent on locality (Contingency table analysis: 
# X2 = 4.1299, df = 1, p = 0.04213)

