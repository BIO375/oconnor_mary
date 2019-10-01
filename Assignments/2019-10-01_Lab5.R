rm(list=ls())
getwd()
library("tidyverse")
tidyverse_update()
obliquity_measurement<-read.csv("datasets/demos/obliquity_measurement.csv")
t.test(obliquity_measurement$obliquity, 
       alternative = "two.sided", mu = 23.4722, conf.level = 0.95)
HeartAttack_short<-read_csv("datasets/demos/HeartAttack_short.csv",col_names = TRUE,
               col_types = cols(
                 group = col_character() )
)
ggplot(HeartAttack_short) +
  geom_histogram(aes(cholest), binwidth = 15)+
  facet_wrap(~group)
ggplot(HeartAttack_short) +
  geom_boxplot(aes(x = group, y = cholest))               
ggplot(HeartAttack_short)+
  geom_qq(aes(sample = cholest, color = group))
summ_cholest <- HeartAttack_short %>%
  group_by(group) %>% 
  summarise(mean_cholest = mean(cholest),
            sd_cholest = sd(cholest),
            n_cholest = n())
ratio <-(max(summ_cholest$sd_cholest))/(min(summ_cholest$sd_cholest))
t.test(cholest ~ group, data = HeartAttack_short, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)
fulmar_data<-read.csv("datasets/quinn/chpt3/furness.csv")
summ_METRATE <- fulmar_data %>%
  group_by(SEX) %>% 
  summarise(mean_METRATE = mean(METRATE),
            sd_METRATE = sd(METRATE),
            n_METRATE = n())
wilcox.test(METRATE ~ SEX, data = fulmar_data, alternative = "two.sided", conf.level = 0.95)
ratio <-(max(summ_METRATE$sd_METRATE))/(min(summ_METRATE$sd_METRATE))
web_data<-read.csv("datasets/quinn/chpt3/elgar.csv")
t.test(web_data$HORIZLIG, web_data$HORIZDIM,
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)
