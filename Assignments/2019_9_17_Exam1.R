library("tidyverse")
tidyverse_update()
install.packages ("tidyr")
brineshrimp<-read.csv("datasets/demos/brineshrimp.csv")
names(brineshrimp)
summ_length<-brineshrimp%>%
  group_by(shrimp)%>%
  summarise(mean_bodylength=mean(length),
            median_bodylength=median(length),
            IQR_bodylength=IQR(length),
            sd_bodylength=sd(length),
            var_bodylength=var(length))
ggplot(brineshrimp)+
  geom_boxplot(aes(x=shrimp,y=length),notch=FALSE, varwidth = TRUE)
data01<-read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")
data01<-data01%>%slice(-105)
names(data01)
data02<-read_csv("datasets/abd/chapter13/chap13e5SagebrushCrickets.csv")
names(data02)
ggplot(data02)+
  geom_histogram(aes(timeToMating), binwidth= 5)+ facet_wrap(~"starved")
# MARY: the correct argument for facet_wrap is the categorical variable, feedingStatus, not the specific
# treatment.  Currently your code shows all values of timeToMating in every histogram, lumping
# observations of both fed and starved together.
ggplot(data02)+
  geom_histogram(aes(timeToMating), binwidth= 5)+ facet_wrap(~"fed")
ggplot(data02)+
  geom_histogram(aes(log(timeToMating+1)), binwidth= 0.5)+ facet_wrap(~"starved")
ggplot(data02)+
  geom_histogram(aes(log(timeToMating+1)), binwidth= 0.5)+ facet_wrap(~"fed")
