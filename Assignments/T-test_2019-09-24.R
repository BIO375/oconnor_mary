library("tidyverse")
tidyverse_update()
install.packages ("tidyr")
birthrate<-read_csv("datasets/demos/birthrate_diff.csv")
ggplot(birthrate) +
  geom_histogram(aes(diff), binwidth = 1)
ggplot(birthrate)+
  geom_boxplot(aes(y=diff),notch=FALSE, varwidth = TRUE)
summ_diff<-birthrate%>%
  summarise(mean_diff=mean(diff),
            median_diff=median(diff),
            IQR_diff=IQR(diff),
            sd_diff=sd(diff),
            var_diff=var(diff))
data01<-read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")
data01<-data01%>%slice(-105)
ggplot(data01)+
  geom_histogram(aes(squamosalHornLength), binwidth= 5)+facet_wrap(~Survival)
ggplot(data01)+
  geom_boxplot(aes(x=Survival,y=squamosalHornLength),notch=FALSE, varwidth = TRUE)
names(data01)
summ_squamosalHornLength<-data01%>%
  group_by(Survival)%>%
  summarise(mean_squamosalHornLength=mean(squamosalHornLength),
            median_squamosalHornLength=median(squamosalHornLength),
            IQR_squamosalHornLength=IQR(squamosalHornLength),
            sd_squamosalHornLength=sd(squamosalHornLength),
            var_squamosalHornLength=var(squamosalHornLength))
p<- 2*(1-pt(q=abs(5.40),df=182))


#### 10/10 Code runs without breaking ####