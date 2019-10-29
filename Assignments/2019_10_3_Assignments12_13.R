library("tidyverse")
tidyverse_update()
install.packages ("tidyr")
gouldianfinch_diff<-read.csv("datasets/demos/gouldianfinch_diff.csv")
ggplot(gouldianfinch_diff) +
  geom_histogram(aes(diff), binwidth = 10)
